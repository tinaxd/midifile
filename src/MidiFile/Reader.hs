module MidiFile.Reader (
    parseSMFBytes,
    parseSMFFile
) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import MidiFile.Data
import Data.Word
import Control.Applicative


type SMFReader' = State ReaderState

type SMFReader a = MaybeT SMFReader' a

liftMaybe m = case m of
                Just a -> return a
                Nothing -> empty

data ReaderState = ReaderState {
    raw :: BS.ByteString,
    pointer :: Int,
    runningStatus :: Maybe Word8
} deriving (Show, Eq)

initReaderState bs = ReaderState bs 0 Nothing

isEOF :: SMFReader Bool
isEOF = do
    bs <- gets raw
    ptr <- gets pointer
    return $ ptr == BS.length bs

moveReaderState n s = s {pointer = (pointer s) + n}
updateRunningStatus r s = s {runningStatus = Just r}
clearRunningStatus s = s {runningStatus = Nothing}

subString a n s = BS.take n $ BS.drop a s 
subInt a n s = BS.unpack $ subString a n s

toFormat 0 = Just Format0
toFormat 1 = Just Format1
toFormat 2 = Just Format2
toFormat _ = Nothing

readNextBytes' :: Int -> Bool -> SMFReader [Word8]
readNextBytes' n move = do
    raw <- gets raw
    ptr <- gets pointer
    let sub = subInt ptr n raw
    when move $ modify $ moveReaderState n
    guard $ length sub == n
    return sub

readNextBytes n = readNextBytes' n True
peekNextBytes n = readNextBytes' n False

limited size m = do
    s <- get
    r <- mapMaybeT (\mm -> withStateT limitData mm) m
    put s
    return r
    where
        limitData rs = rs {pointer = 0, raw = subString (pointer rs) size (raw rs)}

toInt [] = 0
toInt ls = (fromIntegral l) + 256 * (toInt f)
    where
        l = last ls
        f = init ls

toString :: [Word8] -> String
toString = undefined

readNextByte = head <$> readNextBytes 1
readNextWord = toInt <$> readNextBytes 2


peekNextByte = head <$> peekNextBytes 1
peekNextWord = toInt <$> peekNextBytes 2

readVLQ :: SMFReader Int
readVLQ = do
    first <- readNextByte
    if first < 128
        then return $ fromIntegral first
        else do rest <- readVLQ
                return $ (fromIntegral first) * 256 + rest

readMThdWithMarker = do
    magic <- readNextBytes 4
    guard $ magic == [0x4d, 0x54, 0x68, 0x64]
    size <- toInt <$> readNextBytes 4
    guard $ size == 6
    format <-join $ (liftMaybe . toFormat) <$> readNextWord
    tracks <- fromIntegral <$> readNextWord
    ppq <- fromIntegral <$> readNextWord
    return $ MThd $ HeaderInfo format tracks ppq

readTrackWithMarker = do
    magic <- readNextBytes 4
    guard $ magic == [0x4d, 0x54, 0x72, 0x6b]
    size <- toInt <$> readNextBytes 4
    dat <- limited size readTimeEvents
    modify $ moveReaderState size
    return $ Track dat

readMTrksWithMarker = do
    whileM (not <$> isEOF) readTrackWithMarker

readSMF = do
    mthd <- readMThdWithMarker
    mtrks <- readMTrksWithMarker
    return $ makeSMF $ mthd : (map MTrk mtrks)

data MidiEventType = NoteOnTy | NoteOffTy | PolyphonicTy | CCTy | ChPressureTy | PitchBendTy | PCTy
                     deriving (Show, Eq, Enum)

getMidiEventType = do
    i <- peekNextByte
    check i
    where
        channel b i = i - b
        make :: MidiEventType -> Word8 -> Word8 -> Bool -> SMFReader (MidiEventType, Word8)
        make ty ch i move = do modify $ updateRunningStatus i
                               when move $ modify $ moveReaderState 1
                               return $ (ty, ch)
        make' ty b i = MidiEventExact (ty, (channel b i), i)
        checkRunningStatus = do rs <- gets runningStatus
                                case rs of
                                    Nothing -> empty
                                    Just rs' -> case (check' rs') of
                                                    MidiEventExact (ty, ch, i) -> make ty ch i False
                                                    UseRunningStatus -> empty
                                                    MidiEventFail -> empty
        check' i =
            if i >= 0xf0
             then MidiEventFail
             else if i >= 0xe0
              then make' PitchBendTy 0xe0 i
              else if i >= 0xd0
               then make' ChPressureTy 0xd0 i
               else if i >= 0xc0
                then make' PCTy 0xc0 i
                else if i >= 0xb0
                 then make' CCTy 0xb0 i
                 else if i >= 0xa0
                  then make' PolyphonicTy 0xa0 i
                  else if i >= 0x90
                   then make' NoteOnTy 0x90 i
                   else if i >= 0x80
                    then make' NoteOffTy 0x80 i
                    else UseRunningStatus
        check i = case check' i of
                    MidiEventExact (a, b, c) -> make a b c True
                    UseRunningStatus -> checkRunningStatus
                    MidiEventFail -> empty
                                
data MidiEventParseResut = MidiEventExact (MidiEventType, Word8, Word8)
                         | UseRunningStatus
                         | MidiEventFail
                         deriving (Show, Eq)

readMidiEvent :: (MidiEventType, Word8) -> SMFReader MidiEvent
readMidiEvent (ty, ch) = do
    let read1 m = liftM (m ch) readNextByte
        read2 m = liftM2 (m ch) readNextByte readNextByte
        readPB = liftM (PitchBendChange ch) (fromIntegral <$> readNextWord)
    case ty of
        NoteOffTy -> read2 NoteOff
        NoteOnTy -> read2 NoteOn
        PolyphonicTy -> read2 PolyphonicKeyPressure
        CCTy -> read2 ControlChange
        PCTy -> read1 ProgramChange
        ChPressureTy -> read1 ChannelPressure
        PitchBendTy -> readPB

data SysExType = F0Ty | F7Ty deriving (Show, Eq, Enum)

getSysExType = do
    ty <- readNextByte
    case ty of
        0xf0 -> return F0Ty
        0xf7 -> return F7Ty
        _ -> empty

readSysEx :: SysExType -> SMFReader SysExEvent
readSysEx ty = do
    modify $ clearRunningStatus
    case ty of
        F0Ty -> do length <- readVLQ
                   sysExData <- readNextBytes length
                   guard $ last sysExData == 0xf7
                   return $ ExclusiveF0 sysExData
        F7Ty -> do length <- readVLQ
                   sysExData <- readNextBytes length
                   return $ ExclusiveF7 sysExData

readMetaEvent :: SMFReader MetaEvent
readMetaEvent = do
    ff <- readNextByte
    guard $ ff == 0xff
    modify $ clearRunningStatus
    ty <- readNextByte
    case ty of
        0x00 -> do checkLength 2
                   liftM SequenceNumber readNextWord
        x | 0x01 <= x && x <= 0x09 -> readText x
        0x2f -> endOfTrack
        0x51 -> setTempo
        0x54 -> smpte
        0x58 -> timeSignature
        0x59 -> keySignature
        0x7f -> sequencerSpecific
        _ -> empty
    where
        checkLength n = readVLQ >>= \l -> guard $ l == n
        toMeta x = case x of
                     0x01 -> Just TextEvent
                     0x02 -> Just CopyrightNotice
                     0x03 -> Just SequenceTrackName
                     0x04 -> Just InstrumentName
                     0x05 -> Just Lyric
                     0x06 -> Just Marker
                     0x07 -> Just CuePoint
                     _ -> Nothing
        readText x = f =<< toString <$> (readVLQ >>= readNextBytes)
            where
                f s = case toMeta x of
                       Just c -> return (c s)
                       Nothing -> empty
        endOfTrack = checkLength 0 >> return EndOfTrack
        setTempo = do checkLength 3
                      (SetTempo . toInt) <$> readNextBytes 3
        smpte = do checkLength 5
                   liftM5 SMPTEOffset readNextByte readNextByte readNextByte readNextByte readNextByte
        timeSignature = do checkLength 4
                           liftM4 TimeSignature readNextByte readNextByte readNextByte readNextByte
        keySignature = do checkLength 2
                          liftM2 KeySignature readNextByte readNextByte
        sequencerSpecific = SequencerSpecific <$> (readVLQ >>= readNextBytes)

try :: SMFReader a -> SMFReader a
try m = do
    a <- get
    MaybeT $ do
        k <- runMaybeT m
        case k of
            Nothing -> put a >> return Nothing
            Just _ -> return k

readEvent :: SMFReader Event
readEvent = try (getMidiEventType >>= readMidiEvent >>= (return . MidiE))
            <|> try (readMetaEvent >>= (return . MetaE))
            <|> (getSysExType >>= readSysEx >>= (return . SysExE))

readDelta :: SMFReader Int
readDelta = readVLQ

readTimeEvent :: SMFReader TimeEvent
readTimeEvent = do
    delta <- readDelta
    event <- readEvent
    return (delta, event)

whileM cond a = do
    cond' <- cond
    if cond'
        then do n <- a
                ns <- whileM cond a
                return $ n : ns
        else return []

readTimeEvents :: SMFReader [TimeEvent]
readTimeEvents = whileM (not <$> isEOF) readTimeEvent

-- | Parse a ByteString as SMF data.
parseSMFBytes bs = evalState (runMaybeT readSMF) (initReaderState bs)

-- | Load and parse a SMF file.
parseSMFFile p = do
    bs <- BS.readFile p
    return $ evalState (runMaybeT readSMF) (initReaderState bs)

parseTest m bs = runState (runMaybeT m) (initReaderState bs)

testMThd1 = BS.pack [
        0x4d, 0x54, 0x68, 0x64, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x02, 0x00, 0x30
    ]

testMTrk1 = BS.pack [
        0x4d, 0x54, 0x72, 0x6b, 0x00, 0x00, 0x00, 0x0b, 0x00, 0xff, 0x51, 0x03, 0x07, 0xa1, 0x20, 0x00, 0xff, 0x2f, 0x00
    ]

testMTrk2 = BS.pack [
        0x4d, 0x54, 0x72, 0x6b, 0x00, 0x00, 0x00, 0x18, 0x00, 0x90, 0x3c, 0x7f, 0x30, 0x3c, 0x00, 0x00, 0x3e, 0x7f, 0x30, 0x3e, 0x00, 0x00, 0x40, 0x7f, 0x81, 0x49, 0x40, 0x00, 0x00, 0xff, 0x2f, 0x00
    ]

testData1 = BS.concat [testMThd1, testMTrk1, testMTrk2]
