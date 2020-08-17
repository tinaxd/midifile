module MidiFile.Reader where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import MidiFile.Data
import Data.Word
import Control.Applicative


type SMFReader' = StateT ReaderState (Reader BS.ByteString)

type SMFReader a = MaybeT SMFReader' a

liftMaybe m = case m of
                Just a -> return a
                Nothing -> empty

data ReaderState = ReaderState {
    pointer :: Int
} deriving (Show, Eq)

moveReaderState n s = s {pointer = (pointer s) + n}

subString a n s = BS.take n $ BS.drop a s 
subInt a n s = BS.unpack $ subString a n s

toFormat 0 = Just Format0
toFormat 1 = Just Format1
toFormat 2 = Just Format2
toFormat _ = Nothing

readNextBytes :: Int -> SMFReader [Word8]
readNextBytes n = do
    raw <- ask
    ptr <- gets pointer
    let sub = subInt ptr n raw
    modify $ moveReaderState n
    guard $ length sub == n
    return sub

toInt [] = 0
toInt ls = (fromIntegral l) + 256 * (toInt f)
    where
        l = last ls
        f = init ls

readNextByte = head <$> readNextBytes 1
readNextWord = toInt <$> readNextBytes 2

readVLQ :: SMFReader Int
readVLQ = do
    first <- readNextByte
    if first < 128
        then return $ fromIntegral first
        else do rest <- readVLQ
                return $ (fromIntegral first) * 256 + rest

readMThdWithMarker :: SMFReader Chunk
readMThdWithMarker = do
    size <- toInt <$> readNextBytes 4
    guard $ size == 6
    format <-join $ (liftMaybe . toFormat) <$> readNextWord
    tracks <- readNextWord
    ppq <- readNextWord
    return $ MThd $ HeaderInfo format tracks ppq

data MidiEventType = NoteOnTy | NoteOffTy | PolyphonicTy | CCTy | ChPressureTy | PitchBendTy | PCTy
                     deriving (Show, Eq, Enum)

getMidiEventType i = if i >= 0x80
                      then make NoteOffTy 0x80
                      else if i >= 0x90
                       then make NoteOnTy 0x90
                       else if i>= 0xa0
                        then make PolyphonicTy 0xa0
                        else if i >= 0xb0
                         then make CCTy 0xb0
                         else if i >= 0xc0
                          then make PCTy 0xc0
                          else if i >= 0xd0
                           then make ChPressureTy 0xd0
                           else if i >= 0xe0
                            then make PitchBendTy 0xe0
                            else Nothing
    where
        channel b = i - b
        make ty b = Just (ty, (channel b))

readMidiEvent :: SMFReader MidiEvent
readMidiEvent = do
    (ty, ch) <- (liftMaybe . getMidiEventType) =<< readNextByte
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

readSysEx :: SMFReader SysExEvent
readSysEx = do
    sysExType <- readNextByte
    case sysExType of
        0xf0 -> do length <- readVLQ
                   sysExData <- readNextBytes length
                   guard $ last sysExData == 0xf7
                   return $ ExclusiveF0 sysExData
        0xf7 -> do length <- readVLQ
                   sysExData <- readNextBytes length
                   return $ ExclusiveF7 sysExData
        _ -> empty
