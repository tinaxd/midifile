{-# LANGUAGE FlexibleInstances #-}

module MidiFile.Writer (
    SMFWrite(write, write'),
    NoRunningStatus(..)
) where

import MidiFile.Data
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as B


class SMFWrite m where
    write' :: m -> B.Builder
    write :: m -> BS.ByteString
    write = B.toLazyByteString . write'

wconcat = mconcat . (fmap B.word8)
bwconcat = B.toLazyByteString . wconcat
finish = B.toLazyByteString

instance SMFWrite MidiEvent where
    write' (NoteOff ch k v) = wconcat [0x80 + ch, k, v]
    write' (NoteOn ch k v) = wconcat [0x90 + ch, k, v]
    write' (PolyphonicKeyPressure ch k v) = wconcat [0xa0 + ch, k, v]
    write' (ControlChange ch c v) = wconcat [0xb0 + ch, c, v]
    write' (ProgramChange ch p) = wconcat [0xc0 + ch, p]
    write' (ChannelPressure ch v) = wconcat [0xd0 + ch, v]
    write' (PitchBendChange ch vv) = B.word8 ch <> B.int16LE vv

instance SMFWrite SysExEvent where
    write' (ExclusiveF0 d) = B.word8 0xf0 <> mconcat (fmap B.word8 d)
    write' (ExclusiveF7 d) = mconcat (fmap B.word8 d)

metaTextEncode n t = metaStatus' n <> writeVLQ (length t) <> B.string7 t
metaStatus = B.word8 0xff
metaStatus' n = metaStatus <> B.word8 n

writeVLQ n = undefined

instance SMFWrite MetaEvent where
    write' (SequenceNumber a) = metaStatus' 0x00 <> B.word8 0x02 <> B.word16BE a
    write' (TextEvent t) = metaTextEncode 0x01 t
    write' (CopyrightNotice t) = metaTextEncode 0x02 t
    write' (SequenceTrackName t) = metaTextEncode 0x03 t
    write' (InstrumentName t) = metaTextEncode 0x04 t
    write' (Lyric t) = metaTextEncode 0x05 t
    write' (Marker t) = metaTextEncode 0x06 t
    write' (CuePoint t) = metaTextEncode 0x07 t
    write' EndOfTrack = wconcat [0xff, 0x2f, 0x00]
    write' (SetTempo t) = undefined

instance SMFWrite Event where
    write' (MidiE e) = write' e
    write' (SysExE e) = write' e
    write' (MetaE e) = write' e

instance SMFWrite TimeEvent where
    write' (delta, ev) = writeVLQ delta <> write' ev

newtype NoRunningStatus a = NoRunningStatus a
newtype WithRunningStatus a = WithRunningStatus a

instance (Foldable t) => SMFWrite (NoRunningStatus (t TimeEvent)) where
    write' (NoRunningStatus (evs)) = foldMap write' evs

instance SMFWrite (NoRunningStatus Track) where
    write' (NoRunningStatus (Track evs)) = wconcat [0x4d, 0x54, 0x72, 0x6b] <> len <> code
        where
            code = write' (NoRunningStatus evs)
            len = writeVLQ $ BS.length $ B.toLazyByteString code

instance SMFWrite SMFFormat where
    write' Format0 = B.word16BE 0
    write' Format1 = B.word16BE 1
    write' Format2 = B.word16BE 2

instance SMFWrite HeaderInfo where
    write' (HeaderInfo f t p) = wconcat [0x4d, 0x56, 0x68, 0x64] <> write' f <> B.word16BE t <> B.word16BE p

instance SMFWrite (NoRunningStatus Chunk) where
    write' (NoRunningStatus (MThd h)) = write' h
    write' (NoRunningStatus (MTrk t)) = write' (NoRunningStatus t)

instance (Foldable t) => SMFWrite (NoRunningStatus (t Chunk)) where
    write' (NoRunningStatus cs) = foldMap (write' . NoRunningStatus) cs

instance SMFWrite (NoRunningStatus SMF) where
    write' (NoRunningStatus cs) = write' (NoRunningStatus (getAllChunks cs))
