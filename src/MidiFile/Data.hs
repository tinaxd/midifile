module MidiFile.Data (
    Chunk(..),
    HeaderInfo(..),
    SMFFormat(..),
    Track(..),
    TimeEvent,
    Event(..),
    MidiEvent(..),
    SysExEvent(..),
    MetaEvent(..)
) where

import Data.Word
import Data.Int


data Chunk = MThd HeaderInfo
           | MTrk [Track]
           deriving (Show, Eq)

data HeaderInfo = HeaderInfo {
    headerSMFFormat :: SMFFormat,
    headerTrackCount :: Int,
    headerPPQ :: Int
} deriving (Show, Eq)

data SMFFormat = Format0
               | Format1
               | Format2
               deriving (Show, Eq, Enum)

data Track = Track [TimeEvent]
             deriving (Show, Eq)

type TimeEvent = (Int, Event)

data Event = MidiE MidiEvent
           | SysExE SysExEvent
           | MetaE MetaEvent
           deriving (Show, Eq)

data MidiEvent = NoteOn Word8 Word8 Word8
               | NoteOff Word8 Word8 Word8
               | PolyphonicKeyPressure Word8 Word8 Word8
               | ControlChange Word8 Word8 Word8
               | ProgramChange Word8 Word8
               | ChannelPressure Word8 Word8
               | PitchBendChange Word8 Int16
               deriving (Show, Eq)

data SysExEvent = ExclusiveF0 [Word8]
                | ExclusiveF7 [Word8]
                deriving (Show, Eq)

data MetaEvent = SequenceNumber Word8
               | TextEvent String
               | CopyrightNotice String
               | SequenceTrackName String
               | InstrumentName String
               | Lyric String
               | Marker String
               | CuePoint String
               | EndOfTrack
               | SetTempo Int
               | SMPTEOffset Word8 Word8 Word8 Word8 Word8
               | TimeSignature Word8 Word8 Word8 Word8
               | KeySignature Word8 Word8
               | SequencerSpecific [Word8]
               deriving (Show, Eq)
