module Main where

import MidiFile.Data
import MidiFile.Writer
import MidiFile.Reader
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    dat <- parseSMFFile "MIDI_sample.mid"
    case dat of
        Nothing -> putStrLn "could not read smf"
        Just dat' -> do let wdat = write (NoRunningStatus dat')
                        BS.writeFile "written.mid" wdat
