{-# LANGUAGE DataKinds, KindSignatures #-}
module Cut.Analyze
  ( detect
  , Interval(..)
  , Sound
  , Silent
  )
where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Cut.Ffmpeg
import           Cut.Options
import           Data.Foldable
import           Data.Maybe
import           Data.List                      ( partition )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Text.Lens
import           Shelly                  hiding ( find )
import           Text.Regex.TDFA         hiding ( empty )
import qualified System.Log.Heavy.Short        as Log

data IntervalType = Silent | Sound

data Interval (t :: IntervalType) = Interval
  { interval_start       :: Double
  , interval_end         :: Double
  , interval_duration    :: Double
  , interval_input_start :: Text
  , interval_input_end   :: Text
  } deriving Show

detect :: (MonadMask m, MonadUnliftIO m) => Options -> m [Interval Sound]
detect opts = do
  actualLines <- shelly $ detectShell opts
  let (silentLines, noisyLines) = partition isLineSilent actualLines
      chunkedLines              = chunkPair noisyLines
      intervals                 = parseInterval <$> chunkedLines
      logMessage = Text.unlines 
        [ "Actual lines:"
        , Text.unlines actualLines
        , "Silent lines:"
        , Text.unlines silentLines
        , "Lined up:"
        , Text.unlines $ chunkedLines ^.. mapped . to show . packed
        , "Intervals:"
        , Text.unlines $ intervals ^.. mapped . to show . packed
        ]
      fancyResult    = detectSound opts intervals
      negativeResult = find ((0 >) . interval_duration) fancyResult

  if isJust negativeResult
    then do
      liftIO $ traverse_ print fancyResult
      liftIO $ print negativeResult
      error "Found negative durations"
    else pure fancyResult

isLineSilent :: Text -> Bool
isLineSilent text = text ^. unpacked =~ ".*silencedetect.*"

chunkPair :: [a] -> [(a, a)]
chunkPair (x : y : zs) = (x, y) : chunkPair zs
chunkPair _            = []

detectSound :: Options -> [Interval Silent] -> [Interval Sound]
detectSound opts =
  --  -- TODO figure out why these durations get recorded as < 0
  reverse . snd . foldl' (flip (compare' opts)) (Interval 0 0 0 "" "", [])

compare'
  :: Options
  -> Interval Silent
  -> (Interval Silent, [Interval Sound])
  -> (Interval Silent, [Interval Sound])
compare' opts current prev = (current, soundedInterval : snd prev)
 where
  soundedInterval = Interval
    { interval_start       = interval_end $ fst prev
    , interval_end         = interval_start current - margin
    , interval_duration    = (soundEnd - soundStart) + margin
    , interval_input_start =
      interval_input_start (fst prev) <> "," <> interval_input_end (fst prev)
    , interval_input_end   = interval_input_start current
                             <> ","
                             <> interval_input_end current
    }
  soundEnd   = interval_start current
  soundStart = interval_end $ fst prev

  margin     = opts ^. detect_margin


detectShell :: Options -> Sh [Text]
detectShell opt' = ffmpeg
  [ "-i"
  , opt' ^. in_file . packed
  , "-map"
  , "0:" <> opt' ^. voice_track . to show . packed
  , "-filter:a"
                 -- , "silencedetect=noise=-30dB:d=0.5"
  , "silencedetect=noise="
  <> (opt' ^. silent_treshold . to floatToText)
  <> ":d="
  <> (opt' ^. silent_duration . to floatToText)
  , "-f"
  , "null"
  , "-"
  ]

parseInterval :: (Text, Text) -> Maybe (Interval Silent)
parseInterval (l1, l2) = Interval <$> getStart l1 <*> getEnd l2 <*> getDuration l2 <*> pure l2 <*> pure l2

getStart :: Text -> Maybe Double
getStart line = readMaybe $ takeWhile (/= '\'') $ matches ^. _3
 where
  str = Text.unpack line
  matches :: (String, String, String)
  matches = str =~ startMatch

startMatch :: String
startMatch = "(.*)?: "

pipe :: String
pipe = " \\| "

getDuration :: Text -> Maybe Double
getDuration line = readMaybe =<< takeWhile (/= '\'') $ match2 ^. _1
 where
  str = Text.unpack line
  match1 :: (String, String, String)
  match1 = str =~ startMatch
  match2 :: (String, String, String)
  match2 = (match1 ^. _3) =~ pipe

getEnd :: Text -> Double
getEnd line = readMaybe =<< match2 ^. _3
 where
  str = Text.unpack line
  match1 :: (String, String, String)
  match1 = str =~ pipe
  match2 :: (String, String, String)
  match2 = (match1 ^. _1) =~ startMatch
