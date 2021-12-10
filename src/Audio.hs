module Audio where

import           Data.Map                      as M
import           Sound.ALUT

-- https://stackoverflow.com/questions/14005592/play-a-wav-file-with-haskell
-- https://github.com/elisehuard/game-in-haskell/blob/master/src/Music.hs
type TrackMap = M.Map String (IO Source)

loadSound :: FilePath -> IO Source
loadSound path = do
  buf    <- createBuffer (File ("sounds/" ++ path ++ ".wav"))
  source <- genObjectName
  buffer source $= Just buf
  return source

loadTracks :: TrackMap
loadTracks = M.fromList
  [ ("place"    , loadSound "tap")
  , ("placeFail", loadSound "ju")
  , ("subWin"   , loadSound "jujuju")
  , ("subLose"  , loadSound "small-hit")
  , ("superWin" , loadSound "complete")
  , ("superLose", loadSound "big-hit")
  ]
