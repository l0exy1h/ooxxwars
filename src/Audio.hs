module Audio where

import Sound.ALUT
import Data.Map as M

-- https://stackoverflow.com/questions/14005592/play-a-wav-file-with-haskell
-- https://github.com/elisehuard/game-in-haskell/blob/master/src/Music.hs
type TrackMap = M.Map String (IO Source)

loadSound :: FilePath -> IO Source
loadSound path = do
  buf <- createBuffer (File ("sounds/" ++ path ++ ".wav"))
  source <- genObjectName
  buffer source $= Just buf
  return source

loadTracks :: TrackMap
loadTracks = 
  M.fromList 
    [("place", l "tap"),
     ("placeFail", l "ju"),
     ("subWin", l "jujuju"),
     ("subLose", l "small-hit"),
     ("superWin", l "complete"),
     ("superLose", l "big-hit")
    ] 
  where l = loadSound
