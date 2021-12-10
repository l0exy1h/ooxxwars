module Audio where

import Sound.ALUT

-- play a sound synchronously
-- https://stackoverflow.com/questions/14005592/play-a-wav-file-with-haskell
playSound2 :: String -> Duration -> IO ()
playSound2 soundName sleepTime =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    let soundprefix = "sounds/" 
    buffer3 <- createBuffer $ File (soundprefix++soundName++".wav")
    [source] <- genObjectNames 1
    queueBuffers source [buffer3]
    play [source]
    sleep sleepTime
    closeDevice device
    return ()

playSoundPlace = playSound2 "tap" 0.15
playSoundPlaceFail = playSound2 "ju" 0.15
playSoundSubWin = playSound2 "jujuju" 0.6
playSoundSubLose = playSound2 "small-hit" 0.2
playSoundSuperWin = playSound2 "complete" 1.0
playSoundSuperLose = playSound2 "big-hit" 1.0
