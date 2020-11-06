{-# LANGUAGE TypeApplications #-}

import           Protolude
import           Colog.Polysemy.Formatting
import           Polysemy
import qualified WMonad.Core as WM

main :: HasCallStack => IO Int
main = do
  logEnv <- newLogEnv stdout

  let logMessageStdout = logTextStdout & cmap (renderThreadTimeMessage logEnv)
  let logToIO = runLogAction @IO logMessageStdout . addThreadAndTimeToLog

  runFinal . embedToFinal . logToIO . setLogLevel Debug $ WM.runCompositor

  return 0
