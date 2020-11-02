{-# LANGUAGE TypeApplications #-}

import           Colog.Polysemy.Formatting
import           Data.Function
import           Polysemy
import           System.IO
import qualified WMonad

main :: HasCallStack => IO Int
main = do
  logEnv <- newLogEnv stdout

  let logMessageStdout = logTextStdout & cmap (renderThreadTimeMessage logEnv)
  let logToIO = runLogAction @IO logMessageStdout . addThreadAndTimeToLog

  runFinal . embedToFinal . logToIO . setLogLevel Debug $ WMonad.runCompositor

  return 0
