import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.IO.Class

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  n <- ask
  liftIO $ putStrLn $ "Hi: " ++ show n
  reader (+ 1)

rPrintIncAccum :: (Num a, Show a) => StateT a IO String
rPrintIncAccum = do
  n <- get
  liftIO $ putStrLn $ "Hi: " ++ show n
  put $ n + 1
  return $ show n
