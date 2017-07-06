{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Web.Scotty.Trans
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import System.Environment (getArgs)

data Config =
  Config {
    config :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  case M.lookup k m of
    Nothing -> (M.insert k 1 m, 1)
    Just n -> (M.insert k (n + 1) m, n + 1)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    Config cfg pfx <- lift ask
    counters <- liftIO $ readIORef cfg
    let key' = mappend pfx unprefixed
        (newCounters, newInteger) = bumpBoomp key' counters
    liftIO $ writeIORef cfg newCounters
    liftIO $ putStrLn $ TL.unpack key' `mappend` ": " `mappend` show newInteger
    html $ mconcat
      [ "<h1>Success! Count was: "
      , TL.pack $ show newInteger
      , "</h1>"
      ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let cfg = Config counter $ TL.pack prefixArg
      runR r = runReaderT r cfg
  scottyT 3000 runR app
