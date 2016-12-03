{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

main :: IO ()
main = do
  conn <- R.connect R.defaultConnectInfo
  scotty 3000 $ app conn

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs =
  (xs !!) <$> SR.randomRIO (0, length xs - 1)

shortyGen :: IO String
shortyGen = replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply Bool)
saveURI conn shortURI uri =
  R.runRedis conn $ R.setnx shortURI uri

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URI</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: "
            , TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a URL, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

-- TODO: Non 200 status code for errors.
app :: R.Connection -> ScottyM ()
app conn = do
  -- TODO: Shouldn't it be POST because it creates a record?
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' = encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURI conn shorty uri')
        case resp of
          Left reply -> text (TL.pack (show reply))
          Right True -> html (shortyCreated resp shawty)
          Right False -> text "random short URI collided"
      Nothing -> text (shortyAintUri uri)
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI conn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right Nothing -> text "uri not found"
      Right (Just bs) -> html (shortyFound tbs)
        where tbs :: TL.Text
              tbs = TL.fromStrict (decodeUtf8 bs)
