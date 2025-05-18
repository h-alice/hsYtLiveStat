{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wreq
import qualified Data.Text as T
import Control.Monad (when, (>=>))
import Control.Lens ((^.))
import Data.Functor ((<&>))
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

import Text.XML.HXT.Core
    ( ArrowXml(getText), (>>>), ArrowTree(deep), runX )

import Text.HandsomeSoup

import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe, (.:), Parser)

-- OS related libraries
import System.IO (hPutStrLn, stderr) -- For printing errors/warnings
import System.Exit (exitFailure)      -- For exiting the program
import System.Environment (lookupEnv, getArgs) -- For reading configuration


ytInfoPrefix :: T.Text
ytInfoPrefix = "var ytInitialData = " :: T.Text

urlTest :: String
urlTest = "https://www.youtube.com/@RRGT" :: String

youtubeUrlBase :: String
youtubeUrlBase = "https://www.youtube.com" :: String

locateAvatar :: A.Object -> Parser A.Object
locateAvatar =      (.: "header")
                >=> (.: "pageHeaderRenderer")
                >=> (.: "content")
                >=> (.: "pageHeaderViewModel")
                >=> (.: "image")
                >=> (.: "decoratedAvatarViewModel")

getLiveStatus :: A.Object -> Parser T.Text
getLiveStatus =     (.: "liveData")
                >=> (.: "liveBadgeText")

getLiveData :: A.Object -> Parser T.Text
getLiveData =       (.: "rendererContext")
                >=> (.: "commandContext")
                >=> (.: "onTap")
                >=> (.: "innertubeCommand")
                >=> (.: "commandMetadata")
                >=> (.: "webCommandMetadata")
                >=> (.: "url")


loadEnvRequired :: String       -- ^ Environment variable name
                -> IO String    -- ^ Loaded value (program exits if not found)
loadEnvRequired envVarName = do
    maybeValue <- lookupEnv envVarName
    case maybeValue of
        Just val -> return val  -- Value found, return it
        Nothing -> do
            -- Value not found, print error to stderr and exit
            hPutStrLn stderr $ "Unable to load required environment variable: '" ++ envVarName
            exitFailure -- Terminate the program


main :: IO ()
main = do

    -- Check argument
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "Usage: ytLiveStat <channel_name>"
        putStrLn "Please provide the channel name as an argument. (with @)"
        exitFailure

    -- Get the channel content page
    content <- get $ youtubeUrlBase ++ "/" ++ head args

    -- Doc from response body
    let doc = parseHtml $ TL.unpack $ TLE.decodeUtf8 $ content ^. responseBody

    yiInitContent <- runX (doc >>> css "script" >>> Text.XML.HXT.Core.deep getText) <&> filter (T.isPrefixOf ytInfoPrefix) . map T.pack
    case yiInitContent of
        [] -> do
            putStrLn "Cannot extract ytInitialData from the page."
            putStrLn "Either the page structure has changed or the channel does not exist."
            exitFailure
        (jsText:_) -> do
            case A.decodeStrictText $ T.dropEnd 1 $ T.drop (T.length ytInfoPrefix) jsText of
                Nothing -> putStrLn "Failed to parse JSON"
                Just parsedJson -> do
                    case parseMaybe (locateAvatar >=> getLiveData) parsedJson of
                        Nothing -> putStrLn "Failed to extract live data"
                        Just liveData -> print $ youtubeUrlBase ++ T.unpack liveData
