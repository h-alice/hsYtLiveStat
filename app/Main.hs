-- | A command-line tool to extract the live stream URL from a YouTube channel page.
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens ((^.)) -- For accessing fields in data structures
import Control.Monad (when, (>=>) ) -- For monadic operations
import qualified Data.Aeson as A -- For parsing JSON
import Data.Aeson.Types (Parser, parseMaybe, (.:)) -- For defining JSON parsers
import Data.Functor ((<&>)) -- For applying functions to functors
import Data.Text (Text) -- For working with Text


import qualified Data.Text as T
import qualified Data.Text.Lazy as TL -- For lazy Text
import qualified Data.Text.Lazy.Encoding as TLE -- For encoding/decoding lazy Text

import Network.Wreq (get, responseBody) -- For making HTTP requests
import System.Environment (getArgs) -- For accessing command-line arguments
import System.Exit (exitFailure) -- For exiting the program with an error code
-- For printing error messages to stderr
import Text.HandsomeSoup (css, parseHtml) -- For parsing HTML with CSS selectors
import Text.XML.HXT.Core (ArrowTree (deep), ArrowXml (getText), (>>>), runX) -- For working with XML/HTML
import System.IO (hPutStrLn, stderr)



ytInfoPrefix :: T.Text
ytInfoPrefix = "var ytInitialData = " :: T.Text

youtubeUrlBase :: String
youtubeUrlBase = "https://www.youtube.com" :: String

locateAvatar :: A.Object -> Parser A.Object
locateAvatar =      (.: "header")
                >=> (.: "pageHeaderRenderer")
                >=> (.: "content")
                >=> (.: "pageHeaderViewModel")
                >=> (.: "image")
                >=> (.: "decoratedAvatarViewModel")

getLiveData :: A.Object -> Parser Text
getLiveData =       (.: "rendererContext")
                >=> (.: "commandContext")
                >=> (.: "onTap")
                >=> (.: "innertubeCommand")
                >=> (.: "commandMetadata")
                >=> (.: "webCommandMetadata")
                >=> (.: "url")

main :: IO ()
main = do

    -- Check argument
    args <- getArgs
    when (length args /= 1) $ do
        hPutStrLn stderr "Usage: ytLiveStat <channel_name>"
        hPutStrLn stderr "Please provide the channel name as an argument. (with @)"
        exitFailure

    -- Get the channel content page
    content <- get $ youtubeUrlBase ++ "/" ++ head args

    -- Doc from response body
    let doc = parseHtml $ TL.unpack $ TLE.decodeUtf8 $ content ^. responseBody

    yiInitContent <- runX (doc >>> css "script" >>> Text.XML.HXT.Core.deep getText) <&> filter (T.isPrefixOf ytInfoPrefix) . map T.pack
    case yiInitContent of
        [] -> do
            hPutStrLn stderr "Cannot extract ytInitialData from the page."
            hPutStrLn stderr "Either the page structure has changed or the channel does not exist."
            exitFailure
        (jsText:_) -> do
            case A.decodeStrictText $ T.dropEnd 1 $ T.drop (T.length ytInfoPrefix) jsText of
                Nothing -> do
                    hPutStrLn stderr "Failed to parse JSON"
                Just parsedJson -> do
                    case parseMaybe (locateAvatar >=> getLiveData) parsedJson of
                        Nothing -> hPutStrLn stderr "Failed to extract live data"
                        Just liveData -> putStrLn $ youtubeUrlBase ++ T.unpack liveData
