{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wreq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Control.Monad (void, when, (>=>))
import Control.Lens ((^.))
import Data.Functor ((<&>))
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

import Text.XML.HXT.Core

import Lib
import Data.Text.Encoding (decodeUtf8)
import Text.HandsomeSoup
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe, (.:), parse, Parser, parseField)
import qualified Data.Text.Encoding as TE
import Data.ByteString.Builder (toLazyByteString)
import Data.Aeson (Value(Object))

ytInfoPrefix :: T.Text
ytInfoPrefix = "var ytInitialData = " :: T.Text

urlTest :: String
urlTest = "https://www.youtube.com/@soaringforyou" :: String

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


main :: IO ()
main = do
    content <- get urlTest

    -- Doc from response body
    let doc = parseHtml $ TL.unpack $ TLE.decodeUtf8 $ content ^. responseBody
    let z = (runX (doc >>> css "script" >>> Text.XML.HXT.Core.deep getText) <&> map T.pack) <&> filter (T.isPrefixOf ytInfoPrefix)
    a <- z
    case a of
        [] -> putStrLn "No data found"
        (x:_) -> do
            case A.decodeStrictText $ T.dropEnd 1 $ T.drop (T.length ytInfoPrefix) x of
                Nothing -> putStrLn "Failed to parse JSON"
                Just parsedJson -> do                                  

                    let ur = parseMaybe (locateAvatar >=> getLiveData) parsedJson
                    print ur



