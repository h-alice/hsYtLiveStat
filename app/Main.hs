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
import Data.Aeson.Types (parseMaybe, (.:), parse, Parser)
import qualified Data.Text.Encoding as TE
import Data.ByteString.Builder (toLazyByteString)
import Data.Aeson (Value(Object))



ytInfoPrefix :: T.Text
ytInfoPrefix = "var ytInitialData = " :: T.Text

urlTest :: String
urlTest = "https://www.youtube.com/@soaringforyou" :: String

youtubeUrlBase :: String
youtubeUrlBase = "https://www.youtube.com" :: String

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
                    let locateAvatar jsRoot =      jsRoot .: "header"
                                            >>= \hd -> hd .: "pageHeaderRenderer"
                                            >>= \rd -> rd .: "content"
                                            >>= \ct -> ct .: "pageHeaderViewModel"
                                            >>= \vm -> vm .: "image"
                                            >>= \im -> im .: "decoratedAvatarViewModel" :: Parser A.Object
                    -- Get live status from the avatar
                    let getLiveStatus jsRoot =     jsRoot .: "liveData"
                                            >>= \lv -> lv .: "liveBadgeText" :: Parser T.Text

                    -- Get live data from the avatar
                    let getLiveData jsRoot =       jsRoot .: "rendererContext"
                                            >>= \rc -> rc .: "commandContext"
                                            >>= \cc -> cc .: "onTap"
                                            >>= \ot -> ot .: "innertubeCommand"
                                            >>= \ic -> ic .: "commandMetadata"
                                            >>= \cm -> cm .: "webCommandMetadata"
                                            >>= \wm -> wm .: "url" :: Parser T.Text

                    let x = parseMaybe (locateAvatar >=> getLiveData) parsedJson
                    print x



