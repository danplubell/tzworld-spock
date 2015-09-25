{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import           Data.TZworld.Api
import           Web.Spock.Safe
main::IO ()
main= runSpock 8003 $ spockT id $
  do get root $
         sendBadRequest "A location wasn't provided"

     get ("location" ) handler

sendBadRequest::(Control.Monad.IO.Class.MonadIO m) =>String -> ActionCtxT ctx m b
sendBadRequest msg = json $ Message msg

handler :: ActionCtxT ctx IO b
handler  = do
              latp <- param "lat"::ActionCtxT ctx IO (Maybe T.Text )
              lonp <- param "lon"::ActionCtxT ctx IO (Maybe T.Text)
              case (latp,lonp) of
                   (Nothing,Nothing)-> sendBadRequest "The longitude and latitude values are invalid"
                   (Nothing,_)      -> sendBadRequest "The latitude value is invalid"
                   (_,Nothing)      -> sendBadRequest "The longitude value is invalid"
                   (Just la, Just lo) -> do
                                          tze <- liftIO $ handleLocation (E.encodeUtf8 la) (E.encodeUtf8 lo)
                                          case tze of
                                              Left err -> sendBadRequest  err
                                              Right tzstr -> json tzstr
