{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock.Safe
import Data.Monoid
import Data.TZworld.Api
import Control.Monad.IO.Class

main::IO ()
main= runSpock 8003 $ spockT id $
  do get root $
         sendBadRequest "A location wasn't provided"
         
     get ("hello" <//> var) $ \name ->
         text ("Hello " <> name <> "!")
         
sendBadRequest::(Control.Monad.IO.Class.MonadIO m) =>String -> ActionCtxT ctx m b 
sendBadRequest msg = json $ Message msg 


