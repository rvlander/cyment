{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Lib

import qualified Data.Text.IO         as T
import Data.Text (pack)
import Network.HTTP.Types.Status
import Text.StringRandom
import Data.Text         (Text)

main =
  let 
    idPattern = pack . concat $  replicate 8 "[0-9a-z]"
  in scotty 3000 $
   post "/:namespace/:repo" $ rescue (do
     namespace <- param "namespace"
     repo <- param "repo"
     path <- param "path"
     commenter <- param "commenter"
     comment <- param "comment"
    --  redirectUrl <- param "redirect" 
     liftAndCatchIO $ do 
        commentId <- stringRandomIO idPattern
        res <- pushCommitAndMakePR namespace repo commentId path commenter $ buildContent commenter comment
        either (putStrLn . show) (putStrLn . show) res
     html "Thanks, your comment is wating to be approved!") (\msg -> status badRequest400)
