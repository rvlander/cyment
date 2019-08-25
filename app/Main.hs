{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Lib

import qualified Data.Text.IO         as T
import Network.HTTP.Types.Status

-- main = scotty 3000 $
--   post "/:namespace/:repo" $ rescue (do
--     namespace <- param "namespace"
--     repo <- param "repo"
--     -- path <- param "path"
--     -- commenter <- param "commenter"
--     -- comment <- param "comment"
--     liftAndCatchIO $ do 
--       repo <- getRepo namespace repo
--       either (putStrLn . show) (T.putStrLn . formatRepo) repo
--     html $ "toto") (\msg -> status badRequest400)

main = do 
        res <- pushCommit "rvlander" "blog" "my wonderfull comment, fuck the bitches"
        either (putStrLn . show) (putStrLn . show) res