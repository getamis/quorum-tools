{-# LANGUAGE OverloadedStrings #-}

-- Test removing a node from the cluster and adding it back with a new raft ID
module QuorumTools.Test.Istanbul.RestartNonProposerTest where

import           Control.Concurrent.Async   (forConcurrently)
import           Control.Concurrent.Async (wait)
import           Control.Lens
import           Control.Monad            (void)
import           Data.Monoid              ((<>))
import           Turtle                   (liftIO)
import           Data.Map.Strict            (toList)
import           Data.Text (replace)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Control.Monad              (replicateM)
import qualified Data.ByteString.Char8   as B8

import           QuorumTools.Util           (HexPrefix (..), bytes20P, prefixP,
                                             inshellWithJoinedErr, matchOnce,
                                             printHex, tee, textDecode,
                                             textEncode)
import           Turtle                  hiding (bytes, prefix, text)
import           Turtle.Pattern          (Pattern, count, hexDigit, skip,
                                          match)
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS

import           QuorumTools.Cluster
import           QuorumTools.Control
import           QuorumTools.Test.Outline
import           QuorumTools.Types


replaceSlash :: Eq a => a -> a -> [a] -> [a]
replaceSlash a b = map $ \c -> if c == a then b else c

nName :: Integer -> Text
nName gid = format ("geth"%d) gid

print' :: String -> Integer -> IO()
print' x y= do
  void $ liftIO $ mktree $ "gdata" </> fromText (nName y)

ttt :: IO ()
ttt = do
--  k <- forConcurrently [1,2,3] $ \url -> print url
  k <- forConcurrently [1,2,3] (print' "123")
  print $ "111"

restartNonProposerTestMain :: IO ()
restartNonProposerTestMain = do
  let gids = [1..4] :: [GethId]
      clusterSize = length gids
      password = CleartextPassword "abcd"

--  keys <- generateClusterKeys gids password
  keys <- generateClusterNodeKeys gids
  let cEnv = mkIstanbulLocalEnv keys

--  let key0 = nodeKey ((Map.elems (_clusterNodeKeys cEnv)) !! 0)
--  writeNodeKeys ((dataDirPath (Map.keys (_clusterDataDirs cEnv) !! 0)) </> "nodekey") key0
--  ttt

  result <- runTestM cEnv $ do
    [g1, g2, g3,g4] <- wipeAndSetupIstanbulNodes "gdata" gids cEnv
    instruments <- traverse (runIstanbulNode clusterSize) [g1, g2, g3, g4]
    td 2
    awaitAll (assumedRole <$> instruments)
  print result

-- "\"{\\\"nodeKey\\\":\\\"8e01b8ce14ad997dbc4d1d40c718114e70fe86353b2cfa8965bf05aa7cfc65a1\\\",\\\"enode\\\":\\\"09f69ed3e23fe61c3ad228d0fb3f79185411aaa575e6affbecae36d61372d31ff31f88659041088c8b844df2f96cda08e74e7ae4751f790d6dcd2fadb23e4b4c\\\",\\\"address\\\":\\\"0xbd71769244ed73e81758a5f27f7c2088c395f926\\\"}\""
--  kkk <- generateNodeKey "sss"
--  print $ kkk

--  print $ "---"
--  let gg = read (BS.unpack (BS.pack kkk)) :: BS.ByteString
--  print $ gg
--
--  let json = BS.pack $ BS.unpack gg
--  let (Just gy) = decode json :: Maybe KeyPair
--
--  print $ gy
--  wipeLocalClusterRoot $ (dataDirPath (snd (toList (_clusterDataDirs cEnv) !! 0))) </> "geth"
--  print $ "private key:"
--  print $ nodeKey gy
--  print $ show $ dataDirPath (snd (toList (_clusterDataDirs cEnv) !! 0))
--

--  print $ Map.elems (_clusterNodeKeys cEnv)



--  writeFile (show (dataDirPath (snd (toList (_clusterDataDirs cEnv) !! 0)))) (nodeKey (snd (toList (_clusterNodeKeys cEnv)) !! 0))
--  writeFile "gda" $ nodeKey gy


--  result <- runTestM cEnv $ do
--    [g1, g2, g3,g4] <- wipeAndSetupIstanbulNodes Nothing "gdata" gids
--
--
--    instruments <- traverse (runIstanbulNode clusterSize) [g1, g2, g3, g4]
--
--    awaitAll (assumedRole <$> instruments)

--
--     withSpammer [g1, g2, g3] $ td 1
--
     -- remove g1, pause, add it back as g4, with the same blockchain data.
--     g2 `removesNode` g1
--
--     withSpammer [g2, g3] $ td 1
--
--     _ <- liftIO $ killNode (head instruments)
--
--     let g4 = g1 { gethId = 4
--                 , gethJoinMode = JoinExisting
--                 }
--
--     g3 `addsNode` g4
--
--     g4i <- runNode clusterSize g4
--
--     void $ liftIO $ wait $ assumedRole g4i
--
--     withSpammer [g2, g3, g4] $ td 1
--     td 1
--
--     let allInstruments     = instruments <> [g4i]
--         runningInstruments = drop 1 allInstruments
--
--     verify (lastBlock       <$> runningInstruments)
--            (outstandingTxes <$> allInstruments)
--            (nodeTerminated  <$> runningInstruments)

--  print result