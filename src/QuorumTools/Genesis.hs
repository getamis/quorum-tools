{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}

module QuorumTools.Genesis where

import           Control.Lens                 (view)
import           Data.Aeson
import qualified Data.Aeson.Types             as Aeson
import qualified Data.ByteString.Base16       as B16
import           Data.HashMap.Lazy            (HashMap)
import qualified Data.HashMap.Lazy            as HashMap
import qualified Data.Vector                  as V
import qualified Data.Text.Encoding           as T
import           Turtle                       hiding (view, header)
import           Prelude                      hiding (FilePath)

import           QuorumTools.Genesis.Contract (qcContractCode)
import           QuorumTools.Types
import           QuorumTools.Util

createStorage :: [AccountId] -> [AccountId] -> Value
createStorage voters makers = Object $ meta <> votersBlock <> makersBlock
  where threshold = 2 -- of 3
        voterIx = 3
        makerIx = 5

        numVoters = length voters
        numMakers = length makers

        numToHexValue :: Int -> Value
        numToHexValue = String . hexPadKey

        hexPadKey :: Int -> Text
        hexPadKey = hexPrefixed . intToBytes32

        meta = HashMap.fromList
          [ (hexPadKey 1, numToHexValue threshold)
          , (hexPadKey 2, numToHexValue numVoters)
          , (hexPadKey 4, numToHexValue numMakers)
          ]

        votersBlock = mapAddresses voterIx voters
        makersBlock = mapAddresses makerIx makers

mapAddresses :: Int -> [AccountId] -> HashMap Text Value
mapAddresses index addresses =
  let v = V.fromList addresses
      addrToStorage (AccountId (Addr addrBytes)) =
        let Bytes32 key = storageKey index addrBytes
        in (T.decodeUtf8 key, "0x01")
      mapped = addrToStorage <$> v
  in HashMap.fromList (V.toList mapped)

storageKey :: Int -> Bytes20 -> Bytes32
storageKey index address =
  let Bytes32 paddedAddress = padAddress address
      Bytes32 paddedIndex = intToBytes32 index
  in case B16.decode (paddedAddress <> paddedIndex) of
       (encoded, "") -> sha3Bytes encoded
       (_, invalid) -> error (show invalid)

createGenesisJson
  :: (MonadIO m, HasEnv m)
  => [AccountId]
  -> m FilePath
createGenesisJson acctIds = do
    jsonPath <- view clusterGenesisJson
    output jsonPath contents
    return jsonPath

  where
    voter:makers = acctIds

    balances :: [Aeson.Pair]
    balances = fmap
      (\aid -> accountIdToText aid .= object [ "balance" .= t "0" ])
      acctIds

    header = "0x0000000000000000000000000000000000000020" .= object
      [ "code"    .= qcContractCode
      , "storage" .= createStorage [voter] makers
      ]

    contents :: Shell Line
    contents = select $ textToLines $ textEncode $ object
      [ "alloc"      .= object (header:balances)
      , "coinbase"   .= t "0x0000000000000000000000000000000000000000"
      , "config"     .= object
        [ "homesteadBlock" .= (0 :: Int) ]
      , "difficulty" .= t "0x0"
      , "extraData"  .= t "0x0"
      , "gasLimit"   .= t "0xE0000000"
      , "mixhash"    .= t "0x00000000000000000000000000000000000000647572616c65787365646c6578"
      , "nonce"      .= t "0x0"
      , "parentHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000"
      , "timestamp"  .= t "0x0"
      ]

    t = id :: Text -> Text

createIstanbulGenesisJson :: (MonadIO m, HasEnv m)
  => [Text]
  -> Text
  -> m FilePath
createIstanbulGenesisJson acctIds extraData = do

    jsonPath <- view clusterGenesisJson
    output jsonPath contents
    return jsonPath
  where
    balance:: Text
    balance = "999900000000000000000000000000000000000000000"

    balances :: [Aeson.Pair]
    balances = fmap (\aid -> aid .= object [ "balance" .= t balance ]) acctIds

    contents :: Shell Line
    contents = select $ textToLines $ textEncode $ object
      [ "alloc"      .= object (balances)
      , "coinbase"   .= t "0x0000000000000000000000000000000000000000"
      , "config"     .= object
      [
        "chainId" .= (2016 :: Int),
        "istanbul" .= object[]
      ]
      , "difficulty" .= t "0x1"
      , "extraData"  .= t "0x0"
      , "gasLimit"   .= t "0x47e7c4"
      , "mixhash"    .= t "0x63746963616c2062797a616e74696e65206661756c7420746f6c6572616e6365"
      , "nonce"      .= t "0x0"
      , "parentHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000"
      , "timestamp"  .= t "0x0"
      , "extraData"  .= extraData
      ]

    t = id :: Text -> Text