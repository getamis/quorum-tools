{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Bootstraps an AWS cluster
module Mains.AwsBootstrap where

import           Control.Lens         ((.~))
import           Control.Monad.Reader (runReaderT)
import           Data.Bool            (bool)
import           Prelude              hiding (FilePath)
import           Turtle

import           Cluster
import           Cluster.Aws          (dockerHostIp, internalAwsIp)
import           Cluster.Types

data AwsConfig
  = AwsConfig { numSubnets  :: Int
              , rootDir     :: FilePath
              , clusterType :: AwsClusterType
              , clusterSize :: Int
              }

cliParser :: Parser AwsConfig
cliParser = AwsConfig
  <$> optInt  "subnets"       's' "Number of subnets in the region"
  <*> optPath "path"          'p' "Output path"
  <*> fmap (bool SingleRegion MultiRegion)
           (switch  "multi-region" 'm' "Whether the cluster is multi-region")
  <*> optInt "cluster-size"   'n' "Total cluster size across all regions"

mkBootstrapEnv :: AwsConfig -> [GethId] -> ClusterEnv
mkBootstrapEnv config gids = mkClusterEnv mkIp mkDataDir gids
    & clusterGenesisJson    .~ dataRoot </> "genesis.json"
    & clusterPrivacySupport .~ PrivacyEnabled

  where
    dataRoot = rootDir config
    size     = clusterSize config
    subnets  = numSubnets config

    mkDataDir (GethId gid) = DataDir $
      dataRoot </> fromText (format ("geth"%d) gid)

    -- In the multi-region setting, since we are connecting to other nodes over
    -- the open internet, we do so through local SSH tunnels.
    mkIp = case clusterType config of
      SingleRegion -> internalAwsIp size subnets
      MultiRegion  -> const dockerHostIp

awsBootstrapMain :: IO ()
awsBootstrapMain =
  awsBootstrap =<< options "Bootstraps an AWS cluster" cliParser

awsBootstrap :: AwsConfig -> IO ()
awsBootstrap config =
  let gids = clusterGids $ clusterSize config

  in sh $ flip runReaderT (mkBootstrapEnv config gids) $
       wipeAndSetupNodes (Just $ DataDir "/datadir") (rootDir config) gids
