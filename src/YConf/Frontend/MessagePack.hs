{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : YConf.Frontend.MessagePack
Description : Message pack RPC frontend implementation
Copyright   : Copyright 2017, Yahoo Holdings Inc.
License     : Licensed under the terms of the BSD3 license. See LICENSE file for terms.
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

Implementation of a message pack RPC over TCP frontend.
-}
module YConf.Frontend.MessagePack ( MessagePack
                                  , messagePackFrontend) where

import qualified Data.HashMap.Strict    as H
import qualified Data.Map               as M
import           Data.MessagePack       as MP
import           Data.Scientific
import qualified Data.Vector            as V
import           Data.Yaml              hiding ((.:), (.=))
import           Network.MessagePack
import           Network.Simple.TCP
import           System.Logger
import           YConf.Frontend
import           YConf.Frontend.Helpers
import           YConf.Live
import           YConf.Tree
import           YConf.Types
import Data.Text.Encoding

data MessagePack = MessagePack { listenHost :: HostPreference
                               , listenPort :: ServiceName }

messagePackFrontend :: String -> MessagePack
messagePackFrontend bind =
    case break (== ':') bind of
             (port, "")       -> MessagePack HostAny port
             (host, ':':port) -> MessagePack (Host host) port
             _                -> error $ "invalid configuration for message-pack frontend: " ++ bind

instance Frontend MessagePack where

    runFrontend fe lc =
        do let port = listenPort fe
               host = listenHost fe
           info logger $ msg (val "listening HTTP") ~~ "port" .= show port ~~ "host" .= show host
           withSocketsDo $ runRPC methods host port
      where
        logger = clone (Just "msgpack-rpc") $ getLogger lc

        methods = M.singleton "getValue" getValue

        getValue (ObjectArray (c:ps)) =
            do (dims, tree) <- readConfigState lc
               info logger $ msg (val "getValue") ~~ "context" .= show c ~~ "path" .= show ps
               return $ do context <- parseEither (parseContext Forgiving dims) $ toJSON c
                           path    <- parsePath ps
                           let Delta cfgData = projection context tree
                           focus  <- getSubstructure path cfgData
                           parseEither parseJSON focus
           where
              parsePath a = let parseSingle (ObjectString s) = return $ decodeUtf8 s
                                parseSingle _ = Left "invalid path component. Should be a string"
                            in  mapM parseSingle a
        getValue _ = return $ Left "invalid parameters for getValue. Should be an Array of at least one element"

instance FromJSON MP.Object where

    parseJSON (Number n) = return $ either ObjectDouble ObjectInt $ floatingOrInteger n
    parseJSON Null       = return ObjectNil
    parseJSON (Bool b)   = return $ ObjectBool b
    parseJSON (String s) = return $ ObjectString $ encodeUtf8 s
    parseJSON (Array a)  = ObjectArray <$> mapM parseJSON (V.toList a)
    parseJSON (Object o) = let parsePair (k, v) = do v' <- parseJSON v
                                                     return (ObjectString $ encodeUtf8 k, v')
                           in  ObjectMap . M.fromList <$> mapM parsePair (H.toList o)

instance ToJSON MP.Object where

    toJSON (ObjectInt i)    = Number $ fromIntegral i
    toJSON (ObjectUInt i)   = Number $ fromIntegral i
    toJSON (ObjectFloat f)  = Number $ fromFloatDigits f
    toJSON (ObjectDouble d) = Number $ fromFloatDigits d
    toJSON ObjectNil        = Null
    toJSON (ObjectBool b)   = Bool b
    toJSON (ObjectArray a)  = Array $ V.fromList $ map toJSON a
    toJSON (ObjectMap m)    = let pairToJSON (ObjectString s, v) = (decodeUtf8 s, toJSON v)
                                  pairToJSON _                   = error "key should be a string"
                              in Object  $ H.fromList $ map pairToJSON $ M.toList m
    toJSON (ObjectString s) = String $ decodeUtf8 s
    toJSON (ObjectBinary _) = error "binary is not supported"
    toJSON (ObjectExt _ _) = error "extension is not supported"
