{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Compute (listServers, listServersInProject, test) where

-- FROM: https://developer.openstack.org/api-ref/compute/

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
    (FromJSON(parseJSON)
    , (.=), (.=), (.:), (.:?), (.!=)
    , object, withObject, withText)
import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Data.Text (Text, pack)
import Network.HTTP.Req

import Identity (authTokens, revokeToken)

--
-- HELPER
--
apiVersion :: Text
apiVersion = "v2.1"

headerToken :: ByteString -> Option scheme
headerToken val =
    header "X-Auth-Token" val

--
-- DATA
--

data LinkRel = NextLink | SelfLink | BookmarkLink | UndefLinkRel
    deriving (Eq,Show)

data ServerLink = ServerLink
    { serverLinkHRef :: Text
    , serverLinkRel :: LinkRel
    } deriving (Eq,Show)

data Server = Server
    { serverId :: Text
    , links :: [ ServerLink ]
    , name :: Text
    } deriving (Eq,Show)

data ServersListResponse = ServersListResponse
    { responseServers :: [ Server ]
    , responseServersLinks :: [ ServerLink ]
    } deriving (Eq,Show)

--
-- DECODERS
--

instance FromJSON LinkRel where
    parseJSON = withText "link-rel" $ \v ->
        case v of
            "self" -> return SelfLink
            "bookmark" -> return BookmarkLink
            "next" -> return NextLink
            _ -> return UndefLinkRel

instance FromJSON ServerLink where
    parseJSON = withObject "servers-links" $ \v -> ServerLink
        <$> v .: "href"
        <*> v .: "rel"

instance FromJSON Server where
    parseJSON = withObject "server" $ \v -> Server
        <$> v .: "id"
        <*> v .: "links"
        <*> v .: "name"

instance FromJSON ServersListResponse where
    parseJSON = withObject "servers-list-response" $ \v -> ServersListResponse
        <$> v .: "servers"
        <*> v .:? "servers_links" .!= []

--
-- Requests
--

listServers' :: Option Https -> ByteString -> Text -> Text -> Int
    -> IO (Int, ServersListResponse)
listServers' options _token _secret _host _port =
    let url = https _host /: apiVersion /: _secret /: "servers"
        request = req GET url NoReqBody
            (jsonResponse)
            (port _port <> headerToken _token <> options)
            :: Req (JsonResponse ServersListResponse)
        treatment = \res ->
            ( responseStatusCode res
            , responseBody res :: ServersListResponse
            )
        responseMonad = runReq def request
    in fmap treatment responseMonad

listServers :: ByteString -> Text -> Text -> Int
    -> IO (Int, ServersListResponse)
listServers  =
    listServers' mempty

listServersInProject :: Text -> ByteString -> Text -> Text -> Int
    -> IO (Int, ServersListResponse)
listServersInProject projectId =
    listServers' $ "project_id" =: projectId

--
-- TEST
--

test :: IO()
test = do
    -- IO
    let readText = fmap pack getLine
    putStr "Host: "
    _host <- readText
    putStr "Keystone Port: "
    idPort <- readLn :: IO Int
    putStr "Nova Port: "
    compPort <- readLn :: IO Int
    putStr "Nova Secret: "
    compSecret <- readText
    putStr "Login: "
    login <- readText
    putStr "Password: "
    psw <- readText
    
    -- AUTH REQ
    (_, mbToken, _) <- authTokens "Default" login psw _host idPort

    -- PROJECTS REQ
    case mbToken of
        Just _token -> do
            print _token
            (_, serversList) <- listServers _token compSecret _host compPort
            print serversList
            (revkCode) <- revokeToken _token _token _host idPort
            print revkCode
        _ ->
            return ()
