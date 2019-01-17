{-# LANGUAGE OverloadedStrings #-}
module Identity (authTokens) where

-- FROM: https://developer.openstack.org/api-ref/identity/v3

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
    (FromJSON(parseJSON)
    , (.=), (.=), (.:), (.:?), (.!=)
    , object, withObject, withText)
import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Network.HTTP.Req

--
-- HELPER
--
headerToken :: ByteString -> Option scheme
headerToken val =
    header "X-Auth-Token" val

--
-- DATA
--

data Domain = Domain
    { domainId :: Text
    , domainName :: Text
    } deriving (Eq,Show)

data User = User
    { userName :: Text
    , userDomain :: Domain
    , userId :: Text
    , userPasswordExpiresAt :: Maybe UTCTime
    } deriving (Eq,Show)

data Methods = PasswordMethod | UndfMethod
    deriving (Eq,Show)


data Token = Token
    { tokenMethods :: [ Methods ]
    , tokenAuditIds :: [ Text ]
    , tokenExpiresAt :: Maybe UTCTime
    , tokenIssuedAt :: UTCTime
    } deriving (Eq,Show)

data TokenResponse = TokenResponse
    { responseToken :: Token
    } deriving (Eq,Show)

data Links = Links
    { linksNext :: Maybe Text
    , linksPrevious :: Maybe Text
    , linksSelf :: Text
    } deriving (Eq,Show)

data Project = Project
    { projectIsDomain :: Bool
    , projectDescription :: Maybe Text
    , projectDomainId :: Text
    , projectEnabled :: Bool
    , projectId :: Text
    , projectLinks :: Links
    , projectName :: Text
    , projectParentId :: Maybe Text
    , projectTags :: [ Text ]
    } deriving (Eq,Show)

data ProjectsResponse = ProjectsResponse
    { responseLinks :: Links
    , responseProjects :: [ Project ]
    } deriving (Eq,Show)

--
-- DECODERS
--

instance FromJSON Domain where
    parseJSON = withObject "domain" $ \v -> Domain
        <$> v .: "id"
        <*> v .: "name"

instance FromJSON User where
    parseJSON = withObject "user" $ \v -> User
        <$> v .: "name"
        <*> v .: "domain"
        <*> v .: "id"
        <*> v .: "password_expires_at"

instance FromJSON Methods where
    parseJSON = withText "methods" $ \v ->
        case v of
            "password" -> return PasswordMethod
            _ -> return UndfMethod

instance FromJSON Token where
    parseJSON = withObject "token" $ \v -> Token
        <$> v .: "methods"
        <*> v .: "audit_ids"
        <*> v .:? "expires_at"
        <*> v .: "issued_at"

instance FromJSON TokenResponse where
    parseJSON = withObject "token-response" $ \v -> TokenResponse
        <$> v .: "token"

instance FromJSON Links where
    parseJSON = withObject "links" $ \v -> Links
        <$> v .:? "next"
        <*> v .:? "previous"
        <*> v .: "self"

instance FromJSON Project where
    parseJSON = withObject "project" $ \v -> Project
        <$> v .: "is_domain"
        <*> v .:? "description"
        <*> v .: "domain_id"
        <*> v .: "enabled"
        <*> v .: "id"
        <*> v .: "links"
        <*> v .: "name"
        <*> v .:? "parent_id"
        <*> v .:? "tags" .!= []

instance FromJSON ProjectsResponse where
    parseJSON = withObject "projects-response" $ \v -> ProjectsResponse
        <$> v .: "links"
        <*> v .: "projects"

--
-- Requests
--

authTokens :: Text -> Text -> Text
    -> Text -> Int
    -> IO (Int, Maybe ByteString, TokenResponse)
authTokens domain name password _host _port =
    let payload = object
                [ "auth" .= object
                    [ "identity" .= object
                        [ "methods" .= (["password" :: Text])
                        , "password" .= object
                            [ "user" .= object
                                [ "name" .= name
                                , "domain" .= object [ "name" .= domain ]
                                , "password" .= password
                                ]
                            ]
                        ]
                    ]
                ]
        url = https _host /: "v3" /: "auth" /: "tokens"
        request = req POST url
            (ReqBodyJson payload)
            (jsonResponse)
            (port _port)
            :: Req (JsonResponse TokenResponse)
        treatment = \res ->
            ( responseStatusCode res
            , responseHeader res "X-Subject-Token"
            , responseBody res :: TokenResponse
            )
        responseMonad = runReq def request
    in fmap treatment responseMonad

projects :: ByteString -> Text -> Int -> IO (Int, ProjectsResponse)
projects _token _host _port =
    let url = https _host /: "v3" /: "projects"
        request = req GET url NoReqBody jsonResponse
            (port _port <> headerToken _token )
            :: Req (JsonResponse ProjectsResponse)
        treatment = \res ->
            ( responseStatusCode res
            , responseBody res :: ProjectsResponse
            )
        responseMonad = runReq def request
    in fmap treatment responseMonad

revokeToken :: ByteString -> ByteString -> Text -> Int -> IO (Int)
revokeToken toRevoke _token _host _port =
    let url = https _host /: "v3" /: "projects"
        request = req GET url NoReqBody ignoreResponse
            ( port _port
            <> headerToken _token
            <> header "X-Subject-Token" toRevoke)
            :: Req (IgnoreResponse)
        treatment = \res -> ( responseStatusCode res )
        responseMonad = runReq def request
    in fmap treatment responseMonad

--
-- TEST
--

test :: IO()
test = do
    -- IO
    let readText = fmap pack getLine
    putStr "Host: "
    _host <- readText
    putStr "Port: "
    _port <- readLn :: IO Int
    putStr "Login: "
    login <- readText
    putStr "Password: "
    psw <- readText
    
    -- AUTH REQ
    (_, mbToken, _) <- authTokens "Default" login psw _host _port

    -- PROJECTS REQ
    case mbToken of
        Just _token -> do
            print _token
            (_, projectList) <- projects _token _host _port
            print projectList
            (revkCode) <- revokeToken _token _token _host _port
            print revkCode
        _ ->
            return ()
