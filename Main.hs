module Main (main) where

import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..), URIAuth(..))
import System.IO (hPutStrLn, stderr)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (getWorkingDirectory)
import OpenSSL (withOpenSSL)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Network.Wai.Dispatch
import Routes

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

app :: URI -> Int -> Maybe String -> IO ()
app root port portOverride = do
	cwd <- getWorkingDirectory
	run realPort $
		logStdoutDev $ autohead $ acceptOverride $ -- Middleware
		dispatch (staticRoot cwd) $ routes root    -- Do routing
	where
	realPort = maybe port read portOverride

main :: IO ()
main = withOpenSSL $ do
	args <- getArgs
	case args of
		[root, port] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) (Just port)
		[root] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) Nothing
		[] ->
			main' (parseAbsoluteURI "http://localhost:3000/") Nothing
		_ -> hPutStrLn stderr "Usage: ./Main <Root URI> [<port>]"
	where
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ':':port})})) =
		app r (read port)
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ""})})) =
		app r 80
	main' _ = const $ hPutStrLn stderr "Invalid Root URI given"
