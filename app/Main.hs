{-# OPTIONS_GHC -Wno-unsafe #-}
module Main (main) where

import Control.Monad ( filterM, unless )
import Data.List ( intercalate )
import Data.Time.Clock ( getCurrentTime )
import System.FilePath ( (</>), takeFileName )
import System.Environment ( getArgs )
import System.Process ( readProcess )
import System.Directory
    ( createDirectoryIfMissing,
      doesDirectoryExist,
      doesFileExist,
      getPermissions,
      listDirectory,
      Permissions(writable) )
import System.Random ( newStdGen, Random(randomRs) )
import System.Exit ( exitFailure )
import Text.Printf ( printf )

-- utils
safeHead :: [String] -> String
safeHead xs = if null xs then "" else head xs

randFile :: String -> IO String
randFile ext = do
    gen <- newStdGen
    let str = take 10 $ randomRs ('a','z') gen
    return $ str++"."++ext

getTimeString :: IO String
getTimeString = show <$> getCurrentTime

checkLogWritable :: IO ()
checkLogWritable = do
    perms <- getPermissions seclogPath
    unless (writable perms) $ do
        putStrLn $ "Error: Unable to write to " ++ seclogPath
        exitFailure

-- Workspace
seclogPath :: FilePath
seclogPath = "/" </> "var" </> "seclog"

seclogWorkspace :: FilePath
seclogWorkspace = seclogPath </> "workspace"

getWorkspace :: IO String
getWorkspace = do
    workspace <- readFile seclogWorkspace
    return $ if workspace == "" then "default" else workspace

getWorkspacePath :: IO String
getWorkspacePath = do
    workspace <- getWorkspace
    let workspace_ = seclogPath </> workspace
    createDirectoryIfMissing True workspace_
    return workspace_

-- Modules
switchWorkspace :: String -> IO ()
switchWorkspace = writeFile seclogWorkspace

logAction :: String -> IO ()
logAction x = do
    workspacePath <- getWorkspacePath
    time <- getTimeString
    appendFile (workspacePath </> "record.md") $ printf "%s => %s\n" time x

screenshot :: IO ()
screenshot = do
    path <- getWorkspacePath
    fileName <- randFile "png"
    let filePath = path </> fileName
    _ <- readProcess "scrot" [filePath] []
    logAction $ "Screenshot: " ++ fileName
    putStrLn filePath

setup :: IO ()
setup = do
    createDirectoryIfMissing True seclogPath
    fileExists <- doesFileExist seclogWorkspace
    unless fileExists $ writeFile seclogWorkspace ""

cat :: IO ()
cat = do
    workspacePath <- getWorkspacePath
    readFile (workspacePath </> "record.md") >>= putStrLn

ls :: IO ()
ls = do
    fs <- listDirectory seclogPath
    let xs = map (seclogPath </>) fs
    dirs <- filterM doesDirectoryExist xs
    putStrLn $ intercalate "\t" $ map takeFileName dirs

logStdin :: IO ()
logStdin = getContents >>= logAction

main :: IO ()
main = do
    checkLogWritable
    args <- getArgs
    case args of
        "screenshot":_ -> screenshot
        "sc":_ -> screenshot
        "switch":args_ -> switchWorkspace $ safeHead args_
        "setup":_ -> setup
        "cat":_ -> cat
        "ls":_ -> ls
        "ws":_ -> getWorkspace >>= putStrLn
        "workspace":_ -> getWorkspace >>= putStrLn
        _:_ -> logAction $ unwords args
        [] -> logStdin
