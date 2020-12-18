module Main (main) where

import Control.Monad
import Data.List
import Data.Time.Clock
import System.FilePath
import System.Environment
import System.Process
import System.Directory
import System.Random
import System.Exit
import Text.Printf

-- utils
safeHead :: [String] -> String
safeHead xs = if length xs == 0 then "" else head xs

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
switchWorkspace = writeFile $ seclogWorkspace

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

printHelp :: IO ()
printHelp = do
    putStrLn "usage: log [screenshot|switch|ws|cat|'message']"
    exitFailure

setup :: IO ()
setup = do
    createDirectoryIfMissing True seclogPath
    fileExists <- doesFileExist $ seclogWorkspace
    unless fileExists $ writeFile seclogWorkspace ""

cat :: IO ()
cat = do
    workspacePath <- getWorkspacePath
    (readFile $ workspacePath </> "record.md") >>= putStrLn

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
        _:_ -> logAction $ intercalate " " args
        [] -> logStdin
