module Main (main) where

import System.FilePath.Posix
import Data.List
import System.Environment
import System.Process
import System.Directory
import Data.Time.Clock
import System.Random
import System.Exit
import Control.Monad

-- utils
safeHead :: [String] -> String
safeHead xs = if length xs == 0 then "" else head xs

randFile :: String -> IO String
randFile ext = do
    gen <- newStdGen
    let str = take 10 $ randomRs ('a','z') gen 
    return $ str++"."++ext

getTimeString :: IO String
getTimeString = show `fmap` getCurrentTime

checkLogWritable :: IO ()
checkLogWritable = do
    perms <- getPermissions seclogPath
    if not $ writable perms
       then do 
           putStrLn $ "Error: Unable to write to " ++ seclogPath
           exitFailure
       else pure ()

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
    appendFile (workspacePath </> "record.md") (time++" => "++x++"\n")

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
    putStrLn "usage: log [screenshot|switch|'message']"
    exitFailure

setup :: IO ()
setup = do
    createDirectoryIfMissing True seclogPath
    fileExists <- doesFileExist $ seclogWorkspace
    if not fileExists
       then writeFile (seclogWorkspace) ""
       else pure ()

cat :: IO ()
cat = do
    workspacePath <- getWorkspacePath
    d <- readFile $ workspacePath </> "record.md"
    putStrLn d

ls :: IO ()
ls = do
    fs <- listDirectory seclogPath
    let xs = map (\x -> seclogPath </> x) fs
    dirs <- filterM doesDirectoryExist xs
    putStrLn $ intercalate "\t" $ map takeBaseName dirs

runModule :: String -> [String] -> IO ()
runModule cmd args
  | cmd == "screenshot" = screenshot
  | cmd == "sc" = screenshot
  | cmd == "switch" = switchWorkspace $ safeHead args
  | cmd == "setup" = setup
  | cmd == "cat" = cat
  | cmd == "ls" = ls
  | otherwise = logAction $ intercalate " " $ cmd:args

main :: IO ()
main = do
    checkLogWritable
    args <- getArgs
    case args of
      cmd:args_ -> runModule cmd args_
      _ -> printHelp
