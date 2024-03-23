module  Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )

import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude


usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
--Exercise 2.6.1 1.5p + 0.5p
--Implement the handleInit function, which will handle the init command. It should
--create an empty database. If a database file already exists, it should be overwritten.
--Hints:
--To create an empty database, you can use the empty function from the Entry.DB module

handleInit :: TestableMonadIO m => m ()
handleInit = do
  result <- DB.save DB.empty   -- save the empty database
  case result of
    Success _ -> putStrLn "Initialized an empty database."
    Error DB.SaveError -> putStrLn "Failed to save the empty database."

-- | Handle the get command
--Exercise 2.6.2 1.5p + 0.5p
--Implement the handleGet function, which will handle the get command. It should find
--the first entry with the given id, or display an error if there is no such entry.
--Hints:
--To find the first entry that matches a predicate, you can use the findFirst function from
--the Entry.DB module.

handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    (Error err) ->
      putStrLn "Failed to load DB"
    (Success db) ->
      let
        foundEntry = DB.findFirst (\entry -> entryId entry == getOptId getOpts) db
      in
        case foundEntry of
            Nothing ->
              putStrLn "No entry was found"
            Just entry ->
              putStrLn $ entrySnippet entry
 

-- | Handle the search command
--Exercise 2.6.3 3p + 1p
--Implement the handleSearch function, which will handle the search command. It
--should display all entries that match all search queries, using the FmtEntry newtype
--(i.e. use show (FmtEntry entry) ). If no entries match the query, you should print
--"No entries found" .
--Hints:
--To check if an entry is matched by a list of query item, you can use the
--matchedByAllQueries function from the Entry.Entry module. To get all entries that
--satisfy a predicate, you can use the findAll function from the Entry.DB module.

handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    (Error err) ->
      putStrLn "Failed to load DB"
    (Success db) ->
      let
        foundEntries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db
      in
        case foundEntries of
          [] -> putStrLn "No entries found"
          entries -> do
            putStrLn "Found entries:"
            mapM_ (\entry -> putStrLn (showFmtEntry entry)) entries

showFmtEntry :: Entry -> String
showFmtEntry entry = show (FmtEntry entry)

-- | Handle the add command
--Implement the handleAdd function, which will handle the add command. It should
--read the contents of a given file and add a new entry to the database. If an entry with the snippet to be added already exists, you should print the error message
--"Entry with this content already exists: " .
--Hints:
--To add a new entry, you can use the insertWith function from the Entry.DB module. To
--load the database, modify it and then save the modified version, you can use the modify
--function from the Entry.DB module.

handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  fromFile <- readFile (addOptFilename addOpts)
  db <- DB.load
  case db of
    (Error err) ->
      putStrLn "Failed to load DB"
    (Success db) ->
      let
        existingEntry = DB.findFirst (\entry -> entry == makeEntry (entryId entry) fromFile addOpts) db
      in
        case existingEntry of
          Just entry -> do
                putStrLn "Entry with this content already exists: "
                putStrLn $ getEntryIdAndFileName entry
          Nothing -> do
                    DB.modify (DB.insertWith (\id -> makeEntry id fromFile addOpts))
                    return ()
    where
      makeEntry :: Int -> String -> AddOptions -> Entry
      makeEntry id snippet addOpts =
        Entry
          { entryId = id,
            entrySnippet = snippet,
            entryFilename = addOptFilename addOpts,
            entryLanguage = addOptLanguage addOpts,
            entryDescription = addOptDescription addOpts,
            entryTags = addOptTags addOpts
          }

getEntryIdAndFileName :: Entry -> String
getEntryIdAndFileName entry = head $ lines $ show (FmtEntry entry)


-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
