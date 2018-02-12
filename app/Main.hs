{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where
import Prelude hiding (elem,notElem,lookup)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import Control.Monad.State
import Control.Monad.Except
import System.IO
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing,listDirectory)
import Options.Applicative
import Language.PureScript.Externs
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.TsdGen.Module

processModules :: FilePath -> Maybe FilePath -> [String] -> Bool -> ExceptT ModuleProcessingError IO ()
processModules inputDir outputDir modules importAll = do
  let loadOneModule = recursivelyLoadExterns inputDir . moduleNameFromString . T.pack
  (env,m) <- execStateT (mapM_ loadOneModule modules) (initEnvironment, Map.empty)
  forM_ (catMaybes $ Map.elems m) $ \ef -> do
    let moduleName = runModuleName (efModuleName ef)
    modTsd <- processLoadedModule env ef importAll
    case outputDir of
      Just outputDir -> do
        let moduleDir = outputDir </> T.unpack moduleName
        liftIO $ createDirectoryIfMissing True moduleDir
        liftIO $ TL.writeFile (moduleDir </> "index.d.ts") (TB.toLazyText modTsd)
      Nothing -> do -- write to stdout
        liftIO $ TL.putStr (TB.toLazyText modTsd)

data TsdOutput = TsdOutputDirectory FilePath | StdOutput | SameAsInput

data PursTsdGen = PursTsdGen
  { pursOutputDirectory :: FilePath
  , tsdOutput :: TsdOutput
  , importAll :: Bool
  , moduleNames :: [String]
  }

tsdOutputParser :: Parser TsdOutput
tsdOutputParser = (TsdOutputDirectory <$> strOption (long "tsd-directory" <> metavar "<dir>" <> help "Where to write .d.ts files; same as --directory by default"))
                  <|> flag' StdOutput (long "stdout" <> help "Write to stdout (for dry-run)")
                  <|> pure SameAsInput

pursTsdGen :: Parser PursTsdGen
pursTsdGen = PursTsdGen
  <$> strOption (long "directory" <> short 'd' <> metavar "<dir>" <> help "PureScript's output directory")
  <*> tsdOutputParser
  <*> switch (long "import-all" <> help "Import dependent modules even if not referenced")
  <*> (many (strArgument (metavar "<modules>" <> help "List of modules to export (all if omitted). Glob-like patterns '*' and '**' are parsed.")))

-- |
-- >>> filter (testModuleGlob "Foo.*") ["FooBar","Foo.Bar","Foo.Bar.Baz"]
-- ["Foo.Bar"]
-- >>> filter (testModuleGlob "Foo*") ["Foo.Bar","FooBar"]
-- ["FooBar"]
-- >>> filter (testModuleGlob "Foo*Bar") ["FooBar","FooBazBar","FooBaz.Bar"]
-- ["FooBar","FooBazBar"]
-- >>> filter (testModuleGlob "Foo**Bar") ["FooBar","FooBazBar","FooBaz.Bar"]
-- ["FooBar","FooBazBar","FooBaz.Bar"]
testModuleGlob :: String -> String -> Bool
testModuleGlob [] [] = True
-- '**': wildcard, including '.'
testModuleGlob ('*':'*':xs) s = any (testModuleGlob xs) (List.tails s)
-- '*': wildcard, not including '.'
testModuleGlob ('*':xs) s = any (testModuleGlob xs) (tails' s)
  where
    tails' :: String -> [String]
    tails' [] = [[]]
    tails' s@(x:xs) | x == '.' = [s]
                    | otherwise = s : tails' xs
testModuleGlob (x:xs) (y:ys) | x == y = testModuleGlob xs ys
testModuleGlob _ _ = False

main :: IO ()
main = do
  PursTsdGen{..} <- execParser opts
  allModules <- listDirectory pursOutputDirectory
  let selectedModules = case moduleNames of
                          [] -> allModules
                          m -> filter (\t -> any (flip testModuleGlob t) m) allModules
  let tsdOutputDirectory = case tsdOutput of
        TsdOutputDirectory dir -> Just dir
        StdOutput -> Nothing
        SameAsInput -> Just pursOutputDirectory
  result <- runExceptT $ processModules pursOutputDirectory tsdOutputDirectory selectedModules importAll
  case result of
    Left err -> hPutStr stderr (show err)
    Right _ -> return ()
  where
    opts = info (pursTsdGen <**> helper)
      (fullDesc
        <> progDesc "Generate .d.ts files for PureScript modules"
        <> header "purs-tsd-gen - .d.ts generator for PureScript")
