-- | Parses actions from the command line.
module Parse
  ( prompt
  ) where

import Action
import Options.Applicative

prompt :: IO Action
prompt = execParser actionP

actionP :: ParserInfo Action
actionP = info (helper <*> actionP_) actionInfo

serveP :: ParserInfo ()
serveP = info (helper <*> serveP_) serveInfo

compileP :: ParserInfo Compile
compileP = info (helper <*> compileP_) compileInfo

runP :: ParserInfo Run
runP = info (helper <*> runP_) runInfo

serveP_ :: Parser ()
serveP_ = pure ()

actionP_ :: Parser Action
actionP_ = subparser $ mconcat actionCommands

actionCommands :: [Mod CommandFields Action]
actionCommands =
  [ command "serve" $ ActionServe <$ serveP
  , command "compile" $ ActionCompile <$> compileP
  , command "run" $ ActionRun <$> runP
  ]

compileP_ :: Parser Compile
compileP_ = mkCompile <$> srcP <*> outP <*> watchP
  where srcP
           = strArgument
           $ metavar "SRC"
          <> help "path to the source code"
        outP
           = optional
           $ strOption
           $ metavar "OUT"
          <> long "output"
          <> short 'o'
          <> help "path for the compiled executable, defaults to (SRC).tprg"
        watchP
           = switch
           $ long "watch"
          <> short 'w'
          <> help "run every time the source changes"


runP_ :: Parser Run
runP_ = Run <$> execP <*> many argP
  where execP
           = strArgument
           $ metavar "EXEC"
          <> help "path to the source code"
        argP
           = strArgument
           $ metavar "ARG"
          <> help "argument to be passed to the executable"

actionInfo :: InfoMod a
actionInfo
   = fullDesc
  <> progDesc "treescript compile, treescript run, treescript serve"
  <> header "treescript - compile and run TreeScript source code, start the TreeScript language server"
  <> footer "This is the interface for the TreeScript programming language: a DSL to manipulate syntax of other languages.\n\
            \It can compile TreeScript source code into executables and run those executables.\n\
            \It also includes a language server ('treescript serve'), which adds TreeScript support to editors like VSCode."

serveInfo :: InfoMod a
serveInfo
   = fullDesc
  <> progDesc "start the language server"
  <> header "treescript serve - start the language server"
  <> footer "You probably don't want to call this yourself - this command should be called by language clients, \
            \such as the 'treescript-vscode' plugin, once they want to edit TreeScript files. \
            \Only need one server per system, even if there are multiple workspaces and clients."

compileInfo :: InfoMod a
compileInfo
   = fullDesc
  <> progDesc "compile a TreeScript source file (.tscr) into an executable (.tprg)"
  <> header "treescript compile - compile a TreeScript source file (.tscr) into an executable (.tprg)"
  <> footer "In order to be applied to source code, a TreeScript program needs to be compiled into an executable.\
            \You can't run this executable on the source code itself (unless compiling with -s) - instead use 'treescript run'."

runInfo :: InfoMod a
runInfo
   = fullDesc
  <> progDesc "run a TreeScript source file"
  <> header "treescript run - run a TreeScript source file"
  <> footer "TODO"
