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
runP_ = Run <$> execP <*> srcP <*> outP <*> watchP
  where execP
           = strArgument
           $ metavar "EXEC"
          <> help "path to the compiled Descript executable"
        srcP
           = strArgument
           $ metavar "SRC"
          <> help "path to the input source code"
        outP
           = optional
           $ strOption
           $ metavar "OUT"
          <> long "output"
          <> short 'o'
          <> help "path for the output source code, defaults to (SRC).(EXT) (where (EXT) is determined by output, and must not be the same as input)"
        watchP
           = switch
           $ long "watch"
          <> short 'w'
          <> help "run every time the input source changes"

actionInfo :: InfoMod a
actionInfo
   = fullDesc
  <> progDesc "treescript compile, treescript run, treescript serve"
  <> header "descript - compile and run Descript source code, start the Descript language server"
  <> footer "This is the interface for the Descript programming language: a DSL to manipulate syntax of other languages.\n\
            \It can compile Descript source code into executables and run those executables.\n\
            \It also includes a language server ('descript serve'), which adds Descript support to editors like VSCode."

serveInfo :: InfoMod a
serveInfo
   = fullDesc
  <> progDesc "start the language server"
  <> header "descript serve - start the language server"
  <> footer "You probably don't want to call this yourself - this command should be called by language clients, \
            \such as the 'descript-lang' VSCode plugin, once they want to edit Descript files. \
            \Only need one server per system, even if there are multiple workspaces and clients."

compileInfo :: InfoMod a
compileInfo
   = fullDesc
  <> progDesc "compile a Descript source file (.tscr) into an executable (.tprg)"
  <> header "descript compile - compile a Descript source file (.tscr) into an executable (.tprg)"
  <> footer "In order to be applied to source code, a Descript program needs to be compiled into an executable.\
            \You can't run this executable on the source code itself (unless compiling with -s) - instead use 'descript run'."

runInfo :: InfoMod a
runInfo
   = fullDesc
  <> progDesc "run a Descript executable"
  <> header "descript run - run a Descript executable"
  <> footer "The executable will transform the source code in the input file"
