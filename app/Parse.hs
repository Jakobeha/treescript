-- | Parses actions from the command line.
module Parse
  ( prompt
  )
where

import           Action
import           Options.Applicative

prompt :: IO Action
prompt = execParser actionP

actionP :: ParserInfo Action
actionP = info (helper <*> actionP_) actionInfo

serveP :: ParserInfo ()
serveP = info (helper <*> serveP_) serveInfo

compileP :: ParserInfo CompileAction
compileP = info (helper <*> compileP_) compileInfo

evalP :: ParserInfo EvalAction
evalP = info (helper <*> evalP_) evalInfo

serveP_ :: Parser ()
serveP_ = pure ()

actionP_ :: Parser Action
actionP_ = subparser $ mconcat actionCommands

actionCommands :: [Mod CommandFields Action]
actionCommands =
  [ command "serve" $ ActionServe <$ serveP
  , command "compile" $ ActionCompile <$> compileP
  , command "eval" $ ActionEval <$> evalP
  ]

compileP_ :: Parser CompileAction
compileP_ = mkCompileAction <$> srcP <*> outP <*> watchP
 where
  srcP = strArgument $ metavar "SRC" <> help "path to the source code"
  outP =
    optional $ strOption $ metavar "OUT" <> long "output" <> short 'o' <> help
      "path for the compiled executable, defaults to (SRC).tprg"
  watchP = switch $ long "watch" <> short 'w' <> help
    "eval every time the source changes"


evalP_ :: Parser EvalAction
evalP_ = mkEvalAction <$> prgP <*> inpP <*> outP <*> watchP
 where
  prgP = strArgument $ metavar "PRG" <> help "path to the TreeScript"
  inpP = strArgument $ metavar "INP" <> help "path to the input code"
  outP =
    optional
      $  strOption
      $  metavar "OUT"
      <> long "output"
      <> short 'o'
      <> help
           "path for the compiled executable, defaults to (SRC), so the file will be overwritten"
  watchP = switch $ long "watch" <> short 'w' <> help
    "eval every time the source changes"

actionInfo :: InfoMod a
actionInfo =
  fullDesc
    <> progDesc "treescript compile, treescript eval, treescript serve"
    <> header "treescript - simple syntax transformation tool"
    <> footer
         "TreeScript is a DSL to manipulate syntax of other languages.\n\
            \This is the CLI for TreeScript.\n\
            \It also includes a language server ('treescript serve'), which adds TreeScript support to editors like VSCode."

serveInfo :: InfoMod a
serveInfo =
  fullDesc
    <> progDesc "start the language server"
    <> header "treescript serve - start the language server"
    <> footer
         "You probably don't want to call this yourself - this command should be called by language clients, \
            \such as the 'treescript-vscode' plugin, once they want to edit TreeScript files. \
            \Only need one server per system, even if there are multiple workspaces and clients."

compileInfo :: InfoMod a
compileInfo =
  fullDesc
    <> progDesc
         "compile a TreeScript source file (.tscr) into an executable (.tprg)"
    <> header
         "treescript compile - compile a TreeScript source file (.tscr) into an executable (.tprg)"
    <> footer "Compiled TreeScript programs run faster."

evalInfo :: InfoMod a
evalInfo =
  fullDesc
    <> progDesc "apply TreeScript to source code"
    <> header "treescript eval - apply TreeScript to source code"
    <> footer "Evalautes the TreeScript program on the given input."
