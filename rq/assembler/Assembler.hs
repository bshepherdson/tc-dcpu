{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Main where

import Instructions

import Control.Applicative hiding ((<|>), many)
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Array.IO
import Data.Char
import Data.Int
import Data.List.Split
import Data.Word
import Numeric
import System.Environment
import System.Exit
import Text.Parsec hiding (labels)

knownInstructions :: S.Set String
knownInstructions = S.fromList[
	"ADC", "ADD", "AND", "ASR", "BIC", "CMN", "CMP", "EOR", "LSL", "LSR",
	"MOV", "MUL", "MVN", "NEG", "ORR", "ROR", "SBC", "SUB", "TST", "B",
	"BL", "BX", "BLX", "BEQ", "BNE", "BCS", "BCC", "BMI", "BPL", "BVS",
	"BVC", "BHI", "BLS", "BGE", "BLT", "BGT", "BLE", "HWN", "HWQ", "HWI",
	"SWI", "RFI", "RSI", "IFS", "IFC", "MRS", "MSR", "LDR", "STR", "PUSH",
	"POP", "STMIA", "LDMIA"]


type Parser a = Parsec String () a

data Asm = Data [Expr]
         | Macro String String
         | LabelDef String
         | Symbol String Expr
         | Op String [Arg]
         | Include String
  deriving (Show)

data Arg = Reg Word16
         | PC
         | SP
         | Mem Arg
         | MemOffset Arg Arg
         | Literal Expr
         | LabelUse Expr
         | RegList [Word16] Bool
  deriving (Show)

-- TODO: Add more complex expressions like sums.
data Expr = Number Word16
          | Ident String
  deriving (Show)

pSTART :: Parser [Asm]
pSTART = ws *> endBy pItem ws <* eof

pItem :: Parser Asm
pItem = pDirective <|> pLabelDef <|> pInstruction

ws :: Parser ()
ws = skipMany (pLineComment <|> (oneOf " \t\r\n" >> return ()))

ws_ :: Parser ()
ws_ = skipMany (oneOf " \t")

ws1 :: Parser ()
ws1 = oneOf " \t" *> ws_

eol :: Parser ()
eol = ws_ >> (try pLineComment <|> (endOfLine >> return ()))

pLineComment :: Parser ()
pLineComment = char ';' >> skipMany (noneOf "\r\n") >> endOfLine >> return()

pDirective :: Parser Asm
pDirective = (try pDAT <|> try pDEFINE <|> try pMACRO <|> try pINCLUDE <|>
    try pRESERVE <|> pFILL) <* eol

charIC :: Char -> Parser Char
charIC c = (char (toLower c) <|> char (toUpper c)) >> return c

stringIC :: String -> Parser String
stringIC = mapM charIC

pComma :: Parser ()
pComma = ws_ >> char ',' >> ws_

pDAT, pDEFINE, pMACRO, pINCLUDE, pRESERVE, pFILL :: Parser Asm
pDAT = fmap (Data . concat) $ stringIC ".dat" *> ws1 *> sepBy pDatValue pComma
pRESERVE = fmap (Data . flip replicate (Number 0) . fromIntegral) $
    stringIC ".reserve" *> ws1 *> number
pFILL = fmap Data $ replicate <$>
    (fmap fromIntegral $ stringIC ".fill" *> ws1 *> number)
    <*> (pComma *> pNumber)

pDEFINE = Symbol <$> (header *> ws1 *> identifier) <*> (pComma *> pValue)
  where header = try (stringIC ".define") <|> stringIC ".def"

-- TODO: Interpret macros somewhere. Here? They should be turned into functions
-- probably :: [String] -> [Asm].
pMACRO = Macro <$> (stringIC ".macro" *> ws1 *> identifier) <*>
    (pComma *> many (noneOf "\r\n"))

pINCLUDE = Include <$> (stringIC ".include" *> ws1 *> stringLit)

pDatValue :: Parser [Expr]
pDatValue = pString <|> ((:[]) <$> pValue)

pString :: Parser [Expr]
pString = fmap (map $ Number . fromIntegral . ord) $ stringLit

stringLit :: Parser String
stringLit = between (char '"') (char '"') (many $ noneOf "\"")

pValue :: Parser Expr
pValue = choice [pNumber, pIdent]

pNumber :: Parser Expr
pNumber = Number <$> number

number :: Parser Word16
number = choice [try pHexNumber, pDecimalNumber]

pHexNumber :: Parser Word16
pHexNumber = fmap toHex $ stringIC "0x" *> many1 hexDigit
  where toHex = fst . head . readHex

pDecimalNumber :: Parser Word16
pDecimalNumber = (fromIntegral . read) <$> many1 digit

pIdent :: Parser Expr
pIdent = Ident <$> identifier

identifier :: Parser String
identifier = (:) <$> letter_ <*> many alphaNum_
  where letter_   = char '_' <|> letter
        alphaNum_ = char '_' <|> alphaNum


pLabelDef :: Parser Asm
pLabelDef = LabelDef <$> (char ':' *> identifier)

pInstruction :: Parser Asm
pInstruction = (Op <$> identifier <*> (ws1 *> sepBy pArg pComma)) <* eol

-- SimpleArgs are arguments that are not memory references.
-- This keeps them from recursing.
pSimpleArg :: Parser Arg
pSimpleArg = try pPC <|> try pSP <|> try pReg <|> try pLiteral <|>
    (LabelUse . Ident <$> identifier)
  where pLiteral = Literal <$> (char '#' *> pValue)


pReg, pPC, pSP :: Parser Arg
pReg = (Reg . fromIntegral . read . (:[])) <$> (char 'r' *> digit)
pPC = stringIC "PC" >> return PC
pSP = stringIC "SP" >> return SP

pArg :: Parser Arg
pArg = try pMemRef <|> try pRegList <|> pSimpleArg

pMemRef :: Parser Arg
pMemRef = between (char '[' >> ws_) (ws_ >> char ']') $
    try (MemOffset <$> pSimpleArg <*> (pComma *> pSimpleArg)) <|>
    (Mem <$> pSimpleArg)

pRegList :: Parser Arg
pRegList = do
    regs <- between (char '{' >> ws_) (ws_ >> char '}') $ sepBy reg (try pComma)
    f (RegList [] False) regs
  where reg = pPC <|> (stringIC "LR" >> return SP) <|> pReg
        f (RegList rs False) (SP:xs) = f (RegList rs True) xs
        f (RegList rs False) (PC:xs) = f (RegList rs True) xs
        f (RegList rs b) ((Reg r):xs) = f (RegList (r:rs) b) xs
        f rl [] = return rl
        f _ _ = fail "Bad register list"


parseAsm :: String -> String -> Either ParseError [Asm]
parseAsm sourceName contents = parse pSTART sourceName contents


parseFile :: String -> IO [Asm]
parseFile filename = do
  content <- readFile filename
  r <- case parseAsm filename content of
    Left err -> print err >> exitFailure
    Right r  -> return r

  -- Skim through r looking for Includes, and expand them recursively.
  concatMapM expandIncludes r

expandIncludes :: Asm -> IO [Asm]
expandIncludes (Include file) = parseFile file
expandIncludes a = return [a]


concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat $ mapM f xs



-- Emitted as instructions when they can't be resolved yet.
-- This is an impossible instruction in an unused block.
placeholderInstruction :: Word16
placeholderInstruction = 0x8ead

data AsmState = AsmState {
  _i :: Word16,
  _rom :: IOUArray Word16 Word16,
  _symbols :: M.Map String Word16, -- Resolved symbols.
  _macros :: M.Map String String,  -- Known macros.
  _labels :: M.Map String Word16,  -- All labels.
  _isDirty :: Bool, -- Signals that something has changed during this pass.
  _isResolved :: Bool -- Signals that some instructions are unresolved.
}
makeLenses ''AsmState

newtype AM a = AM (StateT AsmState IO a)
  deriving (MonadState AsmState, MonadIO, Monad, Applicative, Functor)

runAM :: AM a -> AsmState -> IO AsmState
runAM (AM f) = execStateT f


collectLabels :: [Asm] -> M.Map String Word16 -> M.Map String Word16
collectLabels [] m = m
collectLabels (LabelDef str:xs) m = collectLabels xs $ M.insert str 0 m
collectLabels (_:xs) m = collectLabels xs m


-- Labels have been collected, so every symbol should be identifiable.
resolveAssembly :: [Asm] -> AM ()
resolveAssembly asm = do
  -- Preserving the labels, but not the symbols.
  isDirty .= False
  isResolved .= True
  macros .= M.empty
  symbols .= M.empty
  i .= 0

  mapM_ assemble asm
  d <- gets _isDirty
  r <- gets _isResolved
  when (d || not r) $ resolveAssembly asm

assemble :: Asm -> AM ()
assemble (Data xs) = mapM_ assembleData xs
assemble (Macro name code) = macros %= (at name .~ Just code)

assemble (LabelDef label) = do
  j <- use i
  mold <- (^. at label) <$> use labels
  labels %= (at label .~ Just j)
  maybeDirty mold j

-- Updating symbols is fine and correct, and doesn't cause dirtying.
assemble (Symbol name x) = do
  val <- resolveExpr x
  (symbols . at name) .= Just val

assemble (Include _) = do
  putStrLn "Can't happen: found .include at assembly time"
  exitFailure

assemble (Op mnemonic args) = do
  -- The operation might be a macro, a known op, or a bad symbol, in that order.
  mmacro <- use $ macros . at mnemonic
  case (mmacro, S.member mnemonic knownInstructions) of
    (Just (Macro _ body), _) -> assembleMacro body args
    (_, True)   -> assembleInstruction mnemonic args
    _           -> do
      putStrLn $ "Unknown operation: " ++ mnemonic
      exitFailure


{-
data Asm = Data [Expr]
         | Macro String String
         | LabelDef String
         | Symbol String Expr
         | Op String [Arg]
         | Include String
  deriving (Show)

data Arg = Reg Word16
         | PC
         | SP
         | Mem Arg
         | MemOffset Arg Arg
         | Literal Expr
         | LabelUse Expr
         | RegList [Word16] Bool
  deriving (Show)

data Expr = Number Word16
          | Ident String
-}


replace :: (Eq a) -> [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace target rep s | and (zipWith (==) target s = rep ++ drop (length target) s
                     | otherwise = replace target rep (tail s)

-- This does a string-replace on the macro, to replace the newlines and fill in
-- the arguments. Then it parses that new string, acquiring an [Asm], which it
-- recursively assembles at the current location.
assembleMacro :: String -> [Arg] -> AM ()
assembleMacro body args = do
  let body' = replace "%n" "\n" body
  -- Replacing each argument with either its text form (%1) or its evaluated
  -- self (%e1). We won't try to evaluate the expressions unless they're
  -- demanded.
  body'' <- foldM macroArg body' $ zip [0..] args


macroArg :: String -> Arg -> AM String
macroArg body arg = do
  let text = argAsCode arg

  -- START HERE: Macros and code are different. We should detect which is which
  -- at parse time. Macro invocations should have [String], and we only try to
  -- parse and evaluate them here if we find uses of %e1.









emit :: Word16 -> AM ()
emit x = do
  j <- use i
  r <- use rom
  liftIO $ writeArray r j x

dirty :: AM ()
dirty = isDirty .= True

maybeDirty :: Eq a => (Maybe a) -> a -> AM ()
maybeDirty Nothing _ = dirty
maybeDirty (Just x) y | x == y = return ()
                      | otherwise = dirty

unresolved :: AM ()
unresolved = isResolved .= False

assembleData :: Expr -> AM ()
assembleData x = resolveExpr x >>= emit

resolveExpr :: Expr -> AM Word16
resolveExpr (Number n) = return n
resolveExpr (Ident  s) = do
  ml <- (^. at s) <$> use labels
  ms <- (^. at s) <$> use symbols
  case (ml, ms) of
    (Just l, _) -> return l
    (_, Just s) -> return s
    _           -> liftIO $ do
      putStrLn $ "Unknown symbol: '" ++ s ++ "'"
      exitFailure


main :: IO ()
main = do
  -- Read the list of input files.
  files <- getArgs
  when (length files == 0) $ do
    putStrLn "Error: No input files."
    exitFailure
  asm <- concatMapM parseFile files

  -- The assembler proceeds by repeatedly making a pass of assembly with isDirty
  -- false and isResolved true. Anything that causes a change makes it dirty,
  -- and any instruction that doesn't have its needs met emits the impossible
  -- instruction and marks unresolved.
  output <- newArray (0, 0xffff) 0
  st <- runAM (resolveAssembly asm) AsmState{
    _i = 0,
    _rom = output,
    _symbols = M.empty,
    _macros = M.empty,
    _labels = collectLabels asm M.empty,
    _isDirty = True,
    _isResolved = False
  }
  putStrLn "Done"
