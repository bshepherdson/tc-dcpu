{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, BinaryLiterals #-}

module Main where

import Control.Applicative hiding ((<|>), many)
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Array.IO
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.List.Split (splitOn)
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
         | MacroCall String [String]
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

-- We check after parsing the identifier whether it's a known operation or not.
-- If it's not, we compile it as a macro, which retains the actual input text
-- rather than parsed Args.
pInstruction :: Parser Asm
pInstruction = do
  mnemonic <- map toUpper <$> identifier
  case S.member mnemonic knownInstructions of
    -- Regular instruction.
    True -> Op <$> pure mnemonic <*> (ws1 *> sepBy pArg pComma) <* eol
    -- Macro.
    False -> do
      ws1
      text <- many (noneOf ";\r\n")
      return $ MacroCall mnemonic (splitOn "," text)

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
  _labels :: M.Map String (Maybe Word16),  -- All labels.
  _isDirty :: Bool, -- Signals that something has changed during this pass.
  _isResolved :: Bool -- Signals that some instructions are unresolved.
}
makeLenses ''AsmState

newtype AM a = AM (StateT AsmState IO a)
  deriving (MonadState AsmState, MonadIO, Monad, Applicative, Functor)

runAM :: AM a -> AsmState -> IO AsmState
runAM (AM f) = execStateT f


collectLabels :: [Asm] -> M.Map String (Maybe Word16) -> M.Map String (Maybe Word16)
collectLabels [] m = m
collectLabels (LabelDef str:xs) m = collectLabels xs $ M.insert str Nothing m
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
  labels %= (at label .~ Just (Just j))
  maybeDirty mold j

-- Updating symbols is fine and correct, and doesn't cause dirtying.
assemble (Symbol name x) = do
  val <- resolveExpr x
  (symbols . at name) .= Just val

assemble (Include _) = liftIO $ do
  putStrLn "Can't happen: found .include at assembly time"
  exitFailure

assemble (MacroCall name args) = do
  -- Look up the macro by name - error if it doesn't exist by now.
  mmacro <- use $ macros . at name
  body <- case mmacro of
    Just body -> return body
    Nothing -> liftIO $ do
      putStrLn $ "Unknown opcode: " ++ name
      exitFailure

  -- This does a string-replace on the macro, to replace the newlines and fill
  -- in the arguments. Then it parses that new string, acquiring an [Asm], which
  -- it recursively assembles at the current location.
  let body' = replace "%n" "\n" body
  -- Replacing each argument with either its text form (%1) or its evaluated
  -- self (%e1). We won't try to evaluate the expressions unless they're
  -- demanded.
  body'' <- foldM macroArg body' $ zip [0..] args
  parsed <- case parseAsm "" body'' of
    Left err -> liftIO $ do
      putStrLn $ "Error while expanding macro " ++ name ++ ": " ++ show err
      exitFailure
    Right asm -> return asm
  -- Now compile that code right here.
  mapM_ assemble parsed



-- It must be a known operation. Look it up and run its assembler.
assemble (Op mnemonic args) = ai mnemonic args


ai :: String -> [Arg] -> AM ()
-- Format 4: ALU operations.
ai "AND" as = format4 "AND" 0 as
ai "EOR" as = format4 "EOR" 1 as
ai "ADC" as = format4 "ADC" 5 as
ai "SBC" as = format4 "SBC" 6 as
ai "ROR" as = format4 "ROR" 7 as
ai "TST" as = format4 "TST" 8 as
ai "NEG" as = format4 "NEG" 9 as
ai "CMN" as = format4 "CMN" 11 as
ai "ORR" as = format4 "ORR" 12 as
ai "MUL" as = format4 "MUL" 13 as
ai "BIC" as = format4 "BIC" 14 as
ai "MVN" as = format4 "MVN" 15 as

-- Format 1: Shifted moves
ai "LSL" [Reg rd, Reg rs, Literal x] = format1 0 rd rs x
ai "LSL" as = format4Shift "LSL" 2 as
ai "LSR" [Reg rd, Reg rs, Literal x] = format1 1 rd rs x
ai "LSR" as = format4Shift "LSR" 3 as
ai "ASR" [Reg rd, Reg rs, Literal x] = format1 2 rd rs x
ai "ASR" as = format4Shift "ASR" 4 as

-- Format 2: ADD/SUB with 3 regs or 2 regs and an immediate.
ai "ADD" [Reg rd, Reg ra, Reg rb] = emit $ 0x1800 .|. regBits [rb, ra, rd]
ai "ADD" [Reg rd, Reg ra, Literal x] = do
  i <- resolveExpr x
  checkUnsigned i 3
  emit $ 0x1c00 .|. regBits [i, ra, rd]
ai "SUB" [Reg rd, Reg ra, Reg rb] = emit $ 0x1a00 .|. regBits [rb, ra, rd]
ai "SUB" [Reg rd, Reg ra, Literal x] = do
  i <- resolveExpr x
  checkUnsigned i 3
  emit $ 0x1e00 .|. regBits [i, ra, rd]

-- Format 3: Move/compare/add/subtract immediate
ai "MOV" [Reg rd, Literal x] = format3 0 rd x
ai "MOV" as = badArgs as "MOV" [["Rd", "U8"]]

ai "CMP" [Reg rd, Literal x] = format3 1 rd x
ai "CMP" as = badArgs as "CMP" [["Rd", "U8"], ["Rd", "Rs"]]

ai "ADD" [Reg rd, Literal x] = format3 2 rd x
ai "SUB" [Reg rd, Literal x] = format3 3 rd x

-- Format 5: BX and BLX
ai "BX"  [Reg ra] = emit $ 0x4600 .|. ra
ai "BX" as = badArgs as "BX" [["Ra"]]
ai "BLX" [Reg ra] = emit $ 0x4640 .|. ra
ai "BLX" as = badArgs as "BLX" [["Ra"]]


-- Format 6: Hardware
ai "HWN" [Reg rd] = emit $ 0x4680 .|. rd
ai "HWQ" [Reg rd] = emit $ 0x46a0 .|. rd
ai "HWI" [Reg rd] = emit $ 0x46c0 .|. rd
ai "HWN" as        = badArgs as "HWN" [["Rd"]]
ai "HWQ" as        = badArgs as "HWQ" [["Rd"]]
ai "HWI" as        = badArgs as "HWI" [["Rd"]]

-- Format 7: Manipulating CPSR
ai "RFI" [] = emit $ 0x4700
ai "RSI" [] = emit $ 0x4720
ai "IFS" [] = emit $ 0x4740
ai "IFC" [] = emit $ 0x4760
ai "RFI" as  = badArgs as "RFI" [["(void)"]]
ai "RSI" as  = badArgs as "RSI" [["(void)"]]
ai "IFS" as  = badArgs as "IFS" [["(void)"]]
ai "IFC" as  = badArgs as "IFC" [["(void)"]]

ai "MRS" [Reg rd] = emit $ 0x4780 .|. rd
ai "MSR" [Reg rd] = emit $ 0x47a0 .|. rd
ai "MRS" as        = badArgs as "MRS" [["Rd"]]
ai "MSR" as        = badArgs as "MSR" [["Rd"]]

-- Format 8: PC-relative LDR
ai "LDR" [Reg rd, MemOffset PC (Literal x)] = do
  imm <- resolveExpr x
  checkUnsigned imm 8
  emit $ 0x4800 .|. (rd `shiftL` 8) .|. imm

-- Format 9: Load/store with register offset.
-- Pre-index, no writeback
ai "STR" [Reg rd, MemOffset (Reg rb) (Reg ra)] = emit $ 0x5000 .|. regBits [ra, rb, rd]
ai "LDR" [Reg rd, MemOffset (Reg rb) (Reg ra)] = emit $ 0x5800 .|. regBits [ra, rb, rd]
-- Post-increment
ai "STR" [Reg rd, Mem (Reg rb), Reg ra] = emit $ 0x5400 .|. regBits [ra, rb, rd]
ai "LDR" [Reg rd, Mem (Reg rb), Reg ra] = emit $ 0x5c00 .|. regBits [ra, rb, rd]

-- Format 10: Unused

-- Format 11: Load/store with immediate offset
ai "STR" [Reg rd, MemOffset (Reg rb) (Literal x)] = emit =<< format11 rd rb x
ai "LDR" [Reg rd, MemOffset (Reg rb) (Literal x)] = emit =<< (.|. 0x1000) <$> format11 rd rb x
ai "STR" [Reg rd, Mem (Reg rb), Literal x] = emit =<< (.|. 0x0800) <$> format11 rd rb x
ai "LDR" [Reg rd, Mem (Reg rb), Literal x] = emit =<< (.|. 0x1800) <$> format11 rd rb x
ai "STR" [Reg rd, Mem (Reg rb)] = emit $ 0x6000 .|. regBits [rb, rd]
ai "LDR" [Reg rd, Mem (Reg rb)] = emit $ 0x7000 .|. regBits [rb, rd]

-- Format 12: Unused.

-- Format 13: SP-relative load/store
ai "LDR" [Reg rd, MemOffset SP (Literal x)] = do
  imm <- resolveExpr x
  checkUnsigned imm 8
  emit $ 0x9800 .|. (rd `shiftL` 8) .|. imm
ai "STR" [Reg rd, MemOffset SP (Literal x)] = do
  imm <- resolveExpr x
  checkUnsigned imm 8
  emit $ 0x9000 .|. (rd `shiftL` 8) .|. imm

ai "LDR" as = badArgs as "LDR" [
    ["Rd", "[Rb, Ra]"],
    ["Rd", "[Rb]", "Ra"],
    ["Rd", "[Rb, U5]"],
    ["Rd", "[Rb]", "U5"],
    ["Rd", "[PC, U8]"],
    ["Rd", "[PC, U8]"]
  ]
ai "STR" as = badArgs as "STR" [
    ["Rd", "[Rb, Ra]"],
    ["Rd", "[Rb]", "Ra"],
    ["Rd", "[Rb, U5]"],
    ["Rd", "[Rb]", "U5"],
    ["Rd", "[SP, U8]"]
  ]

-- Format 14: Load address
ai "ADD" [Reg rd, PC, Literal x] = do
  imm <- resolveExpr x
  checkUnsigned imm 8
  emit $ 0xa000 .|. (rd `shiftL` 8) .|. imm
ai "ADD" [Reg rd, SP, Literal x] = do
  imm <- resolveExpr x
  checkUnsigned imm 8
  emit $ 0xa800 .|. (rd `shiftL` 8) .|. imm

-- Format 15: Adjust SP
ai "SUB" [SP, Literal x] = do
  imm <- resolveExpr x
  checkUnsigned imm 7
  emit $ 0xb080 .|. imm
ai "ADD" [SP, Literal x] = do
  imm <- resolveExpr x
  checkUnsigned imm 7
  emit $ 0xb000 .|. imm

-- Format 16: Push/pop registers
ai "PUSH" [RegList rs lr] = emit $ 0xb400 .|. regListBits rs .|. (if lr then 0x0100 else 0)
ai "POP"  [RegList rs pc] = emit $ 0xbc00 .|. regListBits rs .|. (if pc then 0x0100 else 0)
ai "PUSH" as = badArgs as "PUSH" [["{ Ra, Rb, ... [, LR] }"]]
ai "POP" as = badArgs as "PUSH" [["{ Ra, Rb, ... [, PC] }"]]

-- Format 17: Multiple load/store
ai "STMIA" [Reg _, RegList _ True] = multistoreBadPCLR
ai "LDMIA" [Reg _, RegList _ True] = multistoreBadPCLR
ai "STMIA" [Reg rb, RegList rs False] = emit $ 0xc000 .|. regListBits rs .|. (rb `shiftL` 8)
ai "LDMIA" [Reg rb, RegList rs False] = emit $ 0xc800 .|. regListBits rs .|. (rb `shiftL` 8)
ai "STMIA" as = badArgs as "STMIA" [["Rd", "{ Ra, Rb, ... }"]]
ai "LDMIA" as = badArgs as "LDMIA" [["Rd", "{ Ra, Rb, ... }"]]

-- Format 18: Conditional branches
ai "BEQ" [LabelUse lb] = condBranch  0 lb
ai "BEQ" as             = badArgs as "BEQ" [["label"]]
ai "BNE" [LabelUse lb] = condBranch  1 lb
ai "BNE" as             = badArgs as "BNE" [["label"]]
ai "BCS" [LabelUse lb] = condBranch  2 lb
ai "BCS" as             = badArgs as "BCS" [["label"]]
ai "BCC" [LabelUse lb] = condBranch  3 lb
ai "BCC" as             = badArgs as "BCC" [["label"]]
ai "BMI" [LabelUse lb] = condBranch  4 lb
ai "BMI" as             = badArgs as "BMI" [["label"]]
ai "BPL" [LabelUse lb] = condBranch  5 lb
ai "BPL" as             = badArgs as "BPL" [["label"]]
ai "BVS" [LabelUse lb] = condBranch  6 lb
ai "BVS" as             = badArgs as "BVS" [["label"]]
ai "BVC" [LabelUse lb] = condBranch  7 lb
ai "BVC" as             = badArgs as "BVC" [["label"]]
ai "BHI" [LabelUse lb] = condBranch  8 lb
ai "BHI" as             = badArgs as "BHI" [["label"]]
ai "BLS" [LabelUse lb] = condBranch  9 lb
ai "BLS" as             = badArgs as "BLS" [["label"]]
ai "BGE" [LabelUse lb] = condBranch 10 lb
ai "BGE" as             = badArgs as "BGE" [["label"]]
ai "BLT" [LabelUse lb] = condBranch 11 lb
ai "BLT" as             = badArgs as "BLT" [["label"]]
ai "BGT" [LabelUse lb] = condBranch 12 lb
ai "BGT" as             = badArgs as "BGT" [["label"]]
ai "BLE" [LabelUse lb] = condBranch 13 lb
ai "BLE" as             = badArgs as "BLE" [["label"]]

-- TODO: This isn't quite right. SWIs don't need the # tag.
ai "SWI" [Literal x] = do
  imm <- resolveExpr x
  checkUnsigned imm 8
  emit $ 0xdf00 .|. imm

ai "SWI" as = badArgs as "SWI" [["U8"]]


ai "B"   [LabelUse lb] = do
  imm <- relativeLabel lb
  checkSigned imm 11
  emit $ 0xe000 .|. (fromIntegral imm .&. 0x7ff)

ai "B" as = badArgs as "B" [["label"]]

-- Format 21 - Never assembled. It's the short form absolute BL. We always use
-- the long form.
-- TODO: Include this, it's faster and smaller, when it works.
ai "BL" [LabelUse lb] = do
  imm <- resolveExpr lb
  emit $ 0xf400 .|. (imm `shiftR` 8)
  emit $ 0xf000 .|. (imm .&. 0xff)
ai "BL" as = badArgs as "BL" [["label"]]

ai "ADD" as = badArgs as "ADD" [
    ["Rd", "Ra", "Rb"],
    ["Rd", "Ra", "U3"],
    ["Rd", "U8"],
    ["Rd", "PC", "U8"],
    ["Rd", "SP", "U8"],
    ["SP", "U7"]
  ]

ai "SUB" as = badArgs as "SUB" [
    ["Rd", "Ra", "Rb"],
    ["Rd", "Ra", "U3"],
    ["Rd", "U8"],
    ["SP", "U7"]
  ]


ai op as = liftIO $ do
  putStrLn $ "Can't happen: Unrecognized op: " ++ op ++ " " ++ intercalate ", " (map show as)
  exitFailure


format1 :: Word16 -> Word16 -> Word16 -> Expr -> AM ()
format1 op rd rs x = do
  imm <- resolveExpr x
  checkUnsigned imm 5
  emit $ 0 .|. (op `shiftL` 11) .|. (imm `shiftL` 6) .|. regBits [rs, rd]

format3 :: Word16 -> Word16 -> Expr -> AM ()
format3 op rd expr = do
  imm <- resolveExpr expr
  checkUnsigned imm 8
  emit $ 0x2000 .|. (op `shiftL` 11) .|. (rd `shiftL` 8) .|. imm

-- For the simple ALU operations of format 4, of which there are many.
format4 :: String -> Word16 -> [Arg] -> AM ()
format4 _ opcode [Reg rd, Reg rs] = emit $ 0x4000 .|. (opcode `shiftL` 6) .|. (rs `shiftL` 3) .|. rd
format4 name _ as = badArgs as name [["Rd", "Rs"]]

format4Shift :: String -> Word16 -> [Arg] -> AM ()
format4Shift _ op [Reg rd, Reg rs] = emit $ 0x4000 .|. (op `shiftL` 6) .|. regBits [rs, rd]
format4Shift name _ as = badArgs as name [["Rd", "Rs"], ["Rd", "Rs", "U5"]]

format11 :: Word16 -> Word16 -> Expr -> AM Word16
format11 rd rb x = do
  imm <- resolveExpr x
  checkUnsigned imm 5
  return $ 0x6000 .|. (imm `shiftL` 6) .|. regBits [rb, rd]


-- Emits the list of registers with their bits shifted along. The first item in
-- the list is shifted up the most; the last in the list lands at bits 0-2.
regBits :: [Word16] -> Word16
regBits = foldl' (\c r -> (c `shiftL` 3) .|. r) 0

regListBits :: [Word16] -> Word16
regListBits = foldl' (\c b -> c .|. bit (fromIntegral b)) 0

condBranch :: Word16 -> Expr -> AM ()
condBranch op x = do
  -- It's relative not to the current location, but the next one, since that's
  -- where PC will be pointing.
  offset <- relativeLabel x
  emit $ 0xd000 .|. (op `shiftL` 8) .|. fromIntegral (0xff .&. offset)


relativeLabel :: Expr -> AM Int16
relativeLabel x = do
  target <- resolveExpr x
  base <- (+1) <$> use i
  return $ if target < base
    then -(fromIntegral (base - target))
    else fromIntegral (base - target)


checkUnsigned :: Word16 -> Int -> AM ()
checkUnsigned val bits | val < bit bits = return ()
                       | otherwise = liftIO $ do
                          putStrLn $ "Unsigned immediate value too big: " ++
                             show val ++ " doesn't fit in " ++ show bits ++ " bits"
                          exitFailure

checkSigned :: Int16 -> Int -> AM ()
checkSigned val bits | ((val `shiftL` (16-bits)) `shiftR` (16-bits)) == val = return ()
                     | otherwise = liftIO $ do
                        putStrLn $ "Signed immediate value too big: " ++
                            show val ++ " doesn't fit in " ++ show bits ++ " bits"
                        exitFailure


badArgs :: [Arg] -> String -> [[String]] -> AM()
badArgs as op allowed = liftIO $ do
  putStrLn $ "Bad arguments for " ++ op ++ ": " ++ show as ++ "\nAllowed patterns:"
  mapM_ (\args -> putStrLn $ "\t" ++ op ++ " " ++ intercalate ", " args) allowed
  exitFailure

multistoreBadPCLR :: AM ()
multistoreBadPCLR = liftIO $ do
  putStrLn $ "PC and LR only allowed in register lists for PUSH and POP, not LDMIA and STMIA"
  exitFailure

{-
data Asm = Data [Expr]
         | Macro String String
         | LabelDef String
         | Symbol String Expr
         | Op String [Arg]
         | MacroCall String [String]
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


replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace target rep s | and (zipWith (==) target s) = rep ++ drop (length target) s
                     | otherwise = replace target rep (tail s)


macroArg :: String -> (Int, String) -> AM String
macroArg body (index, arg) = do
  let body' = replace ("%" ++ show index) arg body -- Simple text replacement.
  -- Don't try to expand the evaluated ones unless it's needed.
  let evalTarget = "%e" ++ show index
  case evalTarget `isInfixOf` body' of
    False -> return body'
    True  -> do
      case parse pValue "" arg of
        Left err -> liftIO $ do
          putStrLn $ "Failed to parse macro argument '" ++ arg ++ "' " ++ show err
          exitFailure
        Right a -> do
          val <- resolveExpr a
          return $ replace evalTarget (show val) body'


exprAsString :: Expr -> String
exprAsString (Number n) = show n
exprAsString (Ident s) = s




emit :: Word16 -> AM ()
emit x = do
  j <- use i
  i %= (+1)
  r <- use rom
  liftIO $ writeArray r j x

dirty :: AM ()
dirty = isDirty .= True

maybeDirty :: Eq a => (Maybe (Maybe a)) -> a -> AM ()
maybeDirty Nothing _ = dirty
maybeDirty (Just Nothing) _ = dirty
maybeDirty (Just (Just x)) y | x == y = return ()
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
    (Just (Just l), _) -> return l
    (Just Nothing,  _) -> unresolved >> return 0
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
  let j = _i st
  putStrLn $ "Done, " ++ show j ++ " words"
  rs <- genericTake j <$> getElems (_rom st)
  mapM_ (putStrLn . flip showHex "") rs

