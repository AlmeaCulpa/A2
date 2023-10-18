{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances
import Parser
import Helpers
import GHC.Base
import Data.Foldable (asum)
import Data.List (intercalate)
import Debug.Trace (trace, traceShow)

data ADT = Expression Expr | Statement [Stat]
  deriving Show

-- Current issues:
  -- true2 parses as JSTrue and 2 instead of a variable name
  -- jsConstCall being within jsBase might cause problems. Unsure.

data Expr
  = JSInt Int
  | JSString String
  | JSTrue
  | JSFalse
  | JSList [Expr]
  | JSRedundantBrackets Expr
  | JSUnary Op Expr
  | JSBinary Op Expr Expr
  | JSTernary Expr Expr Expr
  | JSVarName String
  | JSExprCall String [Expr]
  deriving (Show)

data Op = JSPlus 
  | JSMinus 
  | JSTimes 
  | JSDivide 
  | JSPower 
  | JSAnd 
  | JSOr 
  | JSEqual 
  | JSNotEqual 
  | JSLessThan 
  | JSGreaterThan 
  | JSNot
  deriving (Show)

data Stat
  = JSConst Expr Expr
  | JSConstBlock [Stat]
  | JSCodeBlock [Stat]
  | JSIf Expr Stat
  | JSIfElse Expr Stat Stat
  | JSStatCall String [Expr]
  | JSFunctionDef String [Expr] Stat
  | JSReturn Expr
  | JSTailRecursiveFunction String [Expr] Stat
  deriving (Show)

parseExpression :: Parser ADT
parseExpression = Expression <$> jsExpr

parseStatement :: Parser ADT
parseStatement = Statement <$> some jsStat

parseFull :: Parser ADT
parseFull = asum [parseStatement, parseExpression]

prettyFull :: ADT -> String
prettyFull (Expression e) = prettyExpr e
prettyFull (Statement s) = intercalate "\n" (prettyStat <$> s)

jsBase :: Parser Expr
jsBase = asum [jsInt, jsString, jsBool, jsList, jsVarName]

jsInt :: Parser Expr
jsInt = JSInt <$> tok int

jsRedundantBrackets :: Parser Expr
jsRedundantBrackets = do
  charTok '('
  expr <- jsExpr
  charTok ')'
  pure $ JSRedundantBrackets expr

quoteString :: Parser String
quoteString = spaces *> charTok '\"' *> many (tok $ isNot '\"') <* charTok '\"'

jsString :: Parser Expr
jsString = JSString <$> quoteString

jsTrue :: Parser Expr
jsTrue = JSTrue <$ stringTok "true"

jsFalse :: Parser Expr
jsFalse = JSFalse <$ stringTok "false"

jsBool :: Parser Expr
jsBool = asum [jsTrue, jsFalse]

jsList :: Parser Expr
jsList = JSList <$> do
  charTok '['
  values <- sepBy (tok jsExpr) (is ',' <* spaces)
  charTok ']'
  pure values

jsOp :: Parser Expr
jsOp = do
  charTok '('
  expr <- asum [jsUnary, jsBinary, jsTernary]
  charTok ')'
  pure expr

jsUnary :: Parser Expr
jsUnary = do
  op <- asum [jsNot]
  spaces
  expr <- jsExpr 
  pure $ JSUnary op expr

jsNot :: Parser Op
jsNot = JSNot <$ stringTok "!"

jsBinary :: Parser Expr
jsBinary = do
  e1 <- asum [jsExprCall, jsExpr]
  spaces
  op <- asum [jsPlus, jsMinus, jsTimes, jsDivide, jsPower, 
    jsAnd, jsOr, jsEqual, jsNotEqual, jsLessThan, jsGreaterThan]
  spaces
  e2 <- asum [jsExprCall, jsExpr]
  pure $ JSBinary op e1 e2

jsPlus :: Parser Op
jsPlus = JSPlus <$ stringTok "+"

jsMinus :: Parser Op
jsMinus = JSMinus <$ stringTok "-"

jsTimes :: Parser Op
jsTimes = JSTimes <$ stringTok "*"

jsDivide :: Parser Op
jsDivide = JSDivide <$ stringTok "/"

jsPower :: Parser Op
jsPower = JSPower <$ stringTok "**"

jsAnd :: Parser Op
jsAnd = JSAnd <$ stringTok "&&"

jsOr :: Parser Op
jsOr = JSOr <$ stringTok "||"

jsEqual :: Parser Op
jsEqual = JSEqual <$ stringTok "==="

jsNotEqual :: Parser Op
jsNotEqual = JSNotEqual <$ stringTok "!=="

jsLessThan :: Parser Op
jsLessThan = JSLessThan <$ stringTok "<"

jsGreaterThan :: Parser Op
jsGreaterThan = JSGreaterThan <$ stringTok ">"

jsTernary :: Parser Expr
jsTernary = do
  e1 <- jsExpr
  stringTok "?"
  e2 <- jsExpr
  stringTok ":"
  e3 <- jsExpr
  pure $ JSTernary e1 e2 e3

lengthCheck :: String -> Bool
lengthCheck = (> 42) . length

newlineCheck :: String -> Bool
newlineCheck = elem '\n'

multiLineCheck :: String -> Bool
multiLineCheck = liftA2 (||) lengthCheck newlineCheck

jsExpr :: Parser Expr
jsExpr = spaces *> asum [jsOp, jsRedundantBrackets, jsBase]

parseExerciseA :: Parser ADT
parseExerciseA = Expression <$> jsExpr

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA (Expression e) = prettyExpr e

prettyExpr :: Expr -> String
prettyExpr (JSInt int) = show int
prettyExpr (JSString str) = show str
prettyExpr JSTrue = "true"
prettyExpr JSFalse = "false"
prettyExpr (JSList list) = "[" ++ intercalate ", " (prettyExpr <$> list) ++ "]"
prettyExpr (JSUnary op e) =  "(" ++ prettyOp op ++ prettyExpr e ++ ")"
prettyExpr (JSBinary op e1 e2) = "(" ++ prettyExpr e1 ++ prettyOp op ++ prettyExpr e2 ++ ")"
prettyExpr (JSTernary e1 e2 e3) = prettyTernary e1 e2 e3
prettyExpr (JSVarName str) = str
prettyExpr (JSExprCall name args) = name ++ "(" ++ intercalate ", " (prettyExpr <$> args) ++ ")"
prettyExpr (JSRedundantBrackets e) = "(" ++ prettyExpr e ++ ")"

prettyOp :: Op -> String
prettyOp JSNot = "!"
prettyOp JSPlus = " + "
prettyOp JSMinus = " - "
prettyOp JSTimes = " * "
prettyOp JSDivide = " / "
prettyOp JSPower = " ** "
prettyOp JSAnd = " && "
prettyOp JSOr = " || "
prettyOp JSEqual = " === "
prettyOp JSNotEqual = " !== "
prettyOp JSLessThan = " < "
prettyOp JSGreaterThan = " > "

prettyTernary :: Expr -> Expr -> Expr -> String
prettyTernary e1 e2 e3 =
  if multiLineCheck str
    then "(" ++ e1Str ++ "\n? " ++ e2Str ++ " \n: " ++ e3Str ++ ")"
    else str
  where
    e1Str = prettyExpr e1
    e2Str = prettyExpr e2
    e3Str = prettyExpr e3
    str = "(" ++ e1Str ++ " ? " ++ e2Str ++ " : " ++ e3Str ++ ")"

-- | Exercise B

jsVarName :: Parser Expr
jsVarName = JSVarName <$> some (digit <|> alpha <|> charTok '_') <* spaces

jsConst :: Parser Stat
jsConst = do
  stringTok "const "
  name <- spaces *> jsVarName <* spaces
  charTok '='
  expr <- asum [jsExprCall, jsExpr]
  charTok ';'
  pure $ JSConst name expr

jsConstBlock :: Parser Stat
jsConstBlock = do
  consts <- some jsConst
  pure $ JSConstBlock consts

jsCodeBlock :: Parser Stat
jsCodeBlock = do
  charTok '{'
  stats <- many jsStat
  charTok '}'
  pure $ JSCodeBlock stats

jsIf :: Parser Stat
jsIf = do
  stringTok "if"
  charTok '('
  expr <- jsExpr
  charTok ')'
  codeBlock <- jsCodeBlock
  pure $ JSIf expr codeBlock

jsIfElse :: Parser Stat
jsIfElse = do
  stringTok "if"
  charTok '('
  expr <- jsExpr
  charTok ')'
  codeBlock1 <- jsCodeBlock
  stringTok "else"
  codeBlock2 <- jsCodeBlock
  pure $ JSIfElse expr codeBlock1 codeBlock2

indent :: String -> String
indent str = unlines $ ("  " ++) <$> (lines str)

jsStat :: Parser Stat
jsStat = spaces *> asum [jsConstBlock, jsCodeBlock, jsIfElse, jsIf, jsStatCall, jsFunction, jsReturn]

parseExerciseB :: Parser ADT
parseExerciseB = Statement <$> many jsStat

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (Statement s) = intercalate "\n" (prettyStat <$> s)

prettyStat :: Stat -> String
prettyStat (JSConst name e) = "const " ++ prettyExpr name ++ " = " ++ prettyExpr e ++ ";"
prettyStat (JSConstBlock consts) = prettyConstBlock consts
prettyStat (JSCodeBlock s) = prettyCode s False
prettyStat (JSIf e s) = prettyIf e s
prettyStat (JSIfElse e s1 s2) = prettyIfElse e s1 s2
prettyStat (JSStatCall name args) = name ++ "(" ++ intercalate ", " (prettyExpr <$> args) ++ ");"
prettyStat (JSFunctionDef name args codeBlock) = prettyFunction name args codeBlock
prettyStat (JSReturn e) = "return " ++ prettyExpr e ++ ";"

-- >>> parse prettyStatMulti "const x = 1;"

prettyStatMulti :: Stat -> String
prettyStatMulti (JSConstBlock consts) = prettyConstBlock consts
prettyStatMulti (JSCodeBlock s) = prettyCode s True
prettyStatMulti (JSIf e s) = prettyIf e s
prettyStatMulti (JSIfElse e s1 s2) = prettyIfElse e s1 s2
prettyStatMulti (JSStatCall name args) = name ++ "(" ++ intercalate ", " (prettyExpr <$> args) ++ ");"
prettyStatMulti (JSFunctionDef name args codeBlock) = prettyFunction name args codeBlock
prettyStatMulti (JSReturn e) = "return " ++ prettyExpr e ++ ";"
prettyStatMulti (JSConst name e) = 
  error "A JSConst was somehow passed into prettyStatMulti"

prettyConstBlock :: [Stat] -> String
prettyConstBlock consts = intercalate "\n" (prettyStat <$> consts)

prettyCode :: [Stat] -> Bool -> String
prettyCode [] _  = "{ }"
prettyCode xs isMulti =
  let str = concatMap prettyStat xs in
  if multiLineCheck str || isMulti
    then "{\n" ++ indent (intercalate "\n" (prettyStatMulti <$> xs)) ++ "}"
    else "{ " ++ str ++ " }"

prettyIf :: Expr -> Stat -> String
prettyIf e (JSCodeBlock s) =
  let str = "if ( " ++ expr ++ " ) " ++ code in
  if multiLineCheck str
    then "if ( " ++ expr ++ " ) " ++ multiCode
    else str
  where
    expr = prettyExpr e
    code = prettyCode s False
    multiCode = replaceLast (prettyCode s True) "\n}"
prettyIf _ _ = 
  error "A statement other than JSCodeBlock was somehow passed into prettyIf"

prettyIfElse :: Expr -> Stat -> Stat -> String
prettyIfElse e (JSCodeBlock s1) (JSCodeBlock s2) =
  let str = "if ( " ++ expr ++ " ) " ++ code1 ++ " else " ++ code2 in
  if multiLineCheck str
    then "if ( " ++ expr ++ " ) " ++ multiCode1 ++ " else " ++ multiCode2
    else str
  where 
    expr = prettyExpr e
    code1 = prettyCode s1 False
    code2 = prettyCode s2 False
    multiCode1 = replaceLast (prettyCode s1 True) "\n}"
    multiCode2 = prettyCode s2 True
prettyIfElse _ _ _ = 
  error "A statement other than JSCodeBlock was somehow passed into prettyIfElse"

-- | Exercise C

jsExprCall :: Parser Expr
jsExprCall = do
  name <- jsVarName
  charTok '('
  args <- sepBy (tok jsExpr) (is ',' <* spaces)
  charTok ')'
  pure $ JSExprCall (prettyExpr name) args

jsStatCall :: Parser Stat
jsStatCall = do
  name <- jsVarName
  charTok '('
  args <- sepBy (tok jsExpr) (is ',' <* spaces)
  charTok ')'
  charTok ';'
  pure $ JSStatCall (prettyExpr name) args

jsFunction :: Parser Stat
jsFunction = do
  stringTok "function "
  name <- jsVarName
  charTok '('
  args <- sepBy (tok jsExpr) (is ',' <* spaces)
  charTok ')'
  codeBlock <- jsCodeBlock
  pure $ JSFunctionDef (prettyExpr name) args codeBlock

jsReturn :: Parser Stat
jsReturn = do
  stringTok "return"
  expr <- asum [jsExprCall, jsExpr]
  charTok ';'
  pure $ JSReturn expr

isTailRecursive :: String -> Bool
isTailRecursive input = 
  case parse parseFull input of
    Error _ -> False
    Result _ _ -> True

parseExerciseC :: Parser ADT
parseExerciseC = parseFull

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC = prettyFull

prettyFunction :: String -> [Expr] -> Stat -> String
prettyFunction name args (JSCodeBlock s) =
  let str = functionDef ++ code in
  if multiLineCheck str
    then functionDef ++ multiCode
    else str
  where 
    argStr = intercalate ", " (prettyExpr <$> args)
    functionDef = "function " ++ name ++ "(" ++ argStr ++ ") "
    code = prettyCode s False
    multiCode = prettyCode s True
