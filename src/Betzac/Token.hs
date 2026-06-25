module Betzac.Token (Token (..), showToken) where

import Betzac.Alphabet.Stmt (assign, stmtEnd)

data Token
    = TokAtom Char
    | TokDescriptor String
    | TokDirection Char
    | TokBehaviour Char
    | TokLParen
    | TokRParen
    | TokLBracket
    | TokRBracket
    | TokLBrace
    | TokRBrace
    | TokLAngle
    | TokRAngle
    | TokChainStep
    | TokChainSequence
    | TokBang
    | TokSlippery
    | TokNumber Int
    | TokComma
    | TokAssign
    | TokEndStmt
    | TokUsing String
    | TokOverride
    | TokExport
    deriving (Eq, Ord, Show)

showToken :: Token -> String
showToken (TokAtom c) = [c]
showToken (TokDescriptor s) = s
showToken (TokDirection c) = [c]
showToken (TokBehaviour c) = [c]
showToken TokLParen = "("
showToken TokRParen = ")"
showToken TokLBracket = "["
showToken TokRBracket = "]"
showToken TokLBrace = "{"
showToken TokRBrace = "}"
showToken TokLAngle = "<"
showToken TokRAngle = ">"
showToken TokChainStep = "-"
showToken TokChainSequence = "--"
showToken TokBang = "!"
showToken TokSlippery = "0*"
showToken (TokNumber n) = show n
showToken TokComma = ","
showToken TokAssign = [assign]
showToken TokEndStmt = [stmtEnd]
showToken (TokUsing s) = "using " ++ s
showToken TokOverride = "override"
showToken TokExport = "export"
