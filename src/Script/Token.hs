{-|

Tokens for FCL lexer.

--}

{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module Script.Token where

import Data.Text (Text, words)

default (Text)

global   = "global"
local    = "local"
asset    = "asset"
account  = "account"
contract = "contract"
return   = "return"
true     = "True"
false    = "False"
void     = "void"
int      = "int"
float    = "float"
fixed1   = "fixed1"
fixed2   = "fixed2"
fixed3   = "fixed3"
fixed4   = "fixed4"
fixed5   = "fixed5"
fixed6   = "fixed6"
bool     = "bool"
ref      = "ref"
sig      = "sig"
msg      = "msg"
datetime = "datetime"
timedelta = "timedelta"
any      = "any"
state    = "state"

if_      = "if"
else_    = "else"
before   = "before"
after    = "after"
between  = "between"

assign   = "="
mult     = "*"
add      = "+"
sub      = "-"
div      = "/"
and      = "&&"
or       = "||"
equal    = "=="
nequal   = "!="
gequal   = ">="
lequal   = "<="
greater  = ">"
lesser   = "<"
not      = "!"

semi     = ";"
colon    = ":"
at       = "@"
atat     = "@@"

lparen   = "("
rparen   = ")"
lbrace   = "{"
rbrace   = "}"
lbracket = "["
rbracket = "]"

rarrow   = "->"

initial    = "initial"
terminal   = "terminal"
transition = "transition"

intBinOps  = [mult,add,sub,div]
boolBinOps = [and, or]
boolUnops  = [not]

keywords = [
    global
  , local
  , asset
  , contract
  , account
  , sig
  , msg
  , return
  , true
  , false
  , float
  , int
  , fixed1
  , fixed2
  , fixed3
  , fixed4
  , fixed5
  , fixed6
  , bool
  , void
  , datetime
  , timedelta
  , if_
  , else_
  , initial
  , terminal
  , transition
  ]

operators = [
    mult
  , add
  , sub
  , div
  , and
  , or
  , equal
  , nequal
  , gequal
  , lequal
  , greater
  , lesser
  , not
  ]
