module RegexState where

data Atom
  = Literal  Char
  | Class    String
  | NegClass String
  | Any
  | Alternation
  | RangeLow  Int
  | RangeHigh Int
  | RangeBand Int
  | Optional
  -- LParen and RParen are used internally for grouping
  -- they will not appear in the result of shuntRegex
  | LParen
  | RParen


-- turn a regular expression into a postfix expression of atoms
-- Dijkstra's shunting-yard algorithm

type PostfixRegex = [Atom]

data ParenCell = ParenCell { natom, nalt :: Int }
type ShuntYard = ([ParenCell], PostfixRegex)

emptyCell :: ParenCell
emptyCell = ParenCell 0 0

emptyYard :: ShuntYard
emptyYard = ([emptyCell], [])

shuntRegex :: String -> PostfixRegex
shuntRegex input = shuntRegexRec input emptyYard

shuntRegexRec :: String -> ShuntYard -> PostfixRegex
shuntRegexRec "" yard = closeYard yard
shuntRegexRec input yard = let (t, rest) = tokenize input
  in shuntRegexRec rest $ case t of
    _ -> (emptyYard)

tokenize :: String -> (Atom, String)
tokenize = undefined

closeYard :: ShuntYard -> PostfixRegex
closeYard = undefined
