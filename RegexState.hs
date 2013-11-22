module RegexState where

import Control.Applicative
import Control.Arrow
import Control.Monad

type RegexError = String
type Failing a  = Either RegexError a

-- basically every parse error comes down to this
errParen :: Failing a
errParen = Left "Unbalanced parentheses"

(<$$>) :: Either a c -> (a -> b) -> Either b c
Left  x <$$> f = Left (f x)
Right r <$$> _ = Right r

data Atom
  = Literal  Char
  | Class    String
  | NegClass String
  | Any
  | Alternative
  | RangeLow  Int
  | RangeHigh Int
  | RangeBand Int Int
  | Optional
  -- LParen, RParen and Catenate are used internally for grouping
  -- they will not appear in the result of shuntRegex
  | LParen
  | RParen
  | Catenate
    deriving Show


-- turn a regular expression into a postfix expression of atoms
-- Dijkstra's shunting-yard algorithm

type PostfixRegex = [Atom]

data ParenCell = ParenCell { natom, nalt :: Int }
type ShuntYard = ([ParenCell], PostfixRegex)

emptyCell :: ParenCell
emptyCell = ParenCell 0 0

emptyYard :: ShuntYard
emptyYard = ([emptyCell], [])

shuntRegex :: String -> Failing PostfixRegex
shuntRegex input = shuntRegexRec input emptyYard

shuntRegexRec :: String -> ShuntYard -> Failing PostfixRegex
shuntRegexRec "" yard = closeYard yard
shuntRegexRec input yard = do
  (t, rest) <- tokenize input
  shuntRegexRec rest <=< (\f -> f yard) $ case t of
    LParen      -> restack
    RParen      -> destack
    Alternative -> alternate
    Literal _   -> pushAtom t
    Any         -> pushAtom t
    _           -> pure . second (t:)
    

tokenize :: String -> Failing (Atom, String)
tokenize "" = Left "Cannot tokenize an empty string" -- this should be unreachable
tokenize (t:rest) = pure (token, rest)
  where token = case t of
                  '(' -> LParen
                  ')' -> RParen
                  '.' -> Any
                  '*' -> RangeLow 0
                  '+' -> RangeLow 1
                  '?' -> Optional
                  '|' -> Alternative
                  _   -> Literal t

closeYard :: ShuntYard -> Failing PostfixRegex
closeYard yard = do
  (stack, output) <- destack yard
  case stack of
    [] -> pure $ reverse output
    _  -> errParen <$$> (++" (closeYard)")
  

restack :: ShuntYard -> Failing ShuntYard
restack = pure . first (emptyCell:)

destack :: ShuntYard -> Failing ShuntYard
destack (stack, output) = case stack of
  cell:next:rest -> pure ( next { natom = natom next + 1 } : rest
                         ,    replicate (nalt cell) Alternative
                           ++ replicate (natom cell - 1) Catenate
                           ++ output
                         )
  cell:_         -> pure ( []
                         ,    replicate (nalt cell) Alternative
                           ++ replicate (natom cell - 1) Catenate
                           ++ output
                         )
  _              -> errParen <$$> (++" (destack)")

alternate :: ShuntYard -> Failing ShuntYard
alternate (stack, output) = case stack of
  cell:rest -> pure ( cell { natom = 0, nalt = nalt cell + 1 } : rest
                    ,    replicate (natom cell - 1) Catenate
                      ++ output
                    )
  _         -> errParen <$$> (++" (alternate)")

pushAtom :: Atom -> ShuntYard -> Failing ShuntYard
pushAtom a (stack, output) = case stack of
  cell:rest -> pure ( cell { natom = natom cell + 1 } : rest
                    , a:output
                    )
  _         -> errParen <$$> (++" (pushAtom)")
