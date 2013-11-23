module RegexState where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix

type RegexError = String
type Failing a  = Either RegexError a

-- basically every parse error comes down to these
errParen, errAlternate, errRepeat, errRange :: Failing a
errParen = Left "Unbalanced parentheses"
errAlternate = Left "Empty alternation"
errRepeat = Left "Empty repetition"
errRange = Left "Invalid repetition range"

(<$$>) :: Either String b -> String -> Either String b
Left  x <$$> s = Left (x ++ " (" ++ s ++ ")")
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
  shuntRegexRec rest <=< ($ yard) $ case t of
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
    _  -> errParen <$$> "closeYard"
  

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
  _              -> errParen <$$> "destack"

alternate :: ShuntYard -> Failing ShuntYard
alternate (stack, output) = case stack of
  cell:rest -> pure ( cell { natom = 0, nalt = nalt cell + 1 } : rest
                    ,    replicate (natom cell - 1) Catenate
                      ++ output
                    )
  _         -> errParen <$$> "alternate"

pushAtom :: Atom -> ShuntYard -> Failing ShuntYard
pushAtom a (stack, output) = case stack of
  cell:rest -> pure ( cell { natom = natom cell + 1 } : rest
                    , a:output
                    )
  _         -> errParen <$$> "pushAtom"


data RegexState
  = Match    Char RegexState
  | MatchAny      RegexState
  | Branch        RegexState RegexState
  | Success

instance Show RegexState where
  show (Match  c _) = 'L':show c
  show (MatchAny _) = "Dot"
  show (Branch _ _) = "Branch"
  show (Success   ) = "Success"
  

type MachineFragment = RegexState -> RegexState
type FragmentStack   = [MachineFragment]

fromPostfix :: PostfixRegex -> Failing RegexState
fromPostfix atoms = fromPostfixRec atoms []

fromPostfixRec :: PostfixRegex -> FragmentStack -> Failing RegexState
fromPostfixRec [] fragments = case fragments of
  [f] -> pure (f Success)
  _   -> errParen <$$> "fromPostfixRec"
fromPostfixRec (a:as) fragments =
  fromPostfixRec as <=< ($ fragments) $ case a of
    Literal c     -> pure . (Match c:)
    Any           -> pure . (MatchAny:)
    Alternative   -> alternateMachine
    Catenate      -> catenateMachine
    RangeLow  l   -> atLeastMachine l
    RangeHigh h   -> atMostMachine h
    RangeBand l h -> bandMachine l h
    Optional      -> optionalMachine
    LParen        -> pure
    RParen        -> pure
    Class _       -> pure
    NegClass _    -> pure

alternateMachine :: FragmentStack -> Failing FragmentStack
alternateMachine stack = case stack of
  s2:s1:rest -> pure $ (Branch <$> s1 <*> s2):rest
  _          -> errAlternate <$$> "alternateMachine"

catenateMachine :: FragmentStack -> Failing FragmentStack
catenateMachine stack = case stack of
  s2:s1:rest -> pure $ (s1 . s2):rest
  _          -> errAlternate <$$> "catenateMachine"

repeatFragment :: Int -> MachineFragment -> MachineFragment
repeatFragment 0 _ = id
repeatFragment n s = s . repeatFragment (n-1) s

atLeastMachine :: Int -> FragmentStack -> Failing FragmentStack
atLeastMachine n stack = case stack of
  s:rest -> pure $ (manyMachine s . repeatFragment n s):rest
  _      -> errRepeat <$$> "atLeastMachine"

atMostMachine :: Int -> FragmentStack -> Failing FragmentStack
atMostMachine n stack = case stack of
  s:rest -> pure $ repeatFragment n (optionalFragment s) : rest
  _      -> errRepeat <$$> "atMostMachine"

manyMachine :: MachineFragment -> MachineFragment
manyMachine s = \f -> fix (Branch f . s)

bandMachine :: Int -> Int -> FragmentStack -> Failing FragmentStack
bandMachine _ _ []          = errRepeat <$$> "bandMachine"
bandMachine low high (f:fs) = case compare low high of
  LT -> pure $ (repeatFragment (high - low) (optionalFragment f) . repeatFragment low f) : fs
  EQ -> pure $ repeatFragment low f : fs
  GT -> errRange <$$> "bandMachine"

optionalMachine :: FragmentStack -> Failing FragmentStack
optionalMachine stack = case stack of
  s:rest -> pure $ optionalFragment s : rest
  _      -> errRepeat <$$> "optionalMachine"

optionalFragment :: MachineFragment -> MachineFragment
optionalFragment s = Branch <$> s <*> id

compileRegex :: String -> Failing RegexState
compileRegex = shuntRegex >=> fromPostfix

match :: RegexState -> String -> Bool
match r s = matchRec . map (\x -> (x,r)) $ suffixes s

suffixes :: String -> [String]
suffixes = scanr (:) []

succeed :: (a, RegexState) -> Bool
succeed (_, Success) = True
succeed _            = False

matchRec :: [(String, RegexState)] -> Bool
matchRec states = case states of
  [] -> False
  _  -> if any succeed states
        then True
        else matchRec (states >>= advanceState)

advanceState :: (String, RegexState) -> [(String, RegexState)]
advanceState (s, Success) = pure (s, Success)
advanceState (s, Branch a b) = [(s, a), (s, b)]
advanceState ("", _) = []
advanceState (c:cs, Match x r) = if c == x then pure (cs, r) else []
advanceState (_:cs, MatchAny r) = pure (cs, r)
