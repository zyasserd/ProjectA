module Automaton where

import Data.List ( intersperse, zip3, unfoldr )
import Data.List.Split ( splitOn )
import Data.Maybe ( fromJust, fromMaybe, maybeToList )
import Data.Bifunctor ( first )
import Data.Map ( Map, fromList, (!), member )
import qualified Data.Map as Map ( lookup )

{-
    Use State / StateT
-}
{-
    how to implement (no action)/(empty: lambda)


    ("q0", 'a', 'b') -> ("q1", "c")

    user can write
        "a,,b" or "a,λ,b" to mean the same thing
        but it's stored as Nothing

    λ, _
-}

type Q = String     -- recognizing vertices
type A = Char       -- alphabet

lambda :: A
lambda = 'λ'

isLambda :: Char -> Bool
isLambda = (== lambda)

ifEmptyLambda :: [A] -> [A]
ifEmptyLambda [] = [lambda]
ifEmptyLambda xs = xs



data DSType =
    InputT |
    StackT |
    QueueT |
    TapeT
    deriving (Show, Eq)

data DS =
    Input [A] |
    Stack [A] |
    Queue [A] |
    Tape [A] [A]    -- where pointer points to the first character of the second array
    deriving (Show, Eq)

peek :: DS -> Char
peek (Input xs) = last $ ifEmptyLambda xs
peek (Stack xs) = last $ ifEmptyLambda xs
peek (Queue xs) = last $ ifEmptyLambda xs
peek (Tape _ ys) = head ys

loadDS :: DSType -> String -> DS
loadDS InputT xs = Input (reverse xs)
loadDS StackT xs = Stack xs
loadDS QueueT xs = Queue (reverse xs)
loadDS TapeT xs
    | null xs   = Tape [] [lambda]
    | otherwise = Tape [] xs



data DSAction =         -- Bool represent whether to pop (true = pop)
    InputA Bool |
    StackA Bool [A] |
    QueueA Bool [A] |
    TapeA  A Int        -- TapeA A {L, N, R} === {-1, 0, 1}
    deriving (Show)


conditionalExecute :: Bool -> a -> (a -> a) -> a
conditionalExecute b x f    -- if b return (f x), else (x)
    | b         = f x
    | otherwise = x

conditionalPop :: Bool -> [A] -> [A]
conditionalPop b xs = conditionalExecute b xs init

doDSAction :: DS -> DSAction -> DS
doDSAction (Input xs) (InputA b)    = Input (conditionalPop b xs)
doDSAction (Stack xs) (StackA b ys) = Stack (conditionalPop b xs ++ ys)
doDSAction (Queue xs) (QueueA b ys) = Queue (reverse ys ++ conditionalPop b xs)
doDSAction (Tape xs ys) (TapeA dx di)
    | di ==  0  = Tape xs ys'
    | di ==  1  = Tape (xs ++ [head ys']) (ifEmptyLambda $ tail ys')
    | di == -1  = Tape (init xs') (last xs': ys')
    | otherwise = error "[doDSAction] TapeA Int argument value error"
      where ys' = dx : tail ys
            xs' = ifEmptyLambda xs
doDSAction _ _ = error "[doDSAction] Mismatch between DSType and DSAction"

toDSAction :: DSType -- ^ 
    -> Bool          -- ^ True if input to the Automaton was not `lambda`
                     --   meaning that we should pop
    -> String        -- ^ the result of the hashtable
    -> DSAction
toDSAction InputT b xs
    | xs == [lambda]    = InputA b
    | otherwise         = error "[toDSAction] InputT can only have (lambda) as its argument"
toDSAction StackT b xs
    | xs == [lambda]    = StackA b []
    | otherwise         = StackA b xs -- ! xs shouldn't contain λ
toDSAction QueueT b xs
    | xs == [lambda]    = QueueA b []
    | otherwise         = QueueA b xs -- ! xs shouldn't contain λ
toDSAction TapeT _ (a:'|':i:rs) =
    case rs of
        [] -> TapeA a (fromJust (lookup i [('L', -1), ('N', 0), ('R', 1)]))
        _  -> error "[toDSAction] TapeA is more than 3 chars long"
toDSAction TapeT _ _ = error "[toDSAction] TapeA is less than 3 chars long"



data AutomatonWireframe = AutomatonWireframe {
    dsTypes :: [DSType],
    loadingIndex :: Int,
        -- input would be loaded in ds[loadingIndex] when loading
    start :: Q,
    accepted :: [Q],
    delta :: (Q, [Char]) -> [ (Q, [DSAction]) ]
        -- the transition function
}

isF :: AutomatonWireframe -> Q -> Bool
isF wf = (`elem` accepted wf)

type AutomatonState = (Q, [DS])

deltaFromList :: [(String, [String])] -> [DSType] -> (Q, [Char]) -> [ (Q, [DSAction]) ]
deltaFromList hs ts (q0, ps) =
    do
        ps'           <- interleave ps
        listOfEntries <- maybeToList $ Map.lookup (toEntry (q0, ps')) h
        entry         <- listOfEntries
        let areNotLambdas = map (not . isLambda) ps'
        let (q1:xs) = splitOn "," entry
        return  (q1, zipWith3 toDSAction ts areNotLambdas xs)
    where
        h = fromList hs

        toEntry :: (Q, [Char]) -> String
        toEntry (s, xs) =  s ++ "," ++ intersperse ',' xs

        -- remove _ or λ
        interleave :: [Char] -> [[Char]]
        interleave = mapM (\x -> [x, '_', lambda])

loadAutomaton :: AutomatonWireframe -> String -> AutomatonState
loadAutomaton (AutomatonWireframe dsTypes loadingIndex q0 _ _) xs =
    ( q0, [ loadDS t (if i == loadingIndex then xs else "") | (i, t) <- zip [0..] dsTypes ] )

next :: AutomatonWireframe -> AutomatonState -> [AutomatonState]
next (AutomatonWireframe _ _ _ _ delta) (q0, dses) =
    [ (q1, zipWith doDSAction dses as) | (q1, as) <- delta (q0, map peek dses) ]

-- isDeterministic :: AutomatonWireframe -> Bool

-- isFinal :: AutomatonWireframe -> AutomatonState -> Bool



------------------------------------

guardedAny :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
guardedAny _ _ [] = False
guardedAny fTrue fTerminate (x:xs) =
    fTrue x || (not (fTerminate x) && guardedAny fTrue fTerminate xs)

-- ! Create general version with 2 termination modes
iterationAccepted :: AutomatonWireframe -> [AutomatonState] -> Bool
iterationAccepted w = any (\(q, ds) -> isF w q && (ds !! loadingIndex w) == Input "")

pdaAccepted :: AutomatonWireframe -> String -> Bool
pdaAccepted wf s = guardedAny (iterationAccepted wf) null $ iterate (>>= next wf) [loadAutomaton wf s]


nfaDiv3 = AutomatonWireframe [InputT] 0 "q0" ["q0"] (deltaFromList [
        ("q0,0", ["q0,λ"]),
        ("q0,1", ["q1,λ"]),

        ("q1,0", ["q2,λ"]),
        ("q1,1", ["q0,λ"]),

        ("q2,0", ["q1,λ"]),
        ("q2,1", ["q2,λ"])
    ] [InputT])

toBin :: Int -> String
toBin = concat . reverse . unfoldr (\x -> if x == 0 then Nothing else Just (show $ x `mod` 2, x `div` 2))

binDiv3 :: Int -> Bool
binDiv3 = pdaAccepted nfaDiv3 . toBin



---------------------------------

-- Turing Machine to incremeant a binary digit
turingInc = AutomatonWireframe [TapeT] 0 "qBuffer" ["qTerm"] (deltaFromList [
        ("qBuffer,1", ["qBuffer,1|R"]),
        ("qBuffer,0", ["qBuffer,0|R"]),
        ("qBuffer,λ", ["qMain,λ|L"]),
        
        ("qMain,λ", ["qTerm,1|N"]),
        ("qMain,0", ["qTerm,1|N"]),
        ("qMain,1", ["qMain,0|L"])
    ] [TapeT])

aTuring :: AutomatonState
aTuring = loadAutomaton turingInc "100"


-- next turinInc aTuring

