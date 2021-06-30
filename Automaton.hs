module Automaton where

import Data.List ( intersperse, zip3, unfoldr, find )
-- import Data.List.Split ( splitOn )
import Data.Maybe ( fromJust, fromMaybe, maybeToList )
import Data.Bifunctor ( first )
import Data.Map ( Map, fromListWith, (!), member )
import qualified Data.Map as Map ( lookup )




{-
    Helping Types and Functions
-}

type Q = String     -- recognizing vertices
type A = Char       -- alphabet

{-
    Note:   The wildcard (_)
                It can be used as a substitute to a non-void lambda
    Note:   Lambda can have different meaning using diff DSTypes
                if Input/Stack/Queue -> [void] / no action / no input is consumed
                                        so it can always act as another choice for other inputs
                if Tape -> [blank value] / the value of the turing machine is 'λ'
                        so it can't substitute other values
-}
-- epsilon is used instead of lambda
-- cuz there was a problem printing lambda on ghc windows
lambda :: A
lambda = 'ε'

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

-- ! Maybe make more efficient
data DS =
    Input [A] |
    Stack [A] |
    Queue [A] |
    Tape [A] [A]    -- where pointer points to the first character of the second array
    deriving (Show, Eq)

peek :: DS -> A
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

data DSAction =
    InputA |
    StackA [A] |
    QueueA [A] |
    TapeA  A Int        -- TapeA A {L, N, R} === {-1, 0, 1}

instance Show DSAction where
    show InputA      = [lambda]
    show (StackA []) = [lambda]
    show (StackA xs) = xs
    show (QueueA []) = [lambda]
    show (QueueA xs) = xs
    show (TapeA c m) = [c] ++ "|" ++ case m of
       0  -> "N"
       1  -> "R"
       -1 -> "L"
       _  -> "[show DSAction] TapeA Int argument value error"

-- Bool represent whether to pop (true = pop)
type DSActionPop = (Bool, DSAction)

conditionalPop :: Bool -> [A] -> [A]
conditionalPop b xs
    | b         = init xs
    | otherwise = xs

doDSAction :: DS -> DSActionPop -> DS
doDSAction (Input xs) (b, InputA)    = Input (conditionalPop b xs)
doDSAction (Stack xs) (b, StackA ys) = Stack (conditionalPop b xs ++ ys)
doDSAction (Queue xs) (b, QueueA ys) = Queue (reverse ys ++ conditionalPop b xs)
doDSAction (Tape xs ys) (_, TapeA dx di)
    | di ==  0  = Tape xs ys'
    | di ==  1  = Tape (xs ++ [head ys']) (ifEmptyLambda $ tail ys')
    | di == -1  = Tape (init xs') (last xs': ys')
    | otherwise = error "[doDSAction] TapeA Int argument value error"
      where ys' = dx : tail ys
            xs' = ifEmptyLambda xs
doDSAction _ _ = error "[doDSAction] Mismatch between DSType and DSAction"

type IEntry = (Q, [A])
type OEntry = (Q, [DSAction])




{-
    Main Types and Functions
-}

data AutomatonWireframe = AutomatonWireframe {
    dsTypes :: [DSType],
        -- InputT if present can only occur at index 0
    inputIndex :: Int,
        -- input would be loaded in ds[inputIndex] when loading
    outputIndex :: Maybe Int,
    start :: Q,
    accepted :: [Q],
    transitionList :: [(IEntry, [OEntry])]
} deriving (Show)

type AutomatonState = (Q, [DS])

transitionFunction :: AutomatonWireframe -> (Q, [A]) -> [ (Q, [DSActionPop]) ]
transitionFunction (AutomatonWireframe ts _ _ _ _ hs) (q0, ps) =
    do
        q0'           <- [q0, "_"]
        ps'           <- interleave $ zip ps ts
        listOfEntries <- maybeToList $ Map.lookup (q0', ps') h
        (q1, xs)      <- listOfEntries
        let areNotLambdas = map (not . isLambda) ps'
        return  (q1, zip areNotLambdas xs)
    where
        h :: Map IEntry [OEntry]
        h = fromListWith (++) hs

        interleave :: [(A, DSType)] -> [[A]]
        interleave = mapM (\(x, t) -> [x] ++
                                      ['_' | x /= lambda || t == TapeT] ++
                                      [lambda | x /= lambda, t /= TapeT])



loadAutomaton :: AutomatonWireframe -> String -> AutomatonState
loadAutomaton (AutomatonWireframe dsTypes inputIndex _ q0 _ _) xs =
    ( q0, [ loadDS t (if i == inputIndex then xs else "") | (i, t) <- zip [0..] dsTypes ] )
(<<) = loadAutomaton

next :: AutomatonWireframe -> AutomatonState -> [AutomatonState]
next wf (q0, dses) =
    [ (q1, zipWith doDSAction dses as) | (q1, as) <- transitionFunction wf (q0, map peek dses) ]

(>=>) :: AutomatonWireframe -> [AutomatonState] -> [AutomatonState]
wf >=> xs = xs >>= next wf

getOutput :: AutomatonWireframe -> AutomatonState -> Maybe DS
getOutput wf (_, ds) = case outputIndex wf of
                        Nothing -> Nothing
                        Just i  -> Just (ds !! i)




-- returns true if the current (vertex) is accepted
isVertexAccepted :: AutomatonWireframe -> Q -> Bool
isVertexAccepted wf = (`elem` accepted wf)

-- returns true if the current (AutomatonState) is accepted
isAccepted :: AutomatonWireframe -> AutomatonState -> Bool
isAccepted wf (q, dses) =
    if dsTypes wf !! 0 == InputT then
        isVertexAccepted wf q && dses !! 0 == Input ""
    else
        isVertexAccepted wf q

guardedAny :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
guardedAny _ _ [] = False
guardedAny fTrue fTerminate (x:xs) =
    fTrue x || (not (fTerminate x) && guardedAny fTrue fTerminate xs)

-- check if the automaton is given a string, will it ever get accepted
(<<?>>) :: AutomatonWireframe -> String -> Bool
(<<?>>) wf s = guardedAny (any (isAccepted wf)) null $ iterate (wf >=>) [loadAutomaton wf s]

guardedf :: (a -> Maybe b) -- ^ 
  -> (a -> Bool) -- ^ fTerminate
  -> [a] -- ^ 
  -> Maybe b
guardedf _ _ [] = Nothing
guardedf f fTerminate (x:xs) =
    case f x of
        (Just v) -> Just v
        Nothing  -> if fTerminate x then
                        Nothing
                    else
                        guardedf f fTerminate xs

-- compute string and return result; stops with the first accepted result
-- two errors could happen: (1) no output index in wf, (2) didn't reach accepted state
(<<!>>) :: AutomatonWireframe -> String -> Maybe DS
(<<!>>) wf s = do
    d <- guardedf (find (isAccepted wf)) null $ iterate (wf >=>) [loadAutomaton wf s]
    getOutput wf d



-- s0 = [pdaAB << "aabb"]
-- wf >=> it
-- pdaAB ? "aabb" 



{-
    Additional Functions
-}

-- isDeterministic :: AutomatonWireframe -> Bool

-- automatonType :: AutomatonWireframe -> String
-- dsTypes wf == [InputT]                   -> DFA/NFA
-- dsTypes wf == [InputT, StackT]           -> DPDA/PDA
-- dsTypes wf == [InputT, StackT, StackT]   -> DPDA-II/DPDA-II
-- dsTypes wf == [InputT, QueueT]           -> Queue Automaton
-- dsTypes wf == [InputT, TapeT]            -> DTM/NTM
-- dsTypes wf == [InputT, TapeT*n]          -> n-tape DTM/NTM
-- LBA?