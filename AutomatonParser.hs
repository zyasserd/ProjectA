module AutomatonParser ( parseAutomaton, automatonFromString ) where

import Automaton

import Data.Char ( toLower, toUpper )
import Data.List ( intersperse, findIndex )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Either ( isRight, rights, isLeft, lefts )
import Data.Map ( Map, fromList, (!), member, insert, union )
import Data.Bifunctor ( second )
import Control.Monad ( replicateM, void, when )
import Text.ParserCombinators.Parsec hiding ( token )


{-
    File Format Description
-}
{-
    ### DEFINITIONS
        specialChar     = "{}<>|^*-:,\r\n\t `lambda`"
        alphabet        = not specialChar
        alphabetLambda  = alphabet <|> 
        comment         = "#" >> many char >> eol

        -- note no "input"
        -- note case insensitive
        DStype = tape | t | stack | s | queue | q 

    ### HEADER
        [1] dsTypesLine:
            (DSType [ :I | :O | :IO ])*
            if number of (:I) == 0
                add to the beginning InputT, set inputIndex = 0
            if number of (:I) > 1
                error

        note: could be empty


        [2] startVertexLine:
            "start" "<-" word

        optional: default q0


        [3] acceptVertexLine:
            "accept" "<-" word [,word, ...]

        optional: default empty


        [4] variableLine:
            "*" word "<-" many alphabet

        default definitions for:
            *      <- a..z A..Z 0..9
            *alpha <- a..z A..Z
            *num   <-           0..9

        repeated as much as wanted

    ### ENTRIES
        [] inputOutputBlock:
            many inputLine
            many outputLine
            emptyLine
        
        Think of this as a macro that does the cartesian product 
        of the set of inputLines with the set of outputLines

        [.] Primitives of inputLine / outputLine
            1. normal                       (input ✅, output ✅)
                Described below
            2. choices                      (input ✅, output ❌)
                "{normal [,normal ,...]}"
                TODO: add (variable)/(position variable) support in choices
                TODO: add choices feature in outputLine
            3. variables                    (input ✅, output ✅)    
                3.1 global          
                3.2 local
                "*varName"
            4. position variables           (input ✅, output ✅)
                4.1 self            "^"  => can be used only in outputLine to refer to the corresponding input
                4.2 numbered        "^i"
            0. local variable definition    (input ✅, output NA)
                "-> *var"
                can follow any of (1-4) above


        [] The "normal" primitive
            InputT
            I:  alphabetLambda
            O:  Nothing

            StackT
            I:  alphabetLambda
            O:  some alphabet <|> lambda

            TapeT
            I:  alphabetLambda
            O:  alphabetLambda|oneOf "LRN"

-}



{-
    Helper Functions
-}
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString:: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s)

eol :: Parser ()
eol =  void (try (string "\r\n") <|> string "\n")

token :: Parser a -> Parser a
token p =
    do
        onLineSpaces
        x <- p
        onLineSpaces
        return x

braced :: String -> Parser a -> Parser a
braced s p =
    do
        char (s !! 0)
        x <- token p
        char (s !! 1)
        return x

sepByTry :: Parser a -> Parser b -> Parser [a]
sepByTry p sep = try (sepByTry1 p sep) <|> return []

sepByTry1 :: Parser a -> Parser b -> Parser [a]
sepByTry1 p sep = (:) <$> p <*> many (try (sep *> p))

-- whitespaces that can fit on a line
onLineSpace :: Parser ()
onLineSpace = void $ oneOf " \t"

onLineSpaces :: Parser ()
onLineSpaces = void $ many onLineSpace

emptyLine :: Parser ()
emptyLine = onLineSpaces >> eol



{-
    Header Part
-}
type LocalVarsMap = Map String Int
type GlobalVarsMap = Map String [A]

alphabet :: Parser Char
alphabet = noneOf ("{}<>|^*-:,\r\n\t " ++ [lambda])

alphabetLambda :: Parser Char
alphabetLambda = alphabet <|> char lambda

comment :: Parser ()
comment = void (onLineSpaces >> char '#' >> manyTill anyChar (try eol))

-- 0+ comments separated by spaces
comments :: Parser ()
comments = void $ many $ try (comment <|> emptyLine)

endOfStatement :: Parser ()
endOfStatement = try (void $ manyTill onLineSpace (try eol))
                <|> try (onLineSpaces >> comment)



dsTypesLineParser :: Parser ([DSType], Int, Maybe Int)
dsTypesLineParser =
    do
        onLineSpaces
        xs <- sepByTry (try (parseSQT <* caseInsensitiveString ":IO" >>= \d -> return (d, "I", "O"))
                    <|> try (parseSQT <* caseInsensitiveString ":I"  >>= \d -> return (d, "I", "" ))
                    <|> try (parseSQT <* caseInsensitiveString ":O"  >>= \d -> return (d, "" , "O"))
                    <|>     (parseSQT                                >>= \d -> return (d, "" , "" ))) (skipMany1 onLineSpace)
        endOfStatement

        let countI = length $ filter (\(_,x,_) -> x == "I") xs
        let countO = length $ filter (\(_,_,x) -> x == "O") xs

        when (countI > 1) (fail "Error: The number of input DSes has to be at most 1!")
        when (countO > 1) (fail "Error: The number of output DSes has to be at most 1!")

        let indexI = findIndex (\(_,x,_) -> x == "I") xs
        let indexO = findIndex (\(_,_,x) -> x == "O") xs

        let xs' = map (\(x,_,_) -> x) xs

        case indexI of
          Nothing -> return (InputT:xs', 0, (+1) <$> indexO)
          Just i  -> return (xs',        i, indexO)
    where
        parseS = (caseInsensitiveString "stack" <|> caseInsensitiveString "s") >> return StackT
        parseQ = (caseInsensitiveString "queue" <|> caseInsensitiveString "q") >> return QueueT
        parseT = (caseInsensitiveString "tape"  <|> caseInsensitiveString "t") >> return TapeT
        parseSQT = parseS <|> parseQ <|> parseT

startVertexLineParser :: Parser Q
startVertexLineParser =
    do
        token $ string "start"
        token $ string "<-"
        x <- many1 alphabet
        endOfStatement
        return x

acceptVertexLineParser :: Parser [Q]
acceptVertexLineParser =
    do
        token $ string "accept"
        token $ string "<-"
        x <- sepByTry (many1 alphabet) (token $ char ',')
        endOfStatement
        return x

variableLineParser :: Parser (String, [A])
variableLineParser =
    do
        onLineSpaces
        var <- normalVariableParser
        token $ string "<-"
        ls <- sepByTry alphabet (token $ char ',')
        endOfStatement
        return (var, ls)

manyVariableLineParser :: Parser GlobalVarsMap
manyVariableLineParser = someVars (fromList [])
    where
        -- nearly the same function with an accumulator added
        someVars :: GlobalVarsMap -> Parser GlobalVarsMap
        someVars acc =
            do
                (varName, varVal) <- variableLineParser <* comments

                when (varName `member` acc) (fail $ "Global Variable (*" ++ varName ++ ") was declared before!")
                
                let acc' = insert varName varVal acc
                try (someVars acc') <|> return acc'

headerSectionParser :: Parser ([DSType], Int, Maybe Int, Q, [Q], GlobalVarsMap)
headerSectionParser =
    do
        comments; (dsTypes, inputIndex, outputIndex) <- try dsTypesLineParser
                                            <|> return ([InputT], 0, Nothing) -- In case no dsTypesLine
        comments; start     <- try startVertexLineParser
                              <|> return "q0"
        comments; accepted  <- try acceptVertexLineParser
                              <|> return []
        comments; globalVariables <- manyVariableLineParser <|> return (fromList [])


        let gVs = globalVariables `union` fromList [("",      ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']),
                                                    ("alpha", ['a'..'z'] ++ ['A'..'Z']),
                                                    ("num",   ['0'..'9'])]
        return (dsTypes, inputIndex, outputIndex, start, accepted, gVs)



{-
    Body Part
-}

-- ex: { a, b, c }
choices :: Parser a -> Parser [a]
choices p = braced "{}" $ sepByTry1 p (token $ char ',')

data InputTypes = Vals [A] | Var String | Pos (Maybe Int)

-- "^"    -> Nothing
-- "^123" -> Just 123
positionVariableParser :: Parser (Maybe Int)
positionVariableParser =
    do
        char '^'
        try (Just . read <$> many1 digit) <|> return Nothing

normalVariableParser :: Parser String
normalVariableParser = char '*' >> many alphabet

-- [[0,1], [0,2]] +++ [3,4] === [[0,1,3], [0,1,4], [0,2,3], [0,2,4]]
(+++) :: [[a]] -> [a] -> [[a]]
xs +++ ys = (++) <$> xs <*> ((:[]) <$> ys)

-- [[1,2], [3,4]] ++! 0 === [[1,2,1], [3,4,3]]
(++!) :: [[a]] -> Int -> [[a]]
xs ++! i = map (\ x -> x ++ [x !! i]) xs

fromLeft  = head . lefts  . (:[])
fromRight = head . rights . (:[])



inputLineParser :: [DSType] -> GlobalVarsMap -> Parser ([IEntry], LocalVarsMap)
inputLineParser ds gVs =
    do
        onLineSpaces
        v <- choices (many1 alphabet)
            <|> (: []) <$> (many1 alphabet)
        -- xs :: [(InputTypes, Maybe String)]
        xs <- replicateM (length ds) (token (char ',') >>
              (,) <$> (
                    Vals <$> choices alphabetLambda
                <|> Vals . (: []) <$> alphabetLambda
                <|> Var <$> normalVariableParser
                <|> Pos <$> positionVariableParser
              ) <*> (
                    Just <$> try (token (string "->") >> normalVariableParser)
                <|> return Nothing
              ))
        onLineSpaces
        char ':'
        endOfStatement

        case f' xs of
            Left m -> fail m
            Right (computedXs, lVs) -> return ((,) <$> v <*> computedXs, lVs)
    where
        f' xs = f xs 0 [[]] (fromList [])

        f :: [(InputTypes, Maybe String)] -> Int -> [[A]] -> Map String Int -> Either String ([[A]], Map String Int)
        f [] _ y lVs  = Right (y, lVs)
        f ((it, newVar):xs) i y lVs =
            do
                let y' = case it of
                            Vals ls              -> Right $ y +++ ls
                            Var s
                                | s `member` gVs -> Right $ y +++ (gVs ! s)
                            Var s
                                | s `member` lVs -> Right $ y ++! (lVs ! s)
                            Var s
                                | otherwise      -> Left  $ "Error: use of non-declared variable!"
                            Pos (Just j)
                                | j < i          -> Right $ y ++! j
                            Pos _
                                | otherwise      -> Left  $ "Error: use of big index in a position variable!"
                when (isLeft y') (Left $ fromLeft y')

                let lVs' = case newVar of
                            Nothing                             -> Right $ lVs
                            Just newVarS | newVarS `member` gVs -> Left  $ "Error: variable (*" ++ newVarS ++ ") was already defined globally!"
                            Just newVarS | newVarS `member` lVs -> Left  $ "Error: variable (*" ++ newVarS ++ ") was already defined locally!"
                            Just newVarS | otherwise            -> Right $ insert newVarS i lVs
                when (isLeft lVs') (Left $ fromLeft lVs')

                f xs (i+1) (fromRight y') (fromRight lVs')

outputLineParser :: [DSType] -> GlobalVarsMap -> Parser ((IEntry, LocalVarsMap) -> Either String [OEntry])
outputLineParser ds gVs =
    do
        onLineSpaces
        v <- many1 alphabet
        xs <- mapM dsTypeToParser ds
        onLineSpaces
        endOfStatement

        return (\((_, inputs), lVs) -> do
                    let vxs = [f (lVs, inputs, i) | (f, i) <- zip xs [0..]]

                    ((,) v <$>) . sequence <$> sequenceA vxs
            )
    where
        variablePosition :: Parser ((LocalVarsMap, [A], Int) -> Either String [A])
        variablePosition = do
            n <- positionVariableParser
            when (n >= Just (length ds)) (fail "Error: use of big index in a position variable!")
            return (\(_, as, index) -> Right [as !! fromMaybe index n])

        variableNormal :: Parser ((LocalVarsMap, [A], Int) -> Either String [A])
        variableNormal = do
            s <- normalVariableParser
            if s `member` gVs then
                return $ const $ Right (gVs ! s)
            else
                return (\(lVs, as, _) -> if s `member` lVs then
                                                Right [as !! (lVs ! s)]
                                             else
                                                Left "Error")

        variable :: Parser ((LocalVarsMap, [A], Int) -> Either String [A])
        variable = try variablePosition <|> variableNormal

        dsTypeToParser :: DSType -> Parser ((LocalVarsMap, [A], Int) -> Either String [DSAction])
        dsTypeToParser ds = case ds of
            InputT -> return $ const $ Right [InputA]
            StackT -> token (char ',') >> ((const . Right . return . StackA <$> ((string [lambda] >> return []) <|> many1 alphabet))
                                    --    <|> (((StackA . return <$>)<$>)<$>) <$> variable)
                                       <|> ((<$>).(<$>).(<$>).(<$>)) (StackA . return) variable )
            QueueT -> token (char ',') >> ((const . Right . return . QueueA <$> ((string [lambda] >> return []) <|> many1 alphabet))
                                       <|> ((<$>).(<$>).(<$>).(<$>)) (QueueA . return) variable )
            TapeT  -> token (char ',') >> (
                        do
                            c <- alphabetLambda
                            char '|'
                            m <- oneOf "LRN"
                            let i = fromJust $ lookup m [('L', -1), ('N', 0), ('R', 1)]
                            return $ const $ Right [TapeA c i]
                    <|> do
                            nn <- variable
                            char '|'
                            m <- oneOf "LRN"
                            let i = fromJust $ lookup m [('L', -1), ('N', 0), ('R', 1)]
                            return $ ((<$>).(<$>).(<$>)) (`TapeA` i) nn
                        )

inputOutputBlock :: [DSType] -> GlobalVarsMap -> Parser [(IEntry, [OEntry])]
inputOutputBlock ds gVs =
    do
        comments
        inputss  <- many1 $ try $ inputLineParser ds gVs
        outputss <- (many1 $ try $ outputLineParser ds gVs) <?> "Error: No output in this block!"
        onLineSpaces
        eol

        let toReturn = do
            (inputs, lVs) <- inputss
            input <- inputs
            foutputs <- outputss
            let outputs = foutputs (input, lVs)

            return (input, outputs)

        let errors = lefts $ map snd toReturn

        when (not (null errors)) (fail (head errors))

        return $ map (second fromRight) toReturn

-- automatonParser :: Parser AutomatonWireframe
automatonParser :: Parser AutomatonWireframe
automatonParser =
    do
        (dsTypes, inputIndex, outputIndex, start, accepted, gVs) <- headerSectionParser

        deltaList <- concat <$> many (try $ inputOutputBlock dsTypes gVs)

        comments; eof

        return (AutomatonWireframe dsTypes inputIndex outputIndex start accepted deltaList)



{-
    TESTING
-}
par :: Parser a -> String -> Either ParseError a
par myParser = parse myParser []



{-
    Exported Functions
-}
parseAutomaton :: String -> Either ParseError AutomatonWireframe
parseAutomaton s = par automatonParser (s ++ "\n\n")

automatonFromString :: String -> AutomatonWireframe
automatonFromString s =
    case parseAutomaton s of
        Left pError   -> error ("[automatonFromString] The string couldn't be converted to AutomatonWireframe\n"
                              ++ show pError)
        Right wf -> wf
