{-# LANGUAGE QuasiQuotes #-}
module AutomatonParser ( parseAutomaton, automatonFromString ) where

import Automaton

import Data.Char ( toLower, toUpper )
import Data.List ( intersperse, findIndex )
import Data.Maybe ( fromJust, fromMaybe)
import Control.Monad ( replicateM, void, when )
import Text.ParserCombinators.Parsec hiding ( token )



{-
    ### HEADER
        returns:
            ( [DSType], inputIndex :: Int, start :: Q, f :: [Q] )

        definitions:
            specialChar     = "{}|=,:\r\n\t _^`lambda`"
            alphabet        = not specialChar
            vertex          = many1 alphabet
            alphabetPlus    = alphabet <|> lambda <|> "_"
            comment         = "#" >> many char >> eol

            -- note no "input"
            -- note case insensitive
            DStype = tape | t | stack | s | queue | q 

        [1]
        dsTypesLine:
            Empty                       => In that case, [DSType] = [InputT], inputIndex = 0
        |   DStype*                     => In that case there is an implicit (InputT)
        |   DStype* (DStype) DStype*

        [2]
        startVertexLine:
            "start" "=" vertex

        [3]
        acceptVertexLine:
            "accept" "=" many vertex

    ### ENTRIES

        Vertices
        I:  vertex
        O:  vertex

        [not comma]
        InputT
        I:  alphabetPlus
        O:  empty but returns lambda

        StackT
        I:  alphabetPlus
        O:  some alphabet <|> lambda

        QueueT
        I:  alphabetPlus
        O:  ( alphabetLambda , LRN)

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


-- -- eof
-- endInput :: Parser ()
-- endInput = void $ char '\EOT'
--     -- this \/ failed as it accepts the empty string
--     -- do cs <- lookAhead $ many anyChar
--     --    case cs of 
--     --        [] -> return ()
--     --        _  -> fail "expected end of input"




{-
    real work starts
-}
alphabet :: Parser Char
alphabet = noneOf ("{}|=,:\r\n\t _^" ++ [lambda]) -- ! update up

vertex :: Parser String
vertex = many1 alphabet

alphabetLambda :: Parser Char
alphabetLambda = alphabet <|> char lambda

alphabetPlus :: Parser Char
alphabetPlus = alphabet <|> char lambda <|> char '_'

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

        when (countI > 1) (fail "The number of input DSes has to be at most 1")
        when (countO > 1) (fail "The number of output DSes has to be at most 1")

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
        onLineSpaces
        token $ string "start"
        token $ string "="
        x <- vertex
        endOfStatement
        return x

acceptVertexLineParser :: Parser [Q]
acceptVertexLineParser =
    do
        onLineSpaces
        token $ string "accept"
        token $ string "="
        x <- sepByTry vertex (token $ char ',')
        endOfStatement
        return x

headerSectionParser :: Parser ([DSType], Int, Maybe Int, Q, [Q])
headerSectionParser =
    do
        comments; (dsTypes, inputIndex, outputIndex) <- try dsTypesLineParser
                                            <|> return ([InputT], 0, Nothing) -- In case no dsTypesLine
        comments; start    <- try startVertexLineParser
                             <|> return "q0"
        comments; accepted <- try acceptVertexLineParser
                             <|> return []
        comments;
        return (dsTypes, inputIndex, outputIndex, start, accepted)




-- ex: { a, b, c }
choices :: Parser a -> Parser [a]
choices p = braced "{}" $ sepByTry1 p (token $ char ',')

inputLineParser :: [DSType] -> Parser [IEntry]
inputLineParser ds =
    do
        onLineSpaces
        v <- choices vertex
            <|> (: []) <$> vertex
            <|> (: []) <$> string "_"
        xs <- replicateM (length ds) (token (char ',') >> (
                choices alphabetLambda
               <|> (: []) <$> alphabetPlus
              ))
        onLineSpaces
        char ':'
        endOfStatement
        return (do
            v' <- v
            xs' <- sequence xs
            return (v', xs'))


outputLineParser :: [DSType] -> Parser (IEntry -> OEntry)
outputLineParser ds =
    do
        onLineSpaces
        v <- vertex
        xs <- mapM dsTypeToParser ds
        onLineSpaces
        endOfStatement
        return (\(_, inputs) -> (v, [f (inputs, i) | (f, i) <- zip xs [0..]]) )
    where
        -- (^) symbol to denote no change in output
        -- ex. q0, {1,2,3}, 4:
        --     q1, ^
        -- Note: can't be used with the wildcard

        -- "^" -> -1
        -- "^123" -> 123
        variable :: Parser (([A], Int) -> A)
        variable = do
            char '^'
            n <- try (Just . read <$> many1 digit) <|> return Nothing
            when (n >= Just (length ds)) (fail "Variable Index: value greater than number of DSes")
            return (\(as, index) -> as !! fromMaybe index n)

        dsTypeToParser :: DSType -> Parser (([A], Int) -> DSAction)
        dsTypeToParser ds = case ds of
            InputT -> return $ const InputA
            StackT -> token (char ',') >> ((const . StackA <$> ((string [lambda] >> return []) <|> many1 alphabet))
                                       <|> ((StackA . return) . ) <$> variable)
            QueueT -> token (char ',') >> ((const . QueueA <$> ((string [lambda] >> return []) <|> many1 alphabet))
                                       <|> ((QueueA . return) . ) <$> variable)
            TapeT  -> token (char ',') >> (
                        do
                            c <- alphabetLambda
                            char '|'
                            m <- oneOf "LRN"
                            let i = fromJust $ lookup m [('L', -1), ('N', 0), ('R', 1)]
                            return $ const $ TapeA c i
                    <|> do
                            nn <- variable
                            char '|'
                            m <- oneOf "LRN"
                            let i = fromJust $ lookup m [('L', -1), ('N', 0), ('R', 1)]
                            return (TapeA <$> nn <*> pure i)
                        )

{-
    a:
    b #
    c #
    d #
-}
inputOutputBlock :: [DSType] -> Parser [(IEntry, [OEntry])]
inputOutputBlock ds =
    do
        comments
        inputss <- many1 $ try $ inputLineParser ds
        outputs <- (many1 $ try $ outputLineParser ds) <?> "No output in this block!"
        let inputs = concat inputss
        onLineSpaces
        eol

        let toReturn = (do
            i <- inputs
            return (i, outputs <*> [i]))
        let b = elem '_' $ concatMap (\(_,as) -> concatMap show as) $ concatMap snd toReturn
        when b (fail "Can't have wildcard (_) refered to by the caret variable!")
        return toReturn



-- automatonParser :: Parser AutomatonWireframe
automatonParser :: Parser AutomatonWireframe
automatonParser =
    do
        (dsTypes, inputIndex, outputIndex, start, accepted) <- headerSectionParser

        deltaList <- concat <$> many (try $ inputOutputBlock dsTypes)

        comments; eof

        return (AutomatonWireframe dsTypes inputIndex outputIndex start accepted deltaList)






{-
    TESTING
-}
par :: Parser a -> String -> Either ParseError a
par myParser = parse myParser []

-- getRight :: Either a b -> b
-- getRight (Right x) = x
-- getRight _ = error "[getRight] It's not a right"

parseAutomaton :: String -> Either ParseError AutomatonWireframe
parseAutomaton s = par automatonParser (s ++ "\n\n")

automatonFromString :: String -> AutomatonWireframe
automatonFromString s =
    case parseAutomaton s of
        Left pError   -> error ("[automatonFromString] The string couldn't be converted to AutomatonWireframe\n"
                              ++ show pError)
        Right wf -> wf

