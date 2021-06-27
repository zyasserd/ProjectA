{-# LANGUAGE QuasiQuotes #-}
module AutomatonParser ( parseAutomaton, automatonFromString ) where

import Automaton

import Data.Char ( toLower, toUpper )
import Data.List ( intersperse )
import Data.Maybe ( fromJust)
import Control.Monad ( replicateM, void )
import Text.ParserCombinators.Parsec hiding ( token )



{-
    ### HEADER
        returns:
            ( [DSType], loadingIndex :: Int, start :: Q, f :: [Q] )

        definitions:
            specialChar     = "{}()|=,:\r\n_`lambda`"
            alphabet        = not specialChar
            vertex          = many1 alphabet
            alphabetPlus    = alphabet <|> lambda <|> "_"
            comment         = "#" >> many char >> eol

            -- note no "input"
            -- note case insensitive
            DStype = tape | t | stack | s | queue | q 

        [1]
        dsTypesLine:
            Empty                       => In that case, [DSType] = [InputT], loadingIndex = 0
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
alphabet = noneOf ("{}()|=,:\r\n\t _" ++ [lambda])

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



dsTypesLineParser :: Parser ([DSType], Int)
dsTypesLineParser =
    do
        onLineSpaces
        xs <- sepByTry parseSQT (skipMany1 onLineSpace)
        ys <- try ( (:)
                    <$> ((if null xs then return () else skipMany1 onLineSpace)
                        >> braced "()" parseSQT)
                    <*> many (try (skipMany1 onLineSpace *> parseSQT))
                )
                <|> return []
        endOfStatement
        if null ys then
            return (InputT:xs, 0)
        else
            return (xs ++ ys, length xs)
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

headerSectionParser :: Parser ([DSType], Int, Q, [Q])
headerSectionParser =
    do
        comments; (dsTypes, loadingIndex) <- try dsTypesLineParser
                                            <|> return ([InputT], 0) -- In case no dsTypesLine
        comments; start    <- startVertexLineParser
        comments; accepted <- try acceptVertexLineParser
                             <|> return []
        comments;
        return (dsTypes, loadingIndex, start, accepted)




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


outputLineParser :: [DSType] -> Parser OEntry
outputLineParser ds =
    do
        onLineSpaces
        v <- vertex
        xs <- mapM dsTypeToParser ds
        onLineSpaces
        endOfStatement
        return (v, xs)
    where
        dsTypeToParser :: DSType -> Parser DSAction
        -- remove pop var
        dsTypeToParser ds = case ds of
            InputT -> return InputA
            StackT -> token (char ',') >> StackA <$> ((string [lambda] >> return []) <|> many1 alphabet)
            QueueT -> token (char ',') >> QueueA <$> ((string [lambda] >> return []) <|> many1 alphabet)
            TapeT  -> token (char ',') >> do
                        c <- alphabetLambda
                        char '|'
                        m <- oneOf "LRN"
                        return $ TapeA c (fromJust $ lookup m [('L', -1), ('N', 0), ('R', 1)])
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
        inputs  <- inputLineParser ds
        outputs <- many1 $ try $ outputLineParser ds
        onLineSpaces
        try eol
        return (do
            i <- inputs
            return (i, outputs))


-- automatonParser :: Parser AutomatonWireframe
automatonParser :: Parser AutomatonWireframe
automatonParser =
    do
        (dsTypes, loadingIndex, start, accepted) <- headerSectionParser

        deltaList <- concat <$> many (try $ inputOutputBlock dsTypes)
        
        comments; eof

        return (AutomatonWireframe dsTypes loadingIndex start accepted deltaList)






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

