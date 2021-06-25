import Automaton

import Text.ParserCombinators.Parsec hiding ( token )
import Data.Char ( toLower, toUpper )
import Data.List ( intersperse )
import Control.Monad ( replicateM )


{-
### HEADER
returns: ( [DSType], loadingIndex :: Int, start :: Q, f :: [Q] )

    specialChar <- {}()=,:\r\n_`lambda`
    alphabet = not specialChar
    vertex = some alphabet
    alphabetPlus = alphabet <|> lambda <|> "_"

    DStype <- tape | stack | queue
1.
    DStype*                     => In that case there is an implicit (InputT)
|   DStype* (DStype) DStype*

2. start = vertex

Optional
    accept = many vertex

comment
   # many char

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

eol :: Parser String
eol =  try (string "\r\n")
   <|> string "\n"

token :: Parser a -> Parser a
token p =
    do
        spaces
        x <- p
        spaces
        return x

braced :: Parser a -> Parser a
braced p =
    do
        char '('
        x <- token p
        char ')'
        return x


{-
    real work starts
-}
alphabet :: Parser Char
alphabet = noneOf ("{}()=,:\r\n _" ++ [lambda])

vertex :: Parser String
vertex = many1 alphabet

alphabetLambda :: Parser Char
alphabetLambda = alphabet <|> char lambda

alphabetPlus :: Parser Char
alphabetPlus = alphabet <|> char lambda <|> char '_'

comment :: Parser String
comment = char '#' >> manyTill anyChar (try eol)

endOfStatement :: Parser ()
endOfStatement = (try (manyTill space eol) <|> (spaces >> comment)) >> return ()

dsTypeParser :: Parser ([DSType], Int)
dsTypeParser =
    do
        spaces
        xs <- (:) <$> parseSQT <*> many (try (skipMany1 space *> parseSQT))
        ys <- try (skipMany1 space >> (:) <$> braced parseSQT <*> many (try (skipMany1 space *> parseSQT)))
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

startVertexParser :: Parser Q
startVertexParser =
    do
        token $ string "start"
        token $ string "="
        x <- vertex
        endOfStatement
        return x

acceptedParser :: Parser [Q]
acceptedParser =
    do
        token $ string "accepted"
        token $ string "="
        x <- sepBy vertex (try (spaces >> char ',' >> spaces))
        endOfStatementz
        return x

headerParser :: Parser ([DSType], Int, Q, [Q])
headerParser =
    do
        token $ many comment; (dsTypes, loadingIndex) <- dsTypeParser
        token $ many comment; start    <- startVertexParser
        token $ many comment; accepted <- acceptedParser <|> return []
        token $ many comment; many1 (char '-') >> eol
        return (dsTypes, loadingIndex, start, accepted)

inputEntryParser :: [DSType] -> Parser [String]
inputEntryParser ds =
    do
        spaces
        v <- vertex
        xs <- replicateM (length ds) (token (char ',') >> entry)
        spaces
        char ':'
        endOfStatement
        return $ map (((v++",")++) . intersperse ',') (sequence xs)
    where
        entry :: Parser [Char]
        entry =
            do
                token $ char '{'
                -- xs <- sepBy alphabetPlus (token (char ','))
                xs <- (:) <$> alphabetPlus <*> many (try (token (char ',') *> alphabetPlus))
                token $ char '}'
                return xs
            <|>
            (: []) <$> alphabetPlus

-- parseOutputEntry :: [DSType] -> Parser String


-- automatonParser :: Parser AutomatonWireframe
-- automatonParser =
--     do
--         (dsTypes, loadingIndex, start, accepted) <- headerParser
