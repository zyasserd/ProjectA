import Automaton
import AutomatonParser
import qualified Data.Text.IO.Utf8 ( readFile )
import qualified Data.Text ( unpack )
import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))
import System.Environment ( getArgs )


readF :: FilePath -> IO String
readF path = Data.Text.unpack <$> Data.Text.IO.Utf8.readFile path

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    wf <- automatonFromString <$> readF (head args)
    -- print wf
    putStr ">>> "
    s <- getLine
    if s /= "" && head s == '?' then
        print $ wf # tail s
    else
        navigate wf [wf << s]

navigate :: AutomatonWireframe -> [AutomatonState] -> IO ()
navigate wf as = do
    putStr $ concat $ zipWith showAutomatonState [1..] as
    putStr ">>> "   
    s <- getLine
    case s of
        "q" -> return ()
        _   -> navigate wf (wf >=> as)


-- "1 3 31" Bold Italic Red
(!) :: String -> String -> String
s ! options = concatMap (\x -> "\ESC[" ++ x ++ "m") (words options)
           ++ s
           ++ "\ESC[0m"

showDS :: DS -> String
showDS (Input xs)   = "I" ! "42" ++ " " ++ reverse xs
showDS (Stack xs)   = "S" ! "42" ++ " " ++ xs
showDS (Queue xs)   = "Q" ! "42" ++ " " ++ xs
showDS (Tape xs ys) = "T" ! "42" ++ " " ++ xs ++ [head ys] ! "1 31" ++ tail ys

showAutomatonState :: Int -> AutomatonState -> String
showAutomatonState i (q, ds) = "(" ++ show i ! "32" ++ ") "
                            ++ q ! "32" ++ "  "
                            ++ head ls
                            ++ concatMap (replicate (5 + length q + length (show i)) ' ' ++ ) (tail ls)
    where
        ls = map (\x -> showDS x ++ "\n") ds
