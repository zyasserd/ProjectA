import Automaton
import AutomatonParser

import Data.Char ( toLower )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.IO (hSetBuffering, stdout, BufferMode( NoBuffering ))
import qualified Data.Text.IO.Utf8 ( readFile )
import qualified Data.Text ( unpack )


readF :: FilePath -> IO String
readF path = Data.Text.unpack <$> Data.Text.IO.Utf8.readFile path

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    wf <- automatonFromString <$> readF (head args)
    putStr ">>> "
    s <- getLine

    case s of
        ('q': _)  -> die "Goodbye!" -- ! check if works in ghc
        ('p': _)  -> print wf
        ('t': _)  -> do
                        let pIEntry (q, as) = q ++ concatMap (\x -> ", " ++ [x]) as
                        let pOEntry (q, as) = q ++ concatMap (\x -> ", " ++ show x) as
                        putStr $ concat [ pIEntry ie ++ " ->\n" ++ (concat [pOEntry oe ++ "\n" | oe <- oes]) ++ "\n"  | (ie, oes) <- transitionList wf]
        ('?': xs) -> print $ wf <<?>> xs
        ('!': xs) -> putStrLn $ maybe "No result!" showDS (wf <<!>> xs) -- check if there is outputIndex first
        ('/': xs) -> navigate wf [wf << xs]
        _         -> putStrLn "Unrecognized Input\n"

    main

navigate :: AutomatonWireframe -> [AutomatonState] -> IO ()
navigate wf as = do
    putStr $ concat $ zipWith showAutomatonState [1..] as
    putStr "N>> "
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
showDS (Tape xs ys) = "T" ! "42" ++ " " ++ xs ++ [head ys] ! "1 31" ++ tail ys -- remove leading and trailing 'lambda's

showAutomatonState :: Int -> AutomatonState -> String
showAutomatonState i (q, ds) = "(" ++ show i ! "32" ++ ") "
                            ++ q ! "32" ++ "  "
                            ++ head ls
                            ++ concatMap (replicate (5 + length q + length (show i)) ' ' ++ ) (tail ls)
                            ++ "\n"
    where
        ls = map (\x -> showDS x ++ "\n") ds
