import Automaton
import AutomatonParser

import Data.Char ( toLower )
import Data.Maybe ( isNothing )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.IO ( hSetBuffering, stdout, BufferMode( NoBuffering ) )
import qualified Data.Text.IO.Utf8 ( readFile )
import qualified Data.Text ( unpack )



{-
    Helper Functions
-}
readF :: FilePath -> IO String
readF path = Data.Text.unpack <$> Data.Text.IO.Utf8.readFile path

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



{-
    Here the real works starts!
-}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    wf <- automatonFromString <$> readF (head args)

    mainLoop wf

mainLoop :: AutomatonWireframe -> IO ()
mainLoop wf = do
    putStr ">>> "
    s <- getLine

    case s of
        "r"       -> do
                        putStrLn "File Reloaded!\n"
                        main
        "q"       -> putStrLn "Goodbye!\n"
        _         -> do
            case s of
                []        -> return ()
            -- (d)iagnostics
                "d"       -> print wf
            -- (i)nfo
                "i"       -> do
                                putStrLn $ automatonType wf
                                putStrLn ""
                                putStrLn $ automatonEntryFormat wf
                                putStrLn ""
            -- (t)ransitions
                "t"       -> do
                                putStrLn $ "Start  <- " ++ start wf
                                putStrLn $ "Accept <-"  ++ concatMap (" " ++) (accepted wf)
                                putStrLn ""
                                let pIEntry (q, as) = q ++ concatMap (\x -> ", " ++ [x]) as
                                let pOEntry (q, as) = q ++ concatMap (\x -> ", " ++ show x) as
                                putStr $ concat [ pIEntry ie ++ " ->\n" ++ (concat [pOEntry oe ++ "\n" | oe <- oes]) ++ "\n"  | (ie, oes) <- transitionList wf]
                ('?': xs) -> print $ wf <<?>> xs
                ('!': xs) -> if isNothing (outputIndex wf) then
                                putStrLn "No output index is chosen for this automaton!\n"
                             else do
                                putStrLn $ maybe "No result!" showDS (wf <<!>> xs)
                                putStrLn ""

                ('/': xs) -> navigateLoop wf [wf << xs]
                _         -> putStrLn "Unrecognized Input\n"

            mainLoop wf

navigateLoop :: AutomatonWireframe -> [AutomatonState] -> IO ()
navigateLoop wf as = do
    putStr $ concat $ zipWith showAutomatonState [1..] as
    putStr "|>> "
    s <- getLine
    case s of
        "q" -> return ()
        _   -> navigateLoop wf (wf >=> as)


