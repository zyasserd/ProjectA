import Automaton
import AutomatonParser

import Data.Maybe ( isNothing )
import System.Environment ( getArgs )
import System.IO ( hSetBuffering, stdout, BufferMode ( NoBuffering ) )
import qualified Data.Text ( unpack )
import qualified Data.Text.IO.Utf8 ( readFile )



{-
    Helper Functions
-}
readF :: FilePath -> IO String
readF path = Data.Text.unpack <$> Data.Text.IO.Utf8.readFile path

putLn :: IO ()
putLn = putStrLn ""

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


helpStr :: String
helpStr = "\nWelcome to ProjectA!\n\n\
\  1. (r)eload file\n\
\  2. (q)uit\n\
\  3. print (h)elp\n\
\  4. print (d)iagnostics\n\
\  5. print (i)nfo\n\
\  6. print (t)ransitions\n\
\  7. test membership   (?input string)\n\
\  8. print computation (!input string)\n\
\  9. enter navigation  (/input string)\n\
\     where you can press enter to advance one step\n\
\                         q     to quit navigation\n\n"


{-
    Here the real works starts!
-}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    wf <- automatonFromString <$> readF (head args)

    -- Sth to force the strict evaluation of wf
    -- and show any parsing errors
    putStr (seq wf "")
    putStr helpStr

    mainLoop wf


mainLoop :: AutomatonWireframe -> IO ()
mainLoop wf = do
    putStr ">>>> "
    s <- getLine

    case s of
        "r"       -> do
                        putStrLn "File Reloaded!\n"
                        main
        "q"       -> putLn
        _         -> do
            case s of
                []        -> return ()
            -- (h)elp
                "h"       -> do
                                putStr helpStr
            -- (d)iagnostics
                "d"       -> do
                                print wf
                                putLn
            -- (i)nfo
                "i"       -> do
                                putStrLn $ automatonType wf
                                putLn
                                putStrLn $ automatonEntryFormat wf
                                putLn
            -- (t)ransitions
                "t"       -> do
                                putStrLn $ "Start  <- " ++ start wf
                                putStrLn $ "Accept <-"  ++ concatMap (" " ++) (accepted wf)
                                putLn
                                let pIEntry (q, as) = q ++ concatMap (\x -> ", " ++ [x]) as
                                let pOEntry (q, as) = q ++ concatMap (\x -> ", " ++ show x) as
                                putStr $ concat [ pIEntry ie ++ " ->\n" ++ (concat [pOEntry oe ++ "\n" | oe <- oes]) ++ "\n"  | (ie, oes) <- transitionList wf]
            -- [?] is in
                ('?': xs) -> do
                                print $ wf <<?>> xs
                                putLn
            -- [!] compute
                ('!': xs) -> if isNothing (outputIndex wf) then
                                putStrLn "No output index is chosen for this automaton!\n"
                             else do
                                putStrLn $ maybe "No result!" showDS (wf <<!>> xs)
                                putLn
            -- [/] enter navigation mode
                ('/': xs) -> navigateLoop wf [wf << xs]
                _         -> putStrLn "Unrecognized Input\n"

            mainLoop wf

navigateLoop :: AutomatonWireframe -> [AutomatonState] -> IO ()
navigateLoop wf as = do
    putStr $ concat $ zipWith showAutomatonState [1..] as
    putStr "nav> "
    s <- getLine
    case s of
        "q" -> return ()
        _   -> navigateLoop wf (wf >=> as)


