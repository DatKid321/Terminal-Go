import Control.Monad (when, liftM3)
import System.IO (hFlush, stdout)
import Data.Bool (bool)

import Board
import Parser
import Rules
import Types

-- Utils

draw :: Rules -> Position -> String -> IO ()
draw rules pos message = do
    putStr $
        "\ESC[?25l\ESC[H"
        ++ printBoard rules pos
        ++ "\ESC[0J"
        ++ message
        ++ "\ESC[?25h"
    hFlush stdout

finish :: Rules -> History -> IO ()
finish rules (pos : _) = draw rules pos $
    "Black: " ++ show (score rules pos Black) ++ "\n" ++
    "White: " ++ show (score rules pos White) ++ "\n"
finish _ [] = pure ()

stepMoves :: Rules -> History -> [(Player, Turn)] -> IO ()
stepMoves rules history ((player, turn) : turns) = do
    draw rules (head history) "Press Enter for next move (or 'q' to quit): "
    input <- getLine

    when (input /= "q") $
        either
            (putStrLn . ("Illegal move: " ++) . show)
            next
            (play rules player turn history)
  where
    next :: History -> IO ()
    next = liftM3 bool (flip (stepMoves rules) turns) (finish rules) ended

stepMoves rules history [] =
    finish rules history

-- Main

main :: IO ()
main = do
    putStr "\ESC[2J\ESC[H" -- Clear screen
    let rules = Rules
            { size = 9,
              more = ()
            }
        -- sgf = "(;B[ee];W[ge];B[fd];W[cf];B[eg];W[cd];B[gd];W[ec];B[he];W[gb];B[ch];W[hc];B[bg];W[hd];B[hf];W[bf];B[af];W[be];B[dd];W[dc];B[ed];W[dg];B[dh];W[fc];B[cg];W[df];B[ef];W[ae];B[ag];W[de];B[id];W[gc];B[ic];W[ib];B[ie];W[bc];B[gf];W[fh];B[fg];W[eh];B[gh];W[hh];B[gi];W[hg];B[gg];W[cb];B[ig];W[ha];B[ei];W[eb];B[ih];W[fa];B[di];W[bi];B[fi];W[];B[if];W[da];B[hi];W[ad];B[fe];W[];B[bh];W[ai];B[ah];W[eh];B[fh];W[ba];B[ci];W[ab];B[];W[])"
        sgf = "(;B[fe];W[cc];B[ec];W[ef];B[gg];W[ee];B[fd];W[fg];B[gh];W[dc];B[cg];W[dh];B[eb];W[ce];B[ba];W[cb];B[ca];W[ff];B[gf];W[fh];B[fi];W[ei];B[gi];W[ed];B[bb];W[bc];B[db];W[ac];B[ab];W[bf];B[he];W[ch];B[fa];W[da];B[ea];W[eh];B[fc];W[gb];B[gc];W[ga];B[hb];W[ha];B[hc];W[ib];B[ge];W[bh];B[hg];W[id];B[ic];W[ia];B[ie];W[ig];B[if];W[ih];B[hd];W[ii];B[hi];W[bg];B[fb];W[ha];B[ga];W[ia];B[ib];W[ia];B[ha];W[ae];B[hh];W[ig];B[ih];W[bd];B[];W[be];B[];W[ai];B[];W[bi];B[];W[dd];B[];W[af];B[];W[ag];B[];W[ci];B[];W[eg];B[];W[de];B[];W[dg];B[];W[cf];B[];W[])"

        -- sgf = "(;B[jj];W[kk];B[jj];W[jj])"

        handle :: ([(Player, Turn)], String) -> IO ()
        handle (turns, "") = stepMoves rules [blank rules] turns
        handle (_, rest)   = putStrLn $ "Unparsed input: " ++ rest
    maybe (putStrLn "Invalid SGF") handle $ parse parseGame sgf

{-
testBoard :: Board
testBoard p
    | p `elem` [(2,2), (2,3), (3,3)] = Stone Black
    | p == (5,5)                     = Stone White
    | otherwise                      = Empty

main :: IO ()
main = do
    print $ string 5 testBoard (2,2)
    print $ string 5 testBoard (5,5)
    print $ string 5 testBoard (1,1)
-}
 