module Main where

import System.Environment (getArgs)
import Control.Exception (bracket_)
import Control.Applicative (liftA3)
import Control.Monad (when, unless)
import System.IO (hFlush, stdout)
import Data.Bool (bool)

import Board
import Parser
import Decoder
import Rules
import Types

-- Utils

draw :: Rules -> Position -> String -> IO ()
draw rules pos message = do
    putStr $
        "\ESC[H\ESC[0J"
        ++ printBoard rules pos
        ++ message
    hFlush stdout

wait :: IO ()
wait = do
    input <- getLine
    unless (input == "q") wait

finish :: Rules -> History -> IO ()
finish rules ((pos, _) : _) =
    draw rules pos (unlines $ map report [Black, White]) >> wait
  where
    report p = show p ++ ": " ++ show (score rules pos p)

finish _ [] = pure ()

stepMoves :: Rules -> History -> [(Player, Turn)] -> IO ()
stepMoves rules history ((player, turn) : turns) = do
    draw rules (fst $ head history) "Press Enter for next move (or 'q' to quit): "
    input <- getLine
    when (input /= "q") $ either (putStrLn . ("Illegal move: " ++) . show) next (play rules player turn history)
  where
    next :: History -> IO ()
    next = liftA3 bool (flip (stepMoves rules) turns) (finish rules) ended

stepMoves rules history [] = finish rules history

handle :: (Collection, String) -> IO ()
handle ([tree], "") = stepMoves rules [(start, Nothing)] $ decodeTree tree
  where
    rules = decodeRules tree
    start = decodeSetup (root tree) $ blank rules
handle (_, rest)    = putStrLn $ "Unparsed input: " ++ rest

-- Main

screen :: IO a -> IO a
screen = bracket_ (putStr "\ESC[?1049h\ESC[?25l") (putStr "\ESC[?25h\ESC[?1049l")

run :: IO ()
run = do
    [path] <- getArgs
    sgf <- readFile path
    maybe (putStrLn "Invalid SGF") handle (parse parseCollection sgf)

main :: IO ()
main = screen run

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

putStr "\ESC[2J\ESC[H" -- Clear screen
    let -- sgf = "(;SZ[9];B[ee];W[ge];B[fd];W[cf];B[eg];W[cd];B[gd];W[ec];B[he];W[gb];B[ch];W[hc];B[bg];W[hd];B[hf];W[bf];B[af];W[be];B[dd];W[dc];B[ed];W[dg];B[dh];W[fc];B[cg];W[df];B[ef];W[ae];B[ag];W[de];B[id];W[gc];B[ic];W[ib];B[ie];W[bc];B[gf];W[fh];B[fg];W[eh];B[gh];W[hh];B[gi];W[hg];B[gg];W[cb];B[ig];W[ha];B[ei];W[eb];B[ih];W[fa];B[di];W[bi];B[fi];W[];B[if];W[da];B[hi];W[ad];B[fe];W[];B[bh];W[ai];B[ah];W[eh];B[fh];W[ba];B[ci];W[ab];B[];W[])"
        -- sgf = "(;SZ[9];B[fe];W[cc];B[ec];W[ef];B[gg];W[ee];B[fd];W[fg];B[gh];W[dc];B[cg];W[dh];B[eb];W[ce];B[ba];W[cb];B[ca];W[ff];B[gf];W[fh];B[fi];W[ei];B[gi];W[ed];B[bb];W[bc];B[db];W[ac];B[ab];W[bf];B[he];W[ch];B[fa];W[da];B[ea];W[eh];B[fc];W[gb];B[gc];W[ga];B[hb];W[ha];B[hc];W[ib];B[ge];W[bh];B[hg];W[id];B[ic];W[ia];B[ie];W[ig];B[if];W[ih];B[hd];W[ii];B[hi];W[bg];B[fb];W[ha];B[ga];W[ia];B[ib];W[ia];B[ha];W[ae];B[hh];W[ig];B[ih];W[bd];B[];W[be];B[];W[ai];B[];W[bi];B[];W[dd];B[];W[af];B[];W[ag];B[];W[ci];B[];W[eg];B[];W[de];B[];W[dg];B[];W[cf];B[];W[])"
        sgf = "(;GM[1]FF[4]SZ[19]PB[Byun Sangil]BR[9d]PW[Yun Junsang]WR[9d]KM[6.5]RE[B+7.5]DT[2026-05-28]EV[6th Supreme Player tournament semifinal]AP[Go Kifu Viewer];B[pd];W[dc];B[dp];W[qp];B[ce];W[ed];B[oq];W[po];B[cn];W[qc];B[pc];W[qd];B[qf];W[qe];B[pe];W[rf];B[qg];W[rg];B[qh];W[of];B[jd];W[pf];B[oh];W[nd];B[ob];W[qb];B[mc];W[le];B[kc];W[nh];B[oi];W[pb];B[oc];W[ng];B[ni];W[hc];B[li];W[kh];B[hd];W[gd];B[ge];W[fd];B[lh];W[lg];B[ic];W[nc];B[nb];W[md];B[ld];W[mh];B[mi];W[he];B[ie];W[id];B[kg];W[kf];B[jg];W[if];B[hd];W[lb];B[hf];W[lc];B[jf];W[cg];B[ci];W[iq];B[bc];W[be];B[bf];W[cf];B[bd];W[de];B[ae];W[eg];B[gq];W[lq];B[oo];W[on];B[no];W[pr];B[lo];W[io];B[kp];W[eq];B[dq];W[dr];B[cr];W[gr];B[hp];W[ip];B[fr];W[ir];B[go];W[or];B[ei];W[pl];B[nn];W[di];B[dh];W[dj];B[eh];W[ch];B[bi];W[bh];B[ck];W[cb];B[in];W[fg];B[gh];W[ai];B[bj];W[bb];B[om];W[hq];B[fq];W[jn];B[hn];W[ko];B[lp];W[kq];B[kn];W[jo];B[jm];W[pn];B[ke];W[mb];B[ol];W[gg];B[hh];W[lf];B[ag];W[bg];B[aj];W[ah];B[mq];W[mr];B[nq];W[nr];B[pm];W[qm];B[ql];W[rl];B[qk];W[ho];B[gp];W[rk];B[rm];W[qn];B[qj];W[fh];B[fi];W[gi];B[gj];W[hg];B[ih];W[hb];B[rn];W[ro];B[pp];W[qo];B[pq];W[qq];B[gs];W[ig];B[jh];W[rj];B[ri];W[hs];B[hr];W[km];B[ln];W[gr];B[fs];W[ib];B[hr];W[kd];B[je];W[gr];B[sj];W[sn];B[hr];W[kb];B[is];W[jc];B[kr];W[lr];B[js];W[jq];B[og];W[nf];B[dg];W[df];B[fe];W[ee];B[rh];W[id];B[he];W[pg];B[ph];W[ls];B[sg];W[re];B[sk];W[sm];B[hs];W[sf];B[si];W[sl];B[sh];W[hi];B[hj];W[ic];B[jr];W[ef];B[ii];W[ff];B[jp];W[gf];B[ks])"
        -- sgf = "(;GM[1]FF[4]SZ[19]PB[Kim Eunji]BR[9d]PW[Yun Junsang]WR[9d]KM[6.5]RE[W+R]DT[2026-05-02]EV[6th Supreme Player 1st tournament]AP[Go Kifu Viewer];B[pd];W[dc];B[dp];W[qp];B[ce];W[ed];B[oq];W[po];B[ql];W[cq];B[dq];W[cp];B[cn];W[co];B[do];W[bn];B[lp];W[no];B[mq];W[pq];B[op];W[qn];B[nc];W[cm];B[dn];W[cr];B[hc];W[qi];B[qf];W[pk];B[bc];W[df];B[cf];W[dg];B[cg];W[dh];B[ch];W[di];B[he];W[hp];B[hm];W[jp];B[pl];W[ok];B[rj];W[qg];B[qj];W[pf];B[rf];W[oi];B[rh];W[md];B[nd];W[mf];B[fq];W[gq];B[fp];W[kq];B[oo];W[on];B[nn];W[ic];B[id];W[jc];B[jd];W[kc];B[fc];W[gd];B[hd];W[ne];B[mc];W[kd];B[om];W[pn];B[nm];W[ci];B[le];W[ke];B[lf];W[if];B[kf];W[je];B[fd];W[ld];B[me];W[mg];B[lh];W[oe];B[pe];W[lg];B[kg];W[kh];B[jh];W[ki];B[jg];W[ji];B[hh];W[hg];B[ii];W[gh];B[ij];W[og];B[db];W[cb];B[eb];W[cc];B[bb];W[rg];B[od];W[lj];B[gi];W[gg];B[dd];W[qh];B[rm];W[rn];B[rd];W[lb];B[el];W[oa];B[ma];W[mb];B[nb];W[na];B[fj];W[qa];B[rb];W[ee];B[ec];W[in];B[im];W[gr];B[lr];W[kr];B[hn];W[pr];B[io];W[nr];B[or];W[os];B[ms];W[ns];B[nq];W[er];B[fr];W[fs];B[dr];W[ds];B[eq];W[es];B[sn];W[so];B[sm];W[rp];B[jo];W[ll];B[gp];W[ip];B[ln];W[ri];B[kk];W[lk];B[fh];W[fg];B[cl];W[bm];B[ck];W[hb];B[gb];W[ha];B[ks];W[jr];B[bi];W[bj];B[bh];W[ho];B[go];W[jk];B[jj];W[kj];B[kl];W[km];B[jl];W[ko];B[pj];W[oj];B[jn];W[jm];B[il];W[kn];B[in];W[lo];B[mo];W[lm];B[mn];W[bl];B[bk];W[cj];B[al];W[de];B[cd];W[dm];B[em];W[qb];B[ob];W[la];B[qc];W[ge];B[fe];W[gf];B[gc];W[ra];B[pb];W[pa];B[nl];W[sb];B[rc];W[sj];B[sk];W[si];B[qk];W[nk];B[pi];W[ph];B[ga];W[ja];B[aj];W[eh];B[fi];W[ff];B[ro];W[qo];B[ml];W[ek];B[dl];W[ej];B[fk];W[qm];B[rl];W[sf];B[se];W[sg];B[mk];W[mj];B[hr];W[js];B[rr];W[rq];B[sq];W[rs];B[hq];W[gs])"

-}
