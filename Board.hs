module Board where

import Data.List (intercalate)
import Data.Bool (bool)
import Control.Applicative (liftA2, liftA3)
import Control.Monad (ap, join)

import Types
import Rules

data Edge
    = First
    | Middle
    | Last
    deriving (Show, Eq, Enum)

type Location = (Edge, Edge)

data Reset
    = None
    | Store
    | Full
    deriving (Show, Eq)

type RGB = (Int, Int, Int)

-- Helpers

wrap :: String -> String -> String -> String -- Add a prefix and suffix to a string
wrap pre suf = (pre ++) . (++ suf)

tup :: (Show a, Read t) => Int -> a -> t -- Create tuple with value repeated n times (just kinda fun)
tup n = read . wrap "(" ")" . intercalate "," . replicate n . show

ansi :: Reset -> Maybe RGB -> Maybe RGB -> String -> String -- Colours a string
ansi res bg fg str
    | res == None  = ansiString                               -- Keeps colour
    | res == Store = "\ESC7" ++ ansiString ++ "\ESC8\ESC[1C"  -- Resets colour to previous
    | res == Full  = ansiString ++ "\ESC[0m"                  -- Resets colour to default
  where
    ansiColour :: Int -> Maybe RGB -> String
    ansiColour n = maybe "" $ \(r, g, b) -> "\ESC[" ++ show n ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

    ansiString :: String
    ansiString = ansiColour 48 bg ++ ansiColour 38 fg ++ str

-- For creating an empty board

hoshi :: Int -> [Point] -- Gets star points for a board size
hoshi size = liftA2 (,) edge edge ++ [tup 2 mid | odd size]
  where
    mid :: Int
    mid = size `div` 2 + 1

    corners :: [Int]
    corners
        | size > 11 = [4, size - 3]
        | size >= 8 = [3, size - 2]
        | otherwise = []

    sides :: [Int]
    sides = [mid | odd size, size >= 17]

    edge :: [Int]
    edge = corners ++ sides

getLoc :: Int -> Point -> Location -- Gets location of point (corner, side, centre)
getLoc size (x, y) = (loc y, loc x)
  where
    loc :: Int -> Edge
    loc n
        | n == 1    = First
        | n == size = Last
        | otherwise = Middle

-- For printing an established board

printPoint :: Rules -> Point -> Colour -> String
printPoint rules pos colour = case colour of
    Nothing     -> bool (glyph $ getLoc (size rules) pos) "*" $ pos `elem` hoshi (size rules)
    Just player -> ansi Store Nothing (Just $ tup 3 $ 255 * fromEnum player) "\x25CF"
  where
    glyphs :: [[String]]
    glyphs = [["\x250C", "\x252C", "\x2510"], ["\x251C", "\x253C", "\x2524"], ["\x2514", "\x2534", "\x2518"]]

    glyph :: Location -> String
    glyph (row, col) = glyphs !! fromEnum row !! fromEnum col

row :: Rules -> Position -> Int -> [(Point, Colour)]
row rules pos y = map (ap (,) pos) $ (, y) <$> [1 .. size rules]

printBoard :: Rules -> Position -> String
printBoard rules pos = unlines $ map (style . intercalate "\x2500" . render . row rules pos) [1 .. size rules]
  where
    style :: String -> String
    style = ansi Full (Just (242, 176, 108)) (Just (0, 0, 0)) . wrap " " " "

    render :: [(Point, Colour)] -> [String]
    render = map $ uncurry $ printPoint rules

{-
main :: IO ()
main = do
  mapM_ (putStrLn . printBoard . emptyBoard) [5..19]
  let newBoard = emptyBoard 19
  let sgf = ";B[qd];W[dd];B[oc];W[pp];B[do];W[dq];B[fp];W[qi];B[cf];W[fd];B[bd];W[ch];B[ef];W[ck];B[cc];W[dc];B[hc];W[cd];B[bc];W[df];B[dg];W[bf];B[be];W[ce];B[cg];W[bg];B[dh];W[ci];B[db];W[eb];B[ca];W[ff];B[ee];W[fe];B[fg];W[gg];B[fh];W[de];B[gh];W[hg];B[he];W[id];B[hd];W[hf];B[ie];W[hh];B[hi];W[ii];B[ij];W[hj];B[gi];W[ji];B[jj];W[ki];B[hk];W[kj];B[bl];W[bk];B[jg];W[jf];B[if];W[ig];B[kg];W[nd];B[od];W[nf];B[mg];W[oe];B[pe];W[pf];B[qf];W[pg];B[qg];W[ph];B[me];W[ne];B[pj];W[qj];B[pl];W[nc];B[ob];W[ld];B[ke];W[pn];B[ni];W[kd];B[jd];W[lf];B[kf];W[lg];B[lh];W[kh];B[li];W[kl];B[nm];W[oj];B[oi];W[pk];B[pi];W[qk];B[ok];W[mk];B[nj];W[qh];B[pm];W[on];B[lm];W[ml];B[om];W[rm];B[qn];W[qo];B[nn];W[jn];B[rl];W[ql];B[qm];W[rn];B[op];W[pq];B[oq];W[fq];B[gq];W[gp];B[eq];W[fr];B[fo];W[er];B[hp];W[jp];B[jl];W[jk];B[ik];W[kk];B[im];W[ln];B[ko];W[kn];B[mm];W[km];B[fc];W[gc];B[gb];W[mp];B[oo];W[po];B[mo];W[lo];B[pr];W[mr];B[qq];W[rp];B[nr];W[rr];B[qr];W[rq];B[mf];W[le];B[ng];W[nb];B[md];W[mc];B[oa];W[kb];B[fb];W[ed];B[eg];W[fk];B[fj];W[ek];B[gf];W[ge];B[ec];W[ih];B[io];W[gk];B[gj];W[gr];B[hq];W[gm];B[hn];W[ir];B[cq];W[cr];B[cp];W[ep];B[eo];W[bn];B[cm];W[cn];B[dm];W[dn];B[em];W[bm];B[fm];W[hl];B[il];W[cl];B[af];W[bh];B[ej];W[gn];B[go];W[re];B[qe];W[rg];B[rf];W[rh];B[hr];W[hs];B[mq];W[lq];B[np];W[nq];B[qp];W[ro];B[mq];W[cb];B[bb];W[nq];B[og];W[jc];B[ic];W[mq];B[jb];W[kc];B[lb];W[la];B[ma];W[of];B[ka];W[lc];B[ns];W[qs];B[nk];W[sf];B[rd];W[ib];B[na];W[dp];B[co];W[ja];B[jo];W[kp];B[iq];W[jq];B[ms];W[ls];B[kr];W[lr];B[jr];W[is];B[dj];W[dk];B[br];W[bs];B[ar];W[bo];B[bp];W[la];B[gs];W[fs];B[ka];W[ps];B[or];W[la];B[gd];W[gf];B[ka];W[ap];B[se];W[la];B[ds];W[cs];B[ka];W[lj];B[mj];W[ll];B[ao];W[la];B[mb];W[an];B[sg];W[hb]"
  let coords = fst $ fromMaybe ([], "") (parse parseGame sgf)
  let finalBoard = foldl (\b (p, c) -> placeStone b p c) newBoard coords
  putStrLn $ printBoard finalBoard

48 5 94
sep :: Int -> String -> String -> String -> String -> String
sep n s h m l = intercalate s $ h : replicate (pred $ pred n) m ++ [l]

board :: Int -> String
board dim = (\f -> sep dim "\n" (f "\x250C" "\x252C" "\x2510") (f "\x251C" "\x253C" "\x2524") (f "\x2514" "\x2534" "\x2518")) (sep dim "\x2500")

⎛ 1 ⎞ ⎡ a ⎤ ⎧ x ⎫
⎜ 2 ⎟ ⎢ b ⎥ ⎪ y ⎪
⎜ 3 ⎟ ⎢ c ⎥ ⎨ z ⎬
⎜ 4 ⎟ ⎢ d ⎥ ⎪ y ⎪
⎝ 5 ⎠ ⎣ e ⎦ ⎩ x ⎭

┌─■─┐
├─┼─┤
└─■─┘
■   U032x■
 ╳
  ■

putStrLn "\x252C\x252C\x25CB\x25CF"
putStrLn "\x0329\x2192\x0329\x25CB\x25CF\n"

putStrLn "┌─●̩─┐"
putStrLn "├─○̩̍─┤"
putStrLn "└─●̍─┘"

putStrLn "┌─■─┐"
putStrLn "├─┼─┤"
putStrLn "└─■─┘"

Next steps:
Arrow key input - Rather than typing coords, interactive cursor
Letter coordinates - letters, numbers, characters

-}

{-
    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\ESC[1m\x25CF\x0329\ESC[22m"
    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\ESC[1m\x25CF\x0329\x030D\ESC[22m"
    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\ESC[1m\x25CF\x030D\ESC[22m"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\ESC[1m\x25CF\x0329\ESC[22m"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\ESC[1m\x25CF\x0329\x030D\ESC[22m"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\ESC[1m\x25CF\x030D\ESC[22m"
-}