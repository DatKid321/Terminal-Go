module Board where

import Data.Array
import Data.List (intercalate)
import Control.Monad (ap, join)

import Types

data Position
  = First
  | Middle
  | Last
  deriving (Show, Eq, Ord, Enum, Ix)

type Location = (Position, Position)

data Marking
  = Blank
  | Star
  deriving (Show, Eq)

data Intersection
  = Empty Marking
  | Stone Player Marking
  deriving (Show, Eq)

type Board = Array Coord Intersection

data Reset
  = None
  | Store
  | Full
  deriving (Show, Eq)

-- Helpers

dup :: a -> (a, a)
dup = join (,)

extends :: a -> a -> ((a, a), (a, a))
extends x y = (dup x, dup y)

wrap :: String -> String -> String -> String
wrap pre suf = (pre ++) . (++ suf)

colour :: Reset -> Maybe (Int, Int, Int) -> Maybe (Int, Int, Int) -> String -> String -- Colours a string
colour res bg fg str
  | res == None  = ansiString                               -- Keeps colour
  | res == Store = "\ESC7" ++ ansiString ++ "\ESC8\ESC[1C"  -- Resets colour to previous
  | res == Full  = ansiString ++ "\ESC[0m"                  -- Resets colour to default
  where
    ansi :: Int -> Maybe (Int, Int, Int) -> String
    ansi n = maybe "" (\(r, g, b) -> "\ESC[" ++ show n ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m")

    ansiString :: String
    ansiString = ansi 48 bg ++ ansi 38 fg ++ str

-- For creating an empty board

hoshi :: Int -> [Coord] -- Gets star points for a board size
hoshi size =
  let
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
  in
    liftA2 (,) edge edge ++ [dup mid | odd size]

getLoc :: Int -> Coord -> Location -- Gets location of coord (corner, side, centre)
getLoc size (x, y) = (loc y, loc x)
  where
    loc :: Int -> Position
    loc n
      | n == 1    = First
      | n == size = Last
      | otherwise = Middle

emptyBoard :: Int -> Board -- Creates an empty board
emptyBoard size = array extents $ map set $ range extents
  where
    extents = extends 1 size
    stars   = hoshi size

    set :: Coord -> (Coord, Intersection)
    set pos = (pos, Empty $ if pos `elem` stars then Star else Blank)

-- For printing an established board

dim :: Board -> Int -- Get dimension of board
dim = fst . snd . bounds

printIntersection :: Int -> Coord -> Intersection -> String -- Get a string representing an intersection
printIntersection size pos intersection =
  case intersection of
    Empty Star -> "*"
    Empty Blank -> listArray (extends First Last) ["\x250C", "\x252C", "\x2510", "\x251C", "\x253C", "\x2524", "\x2514", "\x2534", "\x2518"] ! getLoc size pos

    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\x25CF"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\x25CF"

{-
    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\ESC[1m\x25CF\x0329\ESC[22m"
    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\ESC[1m\x25CF\x0329\x030D\ESC[22m"
    Stone Black _ -> colour Store Nothing (Just (0, 0, 0)) "\ESC[1m\x25CF\x030D\ESC[22m"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\ESC[1m\x25CF\x0329\ESC[22m"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\ESC[1m\x25CF\x0329\x030D\ESC[22m"
    Stone White _ -> colour Store Nothing (Just (255, 255, 255)) "\ESC[1m\x25CF\x030D\ESC[22m"
-}

row :: Board -> Int -> [(Coord, Intersection)]
row board y = map (ap (,) (board !)) ((, y) <$> [1 .. dim board])

printBoard :: Board -> String
printBoard board = wrap "\ESC[?25l" "\ESC[?25h" . unlines $ map (style . intercalate "\x2500" . render . row board) [1 .. dim board]
  where
    style :: String -> String
    style = colour Full (Just (242, 176, 108)) (Just (0, 0, 0)) . wrap " " " "

    render :: [(Coord, Intersection)] -> [String]
    render = map $ uncurry $ printIntersection $ dim board

-- Placing a stone

placeStone :: Board -> Player -> Coord -> Maybe Board
placeStone board player pos
  | not $ inRange (bounds board) pos = Nothing
  | Empty mark <- board ! pos = Just $ board // [(pos, Stone player mark)]
  | otherwise = Nothing

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