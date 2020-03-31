module Chess.Internal where

import Data.Tuple (swap)
import Data.List (find, sortOn, groupBy)
import Control.Monad (mfilter, join)
import Data.Maybe (maybe, isJust, fromJust)
import Lib.Coll
import Lib.Scalar
import Lib.Bench
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type Coord       = (Integer, Integer)
data Colour      = B | W deriving (Eq, Show, Ord)
data Piece       = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show, Ord)
type Square      = (Colour, Coord)
data Position    = Pos Piece Colour Coord deriving (Eq, Ord)
type Coordinates = Map Coord Position
type Pieces      = Map Colour (Map Piece (Set Coord))

data Move = Capture   Position Position       |
            Advance   Position Coord          | 
            Enpassant Position Coord Position |
            Promote   Position Piece Position | 
            Castle   (Position, Coord) 
                     (Position, Coord)
            deriving (Eq, Show, Ord) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq, Ord)

data Outcome = Illegal        |
               Draw           |
               Stalemate      |
               Forfeit Colour | 
               Checkmate Colour deriving (Show, Eq, Ord)

data Castles = Short | Long | Both | None deriving (Show, Eq, Ord)

data Board = Board { player      :: Colour,
                     check       :: Bool,
                     past        :: [Move],
                     coordinates :: Map Coord Position,
                     pieces      :: Pieces, 
                     blackCastle :: Castles,
                     whiteCastle :: Castles}

instance Show Board where
      show board = unlines [whiteView $ coordinates board, statistics board] -- make this based on the player? 

instance Show Position where
      show (Pos Pawn W xy)   = "Pawn   W " <> show xy
      show (Pos Pawn B xy)   = "Pawn   B " <> show xy
      show (Pos Rook W xy)   = "Rook   W " <> show xy
      show (Pos Rook B xy)   = "Rook   B " <> show xy
      show (Pos Bishop W xy) = "Bishop W " <> show xy
      show (Pos Bishop B xy) = "Bishop B " <> show xy
      show (Pos Knight W xy) = "Knight W " <> show xy
      show (Pos Knight B xy) = "Knight B " <> show xy 
      show (Pos King W xy)   = "King   W " <> show xy
      show (Pos King B xy)   = "King   B " <> show xy
      show (Pos Queen W xy)  = "Queen  W " <> show xy
      show (Pos Queen B xy)  = "Queen  B " <> show xy
      show (Pos Empty _ xy)  = "-        " <> show xy

showPieces :: Board -> IO ()
showPieces = putStrLn . unlines . join . map fanOut . M.toList . pieces
      where fanOut (c, ps) = [(show c) <> " :: "] <> (map row $ M.toList ps)
            row (p, cos)   = "     " <> (show p) <> " - " <> (show $ S.toList cos)

makeRow :: Show a => [a] -> String
makeRow = foldl (\l c -> l <> (show c) <> " | ") "| "

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

blackView :: Coordinates -> String
blackView = unlines . map makeRow . chunksOf 8 . sortOn (swap . coord) . M.elems

whiteView :: Coordinates -> String
whiteView = unlines . map makeRow . reverse . chunksOf 8 . sortOn (swap . coord) . M.elems

statistics :: Board -> String
statistics board = unlines ["Player:     " <> show (player board),
                            "In-Check:   " <> show (check board),
                            "Can castle: " <> show (pickCastle $ player board)]
      where pickCastle B = blackCastle board
            pickCastle W = whiteCastle board

develop :: Dir -> Square -> Square
develop dir (colour, (x, y)) = (colour, towards dir colour)
    where towards L  W = (x - 1, y)
          towards R  W = (x + 1, y)
          towards U  W = (x, y + 1)
          towards D  W = (x, y - 1)
          towards UL W = (x - 1, y + 1)
          towards UR W = (x + 1, y + 1)
          towards DL W = (x - 1, y - 1)
          towards DR W = (x + 1, y - 1)
          towards L  B = (x - 1, y)
          towards R  B = (x + 1, y)
          towards U  B = (x, y - 1)
          towards D  B = (x, y + 1)
          towards UL B = (x - 1, y - 1)
          towards UR B = (x + 1, y - 1)
          towards DL B = (x - 1, y + 1)
          towards DR B = (x + 1, y + 1)

push :: Square -> Position -> Square
push (c, s) (Pos _ _ e) = (c, e)

lookAhead :: Board -> Dir -> Square -> Maybe Position
lookAhead board dir = lookAt board . snd . develop dir

follow :: Board -> Dir -> Square -> [(Square, Position)]
follow board d s = gather $ lookAhead board d s
      where gather (Just p)  = (s, p) : (gather $ lookAhead board d $ push s p)
            gather (Nothing) = []

follow' :: Board -> [Dir] -> Square -> [(Square, Position)]
follow' board dirs s0 = if (length dirs == length path) 
                        then path
                        else []
      where path            = gather dirs s0
            gather [] _     = []
            gather (d:ds) s = case (lookAhead board d s) of
                  (Just p)  -> (s0, p) : (gather ds $ push s p)
                  (Nothing) -> []

other :: Colour -> Colour
other W = B 
other B = W

position :: Move -> Position
position (Advance p _)     = p
position (Capture p _)     = p
position (Promote p _ _)   = p
position (Enpassant p _ _) = p
position (Castle (p, _) _) = p 

piece :: Position -> Piece
piece (Pos p _ _) = p

colour :: Position -> Colour
colour (Pos _ c _) = c

coord :: Position -> Coord
coord (Pos _ _ s) = s

square :: Position -> Square
square (Pos _ c s) = (c, s)

set :: Piece -> Square -> Position
set p (c, s) = Pos p c s

lookAt :: Board -> Coord -> Maybe Position
lookAt board coord = M.lookup coord $ coordinates board

empty :: (Square, Position) -> Bool
empty (_, (Pos p _ _)) = p == Empty

opposites :: (Square, Position) -> Bool
opposites ((c, _), (Pos _ c' _)) = c /= c'

opponent :: (Square, Position) -> Bool
opponent = every [not . empty, opposites]

jumped :: Board -> (Square, Position) -> Bool
jumped board ((colour, _), (Pos _ _ (x', y'))) = isJust $ mfilter opposed $ first $ past board 
      where opposed (Advance (Pos p c (xs, ys)) (xe, ye)) = (Pawn          == p) && -- it's a pawn 
                                                            (colour        /= c) && -- and my opponent
                                                            (abs (y' - ys) == 1) && -- is behind my new position
                                                            (x'           == xe)    -- we have the same file
            opposed _                                     = False


started :: Board -> (Square, Position) -> Bool
started board ((colour, coord), _)       = isJust $ mfilter (pawnOf colour) $ lookAt board coord
      where pawnOf W (Pos Pawn W (_, 2)) = True
            pawnOf B (Pos Pawn B (_, 7)) = True
            pawnOf _ _                   = False

safe :: Board -> (Dir -> (Square, Position) -> Bool)
safe board = let attackers = threats board
             in \dir s -> case (dir, fst $ fst s) of 
                               (R, W) -> null $ attackers [(W, (6, 1)), (W, (7, 1))]
                               (R, B) -> null $ attackers [(B, (6, 8)), (B, (7, 8))]
                               (L, W) -> null $ attackers [(W, (3, 1)), (W, (4, 1))]
                               (L, B) -> null $ attackers [(B, (3, 8)), (B, (4, 8))]
                               (_, _) -> False

backrank :: (Square, Position) -> Bool
backrank ((W, (_, 7)), (Pos _ _ (_, 8))) = True
backrank ((B, (_, 2)), (Pos _ _ (_, 1))) = True
backrank _                             = False

-- Replace dir with `Castle`
canCastle :: Board -> Dir -> (Square, Position) -> Bool
canCastle board L ((W, _), _) = whiteCastle board == Both || whiteCastle board == Long
canCastle board L ((B, _), _) = blackCastle board == Both || blackCastle board == Long
canCastle board R ((W, _), _) = whiteCastle board == Both || whiteCastle board == Short
canCastle board R ((B, _), _) = blackCastle board == Both || blackCastle board == Short
canCastle board _ _           = False

advance :: Piece -> (Square, Position) -> Move
advance piece ((c, s), (Pos _ _ e)) = Advance (Pos piece c s) e

capture :: Piece -> (Square, Position) -> Move
capture piece ((c, s), enemy) = Capture (Pos piece c s) enemy

enpassant :: Board -> (Square, Position) -> Move
enpassant board ((c, s), (Pos _ _ e)) = Enpassant (Pos Pawn c s) e enemy
      where enemy = fromJust $ lookAt board $ snd $ develop D (c, e)

promoteTo :: Piece -> (Square, Position) -> Move
promoteTo piece ((c, s), square) = Promote (Pos Pawn c s) piece square

castle :: (Move, Move) -> Move
castle (Advance k kp, Advance r rp) = Castle (k, kp) (r, rp)

pawnMoves :: Board -> Square -> [Move]
pawnMoves board = conjoin [ keepLast . consume [when (every [started board, empty]) $ advance Pawn]     . follow' board [U, U],
                            consume [when empty                                     $ advance Pawn]     . follow' board [U],
                            consume [once opponent                                  $ capture Pawn]     . follow' board [UR],
                            consume [once opponent                                  $ capture Pawn]     . follow' board [UL],
                            consume [once (every [jumped board, empty])             $ enpassant board]  . follow' board [UR],
                            consume [once (every [jumped board, empty])             $ enpassant board]  . follow' board [UL],
                            consume [once (every [backrank, empty])                 $ promoteTo Queen]  . follow' board [U],
                            consume [once (every [backrank, empty])                 $ promoteTo Knight] . follow' board [U],
                            consume [once (every [backrank, empty])                 $ promoteTo Rook]   . follow' board [U],
                            consume [once (every [backrank, empty])                 $ promoteTo Bishop] . follow' board [U],
                            consume [once (every [backrank, opponent])              $ promoteTo Queen]  . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Knight] . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Rook]   . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Bishop] . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Queen]  . follow' board [UR],
                            consume [once (every [backrank, opponent])              $ promoteTo Knight] . follow' board [UR],
                            consume [once (every [backrank, opponent])              $ promoteTo Rook]   . follow' board [UR],
                            consume [once (every [backrank, opponent])              $ promoteTo Bishop] . follow' board [UR]]

bishopMoves :: Board -> Square -> [Move]
bishopMoves board = conjoin [ consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board UL,
                              consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board UR,
                              consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board DR,
                              consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board DL]

rookMoves :: Board -> Square -> [Move]
rookMoves board = conjoin [ consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board U,
                            consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board D,
                            consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board L,
                            consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board R]

knightMoves :: Board -> Square -> [Move]
knightMoves board = conjoin [ consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [U, U, L],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [U, U, R],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [D, D, L],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [D, D, R],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [L, L, U],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [L, L, D],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [R, R, U],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [R, R, D]]

queenMoves :: Board -> Square -> [Move]
queenMoves board = conjoin  [ consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board U,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board D,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board L,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board R,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board UL,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board UR,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board DL,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board DR]

kingDevelopmentMoves :: Board -> Square -> [Move]
kingDevelopmentMoves board = conjoin [ consume [when empty $ advance King, once opponent $ capture King] . follow' board [U],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [D],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [L],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [R],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [UL],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [UR],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [DL],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [DR]]

kingCastlingMoves :: Board -> Square -> [Move]
kingCastlingMoves board = conjoin [map castle . zipped (keepLast . consume [exactly (every [empty, safeKingside, canCastle board R])  $ advance King] . follow' board [R, R])
                                                       (keepLast . consume [exactly (every [empty, canCastle board R])                $ advance Rook] . follow' board [L, L] . kingside),
                                   map castle . zipped (keepLast . consume [exactly (every [empty, safeQueenside, canCastle board L]) $ advance King] . follow' board [L, L])
                                                       (keepLast . consume [exactly (every [empty, canCastle board L])                $ advance Rook] . follow' board [R, R, R] . queenside)]
      where   safecheck        = safe board
              safeKingside     = safecheck R
              safeQueenside    = safecheck L
              kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))

kingMoves :: Board -> Square -> [Move]
kingMoves board = conjoin [kingDevelopmentMoves board, kingCastlingMoves board] 

threats :: Board -> ([Square] -> [Move])
threats board sqs = filter attacks $ threateningMoves board
      where conflict (Pos _ c _) (Pos p _ e) = any (\(c', s) -> p /= Empty && c /= c' && e == s) sqs
            attacks (Capture pos enemy)      = conflict pos enemy
            attacks (Promote pos _ enemy)    = conflict pos enemy
            attacks (Enpassant pos _ enemy)  = conflict pos enemy 
            attacks _                        = False

threateningMoves :: Board -> [Move]
threateningMoves board = filter threat $ join $ map extract $ filter ((== opponent) . colour) $ M.elems $ coordinates board
      where opponent                  = other $ player board
            extract (Pos King c s)    = kingDevelopmentMoves board (c, s)
            extract (Pos Bishop c s)  = bishopMoves board (c, s)
            extract (Pos Rook c s)    = rookMoves board (c, s)
            extract (Pos Pawn c s)    = pawnMoves board (c, s)
            extract (Pos Queen c s)   = queenMoves board (c, s)
            extract (Pos Knight c s)  = knightMoves board (c, s)
            extract _                 = []
            threat (Capture _ _)      = True
            threat (Promote _ _ _)    = True
            threat (Enpassant _ _ _)  = True
            threat  _                 = False
            
allMoves :: Board -> [Move]
allMoves board = join $ fmap (filter (isJust . permit board) .  movesPosition board) $ M.elems $ coordinates board

movesAt :: Board -> Square -> [Move]
movesAt board = join . spread [pawnMoves board, kingMoves board, rookMoves board, bishopMoves board, knightMoves board, queenMoves board]

movesPosition :: Board -> Position -> [Move]
movesPosition board (Pos Pawn c s)   = pawnMoves board (c, s)
movesPosition board (Pos King c s)   = kingMoves board (c, s)
movesPosition board (Pos Rook c s)   = rookMoves board (c, s)
movesPosition board (Pos Bishop c s) = bishopMoves board (c, s)
movesPosition board (Pos Queen c s)  = queenMoves board (c, s)
movesPosition board (Pos Knight c s) = knightMoves board (c, s)
movesPosition board (Pos Empty _ _)  = []

movesPiece :: Board -> (Piece, Colour) -> [Move]
movesPiece board (p, c) =  filter (isJust . permit board) 
                         $ join 
                         $ map (movesPosition board . Pos p c) 
                         $ maybe [] S.toList 
                         $ M.lookup p
                         $ maybe M.empty id 
                         $ M.lookup c
                         $ pieces board


findPieces :: Board -> (Piece, Colour) -> [Position]
findPieces board (p, c) = map (Pos p c)
                        $ maybe [] S.toList
                        $ M.lookup p
                        $ maybe M.empty id
                        $ M.lookup c
                        $ pieces board 

-- reconstruct by stating which positions should be replaced (Right) and which positions removed (Left)
reconstruct :: [Either Position Position] -> (Coordinates, Pieces) -> (Coordinates, Pieces)
reconstruct updates placement = foldr rewrite placement updates
      where detach (Pos Empty _ _) cs = cs
            detach (Pos _ _ c) cs     = S.delete c cs
            attach (Pos _ _ c) cs     = S.insert c cs
            rewrite (Right pos @ (Pos p c r)) (coordinates, pieces) = (M.insert r pos coordinates, M.adjust (M.adjust (attach pos) p) c pieces)
            rewrite (Left  pos @ (Pos p c r)) (coordinates, pieces) = (M.insert r (Pos Empty W r) coordinates, M.adjust (M.adjust (detach pos) p) c pieces)

castles :: Colour -> Castles -> Move -> Castles
castles W _    (Castle (Pos _ W _, _) _)        = None
castles B _    (Castle (Pos _ B _, _) _)        = None

castles W _    (Advance (Pos King W (5, 1)) _)  = None
castles B _    (Advance (Pos King B (5, 8)) _)  = None
castles W _    (Capture (Pos King W (5, 1)) _)  = None
castles B _    (Capture (Pos King B (5, 8)) _)  = None

castles W Short (Advance (Pos Rook W (8, 1)) _) = None
castles W Long  (Advance (Pos Rook W (1, 1)) _) = None
castles B Short (Advance (Pos Rook B (8, 8)) _) = None
castles B Long  (Advance (Pos Rook B (1, 8)) _) = None

castles W Both (Advance (Pos Rook W (8, 1)) _)  = Long
castles W Both (Capture (Pos Rook W (8, 1)) _)  = Long

castles W Both (Advance (Pos Rook W (1, 1)) _)  = Short
castles W Both (Capture (Pos Rook W (1, 1)) _)  = Short

castles B Both (Advance (Pos Rook B (8, 8)) _)  = Long
castles B Both (Capture (Pos Rook B (8, 8)) _)  = Long

castles B Both (Advance (Pos Rook B (1, 8)) _)  = Short
castles B Both (Capture (Pos Rook B (1, 8)) _)  = Short
castles _ c _                                   = c

commit :: Move -> (Coordinates, Pieces) -> (Coordinates, Pieces)
commit (Capture (Pos p c s) (Pos p' c' e))      = reconstruct [Right (Pos p c e), Left (Pos p c s), Left (Pos p' c' e)]
commit (Advance (Pos p c s) e)                  = reconstruct [Right (Pos p c e), Left (Pos p c s)]
commit (Enpassant (Pos p c s) e (Pos p' c' s')) = reconstruct [Right (Pos p c e), Left (Pos p c s), Left (Pos p' c' s')]
commit (Promote (Pos p c s) p'' (Pos p' c' e))  = reconstruct [Right (Pos p'' c e), Left (Pos p c s), Left (Pos p' c' e)] 
commit (Castle (Pos k kc ks, ke) 
               (Pos r rc rs, re))               = reconstruct [Right (Pos k kc ke), Right (Pos r rc re), Left (Pos k kc ks), Left (Pos r rc rs)]

-- you have to compute the other states aswell
-- this already is the opponent
evaluate :: Board -> Either Outcome Board
evaluate board = Right board
      where mate     = False -- && immoble
            stale    = False -- check this
            king     = findPieces board (King, player board)
            colour'  = player board

checked :: Board -> Bool
checked board = not $ null $ threats board [square king]
      where king = head $ findPieces board (King, player board)

apply :: Board -> Move -> Board
apply board move = let  king     = head $ findPieces board' (King, player board')
                        (cs, ps) = commit move (coordinates board, pieces board)
                        board'   = board { coordinates = cs,
                                           pieces      = ps, 
                                           past        = move : (past board),
                                           whiteCastle = castles W (whiteCastle board) move,
                                           blackCastle = castles B (blackCastle board) move,
                                           player      = other $ player board }
                   in board' { check = checked board' }

permit :: Board -> Move -> Maybe Board
permit board move = let board' = apply board move
                        colour = player board
                        tboard = board' { player = colour } 
                    in case (check board) of True  | not $ checked tboard -> Just board' -- check and can escape
                                             False | not $ checked tboard -> Just board' -- not in check, but don't move into check
                                             _                            -> Nothing

performEval :: Board -> Move -> Either Outcome Board
performEval board move = case (perform board move) of (Right board) -> evaluate board
                                                      result        -> result

performPermit :: Board -> Move -> Either Outcome Board
performPermit board move = maybe (Left Illegal) Right $ join $ fmap (permit board) $ find (== move) $ movesPosition board $ position move

perform :: Board -> Move -> Either Outcome Board 
perform board move = maybe (Left Illegal) Right $ fmap (apply board) $ find (== move) $ movesPosition board $ position move

board :: Board
board = Board { player      = W,
                past        = [],
                check       = False,
                blackCastle = Both,
                whiteCastle = Both,
                coordinates = M.fromList $ map (\p -> (coord p, p)) $ positions,
                pieces      = M.fromList $ map byPiece $ groupOn colour $ positions }
      where figs  = [([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], B),
                     ([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], B)]
            positions = do (y, (ps, c)) <- zip [1..] figs 
                           (x, p)       <- zip [1..] ps
                           return (Pos p c (x, y))
            set (p, es) = (p, S.fromList $ map coord es)
            byPiece (c, ps) = (c, M.fromList $ map set $ groupOn piece $ filter (not . (== Empty) . piece) ps)