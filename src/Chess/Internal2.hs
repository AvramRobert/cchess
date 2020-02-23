module Chess.Internal2 where

import Data.Tuple (swap)
import Data.List (find, sortOn)
import Data.List.NonEmpty (unfoldr, toList)
import Control.Monad (mfilter, join)
import Data.Functor (($>))
import Data.Maybe (maybe, isJust, isNothing, catMaybes, fromJust)

type Coord    = (Integer, Integer)
data Colour   = B | W deriving (Eq, Show)
data Piece    = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show)
type Square   = (Colour, Coord)
data Position = Pos Piece Colour Coord deriving (Eq)

data Move = Capture   Position Coord       |
            Advance   Position Coord       | 
            Enpassant Position Coord Coord |
            Promote   Position Piece Coord |
            Castle   (Position, Coord) 
                     (Position, Coord)
            deriving (Eq, Show) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq)

data Board = Board { check           :: Bool,
                     player          :: Colour,
                     past            :: [Move],
                     pieces          :: [Position],
                     kingsideCastle  :: (Bool, Bool),
                     queensideCastle :: (Bool, Bool)
                     } deriving (Eq)

instance Show Board where
      show = unlines . map makeRow . chunksOf 8 . sortOn (swap . coord) . pieces
            where makeRow    = foldl (\l c -> l <> (show c) <> " | ") "| "
                  chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

instance Show Position where
      show (Pos Pawn W xy)   = "♙ " <> show xy
      show (Pos Pawn B xy)   = "♟ " <> show xy
      show (Pos Rook W xy)   = "♖ " <> show xy
      show (Pos Rook B xy)   = "♜ " <> show xy
      show (Pos Bishop W xy) = "♗ " <> show xy
      show (Pos Bishop B xy) = "♝ " <> show xy
      show (Pos Knight W xy) = "♘ " <> show xy
      show (Pos Knight B xy) = "♞ " <> show xy 
      show (Pos King W xy)   = "♔ " <> show xy
      show (Pos King B xy)   = "♚ " <> show xy
      show (Pos Queen W xy)  = "♕ " <> show xy
      show (Pos Queen B xy)  = "♛ " <> show xy
      show (Pos Empty _ xy)  = "◻ " <> show xy

xor :: Bool -> Bool -> Bool
xor a b = a /= b

spread :: [a -> b] -> a -> [b]
spread fs a = [f a | f <- fs]

spreadM :: [a -> Maybe b] -> a -> [b]
spreadM [] a       = []
spreadM (f : fs) a = case (f a) of (Just b)  -> b : (spreadM fs a)
                                   (Nothing) -> spreadM fs a

every :: [a -> Bool] -> a -> Bool
every (p : ps) a = p a && every ps a

oneOf :: [(a -> Bool)] -> a -> Bool
oneOf [] a = True
oneOf fs a = isJust $ find (\f -> f a) fs

keepUntil :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
keepUntil stop keep (a : as) | keep a && stop a = a : []
keepUntil stop keep (a : as) | keep a = a : (keepUntil stop keep as)
keepUntil stop keep (a : as) | stop a = a : []
keepUntil _ _ _                       = []

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

lookAhead :: Board -> Dir -> Square -> Maybe Position
lookAhead board dir = lookAt board . snd . develop dir

follow :: Board -> Dir -> Square -> [(Square, Position)]
follow board dir square = go [] $ lookAhead board dir square
      where go xs (Just position) = go ((square, position) : xs) (lookAhead board dir $ nextSquare position)
            go xs (Nothing)       = xs
            nextSquare position   = (colour position, coord position)

follow' :: Board -> [Dir] -> Square -> [(Square, Position)]
follow' board all @ (d : ds) square       = go [] ds (lookAhead board d square) 
      where go xs (d : ds) (Just position)       = go ((square, position) : xs) ds (lookAhead board d $ nextSquare position)
            go xs [] _ | length all == length xs = xs
            go xs _ _                            = []
            nextSquare position                  = (colour position, coord position)

piece :: Position -> Piece
piece (Pos p _ _) = p

colour :: Position -> Colour
colour (Pos _ c _) = c

coord :: Position -> Coord
coord (Pos _ _ s) = s

square :: Position -> Square
square (Pos _ c s) = (c, s)

lookAt :: Board -> Coord -> Maybe Position
lookAt board coord' = find ((== coord') . coord) $ pieces board

castle :: Board -> (Square -> Maybe Move) -> (Square -> Maybe Move) -> Square -> Maybe Move
castle board kingf rookf square = do
            kingMove   <- kingf square
            rookMove   <- rookf square
            let castle = createFrom kingMove rookMove
            permitted board $ castle
      where createFrom (Advance k kp) (Advance r rp) = Castle (k, kp) (r, rp)

advanceWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
advnaceWith piece board []                        = Nothing
advanceWith piece board (((c, s), (Pos _ _ e)) : _) = permitted board $ Advance (Pos piece c s) e 

captureWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
captureWith piece board []                        = Nothing
captureWith piece board (((c, s), (Pos _ _ e)) : _) = permitted board $ Capture (Pos piece c s) e  

promoteTo :: Piece -> Board -> [(Square, Position)] -> Maybe Move
promoteTo piece board []                          = Nothing
promoteTo piece board (((c, s), (Pos _ _ e)) : _)   = permitted board $ Promote (Pos Pawn c s) piece e

enpassant :: Board -> [(Square, Position)] -> Maybe Move
enpassant board []                        = Nothing
enpassant board (((c, s), (Pos _ _ e)) : _) = permitted board $ Enpassant (Pos Pawn c s) e (fst s, (snd s) - 1)

empty :: (Square, Position) -> Bool
empty (_, (Pos p _ _)) = p == Empty

opponent :: (Square, Position) -> Bool
opponent ((c, _), (Pos _ c' _)) = c /= c'

jumped :: Board -> (Square, Position) -> Bool
jumped board ((colour, (x, y)), _)  = isJust $ mfilter opposed $ keepFirst $ past board 
      where keepFirst ((Advance p ps) : _)                 = Just (Advance p ps)
            keepFirst []                                   = Nothing
            opposed (Advance (Pos p c (xs, ys)) (xe, ye))  = (Pawn == p) && 
                                                             (colour /= c) &&   
                                                             (abs (xs - x) == 1) && 
                                                             (abs (ye - ys) == 2)

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

canCastle :: Board -> Dir -> (Square, Position) -> Bool
canCastle board L ((W, _), _) = fst $ queensideCastle board
canCastle board L ((B, _), _) = snd $ queensideCastle board
canCastle board R ((W, _), _) = fst $ kingsideCastle board
canCastle board R ((B, _), _) = snd $ kingsideCastle board
canCastle board _ _           = False

threats :: Board -> ([Square] -> [Move])
threats board = let moves = pieces board >>= (movesFor board)
                in \sqs -> filter (oneOf (fmap attacks sqs)) moves 
      where attacks (c, s) (Capture   (Pos _ c' _) e)    = c /= c' && s == e
            attacks (c, s) (Enpassant (Pos _ c' _) _ e)  = c /= c' && s == e
            attacks (c, s) (Promote   (Pos _ c' _) _ e)  = c /= c' && s == e
            attacks _ _                                  = False

king :: Board -> Colour -> Position
king board colour' = fromJust $ find (every [(== colour') . colour, (== King) . piece]) $ pieces board

-- The current threats of a board are constant over a complete pass of move compilation.
-- That means that I could theoretically compute them once and hand them individually to every `*moves` function
permitted :: Board -> Move -> Maybe Move
permitted board move | not $ check board                = Just move
permitted board move | not $ check $ perform board move = Just move
permitted board _                                       = Nothing 

pawnMoves :: Board -> Square -> [Move]
pawnMoves board = spreadM [captureWith Pawn board . takeWhile opponent                       . follow' board [UL],
                           captureWith Pawn board . takeWhile opponent                       . follow' board [UR],
                           advanceWith Pawn board . takeWhile empty                          . follow' board [U],
                           advanceWith Pawn board . takeWhile (every [started board, empty]) . follow' board [U, U],
                           enpassant board        . takeWhile (every [jumped board, empty])  . follow' board [UL],
                           enpassant board        . takeWhile (every [jumped board, empty])  . follow' board [UR],
                           promoteTo Queen board  . takeWhile (every [backrank, empty])      . follow' board [U],
                           promoteTo Knight board . takeWhile (every [backrank, empty])      . follow' board [U],
                           promoteTo Rook board   . takeWhile (every [backrank, empty])      . follow' board [U],
                           promoteTo Bishop board . takeWhile (every [backrank, empty])      . follow' board [U],
                           
                           promoteTo Queen board  . takeWhile (every [backrank, opponent])   . follow' board [UL],
                           promoteTo Knight board . takeWhile (every [backrank, opponent])   . follow' board [UL],
                           promoteTo Rook board   . takeWhile (every [backrank, opponent])   . follow' board [UL],
                           promoteTo Bishop board . takeWhile (every [backrank, opponent])   . follow' board [UL],
                           
                           promoteTo Queen board  . takeWhile (every [backrank, opponent])   . follow' board [UR],
                           promoteTo Knight board . takeWhile (every [backrank, opponent])   . follow' board [UR],
                           promoteTo Rook board   . takeWhile (every [backrank, opponent])   . follow' board [UR],
                           promoteTo Bishop board . takeWhile (every [backrank, opponent])   . follow' board [UR]]

bishopMoves :: Board -> Square -> [Move]
bishopMoves board = spreadM [captureWith Bishop board . keepUntil opponent empty . follow board UR,
                             captureWith Bishop board . keepUntil opponent empty . follow board UL,
                             captureWith Bishop board . keepUntil opponent empty . follow board DR,
                             captureWith Bishop board . keepUntil opponent empty . follow board DL]

rookMoves :: Board -> Square -> [Move]
rookMoves board = spreadM [captureWith Rook board . keepUntil opponent empty . follow board U,
                           captureWith Rook board . keepUntil opponent empty . follow board U,
                           advanceWith Rook board . takeWhile empty . follow board U,
                           advanceWith Rook board . takeWhile empty . follow board U]

knightMoves :: Board -> Square -> [Move]
knightMoves board = spreadM [captureWith Knight board . keepUntil opponent empty . follow' board [U, U, L],
                              captureWith Knight board . keepUntil opponent empty . follow' board [U, U, R],
                             captureWith Knight board . keepUntil opponent empty . follow' board [D, D, L],
                             captureWith Knight board . keepUntil opponent empty . follow' board [D, D, R],
                             captureWith Knight board . keepUntil opponent empty . follow' board [L, L, U],
                             captureWith Knight board . keepUntil opponent empty . follow' board [L, L, D],
                             captureWith Knight board . keepUntil opponent empty . follow' board [R, R, U],
                             captureWith Knight board . keepUntil opponent empty . follow' board [R, R, D],

                             advanceWith Knight board . takeWhile empty . follow' board [U, U, L],
                             advanceWith Knight board . takeWhile empty . follow' board [U, U, R],
                             advanceWith Knight board . takeWhile empty . follow' board [D, D, L],
                             advanceWith Knight board . takeWhile empty . follow' board [D, D, R],
                             advanceWith Knight board . takeWhile empty . follow' board [L, L, U],
                             advanceWith Knight board . takeWhile empty . follow' board [L, L, D],
                             advanceWith Knight board . takeWhile empty . follow' board [R, R, U],
                             advanceWith Knight board . takeWhile empty . follow' board [R, R, D]]
                             

queenMoves :: Board -> Square -> [Move]
queenMoves board = spreadM [captureWith Queen board . keepUntil opponent empty . follow board UR,
                            captureWith Queen board . keepUntil opponent empty . follow board UL,
                            captureWith Queen board . keepUntil opponent empty . follow board DR,
                            captureWith Queen board . keepUntil opponent empty . follow board DL,
                            captureWith Queen board . keepUntil opponent empty . follow board U,
                            captureWith Queen board . keepUntil opponent empty . follow board D,
                            captureWith Queen board . keepUntil opponent empty . follow board L,
                            captureWith Queen board . keepUntil opponent empty . follow board R,
                            
                            advanceWith Queen board . takeWhile empty . follow board UR,
                            advanceWith Queen board . takeWhile empty . follow board UL,
                            advanceWith Queen board . takeWhile empty . follow board DR,
                            advanceWith Queen board . takeWhile empty . follow board DL,
                            advanceWith Queen board . takeWhile empty . follow board U,
                            advanceWith Queen board . takeWhile empty . follow board D,
                            advanceWith Queen board . takeWhile empty . follow board L,
                            advanceWith Queen board . takeWhile empty . follow board R] 

kingMoves :: Board -> Square -> [Move]
kingMoves board = spreadM [captureWith King board . takeWhile opponent . follow' board [U],
                           captureWith King board . takeWhile opponent . follow' board [D],
                           captureWith King board . takeWhile opponent . follow' board [L],
                           captureWith King board . takeWhile opponent . follow' board [R],
                           captureWith King board . takeWhile opponent . follow' board [UL],
                           captureWith King board . takeWhile opponent . follow' board [UR],
                           captureWith King board . takeWhile opponent . follow' board [DL],
                           captureWith King board . takeWhile opponent . follow' board [DR],

                           advanceWith King board . takeWhile empty . follow' board [U],
                           advanceWith King board . takeWhile empty . follow' board [D],
                           advanceWith King board . takeWhile empty . follow' board [L],
                           advanceWith King board . takeWhile empty . follow' board [R],
                           advanceWith King board . takeWhile empty . follow' board [UL],
                           advanceWith King board . takeWhile empty . follow' board [UR],
                           advanceWith King board . takeWhile empty . follow' board [DL],
                           advanceWith King board . takeWhile empty . follow' board [DR],
                           
                           castle board (advanceWith King board . takeWhile (every [safeKingside, empty, canCastle board R]) . follow' board [R, R])
                                        (advanceWith Rook board . takeWhile (every [empty, canCastle board R]) . follow' board [L, L] . kingside),
                           castle board (advanceWith King board . takeWhile (every [safeQueenside, empty, canCastle board L]) . follow' board [L, L])
                                        (advanceWith Rook board . takeWhile (every [empty, canCastle board L]) . follow' board [R, R, R] . queenside)]
        where safecheck        = safe board
              safeKingside     = safecheck R
              safeQueenside    = safecheck L
              kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))

moves :: Board -> Square -> [Move]
moves board = join . spread [pawnMoves board, kingMoves board, rookMoves board, bishopMoves board, knightMoves board, queenMoves board]

movesFor :: Board -> Position -> [Move]
movesFor board (Pos Pawn c s)   = pawnMoves board (c, s)
movesFor board (Pos King c s)   = kingMoves board (c, s)
movesFor board (Pos Rook c s)   = rookMoves board (c, s)
movesFor board (Pos Bishop c s) = bishopMoves board (c, s)
movesFor board (Pos Queen c s)  = queenMoves board (c, s)
movesFor board (Pos Knight c s) = knightMoves board (c, s)

-- reconstruct by telling it which position to add and which coordinates to remove
reconstruct :: [Position] -> [Coord] -> [Position] -> [Position]
reconstruct ((Pos p c s) : ps) cs ((Pos _ _ r) : rs) | s == r = (Pos p c r) : (reconstruct ps cs rs)
reconstruct ps (c : cs) ((Pos _ _ r) : rs)           | c == r = (Pos Empty W r) : (reconstruct ps cs rs)
reconstruct [] [] rs                                          = rs
reconstruct ps cs (r : rs)                                    = r : (reconstruct ps cs rs)
reconstruct ps cs []                                          = []

castled :: Dir -> (Bool, Bool) -> Move -> (Bool, Bool)
castled R (w, b) (Castle (_, d) _)          = (w `xor` (d == (7, 1)), b `xor` (d == (7, 8)))
castled L (w, b) (Castle (_, d) _)          = (w `xor` (d == (3, 1)), b `xor` (d == (3, 8)))

castled R (w, b) (Advance (Pos Rook _ s) _) = (w && (s == (8, 1)), b && (s == (8, 8)))
castled L (w, b) (Capture (Pos Rook _ s) _) = (w && (s == (1, 1)), b && (s == (1, 8)))

castled _ (w, b) (Advance (Pos King _ s) _) = (w && (s == (5, 1)), b && (s == (5, 8)))
castled _ (w, b) (Capture (Pos King _ s) _) = (w && (s == (5, 1)), b && (s == (5, 8)))

castled _ (w, b) _                        = (w, b)

perform :: Board -> Move -> Board
perform board move = let board' = board { pieces          = commit move $ pieces board, 
                                          past            = move : (past board),
                                          kingsideCastle  = castled R (kingsideCastle board) move,
                                          queensideCastle = castled L (queensideCastle board) move,  
                                          player          = if (player board == W) then B else W
                                          }
                         kings  = [square $ king board' W, square $ king board' B]
                     in  board' { check = not $ null $ threats board' kings }
            where commit (Capture (Pos p c s) e)      = reconstruct [(Pos p c e)] [s]
                  commit (Advance (Pos p c s) e)      = reconstruct [(Pos p c e)] [s]
                  commit (Enpassant (Pos p c s) e r)  = reconstruct [(Pos p c e)] [s, r]
                  commit (Promote (Pos p c s) p' e)   = reconstruct [(Pos p' c e)] [s] 
                  commit (Castle ((Pos k kc ks), ke) 
                                 ((Pos r rc rs), re)) = reconstruct [(Pos k kc ke), (Pos r rc re)] [ks, rs]

board :: Board
board = Board { player          = W,
                past            = [],
                check           = False,
                kingsideCastle  = (True, True),
                queensideCastle = (True, True),
                pieces          = do (y, ps) <- zip [1..] figs 
                                     let c = col y
                                     (x, p)  <- zip [1..] ps
                                     return (Pos p c (x, y))
                }
      where col y = if (y == 7 || y == 8) then B else W
            figs  = [[Rook,  Knight, Bishop, Queen, King,  Bishop, Knight, Rook],
                     [Pawn,  Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn],
                     [Empty, Empty,  Empty,  Empty, Empty, Empty,  Empty,  Empty],
                     [Empty, Empty,  Empty,  Empty, Empty, Empty,  Empty,  Empty],
                     [Empty, Empty,  Empty,  Empty, Empty, Empty,  Empty,  Empty],
                     [Empty, Empty,  Empty,  Empty, Empty, Empty,  Empty,  Empty],
                     [Pawn,  Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn],
                     [Rook,  Knight, Bishop, Queen, King,  Bishop, Knight, Rook]]