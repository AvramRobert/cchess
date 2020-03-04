module Chess.Internal2 where

import Data.Tuple (swap)
import Data.List (find, sortOn)
import Data.List.NonEmpty (unfoldr, toList)
import Control.Monad (mfilter, join)
import Control.Applicative ((<|>))
import Data.Maybe (maybe, isJust, isNothing, catMaybes, fromJust)
import Lib.Coll
import Lib.Scalar
import Lib.PipedStream

type Coord    = (Integer, Integer)
data Colour   = B | W deriving (Eq, Show, Ord)
data Piece    = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show, Ord)
type Square   = (Colour, Coord)
data Position = Pos Piece Colour Coord deriving (Eq, Ord)

data Move = Capture   Position Coord       |
            Advance   Position Coord       | 
            Enpassant Position Coord Coord |
            Promote   Position Piece Coord |
            Castle   (Position, Coord) 
                     (Position, Coord)
            deriving (Eq, Show, Ord) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq, Ord)

data Outcome = Continue       |
               Draw           |
               Stalemate      |
               Forfeit Colour | 
               Checkmate Colour deriving (Show, Eq, Ord)

data Board = Board { player          :: Colour,
                     check           :: Bool,
                     past            :: [Move],
                     pieces          :: [Position],
                     kingsideCastle  :: (Bool, Bool),
                     queensideCastle :: (Bool, Bool) } deriving (Eq)

instance Show Board where
      show board = unlines [whiteView $ pieces board, statistics board] -- make this based on the player? 

instance Show Position where
      show (Pos Pawn W xy)   = "♙ W" <> show xy
      show (Pos Pawn B xy)   = "♟ B" <> show xy
      show (Pos Rook W xy)   = "♖ W" <> show xy
      show (Pos Rook B xy)   = "♜ B" <> show xy
      show (Pos Bishop W xy) = "♗ W" <> show xy
      show (Pos Bishop B xy) = "♝ B" <> show xy
      show (Pos Knight W xy) = "♘ W" <> show xy
      show (Pos Knight B xy) = "♞ B" <> show xy 
      show (Pos King W xy)   = "♔ W" <> show xy
      show (Pos King B xy)   = "♚ B" <> show xy
      show (Pos Queen W xy)  = "♕ W" <> show xy
      show (Pos Queen B xy)  = "♛ B" <> show xy
      show (Pos Empty _ xy)  = "-  " <> show xy

makeRow :: Show a => [a] -> String
makeRow = foldl (\l c -> l <> (show c) <> " | ") "| "

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

blackView :: [Position] -> String
blackView = unlines . map makeRow . chunksOf 8 . sortOn (swap . coord)

whiteView :: [Position] -> String
whiteView = unlines . map makeRow . reverse . chunksOf 8 . sortOn (swap . coord)

statistics :: Board -> String
statistics (Board player check past _ kc qc) = unlines ["Player:     " <> show player,
                                                        "In-Check:   " <> show check,
                                                        "Can castle: " <> (verify $ pickCastle player)]
      where pickCastle B = (snd kc, snd qc)
            pickCastle W = (fst kc, fst qc)
            verify (True, True)  = "Both sides"
            verify (True, False) = "King-Side"
            verify (False, True) = "Queen-Side"
            verify _             = "False" 

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
follow board d s = case (lookAhead board d s) of
      (Just p)  -> (s, p) : (follow board d $ square p)
      (Nothing) -> []

follow' :: Board -> [Dir] -> Square -> [(Square, Position)]
follow' board dirs s0 = if (length dirs == length path) 
                        then path
                        else []
      where path            = gather dirs s0
            gather [] _     = []
            gather (d:ds) s = case (lookAhead board d s) of
                  (Just p)  -> (s0, p) : (gather ds $ square p)
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
lookAt board coord' = find ((== coord') . coord) $ pieces board

-- castle :: Board -> (Square -> Maybe Move) -> (Square -> Maybe Move) -> Square -> Maybe Move
-- castle board kingf rookf square = do
--             kingMove   <- kingf square
--             rookMove   <- rookf square
--             let castle = createFrom kingMove rookMove
--             permitted board $ castle
--       where createFrom (Advance k kp) (Advance r rp) = Castle (k, kp) (r, rp)

castle :: Board -> (Square -> Move) -> (Square -> Move) -> Square -> Move
castle board kingf rookf square = let kingMove = kingf square
                                      rookMove = rookf square
                                  in case (kingMove, rookMove) of 
                                        (Advance k kp, Advance r rp) -> Castle (k, kp) (r, rp)


-- these functions are just priority functions
-- they are applied in order. first right and then left. 
-- isn't there a more interesting abstraction at play here?
-- the problem here is, that there's a particular speciality here.
-- If `keepf` stops, then the list consumptions goes to `stopf` and stops once `stopf` yields a value.
-- It doesn't continue with the list
-- The general abstraction behind this would, given a list of handles h :: [a -> Maybe b]
-- try h0 a1, if fail => h1 a1. If success => would continue with a2
-- I want it however to not continue with a2 once h1 was applied.

-- basically, there would be one primary function and a list of `handler` functions
-- the primary function would riffle through the list and the handler functions would be applied iff the primary one fails and the consumption would stop
label :: (a -> Maybe b) -> (a -> Maybe b) -> [a] -> [b]
label stopf keepf []     = []
label stopf keepf (a:as) = case (keepf a) of
      (Just b)   -> b : (label stopf keepf as)
      (Nothing)  -> maybe [] (\a -> a : []) $ stopf a

advanceWith' :: Piece -> Board -> (Square, Position) -> Maybe Move
advanceWith' piece board ((c, s), (Pos _ _ e)) = permitted board $ Advance (Pos piece c s) e

captureWith' :: Piece -> Board -> (Square, Position) -> Maybe Move
captureWith' piece board ((c, s), (Pos _ _ e)) = permitted board $ Capture (Pos piece c s) e

enpassant' :: Board -> (Square, Position) -> Maybe Move
enpassant' board (square @ (c, s), (Pos _ _ e)) = permitted board $ Enpassant (Pos Pawn c s) e (fst s, (snd s) - 1)

promoteTo' :: Piece -> Board -> (Square, Position) -> Maybe Move
promoteTo' piece board ((c, s), (Pos _ _ e)) = permitted board $ Promote (Pos Pawn c s) piece e


--- THESE THINGS NOW ARE REVERSED ---
--- THE LAST ELEMENTS HAVE THE LATEST VALUE ---
advanceWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
advanceWith piece board []                          = Nothing
advanceWith piece board ((square, (Pos _ _ e)) : _) = permitted board $ Advance (set piece square) e 

captureWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
captureWith piece board []                          = Nothing
captureWith piece board ((square, (Pos _ _ e)) : _) = permitted board $ Capture (set piece square) e  

-- promoteTo :: Piece -> Board -> [(Square, Position)] -> Maybe Move
-- promoteTo piece board []                          = Nothing
-- promoteTo piece board ((square, (Pos _ _ e)) : _) = permitted board $ Promote (set Pawn square) piece e

-- enpassant :: Board -> [(Square, Position)] -> Maybe Move
-- enpassant board []                                   = Nothing
-- enpassant board ((square @ (c, s), (Pos _ _ e)) : _) = permitted board $ Enpassant (set Pawn square) e (fst s, (snd s) - 1)

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

pawnOf :: Colour -> Position -> Bool
pawnOf W (Pos Pawn W (_, 2)) = True
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

advance :: Piece -> Board -> (Square, Position) -> Move
advance piece board ((c, s), (Pos _ _ e)) = Advance (Pos piece c s) e

capture :: Piece -> Board -> (Square, Position) -> Move
capture piece board ((c, s), (Pos _ _ e)) = Capture (Pos piece c s) e

enpassant :: Board -> (Square, Position) -> Move
enpassant board ((c, s), (Pos _ _ e)) = Enpassant (Pos Pawn c s) e (fst s, (snd s) - 1)

promotionTo :: Piece -> Board -> (Square, Position) -> Move
promotionTo piece board ((c, s), (Pos _ _ e)) = Promote (Pos Pawn c s) piece e

-- The current threats of a board are constant over a complete pass of move compilation.
-- That means that I could theoretically compute them once and hand them individually to every `*moves` function
permitted :: Board -> Move -> Maybe Move
permitted board move | check board = Nothing -- the move is permitted if, when applied, the board isn't in check anymore.
permitted board move               = Just move   

permitted' :: Board -> [Square -> [Pipe (Square, Position) Move]] -> Square -> [Move]
permitted' board fs = sink . conjoin fs

-- the complete board part can be done by the `conjoin` 
pawnMoves :: Board -> Square -> [Move]
pawnMoves board = permitted' board [accept (capture Pawn board) . while opponent . tap . follow' board [UL],
                                    accept (capture Pawn board) . while opponent . tap . follow' board [UR],
                                    accept (advance Pawn board) . while empty . tap . follow' board [U],
                                    accept (advance Pawn board) . while (every [started board, empty]) . tap . follow' board [U, U],
                                    accept (enpassant board)   . while (every [jumped board, empty])  . tap . follow' board [UR],
                                    accept (enpassant board)   . while (every [jumped board, empty])  . tap . follow' board [UL],
                                    
                                    accept (promotionTo Queen board)  . while (every [backrank, empty]) . tap . follow' board [U],
                                    accept (promotionTo Knight board) . while (every [backrank, empty]) . tap . follow' board [U],
                                    accept (promotionTo Bishop board) . while (every [backrank, empty]) . tap . follow' board [U],
                                    accept (promotionTo Rook board)   . while (every [backrank, empty]) . tap . follow' board [U],

                                    accept (promotionTo Queen board)  . while (every [backrank, opponent]) . tap . follow' board [UL],
                                    accept (promotionTo Knight board) . while (every [backrank, opponent]) . tap . follow' board [UL],
                                    accept (promotionTo Bishop board) . while (every [backrank, opponent]) . tap . follow' board [UL],
                                    accept (promotionTo Rook board)   . while (every [backrank, opponent]) . tap . follow' board [UL],

                                    accept (promotionTo Queen board)  . while (every [backrank, opponent]) . tap . follow' board [UR],
                                    accept (promotionTo Knight board) . while (every [backrank, opponent]) . tap . follow' board [UR],
                                    accept (promotionTo Bishop board) . while (every [backrank, opponent]) . tap . follow' board [UR],
                                    accept (promotionTo Rook board)   . while (every [backrank, opponent]) . tap . follow' board [UR]]


-- bishopMoves :: Board -> Square -> [Move]
-- bishopMoves board = spreadM [captureWith Bishop board . keepUntil opponent empty . follow board UR,
--                              captureWith Bishop board . keepUntil opponent empty . follow board UL,
--                              captureWith Bishop board . keepUntil opponent empty . follow board DR,
--                              captureWith Bishop board . keepUntil opponent empty . follow board DL]

bishopMoves :: Board -> Square -> [Move]
bishopMoves board = permitted' board [terminate (capture Bishop board) . while opponent . accept (advance Bishop board) . while empty . tap . follow board UR,
                                      terminate (capture Bishop board) . while opponent . accept (advance Bishop board) . while empty . tap . follow board UL,
                                      terminate (capture Bishop board) . while opponent . accept (advance Bishop board) . while empty . tap . follow board DR,
                                      terminate (capture Bishop board) . while opponent . accept (advance Bishop board) . while empty . tap . follow board DL]

-- rookMoves :: Board -> Square -> [Move]
-- rookMoves board = spreadM [captureWith Rook board . keepUntil opponent empty . follow board U,
--                            captureWith Rook board . keepUntil opponent empty . follow board U,
--                            advanceWith Rook board . takeWhile empty . follow board U,
--                            advanceWith Rook board . takeWhile empty . follow board U]

rookMoves :: Board -> Square -> [Move]
rookMoves board = permitted' board [terminate (capture Rook board) . while opponent . accept (advance Rook board) . while empty . tap . follow board U,
                                    terminate (capture Rook board) . while opponent . accept (advance Rook board) . while empty . tap . follow board D,
                                    terminate (capture Rook board) . while opponent . accept (advance Rook board) . while empty . tap . follow board R,
                                    terminate (capture Rook board) . while opponent . accept (advance Rook board) . while empty . tap . follow board L]


-- knightMoves :: Board -> Square -> [Move]
-- knightMoves board = spreadM [captureWith Knight board . keepUntil opponent empty . follow' board [U, U, L],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [U, U, R],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [D, D, L],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [D, D, R],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [L, L, U],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [L, L, D],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [R, R, U],
--                              captureWith Knight board . keepUntil opponent empty . follow' board [R, R, D],

--                              advanceWith Knight board . takeWhile empty . follow' board [U, U, L],
--                              advanceWith Knight board . takeWhile empty . follow' board [U, U, R],
--                              advanceWith Knight board . takeWhile empty . follow' board [D, D, L],
--                              advanceWith Knight board . takeWhile empty . follow' board [D, D, R],
--                              advanceWith Knight board . takeWhile empty . follow' board [L, L, U],
--                              advanceWith Knight board . takeWhile empty . follow' board [L, L, D],
--                              advanceWith Knight board . takeWhile empty . follow' board [R, R, U],
--                              advanceWith Knight board . takeWhile empty . follow' board [R, R, D]]
                             

knightMoves :: Board -> Square -> [Move]
knightMoves board = permitted' board [terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [U, U, L],
                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [U, U, R],
                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [D, D, L],
                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [D, D, R],

                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [L, L, U],
                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [L, L, D],
                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [R, R, U],
                                      terminate (capture Knight board) . while opponent . keepLast . accept (advance Knight board) . while empty . tap . follow' board [R, R, D]]

-- queenMoves :: Board -> Square -> [Move]
-- queenMoves board = spreadM [captureWith Queen board . keepUntil opponent empty . follow board UR,
--                             captureWith Queen board . keepUntil opponent empty . follow board UL,
--                             captureWith Queen board . keepUntil opponent empty . follow board DR,
--                             captureWith Queen board . keepUntil opponent empty . follow board DL,
--                             captureWith Queen board . keepUntil opponent empty . follow board U,
--                             captureWith Queen board . keepUntil opponent empty . follow board D,
--                             captureWith Queen board . keepUntil opponent empty . follow board L,
--                             captureWith Queen board . keepUntil opponent empty . follow board R,
                            
--                             advanceWith Queen board . takeWhile empty . follow board UR,
--                             advanceWith Queen board . takeWhile empty . follow board UL,
--                             advanceWith Queen board . takeWhile empty . follow board DR,
--                             advanceWith Queen board . takeWhile empty . follow board DL,
--                             advanceWith Queen board . takeWhile empty . follow board U,
--                             advanceWith Queen board . takeWhile empty . follow board D,
--                             advanceWith Queen board . takeWhile empty . follow board L,
--                             advanceWith Queen board . takeWhile empty . follow board R] 

queenMoves :: Board -> Square -> [Move]
queenMoves board = permitted' board [terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board UR,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board UL,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board DL,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board DR,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board U,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board D,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board L,
                                     terminate (capture Queen board) . while opponent . accept (advance Queen board) . while empty . tap . follow board R]

-- kingMoves :: Board -> Square -> [Move]
-- kingMoves board = spreadM [captureWith King board . takeWhile opponent . follow' board [U],
--                            captureWith King board . takeWhile opponent . follow' board [D],
--                            captureWith King board . takeWhile opponent . follow' board [L],
--                            captureWith King board . takeWhile opponent . follow' board [R],
--                            captureWith King board . takeWhile opponent . follow' board [UL],
--                            captureWith King board . takeWhile opponent . follow' board [UR],
--                            captureWith King board . takeWhile opponent . follow' board [DL],
--                            captureWith King board . takeWhile opponent . follow' board [DR],

--                            advanceWith King board . takeWhile empty . follow' board [U],
--                            advanceWith King board . takeWhile empty . follow' board [D],
--                            advanceWith King board . takeWhile empty . follow' board [L],
--                            advanceWith King board . takeWhile empty . follow' board [R],
--                            advanceWith King board . takeWhile empty . follow' board [UL],
--                            advanceWith King board . takeWhile empty . follow' board [UR],
--                            advanceWith King board . takeWhile empty . follow' board [DL],
--                            advanceWith King board . takeWhile empty . follow' board [DR],
                           
--                            castle board (advanceWith King board . takeWhile (every [safeKingside, empty, canCastle board R]) . follow' board [R, R])
--                                         (advanceWith Rook board . takeWhile (every [empty, canCastle board R]) . follow' board [L, L] . kingside),
--                            castle board (advanceWith King board . takeWhile (every [safeQueenside, empty, canCastle board L]) . follow' board [L, L])
--                                         (advanceWith Rook board . takeWhile (every [empty, canCastle board L]) . follow' board [R, R, R] . queenside)]
--         where safecheck        = safe board
--               safeKingside     = safecheck R
--               safeQueenside    = safecheck L
--               kingside  (W, _) = (W, (8, 1))
--               kingside  (B, _) = (B, (8, 8))
--               queenside (W, _) = (W, (1, 1))
--               queenside (B, _) = (B, (1, 8))


castle' :: (Move, Move) -> Move
castle' (Advance k kp, Advance r rp) = Castle (k, kp) (r, rp)

kingMoves :: Board -> Square -> [Move]
kingMoves board = permitted' board [terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [U],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [D],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [L],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [R],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [UL],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [UR],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [DL],
                                    terminate (capture King board) . while opponent . accept (advance King board) . while empty . tap . follow' board [DR],
                                    mapTo castle' . fork (keepLast . accept (advance King board) . exactly (every [safeKingside, empty, canCastle board R]) . tap . follow' board [R, R])
                                                         (keepLast . accept (advance Rook board) . exactly (every [empty, canCastle board R]) . tap . follow' board [L, L] . kingside) 
                                    -- mergeWith castle' . 
                                    --       wye (lastly (accept (advance King board)) . exactly (every [safeKingside, empty, canCastle board R]) . tap . follow' board [R, R])
                                    --           (lastly (accept (advance Rook board)) . exactly (every [empty, canCastle board R]) . tap . follow' board [L, L] . kingside)
                                              
                                              ]
        where safecheck        = safe board
              safeKingside     = safecheck R
              safeQueenside    = safecheck L
              kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))

moves :: Board -> [Move]
moves board = pieces board >>= (movesFor board)

movesAt :: Board -> Square -> [Move]
movesAt board = join . spread [pawnMoves board, kingMoves board, rookMoves board, bishopMoves board, knightMoves board, queenMoves board]

movesFor :: Board -> Position -> [Move]
movesFor board (Pos Pawn c s)   = pawnMoves board (c, s)
movesFor board (Pos King c s)   = kingMoves board (c, s)
movesFor board (Pos Rook c s)   = rookMoves board (c, s)
movesFor board (Pos Bishop c s) = bishopMoves board (c, s)
movesFor board (Pos Queen c s)  = queenMoves board (c, s)
movesFor board (Pos Knight c s) = knightMoves board (c, s)

movesOpponent :: Board -> [Move]
movesOpponent board = (filter ((== (other $ player board)) . colour) $ pieces board) >>= (movesFor board)

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
castled _ (w, b) _                          = (w, b)

-- you have to compute the other states aswell
evaluate :: Board -> (Outcome, Board)
evaluate board = if      mate  then (Checkmate colour', board)
                 else if stale then (Stalemate, board) 
                 else               (Continue, board)
      where mate     = check board && immoble
            stale    = False -- check this
            immoble  = all (check . apply board) $ movesOpponent board
            king     = fromJust $ find (every [(== colour') . colour, (== King) . piece]) $ pieces board
            colour'  = player board
            
apply :: Board -> Move -> Board
apply board move = let  king   = fromJust $ find (every [(== player board') . colour, (== King) . piece]) $ pieces board'
                        board' = board { pieces          = commit move $ pieces board, 
                                         past            = move : (past board),
                                         kingsideCastle  = castled R (kingsideCastle board) move,
                                         queensideCastle = castled L (queensideCastle board) move,  
                                         player          = other $ player board }
                   in board' { check = not $ null $ threats board' [square king] }
      where commit (Capture (Pos p c s) e)      = reconstruct [(Pos p c e)] [s]
            commit (Advance (Pos p c s) e)      = reconstruct [(Pos p c e)] [s]
            commit (Enpassant (Pos p c s) e r)  = reconstruct [(Pos p c e)] [s, r]
            commit (Promote (Pos p c s) p' e)   = reconstruct [(Pos p' c e)] [s] 
            commit (Castle ((Pos k kc ks), ke) 
                           ((Pos r rc rs), re)) = reconstruct [(Pos k kc ke), (Pos r rc re)] [ks, rs]

perform :: Board -> Move -> (Outcome, Board) 
perform board = evaluate . apply board

board :: Board
board = Board { player          = W,
                past            = [],
                check           = False,
                kingsideCastle  = (True, True),
                queensideCastle = (True, True),
                pieces          = do (y, (ps, c)) <- zip [1..] figs 
                                     (x, p)       <- zip [1..] ps
                                     return (Pos p c (x, y)) }
      where figs  = [([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], B),
                     ([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], B)]