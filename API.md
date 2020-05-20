# cchess API Documentation

*cchess* features mostly all functionality necessary for implementing chess-related apps. These are:
* Game creation 
* Game simulation
* Game querying
* Game rendering
* PGN parsing
* PGN writing


## What to import

`import qualified Chess as C`

Everything is contained in module `Chess`.

Just import that and you'll be fine.

## Types

Before we start using the API, it's best we go over some of *cchess*' most important types and see what they do and how they relate to each other.

*cchess* defines a number of "primitive" and "combinator" types that occur almost everywhere and are used throught the whole entire library.

### Coordinates

```haskell
type Coord = (Int, Int)
```

*cchess* sees a chess board as a 8x8 coordinate grid, with the origin at (1, 1) located at the top-left corner of the grid. 

1 has been chosen as the starting index to better fit with the chess model.

### Colours

```haskell
data Colour = B | W
```
Colours are of course the colours of the game, black and white.


### Squares

```haskell
type Square = (Colour, Coord)
```

Squares model the coordinate of a hypothetical piece of some colour on the board. Given any piece on the board, it sort-of views only its coordinate and colour, omitting its type. 

*NOTE:* A square doesn't refer to the colour of a square on the actual board surface.

### Pieces

```haskell
data Piece = Empty | Pawn | Knight | Bishop | Rook | Queen | King
```

*cchess* models pieces nominally and also considers an empty square as a valid piece. (this approach simplifies some internals in the API)


### Figures

```haskell
type Figure = (Piece, Colour)
```

Figures model a concerete piece, specific to one player (either black or white). 
It omits the *position* of that piece on the board.

This is basically the dual of `Square`.


### Positions

```haskell
data Position = Pos Piece Colour Coord
```

Positions are the union of `Figure` and `Square` and model the concrete position of a player's chess piece on the board. 

### Moves

```haskell
data Move = Capture Position Position
          | Advance Position Coord
          | Enpassant Position Coord Position
          | Promote Position Piece Position
          | Castle (Position, Coord) (Position, Coord)
```

Moves in *cchess* are modelled as an ADT and cover the following types:
* *Captures:*
  * ``` Capture Position Position ``` 
  * Left-hand `Position` captures right-hand `Position`
* *Advances:* 
  * ``` Advance Position Coord ```
  * Left-hand `Position` advances to `Coord`
* *Enpassant captures:* 
  * ``` Enpassant Position Coord Position ```
  * Left-hand `Position` advances to `Coord` and captures right-hand `Position`
  
* *Promote:* with type 
  * ``` Promote Position Piece Position ```
  * Left-hand `Position` promotes to `Piece` and captures (or advances) to the right-hand `Position`
  * *Note*: if there's nothing to capture, the piece of the right-hand `Position` is `Empty`
* *Castle* with type
  * ``` Castle (Position, Coord) (Position, Coord) ```
  * Left-hand tuple models the king's `Position`, which advances to the left-hand `Coord`
  * Right-hand tuple models the rook's `Position`, which advances to the right-hand `Coord`

### Boards

```haskell 
data Board = Board {
    player      :: Colour,
    check       :: Bool,
    past        :: [Move],
    coordinates :: Map Coord Position,
    pieces      :: Map Colour (Map Piece (Set Coord)),
    blackCastle :: Castles,
    whiteCastle :: Castles
}
```

This is the record that models a complete chess board.

* `player`
  * Stores the current player on the board
  
* `check`
  * Stores if the board is in check
* `past`
  * Stores moves done in the past
* `coordinates` and `pieces`
  * Only internally relevant
  * They are two isomorphic structures, that store the current configuration of the board
  * There's two of them, because certain functionality can be done faster by using one as opposed to the other
* `blackCastle` and `whiteCastle`
  * ```haskell
    data Castles = Short | Long | Both | None
    ```
  * Only internally relevant 
  * Stores what type of castling is available to the white and black players respectively


### Tags

```haskell
data Tag =  Event String
          | Site String
          | Date String
          | Round String
          | White String
          | Black String
          | Result Outcome
          | ... -- more in Chess.Game

```
Every game in *cchess* is designed to be a valid, standard-complying chess game. \
As such, every new game created with *cchess* is required to populate the minimum amount of chess game tags the standard forsees. 


They are the event's *name*, *site*, *date*, *round* and the names of the *white*
and *black* players respectively. 

### Games

```haskell
data Game = { tags  :: [Tag], 
              board :: Board }

```

This record models an entire chess game and is the record used throught the entire API. 

Both attributes it contains are rather self-explanatory, but to reiterate:
* `tags`
  * All the chess `Tag`s specified for the game 
* `board`
  * The chess `Board` on which the game is going to be played
  
### Results

```haskell
data Result = Continue Game | Retry Game | Terminate Game Reason
```

where 

```haskell
data Reason = Checkmate | Stalemate | Resignation | ... -- more in Chess.Game
```

Results model the outcomes of transformations applied on the board.
Typically, these are some form of application of moves.

* `Continue Game`
  * The transformation was successful
  * Contains the transformed `Game`
  
* `Retry Game`
  * The transformation is not allowed
  * Contains the untransformed `Game`
* `Terminate Game Reason`
  * The transformation was unsuccessful and/or the game was terminated for some reason
  * Contains the (potentially) transformed `Game`
  * Some termination reasons come from evaluating the board, others can be used by the user himself 

### Errors

```haskell
data Error = Error { variant :: Variant, message :: String }

data Variant = InputError | GameError | ParseError
```

Errors are kept fairly simple and are modelled in terms of variants and simple messages. Each variant represents the domain of origin of the error itself.

In general, actions performed on a board that are error prone, for example parsing a move and then applying it, are modelled to return an `Either Error Result`. 



## Usage 

Now that we know a bit about the nominal details of *cchess*, we can start using its API.

### Creating a game

This can be done in one of two ways.

#### Quick game

```haskell
game :: Game
game = C.quickGame
```

This creates a `Game` and pre-populates the mandatory chess tags with default values.

### New Game

```haskell
C.newGame (C.event "My Event")
          (C.site  "Mother's basement")
          (C.date  "12.11.2020")
          (C.round "Round 1")
          (C.white "Geoff")
          (C.black "Dave")
```

This creates a `Game` wherein the caller himself defines the values of each tag. (you can add additional tags later on) 

All of these parameters are `newtype`s and `Chess` contains functions for creating each one of them.