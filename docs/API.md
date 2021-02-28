# API Documentation

*cchess* features most of the functionality necessary for implementing chess-related apps. These are:
* Game creation 
* Game simulation
* Game querying
* Game rendering
* PGN parsing
* PGN writing


## What to import

```haskell
import qualified Chess as C
```

Everything is contained in module **Chess**.

Just import that and you'll be fine.

## Types

Before we start using the API, it's best we go over some of *cchess*' most important types and see what they do and how they relate to each other.

*cchess* defines a number of *primitive* and *combinator* types that occur almost everywhere and are used throught the whole entire library.

### Coordinates

```haskell
type Coord = (Int, Int)
```

*cchess* sees a chess board as a 8x8 coordinate grid, with the origin at (1, 1) located at the top-left corner of the grid. Internally, it also addresses squares on the board in this manner as opposed to the standard \<letter\>-\<index\> ("c3" for e.g.) notation.

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

**Note:** A square doesn't refer to the colour of a square on the actual board surface.

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

This data type denotes how chess moves are modelled and covers the following instances:
* *Captures:*
  * ```haskell
    Capture Position Position 
    ``` 
  * Left-hand `Position` captures right-hand `Position`
  
* *Advances:* 
  * ```haskell
    Advance Position Coord 
    ```
  * Left-hand `Position` advances to `Coord`
  
* *Enpassant captures:* 
  * ```haskell
    Enpassant Position Coord Position 
    ```
  * Left-hand `Position` advances to `Coord` and captures right-hand `Position`
  
* *Promotions:* 
  * ```haskell
    Promote Position Piece Position 
    ```
  * Left-hand `Position` promotes to `Piece` and captures (or advances) to the right-hand `Position`
  * *Note*: if there's nothing to capture, the piece of the right-hand `Position` is `Empty`
  
* *Castling*
  * ```haskell
    Castle (Position, Coord) (Position, Coord) 
    ```
  * Left-hand tuple marks the king's `Position`, which advances to the left-hand `Coord`
  * Right-hand tuple makes the rook's `Position`, which advances to the right-hand `Coord`

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

This is the record that models a complete chess board. It's only of internal relevance and shouldn't really bother you directly. It keeps track of the following:

* `player`
  * Current player on the board
  
* `check`
  * Is the board in check or not
  
* `past`
  * Moves made in the past
  
* `coordinates` and `pieces`
  * Configuration of the board
  * These two are isomorphic structures, but store the current configuration of the board in slightly different ways
  * There's two of them, because certain things can be done faster by using one as opposed to the other
  
* `blackCastle` and `whiteCastle`
  * ```haskell
    data Castles = Short | Long | Both | None
    ``` 
  * What type of castling is available to the white and black players respectively


### Tags and Entries

```haskell
data Event
data Site
data Date
...

data Tag a b where
  Event  :: Tag Event String
  Site   :: Tag Site String
  Date   :: Tag Date String
  ..

data Entry a where
  Entry :: Tag a -> a -> Entry a
```
These things model the tags (and their respective entries) of a chess game. 

Every game in *cchess* is designed to be a valid, standard-complying chess game. \
As such, every new game created with is required to populate the minimum amount of chess tags the standard forsees. \
Additional entries can be added later-on. (see **Creating a game**) 

These are a tad bit unusual, as they've been modelled in such a way that their usage type-wise guarantees their chess-affine "correctness".

Basically every tag `Tag a b` is composed of two types: the nominal type `a` that guarantees its uniqueness, and a content type `b` that specifies its actual contents.
These are then bolted together using a GADT and act as a type reference for each tag.

The basic idea is, if you want to find/add an actual tag value, you have to specify its type reference from the `Tag` GADT. (example in **Working with tags and entries**)
### Games

```haskell
data Game = { entries   :: [HEntry], 
              gameBoard :: Board }
```

where 
```haskell
data HEntry where
  HEntry :: Entry a -> HEntry
```
This record models an entire chess game and is the record used throught the entire API. 

Both attributes it contains are rather self-explanatory, but to reiterate:
* `entries`
  * All the chess tag entries specified for the game 
* `gameBoard`
  * The chess `Board` on which the game is going to be played
  
### Results

```haskell
data Result = Continue Game | Retry Game | Terminate Game Reason
```

where 

```haskell
data Reason = Checkmate | Stalemate | Resignation | ...
```

Results model the outcomes of transformations applied on the board.
Typically, these are some form of application of moves.

* ```haskell
  Continue Game
  ```
  * The transformation was successful
  * Contains the transformed `Game`
  
* ```haskell
  Retry Game
  ```
  * The transformation is not allowed
  * Contains the untransformed `Game`
* ```haskell
  Terminate Game Reason
  ```
  * The transformation was successful and has terminated the game in some way.
  * Contains the transformed `Game`
  * Some termination reasons are a direct consequence of evaluating the board, others can be used by the user himself

### Errors

```haskell
data Error = Error { variant :: Variant, message :: String }

data Variant = InputError | GameError | ParseError
```

Errors are kept fairly simple and are modelled in terms of variants and simple messages. Each variant represents the domain of origin of the error itself.

In general, actions performed on a board that are error prone, for example parsing a move and then applying it, are modelled to return an: 
```haskell
Either Error Result
``` 

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

#### New Game

```haskell
C.newGame (C.event "My Event")
          (C.site  "Mother's basement")
          (C.date  "12.11.2020")
          (C.round "Round 1")
          (C.white "Geoff")
          (C.black "Dave")
```

This creates a new game, where the caller himself defines the values for all mandatory chess tags. 

As previously mentioned, every game in *cchess* is designed to be a standard-complying game. \
This means that every newly created game has to define values for the following 6 tags: \
**Name**, **Site**, **Date**, **Round** and the names of the **White** and **Black** players respectively. (you can add additional tags later on) 

`Chess` contains functions for creating each one of them.

### Querying and applying moves

After a game is created, it can be used to either inspect details about the chess game itself, or to proceed with it.

#### Querying

You can query various things about the game. Most of these functions are self-explanatory and can be found in **Chess**, but some of the highlights are:

```haskell
game = C.quickgame

C.currentPlayer game -- get's the current player

C.legalMoves game -- returns all legal moves

C.currentPlayerMoves game -- returns all legal moves for the current player

C.movesFor C.W game -- returns all legal moves for a particular colour

C.evaluate game -- tells you if the game is a draw, stalemate or checkmate
-- ... more in Chess
```

#### Applying a move

Once a move has been chosen, it can be applied on the game. The application
will return a `Result` denoting its outcome.

```haskell
game = C.quickGame

legalMove = head $ C.legalMoves game

case (C.applyMove legalMove game) of
  (C.Continue game')         -> putStrLn "Application successful!"
  (C.Retry game')            -> putStrLn "This move cannot be applied. Try with another"
  (C.Terminate game' reason) -> putStrLn "Application successful and it ended game!"
```

#### Working with tags and entries

When working with tags and entries, you have to specify the type of the tag. These types are all put together in the `Tag` GADT and can be accessed from `Chess`.

Example:
1. Extracting a tag from a game:
```haskell
C.locate G.Event game -- => Maybe String
```

2. Creating a tag and addint it to a game: 
```haskell
-- For creating entries properly, `cchess` provides helper functions:

C.tag (C.whiteElo "1233") game
```

## Rendering a game

*cchess* currently provides an ascii rendering of chess games and supports various modes for it. \
(I assume in the future there will probably also be optional graphical versions of this.)

### Display modes

```haskell
data DisplayMode = GameMode | DebugMode | ErrorMode
```

These represent the display modes a *cchess* game can be rendered in.
They primarily vary in how components of the game are shown (pieces, board indexes), but also in what information is provided along-side the game view.

The default `DisplayMode` is `DebugMode` and every `Show` instance uses it.

### Displaying game components

```haskell
game = C.quickGame

D.showGame D.GameMode game  

D.showFigure D.GameMode (C.Pawn, C.W)

D.showPosition D.ErrorMode (C.Pos C.Pawn C.W (2, 3))

show game -- <=> D.showGame D.DebugMode game

-- ... more in Chess
```

There are explicit `show` functions for every data type *cchess* has and they all require a `DisplayMode` when called:

## PGN parsing and writing

*cchess* provides a fully fleged PGN parser and writer.

Given that parsing is an error-prone effect, functions of this kind return:

```haskell
Either Error a
```

### Parsing 

It can either parse PGN files directly or `String`s thereof. \
Conversely, it can either write entire `Game`s or individual moves. 

#### PGN

```haskell
-- loads all games for a `.pgn` file and splits them up 
C.pgnFromFile :: String -> IO [String]

-- loads all games from a `.pgn` file and tries to coerce them to `Game` instances
C.gamesFromFile :: String -> IO [Either Error [Game]]

-- parses a `String` PGN game 
C.parseGame :: String -> Either Error Game
-- more in Chess
```

As mentioned, PGN games can be either directly parsed from files or in-memory strings. 

**Note:** Given that PGN files can be continous, *cchess* makes a distiction in cardinality. There are distinct functions that read/parse either just one file/game or many.

#### Moves

```haskell
game = C.quickGame

case (C.parseMove "c3" game) of
  (Right m)  -> putStrLn "Parsed correctly!"
  (Left err) -> putStrLn ("Could not parse because: " <> C.message err)  
```

Moves can be read and parsed directly from PGN notation. 

#### Reading and applying

```haskell
game = C.quickGame

case (C.parseApplyMove "c3" game) of
  (Right (C.Continue game'))         -> ..
  (Right (C.Retry game'))            -> ..
  (Right (C.Terminate game' reason)) -> ..
  (Left err)                         -> ..
```

And, of course, you can read, parse and apply moves in one go. This will return an
`Either Error Result`.

#### Embedding (into) other parsers

*cchess*' functionality is designed to be embeddable into other parsers. In particular, it's reading and/or application of moves can be generically bound to them.

This library uses `Megaparsec`, but say if you were personally using `Attoparsec`, `Parsec` or any other parser-combinator-esque library, then you could fairly simply embed its functionality by implementing this type class for your parser type.

```haskell 
class ParserTie p where
  getInput :: p String
  setInput :: String -> p ()
  failWith :: Error -> p a
```

For any parser `p :: * -> *`, if you can tell me how to get its current input, set its input and make it fail based on the errors *cchess* uses, then I can make it integrate with your own parser.

There are of course separate functions that return a `Parser` variant of the API's functionality:
s
```haskell
-- parses a `Move`
C.moveParser :: (C.ParserTie p, Monad p) => C.Game -> p C.Move

-- parses and applies the `Move` to the `Game`, returns the new `Game` and the `Move`
C.appliedMoveParser :: (C.ParserTie p, Monad p) => C.Game -> p (C.Game, C.Move) -- parses and

-- parses and applies the `Move`, evaluates the resulting `Game`
C.evaluatedMoveParser :: (C.ParserTie p, Monad p) => C.Game -> p Result
```
*cchess* itself uses this abstraction in its implementation of the console chess game.

### Writing

#### PGN

```haskell
C.writeGame :: Game -> String
```

A `Game` can be directly serialised as a string.

#### Moves

```haskell
C.writeMove :: Move -> Game -> String
```

And, as mentioned, individual moves can be written-out in PGN notation.

*Why does this need a Game?* - you ask

Well, given the algebraic nature of PGN notation, the moves themselves can be written-out in a simplified from by using the games current state.