module API where

import qualified Text.Megaparsec as M 
import qualified Chess.Game as G
import qualified Data.List.NonEmpty as L
import qualified Data.Set as S

type Parser a = M.Parsec Error String a

data Variant = InputError 
             | GameError 
             | ParseError
             deriving (Show, Eq, Ord)

data Error = Error { variant :: Variant, 
                     message :: String }
            deriving (Show, Eq, Ord)

customError :: (e -> a) -> (M.ParseError s e -> a) -> M.ParseErrorBundle s e -> a
customError f ff = derive . L.head . M.bundleErrors
    where derive (M.FancyError _ errors) = f (strip $ S.findMin errors)
          derive (other)                 = ff other 
          strip  (M.ErrorCustom error)   = error

failWith :: Error -> Parser a
failWith error = M.fancyFailure $ S.fromList [M.ErrorCustom error]

runParser :: Parser a -> String -> Either Error a
runParser parser = either (Left . makeError) Right . M.runParser parser ""
    where makeError = customError id (const $ Error ParseError "Something's wrong with the input")

-- FixMe: if users use a different kind of parser, I should create a typeclass that sits in-between my Megaparsec and theirs
-- So that if they want to parse something with their parser, as long as they can provide me an instance of whatever functionality I need
-- I can run their parser and then mine