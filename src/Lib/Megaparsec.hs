module Lib.Megaparsec where

import qualified Text.Megaparsec as M
import qualified Data.List.NonEmpty as L
import qualified Data.Set as S

-- should this be kept to the generality of a NonEmptyList and return NonEmpty a?
customError :: (e -> a) -> (M.ParseError s e -> a) -> M.ParseErrorBundle s e -> a
customError f ff = derive . L.head . M.bundleErrors
    where derive (M.FancyError _ errors) = f (strip $ S.findMin errors)
          derive (other)                 = ff other 
          strip  (M.ErrorCustom error)   = error
