{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s f =
  flip filter (permutations s) <$>
  (flip S.member <$> (S.fromList . hlist . lines <$> readFile f))
-- shorter way:
-- (flip (filter . flip S.member) (permutations s) . S.fromList . hlist . lines) <$> readFile f
instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
