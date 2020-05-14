module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative c Empty     = Empty
derivative c Epsilon   = Empty
derivative c (Char x)  = if x == c then Epsilon else Empty
derivative c (Seq a b) = let
    f = Seq (derivative c a) b
    s = derivative c b
  in
     if nullable a then Alt f s else f
derivative c (Alt a b) = Alt (derivative c a) (derivative c b)
derivative c (Star a)  = Seq (derivative c a) (Star a)

nullable :: Regexp -> Bool
nullable Empty     = False
nullable Epsilon   = True
nullable (Char x)  = False
nullable (Seq a b) = nullable a && nullable b
nullable (Alt a b) = nullable a || nullable b
nullable (Star a)  = True
