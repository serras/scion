module TestR
where

f1 :: String -> String
f1 = undefined

f2 :: Ord a => a-> a
f2 = undefined

main=let
        r1=f1 "titi"
        r2=f2 "toto"
     in r2