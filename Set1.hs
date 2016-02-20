{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- 1.1
fiveRands = take 5 (rands(mkSeed 1))

rands s = fst(rand(s)) : rands(snd(rand(s)))


-- 1.2
randLetter s = (toLetter(fst(rand(s))), snd(rand(s)))

randLetters s = fst(randLetter(s)) : randLetters(snd(randLetter(s)))

randString3 = take 3 (randLetters(mkSeed 1))
