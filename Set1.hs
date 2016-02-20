{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- 1.1
fiveRands = take 5 (rands(mkSeed 1))

rands s = fst(rand(s)) : rands(snd(rand(s)))


-- 1.2
randLetter :: Gen Char
randLetter s = (toLetter(fst(rand(s))), snd(rand(s)))

randLetters s = fst(randLetter(s)) : randLetters(snd(randLetter(s)))

randString3 = take 3 (randLetters(mkSeed 1))


-- 1.3
type Gen t = Seed -> (t, Seed)

randEven :: Gen Integer
randEven s = (fst(rand(s)) * 2, snd(rand(s)))

randOdd :: Gen Integer
randOdd s = (fst(randEven s) + 1, snd(randEven s))

randTen :: Gen Integer
randTen s = (fst(rand(s)) * 10, snd(rand(s)))

generalA :: (Integer -> a) -> Gen a
generalA f s = (f(fst(rand(s))), snd(rand(s)))


-- 1.4
randPair :: Gen (Char, Integer)
randPair s = ((fst(randLetter s), fst(rand (snd(rand s)))), snd(rand (snd(rand s))))

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair f1 f2 s = ((fst(f1 s), fst(f2(snd(f1 s)))), snd(f2(snd(f1(s)))))

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB ga gb f s = ((f (fst(ga s)) (fst(gb(snd(ga s))))), snd(gb(snd(ga(s)))))

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 f1 f2 s = generalB f1 f2 (,) s


-- 1.5
repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
--repRandom (g:t) s = (fst(g(s))) : (repRandom t snd(rand(s)))
repRandom (g:t) s = (fst(g(s)):(fst(repRandom t (snd(g(s))))), (snd(repRandom t (snd(g(s))))))
