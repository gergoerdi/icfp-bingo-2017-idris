module Bingo

import Data.Vect

-- %default total

data IsEven : Nat -> Nat -> Type where
    Times2 : (n : Nat) -> IsEven (n + n) n

data IsOdd : Nat -> Nat -> Type where
    Times2Plus1 : (n : Nat) -> IsOdd (S (n + n)) n

total parity : (n : Nat) -> Either (Exists (IsEven n)) (Exists (IsOdd n))
parity Z = Left $ Evidence _ $ Times2 0
parity (S Z) = Right $ Evidence _ $ Times2Plus1 0
parity (S (S n)) = case parity n of
    Left (Evidence _ (Times2 k)) =>
        Left $ rewrite plusSuccRightSucc k k in Evidence _ $ Times2 (S k)
    Right (Evidence _ (Times2Plus1 k)) =>
        Right $ rewrite plusSuccRightSucc k k in Evidence _ $ Times2Plus1 (S k)

data Bingo : Nat -> Type -> Type where
    Even : IsEven n k -> Vect (n * n) a -> Bingo n a
    Odd : IsOdd n k -> a -> Vect (2 * (k * n + k)) a -> Bingo n a

-- MkBingo : Nat -> Type -> Type
-- MkBingo n a with (parity n)
--   MkBingo n@(k + k) a | Left (Evidence _ (Times2 k)) = Vect (n * n) a -> Bingo n a
--   MkBingo n@(S (k + k)) a | Right (Evidence _ (Times2Plus1 k)) = a -> Vect (2 * (k * n + k)) a -> Bingo n a

total MkBingo : Nat -> Type -> Type
MkBingo n a = case parity n of
  Left (Evidence _ (Times2 k)) => let n = k +k in Vect (n * n) a -> Bingo n a
  Right (Evidence _ (Times2Plus1 k)) => let n = 1 + k + k in a -> Vect (2 * (k * n + k)) a -> Bingo n a

foo2 : Nat -> Nat
foo2 n = case parity n of
  Left (Evidence k (Times2 k)) => let n = k + k in n * n
  Right (Evidence k (Times2Plus1 k)) => let n = S (k + k) in (2 * (k * n + k))

-- total mkBingo : (n : Nat) -> MkBingo n a
-- -- mkBingo n with (parity n)
-- --   mkBingo n@(k + k) | Left (Evidence _ (Times2 k)) = Even (Times2 k)
-- --   mkBingo n@(S (k + k)) | Right (Evidence _ (Times2Plus1 k)) = Odd (Times2Plus1 k)
-- mkBingo n = case parity n of
--   Left (Evidence _ (Times2 k)) => Even (Times2 k)
--   Right (Evidence _ (Times2Plus1 k)) => Odd (Times2Plus1 k)

-- MkBingoType : Nat -> Type -> Type
-- MkBingoType n a with (parity n)
--   MkBingoType n@(k + k) a | Left (k ** Times2 k) = Vect (n * n) a
--   MkBingoType n@(S (k + k)) a | Right (k ** Times2Plus1 k) = (a, Vect (2 * (k * n + k))a)

-- mkBingo : {n : Nat} -> MkBingoType n a -> Bingo n a
-- mkBingo {n = n} mk with (parity n)
--   mkBingo {n = k + k} xs | Left (k ** Times2 k) = Even _ xs
-- --   -- mkBingo (x, xs) | Right prf = ?q2

-- test2 : Bingo 2 String
-- test2 = mkBingo ["A1", "A2", "B1", "B2"]

-- test3 : Bingo 3 String
-- test3 = mkBingo ("F", ["A1", "A2", "A3", "B1", "B3", "C1", "C2", "C3"])

-- test4 : Bingo 4 String
-- test4 = mkBingo $ pure ""

toMatrix : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
toMatrix Z _ [] = []
toMatrix (S n) m xs = let (ys, yss) = splitAt m xs in ys :: toMatrix n m yss

mult2 : (n : Nat) -> n + n = 2 * n
mult2 n = rewrite plusZeroRightNeutral n in Refl

-- grid : Bingo n a -> Vect n (Vect n a)
-- grid (Even prf xs) = toMatrix _ _ xs
-- grid (Odd {n = S (k + k)} (Times2Plus1 k) free xs) =
--     let (before, after) = splitAt (k * n + k) xs in
--     toMatrix _ _ (rewrite lemma in before ++ [free] ++ after)
--   where
--     n : Nat
--     n = S (k + k)

--     lemma:  n * n = ((k * n) + k) + (1 + (((k * n) + k) + 0))
--     lemma = sym $
--       -- ((k * n) + k) + (1 + ((k * n) + k) + 0) =
--       rewrite plusZeroRightNeutral ((k * n) + k) in
--       -- ((k * n) + k) + (1 + (k * n) + k) =
--       rewrite plusAssociative ((k * n) + k) 1 ((k * n) + k) in
--       -- (((k * n) + k) + 1) + (k * n) + k) =
--       rewrite plusCommutative ((k * n) + k) 1 in
--       -- 1 + ((k * n) + k)) + ((k * n) + k) =
--       rewrite mult2 ((k * n) + k) in
--       -- 1 + 2 * ((k * n) + k) =
--       rewrite multDistributesOverPlusRight 2 (k * n) k in
--       -- 1 + 2 * (k * n) + 2 * k
--       rewrite multAssociative 2 k n in
--       -- 1 + (2 * k) * n + 2 * k =
--       rewrite sym (mult2 k) in
--       -- 1 + (k + k) * n + (k + k) =
--       rewrite plusCommutative ((k + k) * n) (k + k) in
--       -- (k + k) * n + (1 + k + k) =
--       -- (k + k) * n + n =
--       -- (1 + k + k) * n =
--       -- n * n
--       Refl
