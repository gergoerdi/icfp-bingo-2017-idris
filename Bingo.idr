module Bingo

import Data.Vect

%default total

public export data Parity : Nat -> Type where
    Even : Parity (n + n)
    Odd : Parity (S (n + n))

public export parity : (n : Nat) -> Parity n
parity Z = Even {n = 0}
parity (S Z) = Odd {n = 0}
parity (S (S n)) with (parity n)
  parity (S (S (k + k))) | Even = rewrite plusSuccRightSucc k k in Even {n = S k}
  parity (S (S (S (k + k)))) | Odd = rewrite plusSuccRightSucc k k in Odd {n = S k}

public export spaceCount : Nat -> (Nat, Type -> Type)
spaceCount n with (parity n)
  spaceCount n@(k + k) | Even {n = k} = (n * n, const ())
  spaceCount n@(S (k + k)) | Odd  {n = k} = (2 * (k * n + k), id)

public export Bingo : Nat -> Type -> Type
Bingo n a with (spaceCount n)
  Bingo _ a | (k, extra) = (Vect k a, extra a)

test2 : Bingo 2 String
test2 = (["A1", "A2", "B1", "B2"], ())

test3 : Bingo 3 String
test3 = (["A1", "A2", "A3", "B1", "B3", "C1", "C2", "C3"], "F")

test4 : Bingo 4 String
test4 = (pure "", ())

toMatrix : (n : Nat) -> (m : Nat) -> Vect (n * m) a -> Vect n (Vect m a)
toMatrix Z _ [] = []
toMatrix (S n) m xs = let (ys, yss) = splitAt m xs in ys :: toMatrix n m yss

mult2 : (n : Nat) -> n + n = 2 * n
mult2 n = rewrite plusZeroRightNeutral n in Refl

export grid : Bingo n a -> Vect n (Vect n a)
grid {n = n} bingo with (parity n)
  grid {n = n@(k + k)} (xs , ()) | Even = toMatrix _ _ xs
  grid {n = S (k + k)}(xs, free) | Odd =
      let (before, after) = splitAt (k * n + k) xs in
      toMatrix _ _ (rewrite lemma in before ++ [free] ++ after)
    where
      n : Nat
      n = S (k + k)

      lemma:  n * n = ((k * n) + k) + (1 + (((k * n) + k) + 0))
      lemma = sym $
        -- ((k * n) + k) + (1 + ((k * n) + k) + 0) =
        rewrite plusZeroRightNeutral ((k * n) + k) in
        -- ((k * n) + k) + (1 + (k * n) + k) =
        rewrite plusAssociative ((k * n) + k) 1 ((k * n) + k) in
        -- (((k * n) + k) + 1) + (k * n) + k) =
        rewrite plusCommutative ((k * n) + k) 1 in
        -- 1 + ((k * n) + k)) + ((k * n) + k) =
        rewrite mult2 ((k * n) + k) in
        -- 1 + 2 * ((k * n) + k) =
        rewrite multDistributesOverPlusRight 2 (k * n) k in
        -- 1 + 2 * (k * n) + 2 * k
        rewrite multAssociative 2 k n in
        -- 1 + (2 * k) * n + 2 * k =
        rewrite sym (mult2 k) in
        -- 1 + (k + k) * n + (k + k) =
        rewrite plusCommutative ((k + k) * n) (k + k) in
        -- (k + k) * n + (1 + k + k) =
        -- (k + k) * n + n =
        -- (1 + k + k) * n =
        -- n * n
        Refl
