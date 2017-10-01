module Shuffle

import Effects
import Effect.Random
import Data.Vect

export shuffle : Vect n a -> Eff (Vect n a) [RND]
shuffle [] = pure []
shuffle {n = S n} xs@(x :: xs') = do
    k <- rndFin n
    let x' = index k xs
    let (_ :: xs') = replaceAt k x xs
    (x' ::) <$> shuffle xs'

-- main : IO ()
-- main = do
--     xs' <- runInit [0] $ shuffle xs
--     print xs'
--     putStrLn ""
--   where
--     xs : Vect 4 Int
--     xs = [1, 2, 3, 4]

-- Local Variables:
-- idris-load-packages: ("contrib" "effects")
-- End:
