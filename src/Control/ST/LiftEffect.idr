module Control.ST.LiftEffect

import Control.ST
import Effects

%default total

export liftEff : (Handler eff m, Monad m) => (var : Var) -> Eff a [MkEff x eff] -> ST m a [var ::: State x]
liftEff var prog = do
    s <- read var
    (r ** [s']) <- lift $ runEnv [s] prog
    write var s'
    pure r
