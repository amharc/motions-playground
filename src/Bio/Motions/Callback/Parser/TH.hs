{- |
Module      : Bio.Motions.Callback.Parser.TH
Description : Contains the Template Haskell stuff.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Bio.Motions.Callback.Parser.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Parser.Parser
import Bio.Motions.Representation.Class
import Bio.Motions.Types
import Control.Monad.State.Strict
import Data.Foldable
import Data.Traversable
import Data.MonoTraversable
import Data.Monoid
import Linear
import qualified Text.Parsec as P
import Data.Proxy
import qualified GHC.TypeLits as TL

type family THCallbackResult (name :: TL.Symbol) :: *

type family THCallbackArity (name :: TL.Symbol) :: Nat

type role THCallback nominal
newtype THCallback (name :: TL.Symbol) = THCallback { getTHCallback :: THCallbackResult name }

deriving instance Eq (THCallbackResult name) => Eq (THCallback name)
deriving instance Ord (THCallbackResult name) => Ord (THCallback name)
deriving instance Num (THCallbackResult name) => Num (THCallback name)
deriving instance Enum (THCallbackResult name) => Enum (THCallback name)
deriving instance Real (THCallbackResult name) => Real (THCallback name)
deriving instance Integral (THCallbackResult name) => Integral (THCallback name)

class LiftProxy a where
    liftProxy :: proxy a -> TypeQ

instance LiftProxy Int where
    liftProxy _ = [t| Int |]

instance LiftProxy Double where
    liftProxy _ = [t| Double |]

instance LiftProxy Zero where
    liftProxy _ = [t| Zero |]

instance LiftProxy n => LiftProxy (Succ n) where
    liftProxy _ = [t| Succ $(liftProxy (Proxy :: Proxy n)) |]

createCallback :: forall n a. (ForEachKNodes n, LiftProxy a, LiftProxy n) => ParsedCallback Lift n a -> Q [Dec]
createCallback ParsedCallback{..} = [d|
    type instance THCallbackResult $(name) = $(liftProxy (Proxy :: Proxy a))
    type instance THCallbackArity $(name) = $(liftProxy (Proxy :: Proxy n))

    instance Monoid (THCallback $(name)) where
        mempty = 0
        mappend = (+)

    instance Monad m => Callback m (THCallback $(name)) where
        runCallback repr = forEachKNodes repr run
          where
            run :: Vec (THCallbackArity $(name)) Atom -> m (THCallback $(name))
            run args =  pure . THCallback $ 
                $(eval EvalCtx { args = mkName "args", repr = mkName "repr" } expr)

        updateCallback = undefined -- TODO
    |]
  where
    CallbackSum expr = callbackResult
    name = litT $ strTyLit callbackName

quoteCallback :: forall proxy n. (ForEachKNodes n, ToNodeEx n, LiftProxy n) => proxy n -> String -> Q [Dec]
quoteCallback p str =
    case P.parse parseCallback "TH" str of
        Right exp -> createCallback (exp :: ParsedCallback Lift n Double)
        Left err -> fail $ show err

callback = QuasiQuoter
    { quoteDec = quoteCallback (Proxy :: Proxy (Succ (Succ Zero)))
    }

data Vec (n :: Nat) a where
    Nil :: Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a

data EvalCtx n = EvalCtx
    { args :: Name --Vec n Atom
    , repr :: Name
    }

access :: Node n -> Vec n a -> a
access FirstNode (Cons h _) = h
access (NextNode n) (Cons _ t) = access n t

eval :: EvalCtx n -> Expr Lift n a -> Q Exp
eval ctx (EAnd lhs rhs) =  [| $(eval ctx lhs) && $(eval ctx rhs) |]
eval ctx (EOr lhs rhs) =   [| $(eval ctx lhs) || $(eval ctx rhs) |]
eval ctx (ENot lhs) =      [| not $(eval ctx lhs) |]

eval ctx (ELt lhs rhs) =   [| $(eval ctx lhs) <  $(eval ctx rhs) |]
eval ctx (ELte lhs rhs) =  [| $(eval ctx lhs) <= $(eval ctx rhs) |]
eval ctx (EGt lhs rhs) =   [| $(eval ctx lhs) >  $(eval ctx rhs) |]
eval ctx (EGte lhs rhs) =  [| $(eval ctx lhs) >= $(eval ctx rhs) |]
eval ctx (EEq lhs rhs) =   [| $(eval ctx lhs) == $(eval ctx rhs) |]
eval ctx (ENeq lhs rhs) =  [| $(eval ctx lhs) /= $(eval ctx rhs) |]

eval ctx (EInt lhs) =      [| floor $(eval ctx lhs) |]
eval ctx (EFlt lhs) =      [| fromIntegral $(eval ctx lhs) |]

eval ctx (EAdd lhs rhs) =  [| $(eval ctx lhs) + $(eval ctx rhs) |]
eval ctx (ESub lhs rhs) =  [| $(eval ctx lhs) - $(eval ctx rhs) |]
eval ctx (EMul lhs rhs) =  [| $(eval ctx lhs) * $(eval ctx rhs) |]
eval ctx (EDiv lhs rhs) =  [| $(eval ctx lhs) / $(eval ctx rhs) |]
eval ctx (EIDiv lhs rhs) = [| $(eval ctx lhs) `div` $(eval ctx rhs) |]
eval ctx (EMod lhs rhs) =  [| $(eval ctx lhs) `mod` $(eval ctx rhs) |]

eval ctx (EMin lhs rhs) =  [| $(eval ctx lhs) `min` $(eval ctx rhs) |]
eval ctx (EMax lhs rhs) =  [| $(eval ctx lhs) `max` $(eval ctx rhs) |]

eval EvalCtx{..} (EBelongs node cls) = [|
    case (cls, access node $(varE args)) of
        (BeadClass x, Bead BeadType{..}) -> x == getBinderType binderType
        (BinderClass x, Binder BinderType{..}) -> x == getBeadType beadType
        _ -> False
    |]


eval EvalCtx{..} (EDist lhs rhs) = [|
    sqrt $ fromIntegral $ qd
        (getPosition $ access lhs $(varE args))
        (getPosition $ access rhs $(varE args))
    |]

eval _ (ELit lit) = [| lit |]

eval ctx EGr = undefined -- TODO
eval EvalCtx{..} (EAtomIx node) = undefined -- TODO
eval EvalCtx{..} (EChainIx node) = undefined -- TODO
eval EvalCtx{..} (EChromoIx node) = undefined -- TODO

instance Lift AtomClass where
    lift (BeadClass cls)   = [| BeadClass cls |]
    lift (BinderClass cls) = [| BinderClass cls |]

instance Lift (Node n) where
    lift FirstNode = [| FirstNode |]
    lift (NextNode n) = [| NextNode n |]

class ForEachKNodes (n :: Nat) where
    forEachKNodes :: (Monoid r, ReadRepresentation m repr, Monad m)
        => repr -> (Vec n Atom -> m r) -> m r

instance ForEachKNodes Zero where
    forEachKNodes _ fun = fun Nil

instance ForEachKNodes n => ForEachKNodes (Succ n) where
    forEachKNodes repr fun = forEachKNodes repr $ \xs ->
        forEachNode repr $ \x -> fun $ Cons x xs

forEachNode :: forall m r repr. (Monoid r, ReadRepresentation m repr, Monad m) => repr -> (Atom -> m r) -> m r
forEachNode repr f = do
    numChains <- getNumberOfChains repr
    binders <- getBinders repr $ go Binder
    beads <- fold <$> traverse (\idx -> getChain repr idx $ go Bead) [0..numChains-1]
    pure $ beads <> binders
  where
    go :: (MonoTraversable c, Element c ~ a) => (a -> Atom) -> c -> m r
    go conv = flip ofoldlM mempty $ \s x -> mappend s <$> f (conv x)
