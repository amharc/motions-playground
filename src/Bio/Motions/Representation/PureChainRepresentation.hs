{- |
Module      : Bio.Motions.Representation.PureChainRepresentation
Description : Contains a pure and ineffective chain-based 'Representation'.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bio.Motions.Representation.PureChainRepresentation(PureChainRepresentation) where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import qualified Bio.Motions.Representation.Dump as D
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type Space = M.Map Vec3 Atom

data PureChainRepresentation = PureChainRepresentation
    { space :: !Space
    , binders :: !(V.Vector BinderInfo)
    , chains :: !(V.Vector (V.Vector BeadInfo))
    , radius :: !Int
    , beadKinds :: !(V.Vector EnergyVector)
    }

instance Applicative m => ReadRepresentation m PureChainRepresentation where
    getBinders PureChainRepresentation{..} f = f binders
    {-# INLINE getBinders #-}

    getNumberOfChains PureChainRepresentation{..} = pure $ V.length chains
    {-# INLINE getNumberOfChains #-}

    getChain PureChainRepresentation{..} ix f = f $ chains V.! ix
    {-# INLINE getChain #-}

    getAtomAt pos PureChainRepresentation{..} = pure $ M.lookup pos space
    {-# INLINE getAtomAt #-}

instance Applicative m => Representation m PureChainRepresentation where
    loadDump dump = pure PureChainRepresentation
        { binders = V.fromList $ D.binders dump
        , chains = V.fromList $ V.fromList <$> D.chains dump
        , space = M.fromList $
                      [(binderPosition b, Binder b) | b <- D.binders dump]
                   ++ [(beadPosition   b, Bead   b) | b <- concat (D.chains dump)]
        , radius = D.radius dump
        , beadKinds = V.fromList $ D.beadKinds dump
        }

    makeDump repr = pure D.Dump
        { binders = V.toList $ binders repr
        , chains = V.toList $ V.toList <$> chains repr
        , radius = radius repr
        , beadKinds = V.toList $ beadKinds repr
        }

    generateMove = undefined -- TODO

    performMove = undefined -- TODO
