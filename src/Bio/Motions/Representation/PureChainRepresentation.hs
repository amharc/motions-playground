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
import Control.Lens
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

type Space = M.Map Vec3 Atom

data PureChainRepresentation = PureChainRepresentation
    { space :: !Space
    , binders :: !(V.Vector BinderInfo)
    , beads :: !(V.Vector BeadInfo)
    , chainIndices :: !(U.Vector Int)
    , radius :: !Int
    , beadKinds :: !(V.Vector EnergyVector)
    }

instance Applicative m => ReadRepresentation m PureChainRepresentation where
    getBinders PureChainRepresentation{..} f = f binders
    {-# INLINE getBinders #-}

    getNumberOfChains PureChainRepresentation{..} = pure $ U.length chainIndices - 1
    {-# INLINE getNumberOfChains #-}

    getChain PureChainRepresentation{..} ix f = f $ V.slice b (e - b) beads
      where
        [b, e] = U.unsafeIndex chainIndices <$> [ix, ix + 1]
    {-# INLINE getChain #-}

    getAtomAt pos PureChainRepresentation{..} = pure $ M.lookup pos space
    {-# INLINE getAtomAt #-}

instance Applicative m => Representation m PureChainRepresentation where
    loadDump dump = pure PureChainRepresentation
        { binders = V.fromList $ D.binders dump
        , beads = V.fromList $ concat $ D.chains dump
        , chainIndices = U.fromList $ scanl' (+) 0 $ map length $ D.chains dump
        , space = M.fromList $
                      [(binderPosition b, Binder b) | b <- D.binders dump]
                   ++ [(beadPosition   b, Bead   b) | b <- concat (D.chains dump)]
        , radius = D.radius dump
        , beadKinds = V.fromList $ D.beadKinds dump
        }

    makeDump repr = pure D.Dump
        { binders = V.toList $ binders repr
        , chains = zipWith (\b e -> V.toList $ V.slice b (e - b) $ beads repr)
                           indicesList (tail indicesList)
        , radius = radius repr
        , beadKinds = V.toList $ beadKinds repr
        }
      where
        indicesList = U.toList $ chainIndices repr

    generateMove = undefined -- TODO

    performMove (MoveFromTo from to) repr
        | Binder binderInfo <- atom = pure $
            let Just idx = V.elemIndex binderInfo $ binders repr
                binders' = binders repr V.// [(idx, binderInfo & position .~ to)]
            in  repr { space = space'
                     , binders = binders'
                     }
        | Bead beadInfo <- atom = pure $
            let beads' = beads repr V.// [(beadAtomIndex beadInfo, beadInfo & position .~ to)]
            in  repr { space = space'
                     , beads = beads'
                     }
      where
        atom = space repr M.! from
        atom' = atom & position .~ to
        space' = M.insert to atom' $ M.delete from $ space repr
