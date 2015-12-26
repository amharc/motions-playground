{- |
Module      : Bio.Motions.Callback.Parser.Parser
Description : Contains the definitions of various 'Callback' parsing-related types.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bio.Motions.Callback.Parser.Parser
    ( CallbackFrequency(..)
    , AtomClass(..)
    , CallbackResult(..)
    , ParsedCallback(..)
    , Expr(..)
    , Node
    , parseCallback) where

import Bio.Motions.Callback.Class
import Data.Proxy
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

-- |Represents the frequency a callback has to be run
data CallbackFrequency = EveryNFrames Int | EveryNAcceptedFrames Int

-- |Represents the class of an atom
data AtomClass = BeadClass Int | BinderClass Int

-- |The return value of a callback
data CallbackResult a where
    CallbackSum     :: Expr a -> CallbackResult a
    CallbackProduct :: Expr a -> CallbackResult a
    CallbackList    :: Expr a -> CallbackResult a

-- |Represents a parsed callback
data ParsedCallback a = ParsedCallback
    { callbackFrequency :: CallbackFrequency
    , callbackName :: String
    , callbackArity :: Int
    , callbackCondition :: Expr Bool
    , callbackResult :: CallbackResult a
    }

-- |A wrapper around node argument identifiers.
newtype Node = Node { getNode :: Int }
    deriving (Eq, Show)

-- |The AST of the callback DSL.
data Expr a where
    EAnd  :: Expr Bool -> Expr Bool -> Expr Bool
    EOr   :: Expr Bool -> Expr Bool -> Expr Bool
    ENot  :: Expr Bool -> Expr Bool

    ELt   :: Ord a => Expr a -> Expr a -> Expr Bool
    ELte  :: Ord a => Expr a -> Expr a -> Expr Bool
    EGt   :: Ord a => Expr a -> Expr a -> Expr Bool
    EGte  :: Ord a => Expr a -> Expr a -> Expr Bool
    EEq   :: Eq a  => Expr a -> Expr a -> Expr Bool
    ENeq  :: Eq a  => Expr a -> Expr a -> Expr Bool

    EDist :: Expr Node -> Expr Node -> Expr Double
    EInt  :: RealFrac a => Expr a -> Expr Int
    EFlt  :: Expr Int -> Expr Double
    EGr   :: Expr Double

    EAdd  :: Num a => Expr a -> Expr a -> Expr a
    ESub  :: Num a => Expr a -> Expr a -> Expr a
    EMul  :: Num a => Expr a -> Expr a -> Expr a
    EDiv  :: Fractional a => Expr a -> Expr a -> Expr a
    EIDiv :: Integral a => Expr a -> Expr a -> Expr a
    EMod  :: Integral a => Expr a -> Expr a -> Expr a

    EMin  :: Ord a => Expr a -> Expr a -> Expr a
    EMax  :: Ord a => Expr a -> Expr a -> Expr a

    ELit  :: a -> Expr a

    EAtomIx   :: Node -> Expr Int
    EChainIx  :: Node -> Expr Int
    EChromoIx :: Node -> Expr Int

    EBelongs  :: Expr Node -> Expr AtomClass -> Expr Bool

-- |An alias, for simplicity.
type Parser a = Parsec String () a

-- |Parses a callback
parseCallback :: Parseable a => Parser (ParsedCallback a)
parseCallback = do
    reserved "CALLBACK"
    name <- stringLiteral

    reserved "EVERY"
    freq <-     (reserved "ACCEPTED" >> EveryNAcceptedFrames . fromIntegral <$> natural)
            <|> EveryNFrames . fromIntegral <$> natural

    reserved "NODES"
    arity <- fromIntegral <$> natural

    reserved "WHERE"
    cond <- expr

    reserved "COMPUTE"
    result <-     (reserved "SUM" >> CallbackSum <$> expr)
              <|> (reserved "PRODUCT" >> CallbackProduct <$> expr)
              <|> (reserved "LIST" >> CallbackList <$> expr)

    pure ParsedCallback
        { callbackFrequency = freq
        , callbackName = name
        , callbackArity = arity
        , callbackCondition = cond
        , callbackResult = result
        }

-- |Parses an expression, where @expr ::= 'term' | 'term' 'addop' expr@
expr :: Parseable a => Parser (Expr a)
expr = term `chainl1` addop

-- |Parses a term, where @term ::= 'factor' | 'factor' 'mulop' term@
term :: Parseable a => Parser (Expr a)
term = factor `chainl1` mulop

-- |Parses a factor, where @factor ::= (expr) | 'atom'@.
factor :: Parseable a => Parser (Expr a)
factor = parens expr <|> atom

-- |An auxiliary class providing common parsers.
class Parseable a where
    -- |Parses an atom.
    atom :: Parser (Expr a)

    -- |Parses an additive binary operator.
    addop :: Parser (Expr a -> Expr a -> Expr a)

    -- |Parses a multiplicative binary operator.
    mulop :: Parser (Expr a -> Expr a -> Expr a)

-- |Integral expressions.
instance Parseable Int where
    addop =   (reservedOp "+" >> pure EAdd)
          <|> (reservedOp "-" >> pure ESub)

    mulop =   (reservedOp "*"  >> pure EMul)
          <|> (reservedOp "//" >> pure EIDiv)
          <|> (reservedOp "%"  >> pure EMod)

    atom  =   literal
          <|> (reserved "ATOM_INDEX"  >> EAtomIx   <$> parens constant)
          <|> (reserved "CHAIN_INDEX" >> EChainIx  <$> parens constant)
          <|> (reserved "CHROMOSOME"  >> EChromoIx <$> parens constant)
          <|> (reserved "INT" >> parens int)
          <|> (reserved "MIN" >> parens (EMin <$> expr <* comma <*> expr))
          <|> (reserved "MAX" >> parens (EMax <$> expr <* comma <*> expr))
            where
              int =   try expr
                  <|> EInt <$> (expr :: Parser (Expr Double))

-- |Floating-point expressions.
instance Parseable Double where
    addop =   (reservedOp "+" >> pure EAdd)
          <|> (reservedOp "-" >> pure ESub)

    mulop =   (reservedOp "*"  >> pure EMul)
          <|> (reservedOp "/"  >> pure EDiv)

    atom  =   literal
          <|> (reserved "GR" >> parens (pure EGr))
          <|> (reserved "DIST" >> parens (EDist <$> literal <* comma <*> literal))
          <|> (reserved "MIN" >> parens (EMin <$> expr <* comma <*> expr))
          <|> (reserved "MAX" >> parens (EMax <$> expr <* comma <*> expr))
          <|> (reserved "FLT" >> parens flt)
            where
              flt =   try expr
                  <|> EFlt <$> (expr :: Parser (Expr Int))

-- |Boolean expressions.
instance Parseable Bool where
    addop = reserved "OR" >> pure EOr

    mulop = reserved "AND" >> pure EAnd

    atom =   (reserved "NOT" >> ENot <$> atom)
         <|> (reserved "BELONGS" >> parens (EBelongs <$> literal <* comma <*> literal))
         <|> polyParse (Proxy :: Proxy Ord) (Proxy :: Proxy '[Int, Double])  (\sub -> do
                lhs <- sub
                op <-  choice
                       [ reservedOp "==" >> pure EEq
                       , reservedOp "!=" >> pure ENeq
                       , reservedOp "<"  >> pure ELt
                       , reservedOp "<=" >> pure ELte
                       , reservedOp ">"  >> pure EGt
                       , reservedOp ">=" >> pure EGte
                       ]
                op lhs <$> sub
             )

-- |A literal.
literal :: ParseConstant a => Parser (Expr a)
literal = ELit <$> constant

-- |A helper class providing 'constant' parsers.
class ParseConstant a where
    -- |Parses an immediate constant.
    constant :: Parser a

-- |Integral constants.
instance ParseConstant Int where
    constant = fromIntegral <$> integer

-- |Floating-point or integral constants.
instance ParseConstant Double where
    constant = try float
             <|> fromIntegral <$> integer

-- |Node (parameter) identifier constants.
instance ParseConstant Node where
    constant = reserved "X" >> Node <$> constant

-- |Atom class constants.
instance ParseConstant AtomClass where
    constant =   (reserved "BEAD"   >> BeadClass   <$> constant)
             <|> (reserved "BINDER" >> BinderClass <$> constant)

-- |Provides a type-polymorphic 'choice'.
--
-- 'xs' is a list of types which will be tried in the specified order.
-- 'constr' is a Constraint which ought to be satisfied by all those types.
class PolyParse constr xs where
    -- |The parsing function
    polyParse ::
           proxy constr
           -- ^A proxy with the constraint
        -> proxy' xs
           -- ^A proxy with the types list
        -> (forall cand. constr cand => Parser (Expr cand) -> Parser a)
           -- ^The parsng function
        -> Parser a
           -- ^The result of the first suceeding parsing function,
           -- called with 'expr' for the respective type.

-- |The base case.
instance PolyParse constr '[] where
    polyParse _ _ _ = fail "PolyParse: no candidate suceeded"

-- |The recursive case.
instance (Parseable x, PolyParse constr xs, constr x)
         => PolyParse constr (x ': xs) where
    polyParse pC _ run = try (run (expr :: Parser (Expr x)))
                       <|> polyParse pC (Proxy :: Proxy xs) run

-- |The language definition.
dslDef :: P.LanguageDef st
dslDef = javaStyle
         { P.reservedOpNames = ["+", "-", "/", "*", "%", "//", "<=", ">=", "==", "!=", "<", ">"]
         , P.reservedNames = ["AND", "OR", "NOT", "DIST", "INT", "GR", "ATOM_INDEX", "CHAIN_INDEX",
                             "CHROMOSOME", "MIN", "MAX", "BELONGS", "X", "BEAD", "BINDER", "CALLBACK",
                             "EVERY", "ACCEPTED", "NODES", "WHERE", "COMPUTE", "SUM", "PRODUCT", "LIST"]
         }

-- |The token parser.
P.TokenParser{..} = P.makeTokenParser dslDef
