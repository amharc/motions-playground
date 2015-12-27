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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Callback.Parser.Parser
    ( CallbackFrequency(..)
    , AtomClass(..)
    , CallbackResult(..)
    , ParsedCallback(..)
    , ParsedCallbackWrapper(..)
    , MaxNConstraint
    , Both
    , Expr(..)
    , Nat(..)
    , Node(..)
    , EC(..)
    , ToNodeEx(..)
    , parseCallback) where

import Bio.Motions.Callback.Class
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)
import GHC.Prim

-- |Represents the frequency a callback has to be run
data CallbackFrequency = EveryNFrames Int | EveryNAcceptedFrames Int

-- |Represents the class of an atom
data AtomClass = BeadClass Int | BinderClass Int

-- |The return value of a callback
data CallbackResult c n a where
    CallbackSum     :: Expr c n a -> CallbackResult c n a
    CallbackProduct :: Expr c n a -> CallbackResult c n a
    CallbackList    :: Expr c n a -> CallbackResult c n a

-- |Represents a parsed callback
data ParsedCallback c n a = ParsedCallback
    { callbackFrequency :: CallbackFrequency
    , callbackName :: String
    , callbackCondition :: Expr c n Bool
    , callbackResult :: CallbackResult c n a
    }

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
    deriving (Eq, Show)

-- |A wrapper around node argument identifiers.
data Node (n :: Nat) where
    FirstNode :: Node (Succ n)
    NextNode :: Node n -> Node (Succ n)

class ToNodeEx (n :: Nat) where
    toNodeEx :: Int -> Node n

instance ToNodeEx Zero where
    toNodeEx _ = error "Out of bounds"

instance ToNodeEx n => ToNodeEx (Succ n) where
    toNodeEx 0 = FirstNode
    toNodeEx n = NextNode $ toNodeEx $ n - 1

type family EC (c :: * -> Constraint) (n :: Nat) (a :: [*]) :: Constraint
type instance EC c n '[] = (ToNodeEx n)
type instance EC c n (t ': ts) = (c t, EC c n ts)

-- |The AST of the callback DSL.
-- 
-- 'c' is a Constraint on the types of all subexpressions.
-- 'n' is the arity.
data Expr c n a where
    EAnd  :: (EC c n '[Bool]) => Expr c n Bool -> Expr c n Bool -> Expr c n Bool
    EOr   :: (EC c n '[Bool]) => Expr c n Bool -> Expr c n Bool -> Expr c n Bool
    ENot  :: (EC c n '[Bool]) => Expr c n Bool -> Expr c n Bool

    ELt   :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    ELte  :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    EGt   :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    EGte  :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    EEq   :: (EC c n '[a, Bool], Eq a)  => Expr c n a -> Expr c n a -> Expr c n Bool
    ENeq  :: (EC c n '[a, Bool], Eq a)  => Expr c n a -> Expr c n a -> Expr c n Bool

    EDist :: (EC c n '[Double]) => Node n -> Node n -> Expr c n Double
    EInt  :: (EC c n '[a, Int], RealFrac a) => Expr c n a -> Expr c n Int
    EFlt  :: (EC c n '[Int, Double]) => Expr c n Int -> Expr c n Double
    EGr   :: (EC c n '[Double]) => Expr c n Double

    EAdd  :: (EC c n '[a], Num a) => Expr c n a -> Expr c n a -> Expr c n a
    ESub  :: (EC c n '[a], Num a) => Expr c n a -> Expr c n a -> Expr c n a
    EMul  :: (EC c n '[a], Num a) => Expr c n a -> Expr c n a -> Expr c n a
    EDiv  :: (EC c n '[a], Fractional a) => Expr c n a -> Expr c n a -> Expr c n a
    EIDiv :: (EC c n '[a], Integral a) => Expr c n a -> Expr c n a -> Expr c n a
    EMod  :: (EC c n '[a], Integral a) => Expr c n a -> Expr c n a -> Expr c n a

    EMin  :: (EC c n '[a], Ord a) => Expr c n a -> Expr c n a -> Expr c n a
    EMax  :: (EC c n '[a], Ord a) => Expr c n a -> Expr c n a -> Expr c n a

    ELit  :: (EC c n '[a]) => a -> Expr c n a

    EAtomIx   :: (EC c n '[Int]) => Node n -> Expr c n Int
    EChainIx  :: (EC c n '[Int]) => Node n -> Expr c n Int
    EChromoIx :: (EC c n '[Int]) => Node n -> Expr c n Int

    EBelongs  :: (EC c n '[Bool]) => Node n -> AtomClass -> Expr c n Bool

-- |An alias, for simplicity.
type Parser a = Parsec String () a

-- |An existential type wrapper, hiding the arity and the result type.
data ParsedCallbackWrapper c cn where
    ParsedCallbackWrapper :: (EC c n '[Int, Double, Bool, a], cn n) => ParsedCallback c n a -> ParsedCallbackWrapper c cn

data CallbackResultWrapper c n where
    CallbackResultWrapper :: EC c n '[Int, Double, Bool, a] => CallbackResult c n a -> CallbackResultWrapper c n

parseCallbackWithArity :: (EC c n '[Int, Double, Bool, a], Parseable c n a) => Parser (CallbackResult c n a)
parseCallbackWithArity = (reserved "SUM" >> CallbackSum <$> expr)
                       <|> (reserved "PRODUCT" >> CallbackProduct <$> expr)
                       <|> (reserved "LIST" >> CallbackList <$> expr)

class TryNatsBelow c (n :: Nat) where
    tryNatsBelow :: Proxy# n -> Proxy# c -> Int -> (forall (m :: Nat). c m => Proxy# m -> x) -> x

instance TryNatsBelow c Zero where
    tryNatsBelow _ _ _ _ = error "Out of bounds"

instance (c (Succ n), TryNatsBelow c n) => TryNatsBelow c (Succ n) where
    tryNatsBelow p pC 0 run = run p
    tryNatsBelow _ pC n run = tryNatsBelow (proxy# :: Proxy# n) pC (n - 1) run

class EC c n a => ECc c a n
instance EC c n a => ECc c a n

type MaxNConstraint c cn maxn = (EC c maxn '[Int, Double, Bool], TryNatsBelow (Both (ECc c '[Int, Double, Bool]) cn) maxn)

class (c1 a, c2 a) => Both c1 c2 a
instance (c1 a, c2 a) => Both c1 c2 a

-- |Parses a callback with arity strictly less than 'maxn', whose internal types
-- must satisfy the constraint 'c'.
parseCallback :: forall c cn maxn. MaxNConstraint c cn maxn
    => Proxy# maxn -> Parser (ParsedCallbackWrapper c cn)
parseCallback pMaxN = do
    reserved "CALLBACK"
    name <- stringLiteral

    reserved "EVERY"
    freq <-     (reserved "ACCEPTED" >> EveryNAcceptedFrames . fromIntegral <$> natural)
            <|> EveryNFrames . fromIntegral <$> natural

    reserved "NODES"
    arity <- fromIntegral <$> natural
    tryNatsBelow pMaxN (proxy# :: Proxy# (Both (ECc c '[Int, Double, Bool]) cn)) arity (rem name freq)
  where
    rem :: forall n. (EC c n '[Int, Double, Bool], cn n) => String -> CallbackFrequency -> Proxy# n -> Parser (ParsedCallbackWrapper c cn)
    rem name freq _ = do
        reserved "WHERE"
        cond <- expr :: Parser (Expr c n Bool)

        reserved "COMPUTE"
        result <- try (CallbackResultWrapper <$> (parseCallbackWithArity :: Parser (CallbackResult c n Int)))
                <|> (CallbackResultWrapper <$> (parseCallbackWithArity :: Parser (CallbackResult c n Double)))

        pure $ case result of
            CallbackResultWrapper result -> ParsedCallbackWrapper ParsedCallback
                { callbackFrequency = freq
                , callbackName = name
                , callbackCondition = cond
                , callbackResult = result
                }

-- |Parses an expression, where @expr ::= 'term' | 'term' 'addop' expr@
expr :: Parseable c n a => Parser (Expr c n a)
expr = term `chainl1` addop

-- |Parses a term, where @term ::= 'factor' | 'factor' 'mulop' term@
term :: Parseable c n a => Parser (Expr c n a)
term = factor `chainl1` mulop

-- |Parses a factor, where @factor ::= (expr) | 'atom'@.
factor :: Parseable c n a => Parser (Expr c n a)
factor = parens expr <|> atom

-- |An auxiliary class providing common parsers.
class EC c n '[a] => Parseable c n a where
    -- |Parses an atom.
    atom :: Parser (Expr c n a)

    -- |Parses an additive binary operator.
    addop :: Parser (Expr c n a -> Expr c n a -> Expr c n a)

    -- |Parses a multiplicative binary operator.
    mulop :: Parser (Expr c n a -> Expr c n a -> Expr c n a)

-- |Integral expressions.
instance EC c n '[Int, Double] => Parseable c n Int where
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
                  <|> EInt <$> (expr :: Parser (Expr c n Double))

-- |Floating-point expressions.
instance EC c n '[Int, Double] => Parseable c n Double where
    addop =   (reservedOp "+" >> pure EAdd)
          <|> (reservedOp "-" >> pure ESub)

    mulop =   (reservedOp "*"  >> pure EMul)
          <|> (reservedOp "/"  >> pure EDiv)

    atom  =   literal
          <|> (reserved "GR" >> parens (pure EGr))
          <|> (reserved "DIST" >> parens (EDist <$> constant <* comma <*> constant))
          <|> (reserved "MIN" >> parens (EMin <$> expr <* comma <*> expr))
          <|> (reserved "MAX" >> parens (EMax <$> expr <* comma <*> expr))
          <|> (reserved "FLT" >> parens flt)
            where
              flt =   try expr
                  <|> EFlt <$> (expr :: Parser (Expr c n Int))

-- |Boolean expressions.
instance EC c n '[Bool, Int, Double] => Parseable c n Bool where
    addop = reserved "OR" >> pure EOr

    mulop = reserved "AND" >> pure EAnd

    atom =   (reserved "NOT" >> ENot <$> atom)
         <|> (reserved "BELONGS" >> parens (EBelongs <$> constant <* comma <*> constant))
         <|> polyParse (proxy# :: Proxy# c) (proxy# :: Proxy# Ord)
                       (proxy# :: Proxy# n) (proxy# :: Proxy# '[Int, Double])  (\sub -> do
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
literal :: (EC c n '[a], ParseConstant a) => Parser (Expr c n a)
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
instance ToNodeEx n => ParseConstant (Node n) where
    constant = reserved "X" >> toNodeEx <$> constant

-- |Atom class constants.
instance ParseConstant AtomClass where
    constant =   (reserved "BEAD"   >> BeadClass   <$> constant)
             <|> (reserved "BINDER" >> BinderClass <$> constant)

-- |Provides a type-polymorphic 'choice'.
--
-- 'xs' is a list of types which will be tried in the specified order.
-- 'c', 'c'' are additional Constraint which ought to be satisfied by all those types.
-- 'n' is the arity, as usual.
class PolyParse c c' n xs where
    -- |The parsing function
    polyParse ::
           Proxy# c
           -- ^A proxy with the constraint
        -> Proxy# c'
           -- ^A proxy with the constraint
        -> Proxy# n
           -- ^A proxy with the arity
        -> Proxy# xs
           -- ^A proxy with the types list
        -> (forall x. (c' x, c x) => Parser (Expr c n x) -> Parser a)
           -- ^The parsng function
        -> Parser a
           -- ^The result of the first suceeding parsing function,
           -- called with 'expr' for the respective type.

-- |The base case.
instance PolyParse c c' n '[] where
    polyParse _ _ _ _ _ = fail "PolyParse: no candidate suceeded"

-- |The recursive case.
instance (Parseable c n x, PolyParse c c' n xs, c' x) => PolyParse c c' n (x ': xs) where
    polyParse pC pC' pN _ run = try (run (expr :: Parser (Expr c n x)))
                       <|> polyParse pC pC' pN (proxy# :: Proxy# xs) run

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
