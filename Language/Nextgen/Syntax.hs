{-# LANGUAGE GADTs, EmptyDataDecls, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Nextgen.Syntax where

import Data.Data (Data)
import Data.Typeable (Typeable, Typeable3)
import Data.Generics (Generic)
import Data.Generics.Uniplate.Direct

data HasHoles
data Complete


deriving instance Typeable HasHoles
deriving instance Data HasHoles
--deriving instance Generic HasHoles

deriving instance Typeable Complete
deriving instance Data Complete
--deriving instance Generic Complete

type family Holes a b :: *

canHaveHolesT :: a -> b -> Holes a b
canHaveHolesT _ _ = undefined

type instance Holes HasHoles Complete = HasHoles
type instance Holes Complete HasHoles = HasHoles
type instance Holes HasHoles HasHoles = HasHoles
type instance Holes Complete Complete = HasHoles

data EInt
data EBool

deriving instance Typeable EInt
deriving instance Data EInt

deriving instance Typeable EBool
deriving instance Data EBool

data Expression k t a where 
  EQuote  :: a -> String -> Expression HasHoles EInt a
  IntLit  :: a -> Int -> Expression Complete EInt a
  BoolLit :: a -> Bool -> Expression EBool Complete a
  EArith  :: a -> ArithOp -> Expression k1 EInt a -> Expression k2 EInt a ->
                             Expression (Holes k1 k2) EInt a
  -- EComp   :: a -> CompOp -> Expression a EInt k1 -> Expression a EInt k2 ->
  --                           Expression a EBool (Holes k1 k2)
  -- EBool  :: a -> BoolOp -> Expression a EBool k1 -> Expression a EBool k2 ->
  --                          Expression a EBool (Holes k1 k2)
  -- VarRef :: a -> String -> Expression a EInt Complete
  
instance Uniplate (Expression k t a) where
  uniplate (EQuote a q) = plate EQuote |- a |- q
  uniplate (IntLit a i) = plate IntLit |- a |- i
  uniplate (BoolLit a b) = plate BoolLit |- a |- b
  uniplate (EArith a op e1 e2) = plate EArith |- a |- op |* e1 |* e2

--deriving instance Typeable3 Expression
--deriving instance Data (Expression a t k)
--deriving instance Generic (Expression EInt Complete)

                           
-- data Statement a k where
--   SAssign :: a -> Var a k1 -> Expression a t k2 -> Statement a (Holes k1 k2)
--   SIf :: a -> Expression a EBool k1 -> Statement a k2 -> Statement a k3 ->
--          Statement a (Holes (Holes k1 k2) k3)
--   SWhile :: a -> Expression a EBool k1 -> Statement a k2 -> 
--             Statement a (Holes k1 k2)

data Var a k where
  Var :: a -> String -> Var a Complete
  VarQuote :: a -> String -> Var a HasHoles

data ArithOp = OpAdd
             | OpSub
             | OpMul
             | OpDiv
               
data CompOp = OpEq
            | OpNEq
            | OpLEq
            | OpL
            | OpGEq
            | OpG
              
data BoolOp = OpAnd
            | OpOr
            | OpImply