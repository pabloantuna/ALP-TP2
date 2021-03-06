module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion (LVar x) = Free (Global x)
conversion (App a b) = conversion a :@: conversion b
conversion (Abs x t) = Lam (ligarEnTerm 0 x (conversion t))

ligarEnTerm :: Int -> String -> Term -> Term
ligarEnTerm prof var (a :@: b) = ligarEnTerm prof var a :@: ligarEnTerm prof var b
ligarEnTerm prof var (Lam t') = Lam (ligarEnTerm (prof + 1) var t')
ligarEnTerm prof var v@(Free (Global ident))
  | ident == var = Bound prof
  | otherwise = v
ligarEnTerm _ _ t = t

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) b = f b
vapp (VNeutral neu) b = VNeutral (NApp neu b)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free na) (gEnv, _) = fromMaybe (VNeutral (NFree na)) $ lookup na gEnv
eval' (t :@: t') env = vapp (eval' t env) (eval' t' env)
eval' (Lam t) (gEnv, lEnv) = VLam (\x -> eval' t (gEnv, x:lEnv))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = quote' 0

quote' :: Int -> Value -> Term
quote' i (VLam f) = Lam (quote' (i + 1) (f (VNeutral (NFree (Quote i)))))
quote' i (VNeutral (NFree g@(Global _))) = Free g
quote' i (VNeutral (NFree (Quote n))) = Bound (i - n - 1)
quote' i (VNeutral (NApp a b)) = quote' i (VNeutral a) :@: quote' i b
