{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.IntLambda where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map

infixr 5 :->
data Type = TInt | Type :-> Type
  deriving Eq

type Name = String
data Exp = EInt Int | EVar Name
         | ELam Name Type Exp | EApp Exp Exp

-- Przykładowe lambda-termy
type Exp1 = Type -> Exp
type Exp2 = Type -> Exp1
type Exp3 = Type -> Exp2

int :: Type
int = TInt

mkI :: Exp1
mkI a = ELam "x" a $ EVar "x"

mkK :: Exp2
mkK a b = ELam "x" a $ ELam "y" b $ EVar "x"

intK = mkK int int

mkS :: Exp3
mkS a b c = ELam "x" a $ ELam "y" b $ ELam "z" c
          $ EApp
             (EApp (EVar "x") (EVar "z"))
             (EApp (EVar "y") (EVar "z"))

intS = mkS (int:->int:->int) (int:->int) int

-- kombinator omega nie typuje sie w prostym rachunku lambda
mkOmega :: Exp1
mkOmega t = ELam "x" t $ EApp (EVar "x") (EVar "x")

intOmega = mkOmega TInt

-- typeCheck

type Env = Map.Map Name Type

newtype TypeChecker a = TypeChecker {
  runTC :: ExceptT String (StateT Env Identity) a
} deriving (Functor, Applicative, Monad, MonadState Env, MonadError String)

evalTypeChecker :: TypeChecker a -> Env -> Either String a
evalTypeChecker m = evalState $ runExceptT $ runTC m

-- ok got transf to work, now on to simple case

typeCheck :: Exp -> IO ()
typeCheck exp = case evalTypeChecker (typeCheck' exp) Map.empty of {
    Right typ -> print typ;
    Left err -> putStrLn err
}

typeCheck' :: Exp -> TypeChecker Type
typeCheck' (EInt _) = return TInt
typeCheck' (EVar n) = do
    maybeValue <- gets $ Map.lookup n
    maybe (throwError "var not defined") return maybeValue
typeCheck' (ELam name typ exp) = do
    modify $ Map.insert name typ
    bodyType <- typeCheck' exp
    return $ typ :-> bodyType
typeCheck' (EApp exp1 exp2) = do
    exp1Type <- typeCheck' exp1
    exp2Type <- typeCheck' exp2
    case exp1Type of {
        exp2Type :-> rest -> return rest;
        _ -> throwError "can't apply type"
    }

-- TODO why error in last example??


-- Show

instance Show Type where
  showsPrec d TInt = showString "int"
  showsPrec d (u :-> v) = showParen (d > arr_prec) $
             showsPrec (arr_prec+1) u .
             showString " -> "       .
             showsPrec arr_prec v
          where arr_prec = 5

instance Show Exp where
  showsPrec d (EVar n) = showString n
  showsPrec d (EInt i) = showsPrec 10 i
  showsPrec d (EApp e1 e2) = showParen (d > ap_prec) $
             showsPrec (ap_prec) e1   .
             showString " "           .
             showsPrec (ap_prec+1) e2
          where ap_prec = 10

  showsPrec d (ELam n t e) = showParen (d > lam_prec) $
             showString ("\\("++n++":"++show t++").") .
             showsPrec (lam_prec) e
          where lam_prec = 1
