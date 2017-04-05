module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map


type Name = String

data Exp = Lit Integer
    | Var Name
    | Plus Exp Exp
    | Abs Name Exp
    | App Exp Exp
    deriving (Show)

data Value = IntVal Integer
    | FunVal Env Name Exp
    deriving (Show)

type Env = Map.Map Name Value


eval0                   ::  Env -> Exp -> Value
eval0 env (Lit i)       =   IntVal i
eval0 env (Var n)       =   fromJust (Map.lookup n env)
eval0 env (Plus e1 e2)  =   let  IntVal i1  = eval0 env e1
                                 IntVal i2  = eval0 env e2
                            in IntVal (i1 + i2)
eval0 env (Abs n e)     =   FunVal env n e
eval0 env (App e1 e2)   =   let  val1  = eval0 env e1
                                 val2  = eval0 env e2
                            in case val1 of
                                 FunVal env' n body -> eval0 (Map.insert n val2 env') body


type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2 ) = do    IntVal i1 <- eval1 env e1
                                IntVal i2 <- eval1 env e2
                                return $ IntVal (i1 + i2 )
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2 ) = do     val1 <- eval1 env e1
                                val2 <- eval1 env e2
                                case val1 of
                                    FunVal env0 n body -> eval1 (Map.insert n val2 env0) body


exampleExp :: Exp
exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)


type Eval2 alpha = ErrorT String Identity alpha

runEval2     :: Eval2 alpha -> Either String alpha
runEval2 ev  = runIdentity (runErrorT ev)

eval2a                   ::  Env -> Exp -> Eval2 Value
eval2a env (Lit i)       =   return $ IntVal i
eval2a env (Var n)       =   maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2a env (Plus e1 e2)  =   do  IntVal i1  <- eval2a env e1
                                 IntVal i2  <- eval2a env e2
                                 return $ IntVal (i1 + i2)
eval2a env (Abs n e)     =   return $ FunVal env n e
eval2a env (App e1 e2)   =   do  val1  <- eval2a env e1
                                 val2  <- eval2a env e2
                                 case val1 of
                                    FunVal env' n body ->
                                       eval2a (Map.insert n val2 env') body


eval2                    ::  Env -> Exp -> Eval2 Value
eval2  env (Lit i)       =   return $ IntVal i
eval2  env (Var n)       =   case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val
eval2  env (Plus e1 e2)  =   do  e1'  <- eval2  env e1
                                 e2'  <- eval2  env e2
                                 case (e1', e2') of
                                   (IntVal i1, IntVal i2) ->
                                       return $ IntVal (i1 + i2)
                                   _ -> throwError "type error in addition"
eval2  env (Abs n e)     =   return $ FunVal env n e
eval2  env (App e1 e2)   =   do  val1  <- eval2  env e1
                                 val2  <- eval2  env e2
                                 case val1 of
                                    FunVal env' n body ->
                                       eval2  (Map.insert n val2 env') body
                                    _ -> throwError "type error in application"



type Eval3 alpha = ReaderT Env (ErrorT String Identity) alpha

runEval3     :: Env -> Eval3 alpha -> Either String alpha
runEval3 env ev  = runIdentity (runErrorT (runReaderT ev env))

eval3               ::  Exp -> Eval3 Value
eval3 (Lit i)       =   return $ IntVal i
eval3 (Var n)       =   do  env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val
eval3 (Plus e1 e2)  =   do  e1'  <- eval3 e1
                            e2'  <- eval3 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
eval3 (Abs n e)     =   do  env <- ask
                            return $ FunVal env n e
eval3 (App e1 e2)   =   do  val1  <- eval3 e1
                            val2  <- eval3 e2
                            case val1 of
                               FunVal env' n body ->
                                  local (const (Map.insert n val2 env'))
                                    (eval3 body)
                               _ -> throwError "type error in application"

-- final version

type Eval6 alpha = ReaderT Env  (ErrorT String
                                (WriterT [String] (StateT Integer IO))) alpha


runEval6           ::  Env -> Integer -> Eval6 alpha -> IO ((Either String alpha, [String]), Integer)
runEval6 env st ev  =
    runStateT (runWriterT (runErrorT (runReaderT ev env))) st


tick :: (Num s, MonadState s m) => m ()
-- tick = do  st <- get
--            put (st + 1)
tick = modify (+1)


eval6               ::  Exp -> Eval6 Value
eval6 (Lit i)       =   do  tick
                            liftIO $ print i
                            return $ IntVal i
eval6 (Var n)       =   do  tick
                            tell [n]
                            env <- ask
                            case Map.lookup n env of
--                             case asks (Map.lookup n) of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val
eval6 (Plus e1 e2)  =   do  tick
                            e1'  <- eval6 e1
                            e2'  <- eval6 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
eval6 (Abs n e)     =   do  tick
                            env <- ask
                            return $ FunVal env n e
eval6 (App e1 e2)   =   do  tick
                            val1  <- eval6 e1
                            val2  <- eval6 e2
                            case val1 of
                               FunVal env' n body ->
                                  local (const (Map.insert n val2 env'))
                                    (eval6 body)
                               _ -> throwError "type error in application"



-- TODO replace ErrorT with ExceptT




