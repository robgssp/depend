{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Prim
import Control.Monad
import qualified Text.Megaparsec.Lexer as L

data Exp var = 
    EInt Integer | EAdd (Exp var) (Exp var) | ELam var (Exp var) (Exp var) 
  | ELet var (Exp var) (Exp var) | EApp (Exp var) (Exp var) | EVar var
  | TInt | TAll var (Exp var) (Exp var) | TType deriving (Read, Show)

instance Eq a => Eq (Exp a) where
  EInt i == EInt j = i == j

type Ctx a = a -> Maybe (Exp a)

type ParseExp = Exp String

empty :: Ctx a
empty c = Nothing
update :: Eq a => a -> Exp a -> Ctx a -> Ctx a
update x v c = \y -> if x == y
                        then Just v
                        else c y

eval :: Ctx String -> ParseExp -> ParseExp
eval c (EInt i) = EInt i
eval c (EAdd l r) = case (eval c l, eval c r) of
                      (EInt x, EInt y) -> EInt (x + y)
                      (l', r') -> EAdd l' r'
eval c (ELam x ty b) = ELam x ty (eval (update x (EVar x) c) b)
eval c (ELet x v b) = let v' = eval c v
                      in eval (update x v' c) b
eval c (EApp f a) = let (ELam x ty b) = eval c f
                        a' = eval c a
                    in eval (update x a' c) b
eval c (EVar v) = case c v of
                    Nothing -> EVar v
                    Just x -> x
eval c TInt = TInt
eval c (TAll v l r) = TAll v (eval c l) (eval c r)
eval c TType = TType

typeof :: Ctx String -> ParseExp -> ParseExp
typeof c (EInt _) = TInt
typeof c (EAdd l r) = let !TInt = typeof c l
                          !TInt = typeof c r
                      in TInt
typeof c (ELam x tx b) = TAll x tx (typeof (update x tx c) b)
typeof c (ELet x v b) = typeof (update x (typeof c v) c) b
typeof c (EApp f a) = let TAll v ta tr = typeof c f
                          ta' = typeof c a
                      in if eval (update v a c) ta == eval c ta'
                            then tr
                            else error "Argument type mismatch"
typeof c (EVar id) = fromJust (c id)
typeof c TInt = TType 
typeof c TType = TType

lexeme :: MonadParsec s m Char => m a -> m a
lexeme = L.lexeme (L.space (void spaceChar) 
                           (L.skipLineComment "--")
                           (L.skipBlockComment "{-" "-}"))
 
name :: MonadParsec s m Char => m String
name = lexeme (some letterChar)

var :: MonadParsec s m Char => m (Exp String)
var = EVar <$> name

lambda :: MonadParsec s m Char => m (Exp String)
lambda = do lexeme (string "\\")
            var <- name
            lexeme (string ":")
            typ <- expr
            lexeme (string ".")
            body <- expr
            return (ELam var typ body)

plus :: MonadParsec s m Char => m (Exp String)
plus = do l <- expr
          lexeme (string "+")
          r <- expr
          return (EAdd l r)

expr :: MonadParsec s m Char => m (Exp String)
expr = try var <|>
       lambda

main = do prog <- getContents
          Right res <- pure $ parse (expr <* eof) "<stdin>" prog
          print res
