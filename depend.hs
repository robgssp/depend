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

type Parser = Parsec String

spaceConsumer :: Parser ()
spaceConsumer = (L.space (void spaceChar) 
                         (L.skipLineComment "--")
                         (L.skipBlockComment "{-" "-}"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

var :: Parser String
var = lexeme ((:) <$> letterChar <*> many alphaNumChar)

expr = lamExpr

lamExpr :: Parser (Exp String)
lamExpr = try (do symbol "\\"
                  arg <- var
                  symbol ":"
                  argt <- expr
                  symbol "."
                  body <- expr
                  return (ELam arg argt body))
          <|> letExpr

letExpr = try (do symbol "let"
                  bind <- var
                  symbol "="
                  arg <- expr
                  symbol "in"
                  body <- expr
                  return (ELet bind arg body))
          <|> allExpr

allExpr = try (do symbol "forall"
                  arg <- var
                  symbol ":"
                  typ <- expr
                  symbol "."
                  body <- expr
                  return (TAll arg typ body))
          <|> addExpr

addExpr = try (do l <- appExpr
                  symbol "+"
                  r <- addExpr
                  return (EAdd l r))
          <|> appExpr

appExpr = foldl1 EApp <$> some primExpr

primExpr = try ((EInt . either id (error "Bad number")) <$> lexeme L.number)
       <|> try (TInt <$ symbol "int")
       <|> try (EVar <$> var)
       <|> between (symbol "(") (symbol ")") expr

main = do prog <- getContents
          Right res <- pure $ parse (expr <* eof) "<stdin>" prog
          print res
          return ()
