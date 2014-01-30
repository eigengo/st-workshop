{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Workshop.Template(pr) where

  import Language.Haskell.TH
  import Control.Monad.Error
  import Control.Monad.Identity

  data Format = Float | Decimal | String | Any | Literal String deriving (Show)

  parse :: String -> ErrorT String Identity [Format]
  parse [] = return []
  parse ('%':s:restExpr) 
    | s == 's'  = do { rest <- parse restExpr; return $ String : rest }
    | s == 'd'  = do { rest <- parse restExpr; return $ Decimal : rest }
    | s == 'f'  = do { rest <- parse restExpr; return $ Float : rest }
    | s == 'a'  = do { rest <- parse restExpr; return $ Any : rest }
    | otherwise = throwError $ "Bad format specifier " ++ [s]
  parse s = do
    let (literal, restExpr) = break ('%' ==) s
    rest <- parse restExpr
    return $ Literal literal : rest

  gen :: [Format] -> Q Exp
  gen (Decimal:rest)   = [| \n -> show n |]
  gen (String:rest)    = [| \s -> s |]
  gen (Any:rest)       = [| \a -> show a |]
  gen (Literal s:rest) = stringE s

  pr :: String -> [a] -> Q Exp
  pr expr params = 
    case (runIdentity $ runErrorT $ parse expr, params) of
      (Right format, params) -> gen format
      (Left  message, _)     -> fail message
