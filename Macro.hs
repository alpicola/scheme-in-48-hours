module Macro where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Error

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Types

-- Macro Transformers

type Macro = SchemeVal -> Either SchemeError SchemeVal
data PatVar = PatVar SchemeVal
            | Many [PatVar]

compileMacro :: SchemeVal -> Either SchemeError Macro
compileMacro form = undefined

-- Environment

type MacroFrame = Map String (Maybe Macro)
type MacroEnv = [MacroFrame]

emptyFrame :: MacroFrame
emptyFrame = Map.empty

makeFrame :: [(String, Maybe Macro)] -> Either SchemeError MacroFrame
makeFrame bindings = maybe (return $ Map.fromList bindings)
                           (throwError . Error . ("duplicate binding: " ++))
                           (findDuplicate . fst . unzip $ bindings) 
  where findDuplicate (x:xs) = if x `elem` xs then Just x else findDuplicate xs
        findDuplicate [] = Nothing

defVar :: MacroFrame -> String -> Either SchemeError MacroFrame
defVar frame var
    | var == "#_" = return frame
    | Map.member var frame = throwError $ Error ("duplicate definition: " ++ var)
    | otherwise = return $ Map.insert var Nothing frame

defMacro :: MacroFrame -> String -> Macro -> Either SchemeError MacroFrame
defMacro frame var macro
    | Map.member var frame = throwError $ Error ("duplicate definition: " ++ var)
    | otherwise = return $ Map.insert var (Just macro) frame

extendEnv :: MacroEnv -> MacroFrame -> MacroEnv
extendEnv = flip (:)

isBound :: MacroEnv -> String -> Bool
isBound env var = any (Map.member var) env

getMacro :: MacroEnv -> String -> Maybe Macro
getMacro env var = join . msum $ map (Map.lookup var) env

-- Expansion

expandTopLevel :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandTopLevel env forms = do
    (_, forms) <- foldM expandForm (emptyFrame, []) forms
    expandBody env $ reverse forms
  where
    expandForm acc@(frame, forms) form@(Pair (Symbol keyword) _)
        | isBound env' keyword =
            case getMacro env' keyword of
                Just macro -> expandMacro env' macro form >>= expandForm acc
                Nothing -> return (frame, wrapExpr form : forms)
        | keyword == "define" = do
            (var, _) <- getSubforms form >>= expandDef . tail
            frame <- defVar frame var
            return (frame, form : forms)
        | keyword == "define-syntax" = do
            (var, macro) <- getSubforms form >>= expandDefSyn . tail
            frame <- defMacro frame var macro
            return (frame, forms)
        | keyword == "begin" = getSubforms form >>= foldM expandForm acc . tail
        | otherwise = return (frame, wrapExpr form : forms)
      where env' = extendEnv env frame
    expandForm (frame, forms) form = return (frame, wrapExpr form : forms)
    wrapExpr form = toSchemeVal [Symbol "define", Symbol "#_", form']
      where form' = toSchemeVal [Symbol "begin", form, Unspecified]

expandBody :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandBody env forms = do
    (frame, bindings, exprs, _) <- foldM expandForm (emptyFrame, [], [], True) forms
    let env' = extendEnv env frame
    if null bindings
        then seqExprs <$> mapM (expandExpr env') exprs
        else do
            bindings <- mapM (second' $ expandExpr env') $ reverse bindings
            exprs <- mapM (expandExpr env') $ reverse exprs
            let body = seqExprs $ map (uncurry Set) bindings ++ exprs
            return $ App (Lambda (fst $ unzip bindings) Nothing body)
                         (map (const $ Val Undefined) bindings)
  where
    expandForm acc@(frame, bindings, forms, isDef) form@(Pair (Symbol keyword) _)
        | isBound env' keyword =
            case getMacro env' keyword of
                Just macro -> expandMacro env' macro form >>= expandForm acc
                Nothing -> return (frame, bindings, form : forms, False)
        | isDef && keyword == "define" = do
            (var, form) <- getSubforms form >>= expandDef . tail
            frame <- defVar frame var
            return (frame, (var, form) : bindings, forms, True)
        | isDef && keyword == "define-syntax" = do
            (var, macro) <- getSubforms form >>= expandDefSyn . tail
            frame <- defMacro frame var macro
            return (frame, bindings, forms, True)
        | keyword == "begin" = getSubforms form >>= foldM expandForm acc . tail
        | otherwise = return (frame, bindings, form : forms, False)
      where env' = extendEnv env frame
    expandForm (frame, bindings, forms, _) form =
        return (frame, bindings, form : forms, False) 
    second' = runKleisli . second . Kleisli

expandDef :: [SchemeVal] -> Either SchemeError (String, SchemeVal)
expandDef (Pair (Symbol var) cdr : forms) = return (var, lambda)
  where lambda = toSchemeVal (Symbol "lambda" : cdr : forms) 
expandDef [Symbol var, form] = return (var, form)
expandDef [Symbol var] = return (var, Unspecified)
expandDef subforms = malformedSyntax "define" subforms

expandDefSyn :: [SchemeVal] -> Either SchemeError (String, Macro)
expandDefSyn [Symbol var, form] = (,) var <$> compileMacro form
expandDefSyn subforms = malformedSyntax "define-syntax" subforms

expandExpr :: MacroEnv -> SchemeVal -> Either SchemeError SchemeExpr
expandExpr env form@(Pair (Symbol keyword) _)
    | isBound env keyword =
        case getMacro env keyword of
            Just macro -> expandMacro env macro form >>= expandExpr env
            Nothing -> getSubforms form >>= expandApp env
    | otherwise =
        case keyword of
            "quote" -> getSubforms form >>= expandQuote env . tail
            "lambda" -> getSubforms form >>= expandLambda env . tail
            "if" -> getSubforms form >>= expandIf env . tail
            "set!" -> getSubforms form >>= expandSet env . tail
            "begin" -> getSubforms form >>= expandBegin env . tail
            "define" -> throwError $ SyntaxError msg form
            "define-syntax" -> throwError $ SyntaxError msg form
            _ -> getSubforms form >>= expandApp env
  where msg = "definition in expression context"
expandExpr env form@(Pair _ _) = getSubforms form >>= expandApp env
expandExpr env (Symbol var)
    | isBound env var = return $ Var var
    | otherwise = throwError $ Error ("unbound variable: " ++ var)
expandExpr env val = return $ Val val

expandApp :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandApp env (proc : args) = App <$> expandExpr env proc <*>
                                      mapM (expandExpr env) args

expandQuote :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandQuote _ [datum] = return $ Val datum
expandQuote _ subforms = malformedSyntax "quote" subforms

expandLambda :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandLambda env (params : body) = do
    (vars, dotted) <- parseParams params
    frame <- makeFrame $ zip (vars ++ maybeToList dotted) (repeat Nothing)
    let env' = extendEnv env frame
    Lambda vars dotted <$> expandBody env' body
expandLambda _ subforms = malformedSyntax "lambda" subforms

expandIf :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandIf env subforms = do
    forms <- mapM (expandExpr env) subforms
    case forms of
        [test, expr, expr'] -> return $ If test expr expr'
        [test, expr] -> return $ If test expr (Val Unspecified)
        _ -> malformedSyntax "if" subforms

expandSet :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandSet env [Symbol var, expr] = Set var <$> expandExpr env expr
expandSet _ subforms = malformedSyntax "set!" subforms

expandBegin :: MacroEnv -> [SchemeVal] -> Either SchemeError SchemeExpr
expandBegin env subforms = seqExprs <$> mapM (expandExpr env) subforms

expandMacro :: MacroEnv -> Macro -> SchemeVal -> Either SchemeError SchemeVal
expandMacro env macro form = undefined

getSubforms :: SchemeVal -> Either SchemeError [SchemeVal]
getSubforms form = catchError (fromSchemeVal form)
                              (const . throwError $ SyntaxError msg form)
  where msg = "proper list required for procedure call or buildin syntax"

parseParams :: SchemeVal -> Either SchemeError ([String], Maybe String)
parseParams (Pair (Symbol var) cdr) = first (var:) <$> parseParams cdr
parseParams (Symbol var) = return ([], Just var)
parseParams Nil = return ([], Nothing)
parseParams val = throwError $ SyntaxError "indentifier required" val

malformedSyntax :: String -> [SchemeVal] -> Either SchemeError a
malformedSyntax keyword subforms = throwError $ SyntaxError msg form
  where msg = "malformed " ++ keyword
        form = toSchemeVal $ Symbol keyword : subforms

seqExprs :: [SchemeExpr] -> SchemeExpr
seqExprs [expr] = expr
seqExprs exprs = Begin exprs
