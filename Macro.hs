module Macro where

import Control.Arrow
import Control.Monad
import Control.Monad.Error

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Types

-- Macro Transformers

type Macro = SchemeVal -> SchemeM SchemeVal
data PatVar = PatVar SchemeVal
            | Many [PatVar]

compileMacro :: SchemeVal -> SchemeM Macro
compileMacro form = undefined

-- Environment

type MacroFrame = Map String (Maybe Macro)
type MacroEnv = [MacroFrame]

emptyFrame :: MacroFrame
emptyFrame = Map.empty

makeFrame :: [(String, Maybe Macro)] -> SchemeM MacroFrame
makeFrame bindings = maybe (return $ Map.fromList bindings)
                           (throwError . Error . ("duplicate binding: " ++))
                           (findDuplicate . fst . unzip $ bindings) 
  where findDuplicate (x:xs) = if x `elem` xs then Just x else findDuplicate xs
        findDuplicate [] = Nothing

defVar :: MacroFrame -> String -> SchemeM MacroFrame
defVar frame var
    | var == "#_" = return frame
    | Map.member var frame = throwError $ Error ("duplicate definition: " ++ var)
    | otherwise = return $ Map.insert var Nothing frame

defMacro :: MacroFrame -> String -> Macro -> SchemeM MacroFrame
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

expandTopLevel :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandTopLevel env forms = do
    (_, forms) <- foldM wrapExpr (emptyFrame, []) forms
    expandBody env forms
  where
    wrapExpr (frame, forms) form@(Pair (Symbol keyword) cdr)
        | isBound env' keyword =
            case getMacro env' keyword of
                Just macro -> expandMacro env' macro cdr >>=
                              wrapExpr (frame, forms)
                Nothing -> return (frame, wrapExpr' form : forms)
        | otherwise = 
            case keyword of
                "define" -> do (var, _) <- getSubforms form >>= expandDef . tail
                               flip (,) (form : forms) `liftM` defVar frame var
                "define-syntax" -> do (var, macro) <- getSubforms form >>=
                                                      expandDefSyn . tail
                                      flip (,) forms `liftM` defMacro frame var macro
                _ -> return (frame, wrapExpr' form : forms)
      where env' = extendEnv env frame
    wrapExpr (frame, forms) form = return (frame, wrapExpr' form : forms)
    wrapExpr' form = toSchemeVal [Symbol "define", Symbol "#_", form]

expandBody :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandBody env forms = do
    (frame, bindings, exprs) <- expandDefs emptyFrame [] forms
    let expand = expandExpr $ extendEnv env frame
    exprs <- liftM2 (++) (mapM (second' expand >=> return . uncurry Set) bindings)
                         (mapM expand exprs)
    return $ App (Lambda (fst $ unzip bindings) Nothing exprs)
                 (map (const $ Val Undefined) bindings)
  where
    expandDefs frame bindings (form@(Pair (Symbol keyword) cdr) : forms)
        | isBound env' keyword =
            case getMacro env' keyword of
                Just macro -> do form <- expandMacro env' macro cdr
                                 expandDefs frame bindings (form : forms)
                Nothing -> return (frame, bindings, form : forms)
        | otherwise =
            case keyword of
                "define" -> do (var, form) <- getSubforms form >>= expandDef . tail
                               frame <- defVar frame var
                               expandDefs frame ((var, form) : bindings) forms
                "define-syntax" -> do (var, macro) <- getSubforms form >>=
                                                      expandDefSyn . tail
                                      frame <- defMacro frame var macro
                                      expandDefs frame bindings forms
                _ -> return (frame, bindings, form : forms)
      where env' = extendEnv env frame
    expandDefs frame bindings forms = return (frame, bindings, forms) 
    second' = runKleisli . second . Kleisli

expandDef :: [SchemeVal] -> SchemeM (String, SchemeVal)
expandDef [Pair (Symbol var) cdr, form] = return (var, lambda)
  where lambda = toSchemeVal [Symbol "lambda", cdr, form] 
expandDef [Symbol var, form] = return (var, form)
expandDef subforms = malformedSyntax "define" subforms

expandDefSyn :: [SchemeVal] -> SchemeM (String, Macro)
expandDefSyn [Symbol var, form] = (,) var `liftM` compileMacro form
expandDefSyn subforms = malformedSyntax "define-syntax" subforms

expandExpr :: MacroEnv -> SchemeVal -> SchemeM SchemeForm
expandExpr env form@(Pair sym@(Symbol keyword) cdr)
    | isBound env keyword =
        case getMacro env keyword of
            Just macro -> expandMacro env macro cdr >>= expandExpr env
            Nothing -> getSubforms form >>= expandApp env
    | otherwise =
        case keyword of
            "quote" -> getSubforms form >>= expandQuote env . tail
            "lambda" -> getSubforms form >>= expandLambda env . tail
            "if" -> getSubforms form >>= expandIf env . tail
            "set!" -> getSubforms form >>= expandSet env . tail
            "define" -> throwError $ SyntaxError msg form
            "define-syntax" -> throwError $ SyntaxError msg form
            _ -> getSubforms form >>= expandApp env
  where msg = "definition in expression context"
expandExpr env form@(Pair _ _) = getSubforms form >>= expandApp env
expandExpr env (Symbol var)
    | isBound env var = return $ Var var
    | otherwise = throwError $ Error ("unbound variable: " ++ var)
expandExpr env val = return $ Val val

expandApp :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandApp env (proc : args) = liftM2 App (expandExpr env proc)
                                         (mapM (expandExpr env) args)

expandQuote :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandQuote _ [datum] = return $ Val datum
expandQuote _ subforms = malformedSyntax "quote" subforms

expandLambda :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandLambda env (params : body) = do
    (vars, dotted) <- parseParams params
    frame <- makeFrame $ zip (vars ++ maybeToList dotted) (repeat Nothing)
    let env' = extendEnv env frame
    expandBody env' body >>= return . Lambda vars dotted . return 
expandLambda _ subforms = malformedSyntax "lambda" subforms

expandIf :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandIf env subforms = do
    forms <- mapM (expandExpr env) subforms
    case forms of
        [test, expr, expr'] -> return $ If test expr expr'
        _ -> malformedSyntax "if" subforms

expandSet :: MacroEnv -> [SchemeVal] -> SchemeM SchemeForm
expandSet env [Symbol var, expr] = expandExpr env expr >>= return . Set var
expandSet _ subforms = malformedSyntax "set!" subforms

expandMacro :: MacroEnv -> Macro -> SchemeVal -> SchemeM SchemeVal
expandMacro env macro form = undefined

getSubforms :: SchemeVal -> SchemeM [SchemeVal]
getSubforms form = catchError (fromSchemeVal form)
                              (const . throwError $ SyntaxError msg form)
  where msg = "proper list required for procedure call or buildin syntax"

parseParams :: SchemeVal -> SchemeM ([String], Maybe String)
parseParams (Pair (Symbol var) cdr) = liftM (first (var:)) $ parseParams cdr
parseParams (Symbol var) = return ([], Just var)
parseParams Nil = return ([], Nothing)
parseParams val = throwError $ SyntaxError "indentifier required" val

malformedSyntax :: String -> [SchemeVal] -> SchemeM a
malformedSyntax keyword subforms = throwError $ SyntaxError msg form
  where msg = "malformed " ++ keyword
        form = toSchemeVal $ Symbol keyword : subforms
