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
                           (throwError . Error . ("multiple definitions: " ++))
                           (findDublicate . fst . unzip $ bindings) 
  where
    findDublicate (x:xs) = if x `elem` xs then Just x else findDublicate xs
    findDublicate [] = Nothing

defVar :: MacroFrame -> String -> SchemeM MacroFrame
defVar frame var
    | var == "#_" = return frame
    | Map.member var frame = throwError $ Error ("multiple definitions: " ++ var)
    | otherwise = return $ Map.insert var Nothing frame

defMacro :: MacroFrame -> String -> Macro -> SchemeM MacroFrame
defMacro frame var macro
    | Map.member var frame = throwError $ Error ("multiple definitions: " ++ var)
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
                Nothing -> return (frame, wrap form : forms)
        | otherwise = 
            case keyword of
                "define" -> getSubforms form >>=
                            expandDef frame . tail >>=
                            return . flip (,) (form : forms)
                "define-syntax" -> getSubforms form >>=
                                   expandDefSyn frame . tail >>=
                                   return . flip (,) forms
                _ -> return (frame, wrap form : forms)
      where
        env' = extendEnv env frame
        wrap form = toSchemeVal [Symbol "define", Symbol "#_", form]
    expandDef frame [Symbol var, form] = defVar frame var
    expandDef _ subforms = malformedSyntax "define" subforms
    expandDefSyn frame [Symbol var, form] = compileMacro form >>=
                                            defMacro frame var
    expandDefSyn _ subforms = malformedSyntax "define-syntax" subforms

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
                "define" -> do (frame, binding) <- getSubforms form >>=
                                                   expandDef frame . tail
                               expandDefs frame (binding : bindings) forms
                "define-syntax" -> do frame <- getSubforms form >>=
                                               expandDefSyn frame . tail
                                      expandDefs frame bindings forms
                _ -> return (frame, bindings, form : forms)
      where
        env' = extendEnv env frame
    expandDefs frame bindings forms = return (frame, bindings, forms) 
    expandDef frame [Symbol var, form] = defVar frame var >>=
                                         return . flip (,) (var, form) 
    expandDef _ subforms = malformedSyntax "define" subforms
    expandDefSyn frame [Symbol var, form] = compileMacro form >>=
                                            defMacro frame var
    expandDefSyn _ subforms = malformedSyntax "define-syntax" subforms
    second' = runKleisli . second . Kleisli

expandExpr :: MacroEnv -> SchemeVal -> SchemeM SchemeForm
expandExpr env form@(Pair sym@(Symbol keyword) cdr)
    | isBound env keyword =
        case getMacro env keyword of
            Just macro -> expandMacro env macro cdr >>= expandExpr env
            Nothing -> expandApp env form
    | otherwise =
        case keyword of
            "quote" -> getSubforms form >>= expandQuote . tail
            "lambda" -> getSubforms form >>= expandLambda . tail
            "if" -> getSubforms form >>= expandIf . tail
            "set!" -> getSubforms form >>= expandSet . tail
            "define" -> throwError $ SyntaxError msg form
            "define-syntax" -> throwError $ SyntaxError msg form
            _ -> expandApp env form
  where
    msg = "definition in expression context"
    expandQuote [datum] = return $ Val datum
    expandQuote subforms = malformedSyntax "quote" subforms
    expandLambda (formals : body) = do
        (vars, dotted) <- parse formals
        frame <- makeFrame $ zip (vars ++ maybeToList dotted) (repeat Nothing)
        let env' = extendEnv env frame
        expandBody env' body >>= return . Lambda vars dotted . return 
      where
        parse (Pair (Symbol var) cdr) = liftM (first (var:)) $ parse cdr
        parse (Symbol var) = return ([], Just var)
        parse Nil = return ([], Nothing)
        parse _ = malformedSyntax "lambda" (formals : body)
    expandLambda subforms = malformedSyntax "lambda" subforms
    expandIf subforms = do
        forms <- mapM (expandExpr env) subforms
        case forms of
            [test, expr, expr'] -> return $ If test expr expr'
            _ -> malformedSyntax "if" subforms
    expandSet [(Symbol var), expr] = expandExpr env expr >>= return . Set var
    expandSet subforms = malformedSyntax "set!" subforms
expandExpr env form@(Pair _ _) = expandApp env form
expandExpr env (Symbol var)
    | isBound env var = return $ Var var
    | otherwise = throwError $ Error ("unbound variable: " ++ var)
expandExpr env val = return $ Val val

expandApp :: MacroEnv -> SchemeVal -> SchemeM SchemeForm
expandApp env form@(Pair car _) = do
    liftM2 App (expandExpr env car)
               (getSubforms form >>= mapM (expandExpr env) . tail)
expandApp _ _ = throwError $ Error "should not happen"

expandMacro :: MacroEnv -> Macro -> SchemeVal -> SchemeM SchemeVal
expandMacro env macro form = undefined

getSubforms :: SchemeVal -> SchemeM [SchemeVal]
getSubforms form = do
    catchError (fromSchemeVal form)
               (const . throwError $ SyntaxError "proper list required" form)

malformedSyntax :: String -> [SchemeVal] -> SchemeM a
malformedSyntax keyword subforms = throwError $ SyntaxError msg form
  where msg = "malformed " ++ keyword
        form = toSchemeVal $ Symbol keyword : subforms
