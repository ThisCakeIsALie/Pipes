module Interpret.Evaluate where
 
import Types
import Interpret.Environment
import Control.Lens
import Streamly
import qualified Streamly.Prelude as S
import qualified Data.Map as M

translateUsingEnv :: Environment -> ParsedExpression -> Expression
translateUsingEnv env (ParsedVar identifier) = case findDef identifier env of
  Just ~(Def value _) -> Value value
  Nothing -> error "Definition not found"
translateUsingEnv env (ParsedValue value) = Value $ case value of
    ParsedString string -> String string
    ParsedNumber number -> Number number
    ParsedBool bool -> Bool bool
    ParsedNone -> None
    ParsedList list -> Pipe $ P (const 0) (\currEnv -> const $ Types.List <$> traverse (evaluateExpr env . translateUsingEnv (slurpGlobals env currEnv)) list)
    ParsedStream stream -> Pipe $ P (const 0) (\currEnv -> const . pure . Types.Stream $ S.fromFoldable stream
        & S.mapM (evaluateExpr env . translateUsingEnv (slurpGlobals env currEnv)))
    ParsedObject obj -> Pipe $ P (const 0) (\currEnv -> const $ Object <$> asObject currEnv obj)
      where asObject currEnv obj = traverse (evaluateExpr env . translateUsingEnv (slurpGlobals env currEnv)) (M.fromList obj)
    ParsedPipe (Anonymous inputs output) -> Pipe $ P (const $ length inputs) (
      \currEnv inputValues -> 
        let
          localEnv = slurpGlobals (defineAllValues (zip inputs inputValues) False env) $ currEnv
        in
          evaluateExpr localEnv (translateUsingEnv localEnv output)
     )
    ParsedPipe (Transform before transformer cont) -> Pipe $ P (\env -> neededArgs env $ translateUsingEnv env before) (
      \currEnv inputValues -> do
          let localEnv = slurpGlobals env currEnv
          let beforeExpr = translateUsingEnv localEnv before
          let transformerExpr = translateUsingEnv localEnv transformer
          let contExpr = translateUsingEnv localEnv cont
          beforeValue <- evaluateExpr localEnv beforeExpr
          beforeValueApplied <- applyValue localEnv beforeValue inputValues
          mapper <- evaluateExpr localEnv (Application transformerExpr [contExpr])
          applyValue localEnv mapper [beforeValueApplied]
      )
  where
    asParsedExpr = ParsedValue . ParsedPipe
translateUsingEnv env (ParsedApplication applicant args) = Application (translateUsingEnv env applicant) (map (translateUsingEnv env) args)

neededArgs :: Environment -> Expression -> Int
neededArgs env (Value (Pipe pipe)) = neededPipeArgs env pipe
neededArgs env (Application applicant args) = neededArgs env applicant - length args
neededArgs env _ = 0

neededPipeArgs :: Environment -> Pipe -> Int
neededPipeArgs env (P remainingArgs _) = remainingArgs env

evaluateExpr :: Environment -> Expression -> Runtime Value
evaluateExpr env (Value value) = normalizeValue env value
evaluateExpr env (Application expr args) = do
  applicant <- evaluateExpr env expr
  args <- traverse (evaluateExpr env) args
  result <- applyValue env applicant args
  normalizeValue env result

normalizeValue :: Environment -> Value -> Runtime Value
normalizeValue env (Pipe pipe) = 
  if (pipe ^. remainingArgs $ env) == 0
    then applyPipe env pipe []
    else pure $ Pipe pipe
normalizeValue env value = pure value


applyValue :: Environment -> Value -> [Value] -> Runtime Value
applyValue env (Pipe pipe) args = applyPipe env pipe args
applyValue env value args = if length args == 0 then pure value else error "Value got too many args"

applyPipe :: Environment -> Pipe -> [Value] -> Runtime Value
applyPipe env ~(P remainingArgs f) args
  | argsLeft env > 0 = pure $ Pipe $ P argsLeft (\nextEnv nextArgs -> f (mergeEnv env nextEnv) (args ++ nextArgs)) 
  | argsLeft env == 0 = f env args
  | otherwise = error "Value got too many args"
  where
    argsLeft env = remainingArgs env - length args

evaluateParsedExpr :: Environment -> ParsedExpression -> Runtime Value
evaluateParsedExpr env = evaluateExpr env . translateUsingEnv env