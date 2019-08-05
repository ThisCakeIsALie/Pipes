{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Types where
 
import Control.Lens.TH
import Data.Map (Map)
import Data.List
import qualified Data.Map as M
import Control.Applicative
import Data.Coerce 
import Control.Lens
import Streamly
 
type Identifier = String
type DefArgument = Identifier
type Environment = Map Identifier Definition
type ObjStore = Map Identifier Value
type ValueStream = SerialT Runtime Value

type Runtime = IO

data Definition = Def { _defValue :: Value, _defGlobal :: Bool }
 
data ParsedValue = ParsedString String
            | ParsedNumber Double
            | ParsedBool Bool
            | ParsedList [ParsedExpression]
            | ParsedObject [(Identifier,ParsedExpression)]
            | ParsedStream [ParsedExpression]
            | ParsedPipe ParsedPipe
            | ParsedNone
            deriving Show

data Value = String String
           | Number Double
           | Bool Bool
           | List [Value]
           | Object ObjStore
           | Stream ValueStream
           | Pipe Pipe
           | None
 
instance Show Value where
    show (String string) = show string
    show (Number number) = show number
    show (Bool bool) = show bool
    show (Types.List list) = show list
    show (Stream stream) = "Stream"
    show (Object obj) = "{ " ++ intercalate ", " (map (\(name,value) -> name ++ ": " ++ show value) $ M.toList obj) ++ " }"
    show (Pipe pipe) = "Pipe (" ++ show (_remainingArgs pipe undefined) ++ " args left)"
    show None = "None"
 
data ParsedExpression = ParsedValue ParsedValue
                | ParsedVar Identifier
                | ParsedApplication ParsedExpression [ParsedExpression]
                deriving Show

data Expression = Value Value
                | Application Expression [Expression]
                deriving Show
            
{-
data Computation (m :: * -> *) a = Pure a | Impure (m a)

instance (Monad m) => Functor (Computation m) where
    fmap f x = x >>= (pure . f)
    
instance (Monad m) => Applicative (Computation m) where
    pure = Pure
    liftA2 f x y = do
        x' <- x
        f x' <$> y

instance (Monad m) => Monad (Computation m) where
    (>>=) (Pure a) f = f a
    (>>=) (Impure a) f = Impure $ do
            ay <- a
            case f ay of
                Pure val -> pure val
                Impure val -> val

compToEither :: (Monad m) => Computation m a -> Either a (m a)
compToEither (Pure x) = Left x
compToEither (Impure x) = Right x

impurify :: (Monad m) => Computation m a -> m a
impurify (Pure val) = pure val
impurify (Impure val) = val

purify :: Computation m a -> Maybe a
purify (Pure val) = Just val
purify (Impure _) = Nothing
-}

data Pipe = P { _remainingArgs :: Environment -> Int, _pipeFunc :: Environment -> [Value] -> Runtime Value } --Pass current env so undefined vars can be taken from there

data ParsedPipe = Anonymous {_inputs :: [Identifier], _output :: ParsedExpression}
          | Transform {_before :: ParsedExpression, _transformer :: ParsedExpression, _cont :: ParsedExpression}
          deriving Show
 
makeLenses ''Definition
makeLenses ''Pipe
makePrisms ''ParsedPipe
makePrisms ''Expression