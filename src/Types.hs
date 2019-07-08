{-# LANGUAGE TemplateHaskell #-}
module Types where
 
import Control.Lens.TH
import Data.Map (Map)
import Control.Monad.State
import Streamly
import Streamly.Prelude as S
import System.IO.Unsafe
 
-- Other
 
 
type ValueStream = SerialT IO PValue
 
type Identifier = String

type Definition = PValue
 
type Environment = Map Identifier Definition

 
-- Syntax
 
 
 
 
 
 
data PValue = PString String
            | PNumber Double
            | PBool Bool
            | PList ValueStream
            | PPipe Pipe
            | PError String
            | None
 
display :: PValue -> IO String
display (PString string) = return $ show string
display (PNumber number) = return $ show number
display (PBool bool) = return $ show bool
display (PList list) = fmap show . toList $ S.mapM display list
display (PPipe line) = display (PError "Pipelines cannot be viewed")
display (PError error) = return $ "Unexpected Error occured: " ++ error
display None = return $ "None"
 
instance Show PValue where
    show (PString string) = show string
    show (PNumber number) = show number
    show (PBool bool) = show bool
    show (PList list) = show $ unsafePerformIO $ toList $ list
    show (PPipe (Anonymous _ pipeEnv inputs output)) = show inputs ++ " -> " ++ show output ++ " bound: " ++ show pipeEnv
    show (PPipe line) = show (PError "Pipelines cannot be viewed")
    show (PError error) = show $ "Unexpected Error occured: " ++ error
    show None = "None"
 
data Expression = Value PValue
                | Var Identifier
                | Application Expression [Expression]
                deriving Show
 
 
 
data Pipe = Anonymous {_closure :: Environment, _bound :: [Expression], _inputs :: [Identifier], _output :: Expression}
          | Builtin {_bound :: [Expression], _argsLeft :: [Expression] -> Int, _transformer :: [PValue] -> [PValue] -> IO PValue}
          | Apply Expression
          | Transform Expression Pipe Pipe
 
 
type DefArgument = Identifier -- | Defined Identifier Expression
 
 
makeLenses ''Pipe
makePrisms ''Expression
makePrisms ''PValue


--TODO: Add tag as expr or value that contains scope (This avoids recursively updating scopes)