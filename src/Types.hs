{-# LANGUAGE TemplateHaskell #-}
module Types where
 
import Control.Lens.TH
import Data.Map (Map)
import Control.Monad.State
import Streamly
import Streamly.Prelude as S
import System.IO.Unsafe
 
-- Other
 
 
 
 
--type Runtime = StateT Environment (SerialT IO)
 
type ValueStream = SerialT IO PValue
 
type Identifier = String
 
data Environment = Environment {_defs :: Map Identifier Definition} deriving Show
 
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
    show (PPipe (Yield exprs pipe)) = show exprs ++ " " ++ show (PPipe pipe)
    show (PPipe line) = show (PError "Pipelines cannot be viewed")
    show (PError error) = show $ "Unexpected Error occured: " ++ error
    show None = "None"
 
data Expression = Value PValue
                | Var Identifier
                | Application Expression [Expression]
                deriving Show
 
 
 
 
 
 
 
 
data PipeComponent = Assign Identifier Expression
 
 
data Definition = Def Expression deriving Show
 
 
data Pipe = Anonymous {_components :: [PipeComponent], _env :: Environment, _inputs :: [Identifier], _output :: Expression}
          | Apply Expression
          {-| Builtin {_scopeUpdater :: Map Identifier Definition -> Pipe
                   , _evalUpdater :: Environment -> [Expression] -> (Pipe,Bool)
                   , _transformer :: Environment -> ValueStream -> ValueStream
                    }
            -}
          | Builtin {_env :: Environment, _argsLeft :: Environment -> Int, _transformer :: [(Identifier, PValue)] -> [PValue] -> IO [PValue]}
          | Yield [Expression] Pipe
          | Connect Pipe Pipe
          | Gather Pipe Pipe
          | Spread Pipe Pipe
 
 
 
type DefArgument = Identifier -- | Defined Identifier Expression
 
 
makeLenses ''Environment
makeLenses ''Pipe
makePrisms ''Definition
makePrisms ''PipeComponent
makePrisms ''Expression
makePrisms ''PValue