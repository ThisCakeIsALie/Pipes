{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens.TH
import Data.Map (Map)
import Control.Monad.State
import Streamly
import Streamly.Prelude as S

-- Other




--type Runtime = StateT Environment (SerialT IO)

type ValueStream = SerialT IO PValue

type Identifier = String

data Environment = Environment { _vars :: Map Identifier PValue, _defs :: Map Identifier Definition}

-- Syntax






data PValue = PString String
            | PNumber Double
            | PBool Bool
            | PList ValueStream
            | PPipeline Pipeline
            | PError String
            | None

display :: PValue -> IO String
display (PString string) = return $ show string
display (PNumber number) = return $ show number
display (PBool bool) = return $ show bool
display (PList list) = fmap show . toList $ S.mapM display list
display (PPipeline line) = display (PError "Pipelines cannot be viewed")
display (PError error) = return $ "Unexpected Error occured: " ++ error
display None = return $ "None"

data Expression = Value PValue
                | Var Identifier
                | Application PValue --If PValue is PPipeline apply. Otherwise Error








data PipeComponent = Assign Identifier Expression


data Definition = Def [Identifier] Expression | Builtin (Environment -> [Expression] -> ValueStream -> ValueStream)

data Pipe = Pipe {_components :: [PipeComponent], _inputs :: [Identifier], _outputs :: [Expression]}

data PipeCall = Local Pipe [Expression] | Call Identifier [Expression]

data Pipeline = Connect PipeCall Pipeline
              | Gather PipeCall Pipeline
              | Spread PipeCall Pipeline
              | End



type DefArgument = Identifier -- | Defined Identifier Expression


makeLenses ''Environment
makeLenses ''Pipe
makePrisms ''Definition
makePrisms ''PipeCall
makePrisms ''PipeComponent
makePrisms ''Pipeline
makePrisms ''Expression
makePrisms ''PValue