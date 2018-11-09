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

data Environment = Environment { _vars :: Map Identifier PValue, _defs :: Map Identifier Definition }

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
                | Application Expression PValue --If PValue is PPipeline apply. Otherwise Error








data PipeComponent = In Identifier
                   | Out Expression
                   -- | Assign Identifier Expression






data Pipe = Anonymous [PipeComponent] {- Scope -} | Call Identifier [PValue]

data Pipeline = Connect Pipe Pipeline
              | Gather Pipe Pipeline
              | Spread Pipe Pipeline
              | End



type DefArgument = Identifier -- | Defined Identifier Expression






data Definition = Def {_arguments :: [DefArgument] , _value :: Expression}


makeLenses ''Environment
makeLenses ''Definition
makePrisms ''PipeComponent
makePrisms ''Pipe
makePrisms ''Pipeline
makePrisms ''Expression
makePrisms ''PValue