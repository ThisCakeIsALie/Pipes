{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens.TH
import Data.Map (Map)
import Control.Monad.State
import Streamly

-- Other




type Runtime = StateT Environment (SerialT IO)

type Identifier = String

data Environment = Environment { _vars :: Map Identifier PValue, _defs :: Map Identifier Definition }

-- Syntax






data PValue = PString String
            | PNumber Double
            | PBool Bool
            | PList [PValue]
            | PPipeline Pipeline
            | PError String
            | None

instance Show PValue where
    show (PString string) = string
    show (PNumber number) = show number
    show (PBool bool) = show bool
    show (PList list) = show list
    show (PPipeline line) = show (PError "Pipelines cannot be viewed")
    show (PError error) = "Unexpected Error occured: " ++ error
    show None = "None"

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



data DefArgument = Named Identifier -- | Defined Identifier Expression






data Definition = Def {_identifier :: Identifier,  _arguments :: [DefArgument] , _value :: Expression}


makeLenses ''Environment
makeLenses ''Definition