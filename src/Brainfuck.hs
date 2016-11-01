module Brainfuck
    ( Command
    , TuringMachine
    , newTM
    , parse
    , eval
    , run
    ) where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Text.Parsec hiding (parse)
import Text.Parsec.Char

-- Definitions for the 'Turing machine'

data Command = Increment | Decrement | MoveLeft | MoveRight | Out | In | While [Command]
    deriving (Show)

data TuringMachine = TuringMachine {band :: Map.Map Int Integer, pointer :: Int}

-- Actions on TM
-- TODO: Read and write TM to/from file

newTM = TuringMachine Map.empty 0

changeVal k (TuringMachine b p) = TuringMachine (modifyValue (+k) p b) p
movePointer k (TuringMachine b p) = TuringMachine b (p+k)
getValue (TuringMachine b p) = maybe 0 id (Map.lookup p b)
setValue v (TuringMachine b p) = TuringMachine (Map.insert p v b) p

modifyValue f = Map.alter (Just . f . (maybe 0 id))

-- Parse BF code using parsec

parseChar :: Char -> Command
parseChar '+' = Increment
parseChar '-' = Decrement
parseChar '>' = MoveLeft
parseChar '<' = MoveRight
parseChar '.' = Out
parseChar ',' = In

parsePrimitives = parseChar <$> satisfy (\x -> elem x ".,+-><")
parseBrackets = between (char '[') (char ']')
parseBF = many (parsePrimitives <|> (While <$> parseBrackets parseBF))
parse fname = runParserT parseBF () fname . reduce

reduce = filter (\x -> elem x ".,+-><[]")

-- do IO stuff

getInteger :: IO Integer
getInteger = getLine >>= readIO

printChar :: Integer -> IO ()
printChar = print

-- our monadic loop function

while :: Monad m => m Bool -> m a -> m ()
while b f = b >>= \bool -> if bool then f >> while b f else return ()

-- evaluate the code

evalCmd Increment = modify (changeVal 1)
evalCmd Decrement = modify (changeVal (-1))
evalCmd MoveRight = modify (movePointer 1)
evalCmd MoveLeft  = modify (movePointer (-1))
evalCmd In = liftIO getInteger >>= \x -> modify (setValue x)
evalCmd Out = get >>= \tm -> liftIO (printChar (getValue tm))
evalCmd (While commands) = while (liftM ((/=0) . getValue) get) (mapM evalCmd commands)

-- enduser functions

eval initialState prog = evalStateT (mapM_ evalCmd prog) initialState
run fname input = parse fname input >>= either print (eval newTM)
