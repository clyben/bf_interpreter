{-# LANGUAGE LambdaCase #-}
import Data.Char
-- import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import System.Environment   


data Token = Less | Greater | Plus | Minus | Dot | Comma 
                | LBracket | RBracket | Unknown | EndOfTape deriving Show

toTokenList :: String -> [Token]
toTokenList s = filter (\case
                            Unknown -> False
                            _ -> True ) $ map toToken s
    where toToken c = case c of
            '<' -> Less
            '>' -> Greater
            '+' -> Plus
            '-' -> Minus
            '.' -> Dot
            ',' -> Comma
            '[' -> LBracket
            ']' -> RBracket
            _   -> Unknown

data Zipper a = Zipper [a] a [a] deriving Show

moveRight :: Zipper a -> Zipper a
moveRight m =
    case m of
        Zipper l i (x:xs) -> Zipper (i:l) x xs
        Zipper _ _ [] -> error "cannot move right"

moveLeft :: Zipper a -> Zipper a
moveLeft m =
    case m of
        Zipper (x:xs) i r -> Zipper xs x (i:r)
        Zipper [] _ _ -> error "cannot move left"

inc :: Num a => Zipper a -> Zipper a
inc (Zipper l i r) = Zipper l (i+1) r

dec :: Num a => Zipper a -> Zipper a
dec (Zipper l i r) = Zipper l (i-1) r

getter :: Num a => Zipper a -> a
getter (Zipper l i r) = i

setter :: Num a => Zipper a -> a -> Zipper a
setter (Zipper l _ r) num = Zipper l num r

-- test --
testProgram :: String
testProgram = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

tokens :: [Token]
tokens = toTokenList testProgram

type PState = (Zipper Token, Zipper Int)

begin :: PState
begin = let (t:ts) = tokens in (Zipper ts t [], Zipper [0,0..] 0 [0,0..])


toNextCell :: StateT PState IO()
toNextCell = do
    (pro, mem) <- get
    put (moveRight pro, moveRight mem)
    return ()


toPrevCell :: StateT PState IO()
toPrevCell = do
    (pro, mem) <- get
    put (moveRight pro, moveLeft mem)
    return ()

incCell :: StateT PState IO()
incCell = do
    (pro, mem) <- get
    put (moveRight pro, inc mem)
    return ()

decCell :: StateT PState IO()
decCell = do
    (pro, mem) <- get
    put (moveRight pro, dec mem)
    return ()

outputChar :: StateT PState IO()
outputChar = do
    (pro, mem) <- get
    --liftIO $ print (show mem)
    liftIO $ putChar (chr $ getter mem)
    put (moveRight pro, mem)
    return ()


inputChar :: StateT PState IO()
inputChar = do
    (pro, mem) <- get
    c <- liftIO getChar
    put (moveRight pro, setter mem $ ord c)
    return ()


-- type Stack = [Token]
-- pop :: State Stack Token
-- pop = state $ \(x:xs) -> (x, xs)

-- peek :: State Stack Token
-- peek = state $ \(x:xs) -> (x, x : xs)

-- push :: Token -> State Stack ()
-- push i = state $ \xs -> ((), i : xs)


findNext :: Zipper Token -> [Token] -> Zipper Token
findNext (Zipper l LBracket r) stack = findNext (moveRight (Zipper l LBracket r)) (LBracket : stack)
findNext (Zipper l RBracket r) [LBracket] = Zipper l RBracket r
findNext (Zipper l RBracket r) (LBracket:rest) = findNext (moveRight (Zipper l RBracket r)) rest
findNext zip stack = findNext (moveRight zip) stack

findBack :: Zipper Token -> [Token] -> Zipper Token
findBack (Zipper l RBracket r) stack = findBack (moveLeft (Zipper l RBracket r)) (RBracket : stack)
findBack (Zipper l LBracket r) [RBracket] = Zipper l LBracket r
findBack (Zipper l LBracket r) (RBracket:rest) = findBack (moveLeft (Zipper l LBracket r)) rest
findBack zip stack = findBack (moveLeft zip) stack

-- test --
test1 :: Zipper Token
test1 = findNext (Zipper (toTokenList "") LBracket (toTokenList "][][][][]")) []

test3 = findNext (Zipper [] LBracket (toTokenList "[++--.[**,[**++**,.]]]].")) []

test2 :: Zipper Token
test2 = findBack (Zipper [LBracket] RBracket (toTokenList "[][]")) []



jmpNext :: StateT PState IO()
jmpNext = do
    (pro, mem) <- get
    case getter mem of
        0 -> put (moveRight $ findNext pro [], mem)
        _ -> put (moveRight pro, mem)
    return ()


jmpBack :: StateT PState IO()
jmpBack = do
    (pro, mem) <- get
    case getter mem of
        0 -> put (moveRight pro, mem)
        _ -> put (moveRight $ findBack pro [], mem)
    return ()

link :: StateT PState IO ()
link = do
    (Zipper l m r, mem) <- get
    --liftIO $ print $ show (Zipper l m r)
    -- case r of 
    --     [] -> return ()
    --     _ -> let nextStep = case m of
    --                             Less -> toPrevCell
    --                             Greater -> toNextCell
    --                             Plus -> incCell
    --                             Minus -> decCell
    --                             Dot -> outputChar
    --                             Comma -> inputChar
    --                             LBracket -> jmpNext
    --                             RBracket -> jmpBack
    --                             Unknown -> error ""
    --             in 
    --                 nextStep >> link
    case m of
        EndOfTape -> return ()
        _ -> nextStep >> link
            where nextStep = case m of
                                Less -> toPrevCell
                                Greater -> toNextCell
                                Plus -> incCell
                                Minus -> decCell
                                Dot -> outputChar
                                Comma -> inputChar
                                LBracket -> jmpNext
                                RBracket -> jmpBack
                                _ -> error ""

run :: String -> IO()
run s = 
    let (t:ts) = toTokenList s in
    let begin = (Zipper [] t (ts ++ [EndOfTape]), Zipper (repeat 0) 0 (repeat 0)) in
        do
        runStateT link begin
        return ()

main :: IO()
main = do
    args <- getArgs
    case length args of
        1 -> let filename = head args in
                do 
                content <- readFile filename
                run content
        _ -> error $ "usage: ./bf filename" ++ show (length args)
