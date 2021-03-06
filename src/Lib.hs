{-# LANGUAGE CPP #-}

module Lib
    ( someFunc
    ) where

-- import Prelude
-- import Data.Char
-- import Text.Read
-- import Control.Applicative
-- import Control.Monad
-- import Control.Monad.Error
-- import System.Environment
-- import Text.Printf

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- args :: IO ()
-- args = getArgs >>= mymapM_ putStrLn
--
-- mymapM_ :: (Monad m) => (a -> m ()) -> [a] -> m ()
-- mymapM_ _ [] = return ()
-- mymapM_ f (x:xs) = do
--     f x
--     mymapM_ f xs
--
-- mymapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
-- mymapM _ [] = return []
-- mymapM f (x:xs) = do
--     y <- f x
--     ys <- mymapM f xs
--     return (y : ys)
--
-- favourite :: IO ()
-- favourite = do
--     putStrLn "What is your favourite programming lang?"
--     lang <- getLine
--     if lang == "Haskell"
--     then putStrLn "Correct"
--     else favourite
--
-- -- | Count lines, words, and chars
-- wc :: IO ()
-- wc = do
--     filename <- fmap head getArgs
--     contents <- readFile filename
--     putStrLn $ "Lines: " ++ (show . length $ lines contents)
--     putStrLn $ "Words: " ++ (show . length $ words contents)
--     putStrLn $ "Chars: " ++ show (length contents)
--
--
-- -- convert and error
--
-- data ParseError = Err {location::Int, reason::String}
--
-- instance Show ParseError where
--     show e = show (location e) ++ ": " ++ reason e
--
-- instance Error ParseError where
--     noMsg = Err {location=__LINE__, reason="Unknown"}
--     strMsg str = Err {location=__LINE__, reason=str}
--
-- type ParseMonad = Either ParseError
--
-- parseHexDigit :: Char -> Int -> ParseMonad Int
-- parseHexDigit c i
--     | c `elem` ['0'..'9'] = Right $ digitToInt c * 16^i
--     | c `elem` ['a'..'f'] = Right $ (ord c - 87) * 16^i
--     | otherwise = throwError Err{location=i, reason=show c}
--
-- parseHex :: String -> ParseMonad Int
-- parseHex = parseHex' . zip [0..] . reverse
--
-- parseHex' :: [(Int, Char)] -> ParseMonad Int
-- parseHex' [] = return 0
-- parseHex' ((i, c) : xs) = do
--     digit <- parseHexDigit c i
--     rest <- parseHex' xs
--     return $ digit + rest
--
-- -- parseHex = foldM (\a b -> Right $ a + b) 0
-- --     . map (uncurry . flip $ parseHexDigit)
-- --     . zip [0..] . reverse
--
-- toString :: Int -> ParseMonad String
-- toString = return . printf "%x"
--
--
-- -- | convert zamienia napis z liczba szesnastkowa
-- -- |  na napis z liczba dziesietna
-- convert :: String -> String
-- convert s = str where
--  (Right str) = tryParse s `catchError` printError
--  tryParse s = parseHex s >>= toString
--  printError e = return $ show e
--
-- -- TODO add gitignore for Stack, commit to git
-- -- TODO enable fancy tools in HaskForce
--
-- --  end convert and error
--
-- readInts2 :: [String] -> Maybe [Int]
-- readInts2 [] = pure []
-- readInts2 (x:xs) = (:) <$> (readMaybe x :: Maybe Int) <*> readInts2 xs
--
-- readInts3 :: [String] -> Maybe [Int]
-- readInts3 [] = pure []
-- readInts3 (x:xs) = liftA2 (:) (readMaybe x :: Maybe Int) (readInts2 xs)
--
-- readInts4 :: [String] -> Maybe [Int]
-- readInts4 xs = sequenceA $ map (\x -> readMaybe x :: Maybe Int) xs
--
-- readInts5 :: [String] -> Either String [Int]
-- -- readInts5 xs = sequenceA $ map nan xs
-- -- readInts5 xs = sequence $ map nan xs
-- -- readInts5 xs = traverse nan xs
-- readInts5 = mapM nan
--     where
--         eitherMaybe _ (Just x) = Right x
--         eitherMaybe s Nothing = Left s
--         readEitherMaybe e s = eitherMaybe e $ readMaybe s
--         nan s = readEitherMaybe ("Not a number: " ++ s) s
--
-- readTwoIntsAndSum :: [String] -> Maybe [Int]
-- readTwoIntsAndSum xs = do
--     let (s1:ys) = xs
--     n1 <- readMaybe s1 :: Maybe Int
--     let (s2:_) = ys
--     n2 <- readMaybe s2 :: Maybe Int
--     return [n1, n2, n1 + n2]
--
-- readInts6 :: [String] -> Maybe [Int]
-- readInts6 [] = return []
-- readInts6 (x:xs) = do
--     n <- readMaybe x :: Maybe Int
--     ns <- readInts6 xs
--     return (n : ns)
