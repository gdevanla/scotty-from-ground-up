{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main where

import qualified Data.Map as Map
import qualified Data.Vector as V

data Sales = Sales (V.Vector Double) deriving (Show)
data Cashflow = Cashflow (V.Vector Double) deriving (Show)
data Dividends = Dividends (V.Vector Double) deriving (Show)
data BTOV = BTOV (V.Vector Double) deriving (Show)

data CIdx = CIdx (V.Vector Integer) deriving (Show)

data FundamentalData = FD {fdcidx :: CIdx,
                           fundSales :: Sales,
                           fundDividend :: Dividends,
                           fundBToV :: BTOV} deriving (Show)


buildFundamentalData::FundamentalData
buildFundamentalData = undefined

data Score = Score {companyIndex :: CIdx,
                    scores:: V.Vector Double} deriving (Show)

data ScreenScore = ScreenScore {sscidx :: CIdx,
                                ssScore:: V.Vector Bool} deriving (Show)
                   
data Scores = SalesScore Score | DividedScores Score | ScreenScores ScreenScore deriving (Show)



data IndexTypes = IntIndexType Int |
  StringIndexType String |
  IntegerIndexType Integer deriving (Show)

useIndexTypes:: IndexTypes -> String
useIndexTypes a = case a of
  (IntIndexType b) -> show b

-- data IndexTypes2 a where
--   StringIndexType2 :: String -> IndexTypes2 String
--   IntIndexType2 :: Int -> IndexTypes2 Int

data family IndexTypes2 a
data instance IndexTypes2 Float = StringIndexType2 String
data instance IndexTypes2 Double = IntIndexType2 Int
  

-- instance Show (IndexTypes2 String)  where
--   show (StringIndexType2 s) = s

-- instance Show (IndexTypes2 Int)  where
--   show (IntIndexType2 i) = (show i)

instance Show (IndexTypes2 Float)   where
  show (StringIndexType2 i) = i

instance Show (IndexTypes2 Double)   where
  show (IntIndexType2 a) = (show a)
  

-- instance (Show a ) => Show (IndexTypes2 a) where
--   show (IntIndexType2 i) = (show i)
--   show (StringIndexType2 i) = (show i)

-- useIndexType2 :: IndexTypes2 String -> String
-- useIndexType2 a = show a

-- test1 = useIndexTypes (StringIndexType "test")


-- data AltIndexTypes f where
--   AltIndexInt ::  Integer -> AltIndexTypes Integer 
--   AltIndexString :: String -> AltIndexTypes String

-- deriving instance Show (AltIndexTypes f)  

-- data ValueTypes = VInteger Integer
--                 | VDouble Double
--                 | VString String
--                 deriving (Show)


-- instance Num ValueTypes where
--   (VDouble i) + (VDouble j) = VDouble $ i + j
--   (VDouble i) + (VInteger j) = VDouble $ i + (fromInteger j)
--   (VInteger i) + (VDouble j) = VDouble $ (fromInteger i) + j
--   (VInteger i) + (VInteger j) = VInteger $ (i + j)

--   (VDouble i) * (VDouble j) = VDouble $ i * j
--   (VDouble i) * (VInteger j) = VDouble $ i * (fromInteger j)
--   (VInteger i) * (VDouble j) = VDouble $ (fromInteger i) * j
--   (VInteger i) * (VInteger j) = VInteger $ (i * j)

--   abs (VDouble i) = VDouble (abs i)
--   abs (VInteger i) = VInteger (abs i)

--   fromInteger i = VInteger $ fromInteger i
--   negate (VDouble i) = VDouble (negate i)
--   negate (VInteger i) = VInteger (negate i)

--   signum (VDouble i) = VDouble (signum i)
--   signum (VInteger i) = VInteger (signum i)




  

  
  
  -- (VDouble i) + (VDouble j) = VDouble $ (i + j)
  -- (VDouble i) + (VDouble j) = VDouble $ (i + j)
  
  
  
  
-- data Index  = Index (V.Vector IndexTypes) deriving (Show)
--data Values = Values (V.Vector ValueTypes) deriving (Show)

-- instance Num Values where
--   (Values v1) + (Values v2) = 


-- addValues:: Values2 a -> Values2 b -> Values2 c
-- addValues (Values2 d1) (Values2 d2) = V.zipWith (+) d1 d2p  
  
-- addValueTypes::ValueTypes -> ValueTypes -> ValueTypes
-- addValueTypes v1 v2 = case (v1, v2) of
--   (VInteger i, VInteger j) -> VInteger (i + j)
--   (VInteger i, VFloat j) -> VFloat (fromIntegral i + j)

-- data Series a = Series {seriesIdx:: Index ,
--                         serieValues::Values2 a}


-- data DataFrame = DataFrame {
--                            dfIdx::(V.Vector IndexTypes),
--                            dfColumns::(V.Vector String),
--                            dfValues::(V.Vector (V.Vector ValueTypes))
--                            }


-- +, -, / , * , exp operations on a  series, with series, with scalar value
-- print series



-- newtype MyDF = MyDF { unType:: DataFrame }


-- newtype CompanyIdx = CompanyIdx { untype::Index}


-- doSomething:: Integer -> CompanyIdx 
-- doSomething x = undefined


-- data U = U1 Integer | U2 String deriving (Show)



-- -- instance Show U where
-- --   show s = case s of
-- --     U1 x -> "integer x" ++ show x
-- --     U2 x -> "string x" ++ show x

-- class MyOperation a where
--   someFunc1::a -> U

-- instance MyOperation String where
--   someFunc1 a = U2 a

-- instance MyOperation Integer where
--   someFunc1 a = U1 a

-- class MyOperation1 a where
--   someFunc2::a->String

-- instance MyOperation1 U where
--   someFunc2 a = case a of
--     (U1 x) -> "test"
--     (U2 x) -> "second test"
  
-- newtype Un = Un {unU::U} deriving (Show)

-- deriving instance MyOperation1 Un

-- data S = S deriving (Show)
-- data I = I deriving (Show)

-- data F f where
--   Lit::String ->  F S 
--   Val::Integer -> F I

-- type family Ii f a
-- type instance Ii f a = Integer
  
-- useF:: F f -> Ii f String
-- useF (Lit s) =  10
-- useF (Val i) = undefined

-- useInnerF :: S -> String
-- useInnerF x = "F S"


func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = (\x -> (x, x)) <$> xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a, a)
func3' xs = (\x -> (x, x)) <$> (xs <* xs)

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' xs ys =  (+) <$> ((+1) <$> xs) <*> ((+1) <$> ys)

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' xs = (\x -> if (x > 0) then (x, 0) else (0, x)) <$> xs

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func9' xs ys zs = (\x -> if even x then ys else zs) <$> xs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)
    

data Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> (String -> Maybe (a, String))
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse (P p) i = case p i of
  Just x -> Just $ fst x
  Nothing -> Nothing

noParser :: Parser a
noParser  = P (\x -> Nothing)

pureParser :: a -> Parser a
pureParser x = P (\input -> Just (x, input))

-- instance Functor Parser where
--   fmap :: (Functor f) => (a -> b) -> Parser a -> Parser b
--   fmap f p = fmap f $ parse p

instance Functor Parser where
  fmap f p = P p'
    where
      p' input = case runParser p input of
                   Just (x, xs) -> Just (f x, xs)
                   Nothing -> Nothing

instance Applicative Parser where
  pure = pureParser
  fp <*> fs = P $ r fp fs
    where
      r p1 p2 input = case runParser p1 input of
                        Just (x, xs) -> case runParser p2 xs of
                                          Just (y, xs1) -> Just(x y, xs1)
                                          Nothing -> Nothing

instance Monad Parser where
  return = pureParser
  fa >>= k = P (\input -> case runParser fa input of
                            Just (x, xs) -> runParser (k x) xs
                            Nothing -> Nothing)

anyChar :: Parser Char
anyChar = P (\input -> case input of
                         (c:xs) -> Just (c, xs)
                         [] -> Nothing)

char :: Char -> Parser ()
char c = do
  c' <- anyChar
  if c ==  c' then return () else noParser
  

anyCharBut :: Char -> Parser Char
anyCharBut c = do
  c' <- anyChar
  if c /= c' then return c' else noParser

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P $ \input -> case runParser p1 input of
      Just r -> Just r
      Nothing -> runParser p2 input

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 >> p1))) `orElse` return []


main :: IO ()
main = putStrLn "in main"


