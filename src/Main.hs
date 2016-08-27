{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, Rank2Types #-}
{-# LANGUAGE DeriveFunctor, ScopedTypeVariables, PackageImports #-}
module Main where

import Haste
import Haste.Foreign hiding (get)
import Haste.JSON
import Haste.Serialize
import MakeLense
import Lens.Family2
import Lens.Family2.Stock
import Lens.Family2.State.Lazy
import Lens.Family2.Unchecked
import Data.IORef
import Data.List
import qualified Data.Map as M
import "mtl" Control.Monad.State
import Debug.Trace

data LambdaT a = LVar a | LAbs Lambda | LApp Lambda Lambda deriving (Eq, Show, Functor)
type Lambda = LambdaT Int
data SKI = S | K | I | CApp SKI SKI deriving (Eq, Show, Read)
data CTerm = CLambda Lambda | CSKI SKI deriving (Eq, Show)

instance ToAny SKI where
  toAny = toAny . show

instance FromAny SKI where
  fromAny k = read <$> fromAny k

data LambdaS = LVar' Int | LAbs' Int LambdaS | LApp' LambdaS LambdaS

lambdaS :: Lambda -> LambdaS
lambdaS x = go 0 x [] where
  go :: Int -> Lambda -> [Int] -> LambdaS
  go n (LVar v) r = LVar' (r !! v)
  go n (LAbs m) r = LAbs' n (go (n+1) m (n:r))
  go n (LApp m1 m2) r = LApp' (go n m1 r) (go n m2 r)

instance Show LambdaS where
  show (LVar' v)
    | v == 0 = "x"
    | v == 1 = "y"
    | v == 2 = "z"
    | otherwise = "v" ++ show v
  show (LAbs' v m) = "λ" ++ show (LVar' v) ++ ". " ++ show m
  show (LApp' m1 m2) = paren m1 ++ paren m2 where
    isLVar' (LVar' _) = True
    isLVar' _ = False

    paren k = if isLVar' k then show k else "(" ++ show k ++ ")"

shiftOver :: Int -> Int -> Lambda -> Lambda
shiftOver dp ofs (LVar v)
  | v >= dp = LVar (v + ofs)
  | otherwise = LVar v
shiftOver dp ofs (LAbs m) = LAbs (shiftOver (dp + 1) ofs m)
shiftOver dp ofs (LApp m1 m2) = LApp (shiftOver dp ofs m1) (shiftOver dp ofs m2)

unshift :: Int -> Lambda -> Lambda
unshift n = fmap (subtract n)

subst :: Lambda -> Lambda -> Lambda
subst m1 m2 = go 0 (unshift 1 m1) where
  go n (LVar v')
    | n == v' = shiftOver 0 n m2
    | otherwise = LVar v'
  go n (LAbs m1') = LAbs (go (n+1) m1')
  go n (LApp m1' m2') = LApp (go n m1') (go n m2')

unsafeEval :: Lambda -> Lambda
unsafeEval (LVar v) = LVar v
unsafeEval (LAbs m) = LAbs (unsafeEval m)
unsafeEval (LApp (LVar v) m2) = LApp (LVar v) (unsafeEval m2)
unsafeEval (LApp (LAbs m1) m2) = unsafeEval (subst m1 m2)
unsafeEval (LApp m1 m2) = LApp (unsafeEval m1) (unsafeEval m2)

psubterms :: Lambda -> [Lambda]
psubterms (LVar v) = []
psubterms (LAbs m) = psubterms m
psubterms (LApp m1 m2) = psubterms m1 ++ psubterms m2

leval :: Lambda -> (Int, Lambda)
leval m0 = go 0 m0 where
  go n z | n >= 2^(length $ show m0) = (-1, z)
  go n (LVar v) = (n,LVar v)
  go n (LAbs m) = go (n+1) m
  go n (LApp (LVar v) m2) = let (t,k) = go (n+1) m2 in (t, LApp (LVar v) k)
  go n (LApp (LAbs m1) m2) = go (n+1) (subst m1 m2)
  go n (LApp m1 m2) =
    let (t1,k1) = go (n+1) m1;
        (t2,k2) = go (n+1) m2 in (t1+t2, LApp k1 k2)

seval :: SKI -> (Int, SKI)
seval m0 = go 0 m0 where
  go n (CApp I m) = go (n+1) m
  go n (CApp (CApp K m1) m2) = go (n+1) m1
  go n (CApp (CApp (CApp S m1) m2) m3) = go (n+1) (CApp (CApp m1 m3) (CApp m2 m3))
  go n m = (n,m)

_mayOf :: a -> Lens' (Maybe a) a
_mayOf d = lens (maybe d id) (\m x -> Just x)

data MachineType = Miner | Pipe | Factory deriving (Eq, Show, Enum, Read)

inA :: MachineType -> Int
inA Miner = 0
inA Factory = 10
inA Pipe = 1

outA :: MachineType -> Int
outA Miner = 10
outA Factory = 0
outA Pipe = 1

instance ToAny MachineType where
  toAny = toAny . show

instance FromAny MachineType where
  fromAny k = read <$> fromAny k

instance Serialize MachineType where
  toJSON = Num . fromIntegral . fromEnum
  parseJSON n = toEnum <$> parseJSON n

type Machine = UnionT '[
  "mtype" :< MachineType,
  "position" :< (Int, Int),
  "items" :< M.Map SKI Int,
  "amount" :< Double
  ]

type Game = UnionT '[
  "machines" :< M.Map (Int,Int) Machine,
  "diff" :< M.Map (Int,Int) Double,
  "connects" :< [((Int,Int),(Int,Int))],
  "minemap" :< M.Map (Int,Int) (SKI,Double)
  ]

mtype :: Has (Union xs) "mtype" out => Lens' (Union xs) out; mtype = lenses (Name :: Name "mtype")
position :: Has (Union xs) "position" out => Lens' (Union xs) out; position = lenses (Name :: Name "position")
items :: Has (Union xs) "items" out => Lens' (Union xs) out; items = lenses (Name :: Name "items")
amount :: Has (Union xs) "amount" out => Lens' (Union xs) out; amount = lenses (Name :: Name "amount")
parents :: Has (Union xs) "parents" out => Lens' (Union xs) out; parents = lenses (Name :: Name "parents")
machines :: Has (Union xs) "machines" out => Lens' (Union xs) out; machines = lenses (Name :: Name "machines")
diff :: Has (Union xs) "diff" out => Lens' (Union xs) out; diff = lenses (Name :: Name "diff")
connects :: Has (Union xs) "connects" out => Lens' (Union xs) out; connects = lenses (Name :: Name "connects")
minemap :: Has (Union xs) "minemap" out => Lens' (Union xs) out; minemap = lenses (Name :: Name "minemap")

initGame :: IO (Opaque Game)
initGame = return $ toOpaque $
  sinsert (Tag M.empty :: "machines" :< M.Map (Int,Int) Machine) $
  sinsert (Tag M.empty :: "diff" :< M.Map (Int,Int) Double) $
  sinsert (Tag []) $
  sinsert (Tag (M.fromList [((0,0), (S,100))])) $
  Union HNil

newMachine :: String -> (Int, Int) -> StateT Game IO ()
newMachine st p = do
  machines %= M.insert p mch
  connectMF
  where
    typ = read st
    mch =
      sinsert (Tag typ) $
      sinsert (Tag p) $
      sinsert (Tag M.empty) $
      sinsert (Tag 0) $
      Union HNil

machinesOnScreen :: (Int, Int) -> Opaque Game -> IO [((Int,Int), MachineType)]
machinesOnScreen (px, py) g = return $ fmap (\m -> (m^.position, m^.mtype)) $ filter (\m -> (m^.position) `isIn` ((px - 320, py - 200), (px + 320, py + 200))) (M.elems $ fromOpaque g ^. machines)
  where
    isIn :: (Ord a) => (a,a) -> ((a,a),(a,a)) -> Bool
    isIn (x,y) ((rx1,ry1),(rx2,ry2)) = rx1 <= x && x <= rx2 && ry1 <= y && y <= ry2

connectMF :: StateT Game IO ()
connectMF = do
  ms <- M.keys . M.filter (\m -> m^.mtype == Miner) <$> use machines
  imachines <- use machines
  let ps = concat $ fmap (\m -> fmap ((,) m) $ go m imachines [m]) ms
  connects .= ps

  where
    go :: (Int,Int) -> M.Map (Int,Int) Machine -> [(Int,Int)] -> [(Int,Int)]
    go (x,y) mp trac = concat $ [run p mp trac | p <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], M.member p mp, p `notElem` trac]

    run :: (Int,Int) -> M.Map (Int,Int) Machine -> [(Int,Int)] -> [(Int,Int)]
    run p mp trac = case (mp M.! p)^.mtype of
      Miner -> []
      Pipe -> go p mp (p:trac)
      Factory -> [p]

tick :: StateT Game IO ()
tick = do
  mchs <- use machines
  ds <- use diff

  mapM_ (uncurry runMachine) $ M.toList mchs
  forM_ (M.keys mchs) $ \p -> do
    machines . at p . _Just . amount += ds ^. at p ^. _mayOf 0

  diff .= M.empty

  where
    neighbors :: (Int, Int) -> StateT Game IO [(Int, Int)]
    neighbors (x,y) = do
      mch <- use machines
      return [p | p <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], M.member p mch]

    send :: (Int, Int) -> (Int, Int) -> Double -> StateT Game IO ()
    send source target m = do
      diff . at source . _mayOf 0 -= m
      diff . at target . _mayOf 0 += m

    runMachine :: (Int, Int) -> Machine -> StateT Game IO ()
    runMachine p m = do
      case m^.mtype of
        Miner -> do
          when (m ^. amount < 100) $ do
            machines . at p . _Just . amount += 10
        _ -> return ()

      let a = m ^. amount
      mch <- use machines
      ns <- filter (\x -> mch M.! x ^. amount < a) <$> neighbors p
      let a' = (/ (fromIntegral $ length ns + 1)) $ (a +) $ sum $ fmap (\x -> mch M.! x ^. amount) ns
      forM_ ns $ \n -> do
        send p n $ a - (mch M.! n ^. amount)

liftO :: StateT a IO () -> Opaque a -> IO (Opaque a)
liftO m op = toOpaque <$> execStateT m (fromOpaque op)

main = do
  export (toJSString "initGame") initGame
  export (toJSString "tick") (liftO tick)
  export (toJSString "newMachine") (\x y -> liftO $ newMachine x y)
  export (toJSString "machinesOnScreen") machinesOnScreen
  export (toJSString "connects") exconnects
  export (toJSString "fromEnum") (return . fromEnum . head :: String -> IO Int)
  export (toJSString "minemap") ((\g x -> return $ M.lookup x (fromOpaque g ^. minemap)) :: Opaque Game -> (Int,Int) -> IO (Maybe (SKI,Double)))
  export (toJSString "getAmount") getAmount

  where
    exconnects :: Opaque Game -> (Int,Int) -> IO Bool
    exconnects g p = return $ elem p $ concat $ fmap (\(x,y) -> [x,y]) $ (fromOpaque g)^.connects

    getAmount :: Opaque Game -> (Int,Int) -> IO Double
    getAmount g p = return $ (fromOpaque g ^. machines) M.! p ^. amount
