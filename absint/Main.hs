{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import qualified Data.Map.Strict as M
import Data.List(intercalate)
import Data.Monoid


-- implement an interpreter that implements the "real" semantics
-- show how to abstract "real" semantics into "faster" abstract semantics


type Value = Int
type Label = String
type Var = String

data Stmt = SSet Var Int | SLabel Label | SIf Var Block Block deriving (Eq)

showIndent :: Int -> String; showIndent i = replicate i ' '

showStmt :: Int -> Stmt -> String
showStmt indent (SSet v i) = showIndent indent <> v <> " = " <> show i
showStmt indent (SLabel l) = showIndent indent <> l <> ":"
showStmt indent (SIf v t e) = 
  showIndent indent <> "if (" <> v <> ")" <>  
  "\n" <> showIndent indent <> "then:\n" <> showBlock (indent+1) t <>
  "\n" <> showIndent indent <> "else:\n" <> showBlock (indent+1) e

instance Show Stmt where
  show s = showStmt 0 s

showBlock :: Int -> Block -> String
showBlock indent (Block ss)  = intercalate "\n" (map (showStmt (indent+1)) ss)

instance Show Block where
  show block = showBlock 0 block

data Block = Block [Stmt]  deriving(Eq)

-- my Store / intermediate "states" ought to be a JOIN SEMILATTICE
-- bottom element
-- union operation

type Store = LatMap Var (Lifted Value) -- key -> value

  


-- | given a variable and a value, store this value into the store.
-- instance AbstractStore M Int

class (JoinSemilattice s, JoinSemilattice a, Eq a) => AbstractStore s a | s -> a, a -> s where
   insertAbstractStore :: Var -> Value -> s -> s
   lookupAbstractStore :: Var -> s -> a
   injectValue :: Value -> a
  -- TODO: do we use ==? or something weaker to compare real abstract values and
  -- injected values?
  -- magic word: Filters (set theoretic sense)

instance AbstractStore (LatMap Var (Lifted Value)) (Lifted Value) where
   insertAbstractStore var val s = insertLatMap var (Lifted val) s
   lookupAbstractStore var s = lookupLatMap var s
   injectValue = Lifted
  
initialState :: AbstractStore s a => s; initialState = bottom
interpretStmt :: AbstractStore s a => s -> Stmt -> s
interpretStmt sto (SLabel _ ) = sto
interpretStmt sto (SSet v i) = insertAbstractStore v i sto
interpretStmt sto (SIf var t e) = 
  let varValue =  lookupAbstractStore var sto
  in if varValue == bottom
     then (interpretBlock sto t) `join` (interpretBlock sto e)
     else if varValue == injectValue 0
          then interpretBlock sto e
          else interpretBlock sto t
      

interpretBlock :: AbstractStore s a => s -> Block -> s
interpretBlock sto (Block ss) = foldl interpretStmt sto  ss


f :: AbstractStore s a => (s, M.Map Label s) -> Stmt -> (s, M.Map Label s)
f (sto, label2st) stmt =
  let sto' = interpretStmt sto stmt
  in case stmt of
         SLabel lbl -> (sto', M.insert lbl sto' label2st)
         _ -> (sto', label2st)


-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
interpretProgramCollecting :: AbstractStore s a => Block -> M.Map Label s
interpretProgramCollecting (Block ss) = 
  snd $ foldl f (initialState, M.empty) ss


-- All /information/ ought to be join semilattice.
-- | join is commutative and associative.
-- | bottom is identity of join.
-- | join is idempotent.
-- | Commutative: a `join` b = b `join` a
-- | Assoc: a `join` (b `join` c) = (a `join` b) `join` c
-- | Idemp: a `join` a = a
-- | Id: a `join` bottom = bottom `join` a = a
-- | Join semilattice ~= idempotent commutative monoid
class JoinSemilattice a where
  bottom :: a
  join :: a -> a -> a

-- | join semilattice
data Lifted a = LBot | LTop | Lifted a deriving(Eq)

instance Show a => Show (Lifted a) where 
  show LBot = "⊥"
  show LTop = "T"
  show (Lifted a) = "Lifted(" <> show a <> ")"

instance (Eq a) => JoinSemilattice (Lifted a) where
  bottom = LBot 
  join LBot x = x
  join x LBot = x
  join (Lifted a) (Lifted a') = if a == a' then (Lifted a) else LTop
  join LTop x = LTop
  join x LTop = LTop


instance JoinSemilattice a => JoinSemilattice (Maybe a) where
  bottom = Nothing
  join Nothing x = x
  join x Nothing = x
  join (Just x) (Just x') = Just (x `join` x')
  


newtype LatMap k v = LatMap (M.Map k v) deriving(Eq, Show)
lookupLatMap :: (Ord k, JoinSemilattice v) => k -> LatMap k v -> v
lookupLatMap k (LatMap k2v) = 
  case M.lookup k k2v of
      Just v -> v
      Nothing -> bottom

-- unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]
joinLatMap :: (Ord k, JoinSemilattice v) => LatMap k v -> LatMap k v -> LatMap k v
joinLatMap (LatMap m1) (LatMap m2) = LatMap $ (M.unionWith join) m1 m2

-- [(k, v)] \/ m
insertLatMap :: (Ord k, JoinSemilattice v) => k -> v 
  -> LatMap k v -> LatMap k v
insertLatMap k v (LatMap m) = LatMap (M.insert k v m)
   -- (LatMap (M.singleton k v)) `join` lm

-- [v is a join semilattice] => 
--    function space [k -> v] is also a join semilattice
-- using this for Show instance
instance (Ord k, JoinSemilattice v) => JoinSemilattice (LatMap k v) where
  bottom = LatMap M.empty -- (\k -> bot)
  join = joinLatMap -- f \/ g = (\k -> f k \/ g k)

-- | actual structure
instance JoinSemilattice v => JoinSemilattice (k -> v) where
  bottom = \_ -> bottom
  join f g = \k -> (f k) `join` (g k)

-- at each label, provide the Store at the label.
-- data Collecting = Collecting [(Label, [(Var, Value)]]

p1 :: Block
p1 = Block $ [SLabel "start", SSet "a" 10, SLabel "1", SSet "a" 20, SLabel "2"]

mainp1 :: IO ()
mainp1 = do
  print "P1:"
  print p1
  -- print (interpretBlock initialState p1)
  print (interpretProgramCollecting p1 :: M.Map Label (LatMap Var (Lifted Value)))
  print "===="

p2 :: Block
p2 = 
  Block $ 
   [SLabel "start", 
    SSet "a" 10, 
    SLabel "1", 
    SSet "a" 20, SLabel "2",
    SIf "b" thenBlock elseBlock,
    SLabel "3"]
  where thenBlock = Block $ [SSet "a" 42]
        elseBlock = Block $ [SSet "a" 43]


mainp2 :: IO ()
mainp2 = do
  print "P2:"
  print p2
  print (interpretProgramCollecting p2 :: M.Map Label (LatMap Var (Lifted Value)))
  print "===="

main :: IO ()
main = do 
  mainp1
  mainp2
