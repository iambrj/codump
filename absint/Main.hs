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



initialState :: Store; initialState = bottom

interpretStmt :: Store -> Stmt -> Store
interpretStmt sto (SLabel _ ) = sto
interpretStmt sto (SSet v i) = insertLatMap v (Lifted i) sto
interpretStmt sto (SIf var t e) = 
  case lookupLatMap var sto of
    Lifted 0 ->  interpretBlock sto e
    Lifted _ ->  interpretBlock sto t
    -- _ -> error $ "unable to find variable: |" <> var <> "| in store: |" <> show sto <> "|"
    _ -> (interpretBlock sto t) `join` (interpretBlock sto e)
      

interpretBlock :: Store -> Block -> Store
interpretBlock sto (Block ss) = foldl interpretStmt sto  ss


f :: (Store, LatMap Label Store) -> Stmt -> (Store, LatMap Label Store)
f (sto, label2st) stmt =
  let sto' = interpretStmt sto stmt
  in case stmt of
         SLabel lbl -> (sto', insertLatMap lbl sto' label2st)
         _ -> (sto', label2st)


-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
interpretProgramCollecting :: Block -> LatMap Label Store
interpretProgramCollecting (Block ss) = 
  snd $ foldl f (initialState, bottom) ss


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
data Lifted a = LBot | LTop | Lifted a

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
  print (interpretBlock initialState p1)
  print (interpretProgramCollecting p1)
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
        elseBlock = Block $ [SSet "a" 42]


mainp2 :: IO ()
mainp2 = do
  print "P2:"
  print p2
  print (interpretProgramCollecting p2)
  print "===="

main :: IO ()
main = do 
  mainp1
  mainp2
