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
instance Show Stmt where
  show (SSet v i) = v <> " = " <> show i
  show (SLabel l) = l <> ":"

data Block = Block [Stmt]  deriving(Eq)

type Store = M.Map Var Value
initialState :: Store; initialState = M.empty

interpretStmt :: Store -> Stmt -> Store
interpretStmt st (SLabel _ ) = st
interpretStmt st (SSet v i) = M.insert v i st

interpretBlock :: Block -> Store
interpretBlock (Block ss) = foldl interpretStmt initialState  ss


f :: (Store, M.Map Label Store) -> Stmt -> (Store, M.Map Label Store)
f (st, label2st) stmt =
  let st' = interpretStmt st stmt
  in case stmt of
         SSet _ _ -> (st', label2st)
         SLabel lbl -> (st', M.insert lbl st' label2st)


-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
interpretProgramCollecting :: Block -> M.Map Label Store
interpretProgramCollecting (Block ss) = 
  snd $ foldl f (initialState, M.empty) ss
            

instance Show Block where
  show (Block ss) = intercalate "\n" (map show ss)
-- at each label, provide the Store at the label.
-- data Collecting = Collecting [(Label, [(Var, Value)]]

p1 :: Block
p1 = Block $ [SLabel "start", SSet "a" 10, SLabel "1", SSet "a" 20, SLabel "2"]


main :: IO ()
main = do
  print p1
  print (interpretBlock p1)
  print (interpretProgramCollecting p1)
