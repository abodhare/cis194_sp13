module Party where

import Employee

import Data.Monoid
import Data.Semigroup
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons x@Emp {empFun = f} (GL xs fs) = GL (x:xs) (f + fs)

instance Monoid GuestList where
    mempty = GL [] 0

instance Semigroup GuestList where
    (GL x fx) <> (GL y fy) = GL (x ++ y) (fx + fy)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> (a -> b) -> Tree a -> b
treeFold f g x = case x of
    Node {subForest = []} -> g (rootLabel x)
    _                     -> f (rootLabel x) (map (treeFold f g) (subForest x))

-- The first entry is when we invite the boss (employee at tree root)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel a x = (glCons a (mconcat (map snd x)), mconcat (map fst x))

maxFun :: Tree Employee -> GuestList
maxFun a = uncurry max $ treeFold nextLevel (\x -> (GL [x] (empFun x), mempty)) a

guestListRepr :: GuestList -> String
guestListRepr (GL x f) = unlines (("Total fun: " ++ show f) : sort (map empName x))

main :: IO ()
main = readFile "company.txt" >>= (putStrLn . guestListRepr . maxFun . read)
