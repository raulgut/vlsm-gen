{-# LANGUAGE DeriveFoldable #-}
-----------------------------------------------------------------------------
-- | VLSM Generator - Main
-- 
-- Parameters: 
-- 1) IP address
-- 2) Mask
-- 3) Strategy: 
--      * MINIMUM: Allocate in order but using minimum space
--      * SEQUENTIAL: Allocate in order, addresses are sequential 
-- 4) Network sizes
-----------------------------------------------------------------------------
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Map (fromList)
import Data.List (intersperse)
import Control.Monad.State (State (..), get, put, execState)
import Data.Maybe (isJust)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Possible NodeTrees
data NodeTree
  = NodeZero 
  | NodeOne
  | NodeUsed
  | NodeTop 
  deriving (Eq, Ord)

-- | Binary Tree
data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            deriving Foldable

-- | Strategy
data Strategy = MINIMUM | SEQUENTIAL
              deriving (Eq, Show, Read)

-- | Configuration
data Configuration a
  = Configuration { ispAddress :: [String]
                  , ispMask :: Int
                  , strategy :: Strategy
                  , addressSpace :: Tree a
                  , networks :: [([a],Int)]
                  }
                  deriving Show

-- | Network Info
data NetworkInfo
  = NetworkInfo { networkIP :: [Int] 
                , networkMask :: Int 
                }

-----------------------------------------------------------------------------
-- Classes
-----------------------------------------------------------------------------

-- | Node Tree Class
class IsNodeTree a where
  nodeTop :: a
  nodeZero :: a 
  nodeOne :: a 
  nodeUsed :: a 
  isNodeTop :: a -> Bool 
  isNodeZero :: a -> Bool 
  isNodeOne :: a -> Bool
  isNodeUsed :: a -> Bool

-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------

-- IsNodeTree

instance IsNodeTree NodeTree where 
  nodeTop = NodeTop
  nodeZero = NodeZero
  nodeOne = NodeOne 
  nodeUsed = NodeUsed 
  isNodeTop n = n == nodeTop
  isNodeZero n = n == nodeZero
  isNodeOne n = n == nodeOne 
  isNodeUsed n = n == nodeUsed 

-- Show

instance Show NodeTree where 
  show NodeTop = "/\\"
  show NodeZero = "0"
  show NodeOne = "1"
  show NodeUsed = "*"

{--
Case 30:

Node /\ (Node 0 (Leaf 0) (Leaf 1)) (Node 1 (Leaf 0) (Leaf 1))

" - - " 
"| |0|"
"|0 - "
"| |1|"
" - - " 
"| |0|"
"|1 - "
"| |1|"
" - - " 
--}
instance (Show a, IsNodeTree a) => Show (Tree a) where 
--  show t = ""    
  show t
    = let dpth = depthTree t
          sep = (concat $ replicate dpth " -") ++ "\n"
      in sep ++ (concat . intersperse "\n" . showList dpth $ t) ++ "\n" ++ sep
    where
      showList _ (Leaf l) = if isNodeTop l then [" - \n| |\n -"] else ["|" ++ show l ++ "|"]
      showList dpth t@(Node n ltree rtree)  
        = let sep = concat $ replicate dpth " -" 
              topStr = showList (dpth - 1) $ ltree
              bottomStr = showList (dpth - 1) $ rtree
          in if isNodeTop n then
              topStr
              ++ [sep] 
              ++ bottomStr
             else 
              (map ("| " ++) topStr)
              ++ ["|" ++ show n ++ sep] 
              ++ (map ("| " ++) bottomStr)

instance Show NetworkInfo where
  show (NetworkInfo ip mask)
    = "\nIP Address: " ++ (toIP ip) ++ "/" ++ (show mask)
      ++ "\n -> First Device: " ++ (toIP (common ++ (init machines ++ [1])))
      ++ "\n -> Last Device: " ++ (toIP (common ++ (init broadcast ++ [0])))
      ++ "\n -> Broadcast: " ++ (toIP (common ++ broadcast))
    where toInt acc [] = acc
          toInt acc [x] = (acc * 2) + x
          toInt acc (x:xs) = toInt ((acc * 2) + x) xs
          (common,machines) = splitAt mask ip
          broadcast = replicate (length machines) 1
          toIP inputIP = let (byte1, rest1) = splitAt 8 inputIP
                             (byte2, rest2) = splitAt 8 rest1
                             (byte3, byte4) = splitAt 8 rest2
                         in (show . toInt 0 $ byte1) ++ "."
                            ++ (show . toInt 0 $ byte2) ++ "."
                            ++ (show . toInt 0 $ byte3) ++ "."
                            ++ (show . toInt 0 $ byte4) 

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

-- | createTree generates a binary tree with all the possibilities
createTree :: (IsNodeTree a) => String -> Tree a
createTree [] = Leaf nodeTop
createTree ("0") = Node nodeTop (Leaf nodeZero) (Leaf nodeOne)
createTree ('1':mask) = createTree mask
createTree ('0':mask) 
  = case createTree mask of
      Leaf _ -> error $ "Error: there is a non-zero element on the mask " ++ ('0':mask) 
      Node _ ntree rtree -> Node nodeTop (Node nodeZero ntree rtree) (Node nodeOne ntree rtree)

-- | depthTree returns the maximum depth of the tree
depthTree :: Tree a -> Int
depthTree (Leaf _) = 0
depthTree (Node _ ltree rtree) = (max (depthTree ltree) (depthTree rtree)) + 1

-- | findAllocation finds an empty spot in then addressSpace
findAllocation :: (IsNodeTree a, Show a) => Tree a -> Int -> Maybe [a]
findAllocation (Leaf l) 0 = if isNodeUsed $ l then 
                                Nothing 
                              else 
                                Just []
findAllocation (Leaf l) _ = error $ "Error: subnetwork address is too long" 
findAllocation tree 0
  = if foldr (\x y -> isNodeUsed x || y) False tree then
      Nothing
    else 
      Just []
findAllocation (Node n ltree rtree) 1
  | isNodeTop n
      = case findAllocation ltree 1 of
            Just lalloc -> Just lalloc
            Nothing -> case findAllocation rtree 1 of 
                         Just ralloc -> Just ralloc 
                         Nothing -> Nothing
  | isNodeUsed n
      = Nothing
  | otherwise
      = if (isJust $ findAllocation ltree 0) && (isJust $ findAllocation rtree 0) then
          Just [n]
        else 
          Nothing
findAllocation (Node n ltree rtree) nAddr
  | isNodeTop n
      = case findAllocation ltree nAddr of
            Just lalloc -> Just lalloc
            Nothing -> case findAllocation rtree nAddr of 
                         Just ralloc -> Just ralloc 
                         Nothing -> Nothing
  | isNodeUsed n
      = Nothing
  | otherwise
      = case findAllocation ltree (nAddr - 1) of
            Just lalloc -> Just $ n:lalloc
            Nothing -> case findAllocation rtree (nAddr - 1) of 
                         Just ralloc -> Just $ n:ralloc
                         Nothing -> Nothing

-- | updateTree marks the machine nodes of the network as used
updateTree :: (IsNodeTree a, Eq a, Ord a) => Strategy -> [a] -> Tree a -> Tree a
updateTree _ [] (Leaf l) = if isNodeTop l then
                           Leaf l 
                         else 
                           Leaf nodeUsed
updateTree strat [] (Node n ltree rtree) 
  = if isNodeTop n then 
      Node n (updateTree strat [] ltree) (updateTree strat [] rtree)
    else
      Node nodeUsed (updateTree strat [] ltree) (updateTree strat [] rtree)
updateTree strat (x:xs) (Leaf l) 
  = Leaf l
updateTree strat (x:xs) tree@(Node n ltree rtree) 
  = if n == x then 
      Node n (updateTree strat xs ltree) (updateTree strat xs rtree)
    else
      if (strat == SEQUENTIAL) && (n < x) then
        Node nodeUsed (updateTree strat [] ltree) (updateTree strat [] rtree)
      else 
        if isNodeTop n then -- nodeTops do not consume the chain
          Node n (updateTree strat (x:xs) ltree) (updateTree strat (x:xs) rtree)
        else 
          tree

-- | allocateNetworks updates the addressSpace
allocateNetworks :: (IsNodeTree a, Eq a, Ord a, Show a) => [Int] -> State (Configuration a) ()
allocateNetworks [] = return ()
allocateNetworks (e:es) 
  = do { let maskSize = ceiling . logBase 2 . fromIntegral $ e
       ; conf <- get
       ; let nAddrSize = 32 - (ispMask conf) - maskSize
       ; if nAddrSize < 1 then 
           error $ "Error: The nework of size " ++ (show e) ++ " is too big!" 
         else 
           do let actualAddressSpace = addressSpace conf
              let option = findAllocation actualAddressSpace nAddrSize
              --- update configuration
              case option of
                Just validOption
                  -> do let newAddressSpace = updateTree (strategy conf) validOption actualAddressSpace 
                        put conf { addressSpace = newAddressSpace 
                                , networks = networks conf ++ [(validOption, nAddrSize)]}
                        allocateNetworks es
                Nothing -> error $ "Error: The nework of size " ++ (show e) ++ " cannot be allocated"
       }

-- | getNetworkInfo translate the internal info into a NetworkInfo structure
getNetworkInfo :: (Show a) => [String] -> Int -> ([a],Int) -> NetworkInfo
getNetworkInfo ipAddr mask (subnet,subnetSize)
  = let commonNewtworkPart = take mask . concat . map ((\xs -> replicate (8 - length xs) 0 ++ xs) . toBinary) . map read $ ipAddr
        newNetworkPart = map (read . show) $ subnet
        rest = (replicate (32 - mask - subnetSize) 0)
    in NetworkInfo { networkIP = commonNewtworkPart ++ newNetworkPart ++ rest
                   , networkMask = mask + subnetSize}
  where toBinary 0 = []
        toBinary n = let (d,m) = divMod n 2 in (toBinary d) ++ [m]

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (ip:msk:strat:entries) = (map (filter (/= '\r')) . lines $ input) :: [String]
  let ipaddress = splitOn "." $ ip 
  let mask = let mlen = read msk 
             in (replicate mlen '1') ++ (replicate (32 - mlen) '0')
  let hosts = (map read entries) :: [Int]
  putStrLn $ "\nParameters:" 
               ++ "\n -> ISP Network Address: " ++ ip ++ "/" ++ msk 
               ++ "\n -> Subnets (sizes): " ++ (concat . intersperse ", " $ entries)
  let maskTree = (createTree mask :: Tree NodeTree)
  let allocation = execState (allocateNetworks (map read entries)) (Configuration ipaddress (read msk) (read strat) maskTree [])
  putStrLn $ "\nSolution:"
  putStrLn . concat . intersperse "\n" . map show $ map (getNetworkInfo (ispAddress allocation) (ispMask allocation)) (networks allocation)
