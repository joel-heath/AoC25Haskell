module Graph (
    Coord(..),
    getX,
    getY,
    getZ,
    toCoord,
    Graph(..),
    connect,
    connectUndirected,
    buildGraph
) where

data Coord = Coord { x :: Int, y :: Int, z :: Int } deriving (Eq, Show, Ord)
getX, getY, getZ :: Coord -> Int
getX = x
getY = y
getZ = z

toCoord :: [Int] -> Coord
toCoord [x, y, z] = Coord { x = x, y = y, z = z }


data Graph = Graph { nodes :: [Coord], edges :: [(Coord, [Coord])] }

connect :: Graph -> Coord -> Coord -> Graph
connect graph a b =
    let updateAdj c1 c2 g =
            case lookup c1 g of
                Just adj -> (c1, c2 : adj) : filter ((/= c1) . fst) g
                Nothing  -> (c1, [c2]) : g
        newEdges = updateAdj a b (edges graph)
        newNodes = nodes graph ++ [a | a `notElem` nodes graph] ++ [b | b `notElem` nodes graph]
    in Graph { nodes = newNodes, edges = newEdges }

connectUndirected :: Graph -> Coord -> Coord -> Graph
connectUndirected graph a b = connect (connect graph a b) b a


buildGraph :: [(Coord, Coord)] -> Graph
buildGraph conns =
    let addConn (a, b) graph = connectUndirected graph a b
    in foldr addConn (Graph { nodes = [], edges = [] }) conns