module StringGraph (
    StringGraph,
    getGraph,
    getStrToIndex,
    lookupIndex,
    neighbours,
    buildStringGraph
) where

import Data.Maybe (fromMaybe)

data StringGraph = StringGraph { graph :: [(Int, [Int])], strToIndex :: [(String, Int)] } deriving (Show, Eq)
getGraph :: StringGraph -> [(Int, [Int])]
getGraph = graph
getStrToIndex :: StringGraph -> [(String, Int)]
getStrToIndex = strToIndex

lookupIndex :: StringGraph -> String -> Int
lookupIndex graph str = fromMaybe (error "String not found") (lookup str (strToIndex graph))

neighbours :: StringGraph -> Int -> [Int]
neighbours graph idx = fromMaybe (error "Index not found") (lookup idx (getGraph graph))

addNode :: StringGraph -> String -> (StringGraph, Int)
addNode graph str =
    case lookup str (strToIndex graph) of
        Just idx -> (graph, idx)
        Nothing  ->
            let newIndex = length (getGraph graph)
                newNodes = getGraph graph ++ [(newIndex, [])]
                newStrToIndex = (str, newIndex) : getStrToIndex graph
            in (StringGraph { graph = newNodes, strToIndex = newStrToIndex }, newIndex)

addEdge :: StringGraph -> Int -> Int -> StringGraph
addEdge graph from to =
    let updatedGraph =
            map (\(idx, edges) -> if idx == from then (idx, to : edges) else (idx, edges))
            (getGraph graph)
    in graph { graph = updatedGraph, strToIndex = getStrToIndex graph }

buildStringGraph :: [(String, String)] -> StringGraph
buildStringGraph edgesList =
    let initialGraph = StringGraph { graph = [], strToIndex = [] }
        finalGraph = foldl go initialGraph edgesList
        go g (str, neighbour) =
            let (g', fromIdx) = addNode g str
                (g'', toIdx) = addNode g' neighbour
            in addEdge g'' fromIdx toIdx
    in finalGraph
