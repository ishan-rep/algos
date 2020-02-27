import Prelude
import qualified Data.Map as DM
import Data.Maybe
import Data.List
import qualified Data.List as L
import Control.Monad

data GNode =
    GNode {
        vertex :: Int
      , connectedTo :: [Int]
    } deriving (Show)

ifOneCharDiff :: Int -> String -> String -> Bool
ifOneCharDiff i [] [] = i == 1
ifOneCharDiff 0 (c:cs) (s:ss) = if c == s then ifOneCharDiff 0 cs ss
                                  else ifOneCharDiff 1 cs ss
ifOneCharDiff 1 (c:cs) (s:ss) = c == s && ifOneCharDiff 1 cs ss
ifOneCharDiff _ _ _ = False

getPosition :: [String] -> String -> Int
getPosition wordList word =
    fromMaybe (-1) (elemIndex word wordList)

makeGraphNode :: [String] -> String -> GNode
makeGraphNode wordList nodeString = do
    let posStr = fromMaybe 0 (elemIndex nodeString wordList)
        connectedStrings = filter (ifOneCharDiff 0 nodeString) wordList
        connectedNodes = map (getPosition wordList) connectedStrings
    GNode {
        vertex = posStr
      , connectedTo = connectedNodes
    }

updateList :: Int -> Int -> [Int] -> [Int]
updateList index value valueList = do
    let (xs,_:ys) = splitAt index valueList
    xs ++ value : ys

findNewMinDistanceList :: [Int] -> Int -> [Int] -> [Int]
findNewMinDistanceList minPathList _ [] = minPathList
findNewMinDistanceList minPathList fromVertex (toVertex:connectedTos) = do
    let nextDist = (minPathList !! fromVertex) + 1
    if (minPathList !! toVertex) < nextDist
        then findNewMinDistanceList minPathList fromVertex connectedTos
        else do
            let newMinPathList = updateList toVertex nextDist minPathList
            findNewMinDistanceList newMinPathList fromVertex connectedTos

findMinPaths :: GNode -> [GNode] -> [Int] -> [Int]
findMinPaths startNode graph minPathList = do
    let connectedTos = connectedTo startNode
        startVertex = vertex startNode
        newMinPathList = findNewMinDistanceList minPathList startVertex connectedTos
    newMinPathList


minimumDistancePath :: [GNode] -> [GNode] -> [Int] -> [Int] -> ([Int], [Int])
minimumDistancePath [startNode] graph visited minPathList
  | (length visited == length graph) || (vertex startNode `elem` visited) = (visited, minPathList)
  | vertex startNode `notElem` visited = do
    let minPathList' = findMinPaths startNode graph minPathList
    minimumDistancePath (map (graph !!) (connectedTo startNode)) graph (vertex startNode : visited) minPathList'
  | otherwise = minimumDistancePath (map (graph !!) (connectedTo startNode)) graph (vertex startNode : visited) minPathList
minimumDistancePath (startNode:restNodes) graph visited minPathList = do
    let (visited', minPathList') = minimumDistancePath [startNode] graph visited minPathList
    minimumDistancePath restNodes graph visited' minPathList'

createSolutionListOfStrings :: [String] -> [GNode] -> [Int] -> [Int] -> [String]
createSolutionListOfStrings wordList graph _ [0] = [head wordList]
createSolutionListOfStrings wordList graph minDistList [toVertex] = do
    --Find connectedTos for toNode and then get all the wordNodes in this list having value 1 less than toNode minDist
    let toNode = graph !! toVertex
        connectedTos = connectedTo toNode
        nextLesserDist = (minDistList !! toVertex) - 1
        fromNodes = L.elemIndices nextLesserDist minDistList `intersect` connectedTos
    map (\x -> x ++ " -> " ++ (wordList !! toVertex)) (createSolutionListOfStrings wordList graph minDistList fromNodes)
createSolutionListOfStrings wordList graph minDistList (toVertex:restVertex) = do
    let firstSolns = createSolutionListOfStrings wordList graph minDistList [toVertex]
    firstSolns ++ createSolutionListOfStrings wordList graph minDistList restVertex

main :: IO ()
main = do
    noOfWordsString <- getLine 
    let noOfWords = read noOfWordsString :: Int
    -- let wordList = ["hit","hot","dot","dog","lot","log","cog"]
    wordList <- replicateM noOfWords getLine
    -- wordList <- read
    let graph = map (makeGraphNode wordList) wordList
        len = L.length graph
        minPathListInit = 0 : replicate (len-1) 100000
        visited = []
        (visited', minDistList) = minimumDistancePath [head graph] graph visited minPathListInit
        sols = createSolutionListOfStrings wordList graph minDistList [vertex (last graph)]
    print sols