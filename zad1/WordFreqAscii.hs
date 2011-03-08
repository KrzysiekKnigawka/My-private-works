import Dict
import List
import System (getArgs, getProgName)
import System.IO


main :: IO()
main = do
    args <- getArgs
    case args of
        [file] -> processWord file

-- read file and print results from freq function
processWord :: String -> IO()
processWord filename = do
        fContent <- readFile filename
        mapM_ (\(word,fr) -> putStrLn $ word ++ ' ':(show fr)) $ freq fContent

-- make word statistics from given text, uses sortBy and compareTup function
freq :: String -> [(String, Int)]
freq s = (take 10).sortBy compareTup $ processList [(x, 1) | x<-words s, length x > 2]    

-- method for sorting list of tuples (key, value)
compareTup :: (Ord v) => (k, v) -> (k, v) -> Ordering
compareTup (k1,v1) (k2, v2) | v1 > v2 = LT
                            | otherwise = GT

-- take pattern and list, return list without key equal to pattern
withNo :: String -> [(String, Int)] -> [(String, Int)]
withNo kN list = [(x, y) | (x, y) <- list, x /= kN]

getSucc :: (Maybe Int) -> Int 
getSucc Nothing = 1 
getSucc (Just a) = succ a

-- take list and return list without repetition of keys, increasing count (second parameter of tuple) by one on each occourence of key
processList :: [(String, Int)] -> [(String, Int)]
processList list =  let myInsert dict (k,v) = Dict.insert k (getSucc (Dict.lookup k dict)) dict in 
                    toList $ foldl myInsert (fromList []) list 

