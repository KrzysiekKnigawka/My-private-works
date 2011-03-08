module Dict
( Dict
, insert
, Dict.lookup
, fromList
, toList
) where

data Dict key value = Leaf | Node key value (Dict key value) (Dict key value) deriving (Eq)

insert :: (Ord key) => key -> value -> Dict key value -> Dict key value
insert key value Leaf = Node key value Leaf Leaf
insert key value (Node k v lson rson) = 
	if key<k then Node k v (insert key value lson) rson
	else if key==k then Node key value lson rson
	else Node k v lson (insert key value rson)

continue :: Maybe c -> Maybe c -> Maybe c 
continue r Nothing = r
continue r c = c

lookup :: (Ord key) => key -> Dict key value -> Maybe value
lookup key Leaf = Nothing
lookup key (Node k v lson rson)   | key == k = Just v
                            	  | key < k = Dict.lookup key lson 
                                  | otherwise = Dict.lookup key rson

fromList :: Ord key => [(key ,value)] -> Dict key value
fromList [] = Leaf
fromList ((k, v):xs) = insert k v (fromList xs)
--
toList :: Dict key value -> [(key, value)]
toList Leaf = []
toList (Node k v lson rson) = toList rson ++ [(k,v)] ++ toList lson

