import qualified Data.HashSet as H
import qualified Data.Sequence as S
import Data.Hashable

bfs :: (Ord a, Hashable a) => (a -> [a]) -> (a -> Bool) -> a -> Maybe a
bfs reachable_nodes predicate start_node = bfs_helper (S.singleton start_node) H.empty
    where bfs_helper queue visited_set
            | S.null queue = Nothing
            | predicate node = Just node
            | otherwise =
                let new_nodes = filter (`H.notMember` new_visited_set) $ reachable_nodes node
                    new_queue = (S.drop 1 queue) S.>< S.fromList new_nodes
                    new_visited_set = H.insert node visited_set
                in bfs_helper new_queue new_visited_set
            where node = queue `S.index` 0
