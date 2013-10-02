import qualified Data.Sequence as Seq
-- 1
quicksort :: Seq.Seq Int -> Seq.Seq Int
quicksort empty = empty
quicksort (singleton x) = singleton x
--quicksort (x:xs) = (quicksort menores) ++ [x] ++ (quicksort mayores)
--    where menores = filter (<x) xs
--          mayores = filter (>=x) xs
