import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>))

quicksort :: Seq.Seq Int -> Seq.Seq Int
quicksort xxs | Seq.null xxs = Seq.empty
quicksort xxs = (quicksort menores) >< (Seq.singleton x) >< (quicksort mayores)
    where menores = Seq.filter (<x) cola
          mayores = Seq.filter (>=x) cola
          x = Seq.index xxs 0 
          cola = Seq.drop 1 xxs
