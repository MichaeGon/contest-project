import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = undefined 

solve :: Int
solve = undefined

readLine :: IO [Int]
readLine = unfoldr (fmap (second (maybe B.empty snd . B.uncons)) . B.readInt) <$> B.getLine

readContents :: Int -> IO [[Int]]
readContents = flip replicateM readLine