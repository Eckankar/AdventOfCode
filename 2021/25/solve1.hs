import qualified Data.Map as M
import qualified GHC.Arr as A

import Data.Maybe (isNothing)

add h w (x, y) (x', y') = ((x+x') `mod` h, (y+y') `mod` w)

moveCucumbers h w s d m = M.foldlWithKey moveCucumber m $ M.filter (== s) m
    where moveCucumber m' p _ =
            if isNothing $ M.lookup p' m then m'' else m'
            where p' = add h w p d
                  m'' = M.insert p' s $ M.delete p m'

countFix f m = if m == m' then 1 else 1 + countFix f m'
    where m' = f m

main :: IO ()
main = do
    m <- fmap lines getContents
    let (height, width) = (length m, length $ head m)
    let inputMap = M.filter (/= '.') $ M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ concat m
    let mc = moveCucumbers height width
    print $ countFix (mc 'v' (1, 0) . mc '>' (0, 1)) inputMap
