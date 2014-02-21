import Data.List
import Data.List.Split
main = do
	[legs,arms] <- sequence $ map readFile ["1002.txt","lvliang.txt"]
	putStr $ unlines $ map (\l->untokens (g (length $ filter (isPrefixOf "l,") $ lines legs) $ tokens l)) (lines arms)
--	putStr $ unlines $ map (\l->if "lsel" `isPrefixOf` l then (f l (\s->show $ read s+(length $ filter (isPrefixOf "l,") $ lines legs))) else l) (lines arms)
--	putStr $ unlines $ map (\l->if "lsel" `isPrefixOf` l then (f l (\s->"***")) else l) (lines arms)
	where 
	tokens = splitOn ","
	untokens = concat.intersperse ","
	g n (op:ts) = op:case op of
		"k"-> f 1 41000
		"lsel"-> f 2 n
		_->ts
		--where f m off = (\(xs,ys)->map (show.(+off).read) xs++ys) $ splitAt m ts
		where f m off = reverse $ (\(xs,ys)->map (show.(+off).read) xs++ys) $ splitAt m $ reverse ts
