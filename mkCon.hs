main = putStr $ unlines $ concat $ map posLock (a++b)++map ortLock b where
	a = [(3004,1472),(281,1468),(3002,1365),(288,1369)]
	b = [(1,522),(8,521),(29,390),(36,389)]
