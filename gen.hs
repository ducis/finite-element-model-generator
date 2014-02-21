import Text.Printf
import Data.Array

data Sec = I400|I280|I160|T125|T90|T75|LL75|C160|L75|I200|T80|T125W
secID::Sec->Int
secID = f where
	f I400 = 1
	f I280 = 2
	f I160 = 3
	f T125 = 4
	f T90 = 5
	f T75 = 6
	f LL75 = 7
	f C160 = 8
	f L75 = 9
	f I200 = 10
	f T80 = 11
	f T125W = 12
secColorID::Sec->Int
secColorID = f where
	f I400 = 1
	f I280 = 2
	f I160 = 3
	f T125 = 4
	f T90 = 5
	f T75 = 6
	f LL75 = 7
	f C160 = 8
	f L75 = 9
	f I200 = 10
	f T80 = 11
	f T125W = 12
type Model = ([[Double]],[(Int,Int,Int,Sec)])	
main = readFile "header" >>= (\h->putStr (h++unlines code))
nE = 280::Int
code = ("! Define Element Type":elemTypeDefs nE) 
	++ ("! Model":((\(ns,es)->zipWith ndStmt [1..] ns++zipWith elStmt [1..] es) mainBeam))
ndStmt id [x,y,z] = printf "N,%d,%f,%f,%f,0,0,0" (id::Int) x y z
elStmt id (i,j,k,t) = printf "SECNUM,%d\nEN,%d,%d,%d,%d\n/COLOR,ELEM,%d,%d" (secID t) (id::Int) (i+1) (j+1) (k+1) (secColorID t) id
elemTypeDefs n = map (printf "ET,%d,BEAM44") [1..n]
mainBeam::Model
mainBeam = foldl1 combine $ extrude os segs
	where
	extrude zs = zipWith (\z s->offset [0,0,z] s) zs
	os = [0,2500]++[5000+i*sl|i<-[0..]]::[Double]
	segs = zipWith mainBeamSeg (map (\w->[x0,y0,w]) (p 11 325++p 5 200++p 4 0)) (cycle [1,0]) ++ [neck,skull]
	p n x = take n (repeat x)
	x0 = 1250
	y0 = 3000
	x1 = 750
	y1 = 1500
	sl = 2000::Double
	(n,n') = mainSliceTopo
	neck = (mainSliceNs [x0,y0,0],es) where
		es = dbl (mvIJK n') [(0,6,-n,I200),(6,1,-n,I200)] ++ dbl (mvIJ 1) [(0,n',2,LL75)]
	skull = (ns++ns'::[[Double]],es) where
		--ns = []::[[Double]]
		ns = concat $ zipWith (\z s->offset' [0,0,z] s) [i*sl|i<-[0..]] (map mainSliceNs szs) :: [[Double]]
		nns = length ns
		ns' = [	a,
			m a,
			lerpV (f (sx 0) 500) [0,sy 0,0] [0,y0,-sl],
			cz 0,
			cz 2,
			cz l,
			b,
			m b,
			c,
			m c
			] where 
			m = (\(x:yz)->(-x:yz))
			l = 4::Int
			a = lerpV (f (sy 0) y0) [sx 0,sy 0,0] [x0,0,-sl]
			sx i = szs!!i!!0
			sy i = szs!!i!!1
			cz i = [0,sy i/2,fromIntegral i*sl]
			b = lerpV (f (sy l) (sy l')) [sx l,sy l,fromIntegral l*sl] [sx l',0,fromIntegral l'*sl] where l'=l-1
			c = [500,y0,-sl]
		f x y = x/(x+y)
		szs = map (\s->[x0+(x1-x0)*s,y0+(y1-y0)*s,0]) [i/5|i<-[1..6]]
		pat5 = concat.take 5.iterate (map $ mvIJK n)
		es = 	pat5 ( dbl (mvIJK n') [(0,1,n,T80),(0,-n,1,T125W),(1,1-n,0,T125W)]++[(1,n'+1,n+1,T80),(0,n',1,T125W)] ) ++
			dbl (mvIJK n') [(1,n,0,T80),(n,2*n+1,2*n,T80),(2*n+1,3*n,2*n,T80)] ++ [(0,-n+n',1,T125W)] ++
			concat ( map (\(i,j,k)->[(i,j,i+1,T125W),(i+1,j+1,k+1,T80)]) $ dbl (\(i,j,k)->(i+2*n,j+2*n,k+2*n)) [(0,n+n',n),(n+n',2*n,n)]) ++
			map (\[i,j,k]->(i,j,k,T80)) (
				dbl (map (\i->if i>=nns then i+1 else i+n')) [
					[0,nns,-n],[nns,-n+1,-n],[1,nns,0],[nns,-n,0],
					[3*n,nns+6,4*n],[nns+6,4*n+1,4*n],[3*n+1,nns+6,3*n],[nns+6,4*n,3*n]
				]++
				[[1,nns+2,1+n'],[nns+2,nns+9,1+n'],[1+n',nns+2,1+n'-n],[nns+2,nns+8,1+n'-n]] ++
				(concat $ take 3 $ iterate (map (map (\i->if i>=nns then i+1 else i+2*n))) [[0,nns+3,n'],[nns+3,n'+1,n'],[1,nns+3,0],[n',nns+3,0]])
			)
lerp r s0 s1 = s1*r+s0*(1-r)
lerpV r = zipWith (lerp r)
offset'::[Double]->[[Double]]->[[Double]]
offset' o = map (zipWith (+) o)
offset o (ns,es) = (offset' o ns,es)
mainBeamSeg sz b = (ns,mainBeamSegEs n n' b) --D-D:b=0
	where (ns,n,n') = mainSlice sz
mainBeamSegEs n n' b = mirror [(0,n+0,2,I280),(1,n+1,2,I400),(0,1,n+0,T90)]
		++ map (\(s,e)->(s,e,n+s,LL75)) (dbl (\(i,j)->(j+n',i+n')) [v])
		++ [(1,n'+1,n+1,C160),(0,n',5,I160)]
		++ map ( (\(i,j) (o,t)->(o+i,o+j,2+i,t)) v' ) [(0,LL75),(1,LL75)]
		++ mirror [(\(i,j,k)->(i,j,k,T125)) v'']
	where
	mirror = dbl $ mvIJK n'
	v = f b where
		f 0 = (1,3)
		f 1 = (0,4)
	v' = f b where
		f 0 = (n',n)
		f 1 = (0,n'+n)
	v'' = f b where
		f 0 = (1,n,0)
		f 1 = (0,n+1,n)
mainSliceNs sz = ns where (ns,_,_) = mainSlice sz
mainSliceTopo = (n,n') where (_,n,n') = mainSlice [0,0,0]
mainSlice [x,y,w] = (ns, length ns, length ns') where
	ns' = [[x,0],[x,y],[x,2*y],[x',0],[x'',y],[x,-y],[x,y/2]]
	ns = map (++[0]) $ dbl (\[x,y]->[-x,y]) ns'
	x' = w/2
	x'' = 100
mvIJK o (i,j,k,t) = (i+o,j+o,k+o,t)
mvIJ o (i,j,k,t) = (i+o,j+o,k,t)
dbl f xs = xs++map f xs
combine (ns,es) (ns',es') = (ns++ns',es++map (mvIJK $ length ns) es')
