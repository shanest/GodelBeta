module GodelBeta
where
import Data.List

builtinbeta ls idx = (!!) ls idx

--listeq::Ord a => [a] -> [a] -> Bool
--listeq a b = and ((length a == length b) : (zipWith (==) (sort a) (sort b)))
listeq::Ord a => [a] -> [a] -> Bool
listeq a b = and ((length a == length b) : (zipWith (==) a b))

factorial::Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

remlist::Integral a => a -> [a] -> [a]
remlist c ls = map (\x -> c `mod` x) ls

beta_godel::Integral a => a -> a -> a -> a
beta_godel c d i = c `mod` (1 + (i + 1)*d)

beta::Integral a => [a] -> a -> a
beta ls idx =
	let d = factorial (maximum (((genericLength ls)-1):ls))
	in let dlist = map (\i -> 1 + (i+1)*d) [0 .. ((genericLength ls)-1)]
	in let p = product dlist
	in let c = find (\x -> listeq (remlist x dlist) ls) [0 .. (p-1)]
	in case c of
		Just c1 -> beta_godel c1 d idx
		Nothing -> 0

{-
pairing::Integral a => (a,a) -> a
pairing (a,b) = ((a+b)*(a+b) + 3*a + b) `div`  2

inverse_pairing::Integral a => a -> (a, a)
inverse_pairing z =
	let w = floor ((sqrt(fromIntegral (8*z - 1)) - 1) / 2)
	in let t = (w*w + w) `div` 2
	in let x = z - t
	in (x, w-x)

beta_godel0::Integral a => (a,a) -> a
beta_godel0 (a, i) = 
	let m = inverse_pairing a
	in (fst m) `mod` ((i+1) * (snd m) + 1)
-}
