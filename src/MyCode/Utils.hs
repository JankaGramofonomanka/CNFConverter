module MyCode.Utils where


import FromBNFC.ErrM

paste :: [String] -> String -> String
paste [] _ = "" 
paste [x] _ = x
paste (x:xs) sep = x ++ sep ++ (paste xs sep)


errConcat :: [Err a] -> Err [a]
errConcat [Ok x] = Ok [x]
errConcat [Bad s] = Bad s
errConcat (x:xs) = case x of
  Ok y  -> do
      ys <- errConcat xs
      return $ y:ys

  Bad s -> Bad s


makeSpace :: Int -> String
makeSpace i
  | i <= 0    = ""
  | otherwise = " " ++ (makeSpace $ i - 1)
