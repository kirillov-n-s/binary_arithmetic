module Main where

import Bitset

main :: IO ()
main =
  print' x >> putDelim >> print bx >>
  print' y >> putDelim >> print by >>
  print' f >> putDelim >> print bf

  where
    x = 42 :: Int
    y = 24 :: Int
    f = unbits bf'

    bx' = bits x
    by' = bits y
    bf' = bx' -|- by'

    print' :: Int -> IO ()
    print' = putStr . show
    putDelim = putStr "\t: "
    
    pax = padMax [bx', by', bf']
    
    bx = pax bx'
    by = pax by'
    bf = pax bf'

