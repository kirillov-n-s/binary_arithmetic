module Main where

import Bitset

main :: IO ()
main =
  putStr (show x) >> putDelim >> print bx >>
  putStr (show y) >> putDelim >> print by >>
  putStr (show f) >> putDelim >> print bf

  where
    x = 42 :: Int
    y = 24 :: Int
    f = unbits bf'

    bx' = bits x
    by' = bits y
    bf' = bx' -|- by'

    pax = padMax [bx', by', bf']
    putDelim = putStr "\t: "

    bx = pax bx'
    by = pax by'
    bf = pax bf'

