{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RWC where

data Guy = Guy { stuff :: Int }

-- boring way
unpack :: Guy -> IO ()
unpack Guy { stuff = s } = print s

-- needs -XNamedFieldPuns
unpack' :: Guy -> IO ()
unpack' Guy { stuff } = print stuff

-- needs -XRecordWildCards
unpack'' :: Guy -> IO ()
unpack'' Guy { .. } = print stuff
