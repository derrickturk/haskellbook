-- why would you permit this?

data Evil = Thing
          | Struct { x :: Int, y :: Int }
          | Struct2 { x :: Int, z :: Double }

ok = x (Struct 17 23)
alsoOk = x (Struct2 17 23.0)
-- bad = z (Struct 17 23)
-- alsoBad = x Thing
