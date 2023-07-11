{-# LANGUAGE Strict #-}
-- n.b. Strict implies StrictData

module StrictMirrorUniverse where

data StrictGuy = StrictGuy Int String
data TildeTilde = TildeTilde ~Int ~String

badStrict = StrictGuy 5 undefined
badTilde = TildeTilde 5 undefined

strictStrictInt :: StrictGuy -> Int
strictStrictInt (StrictGuy x _) = x

strictLazyInt :: TildeTilde -> Int
strictLazyInt (TildeTilde x _) = x

lazyStrictInt :: StrictGuy -> Int
lazyStrictInt (StrictGuy ~x _) = x

lazyLazyInt :: TildeTilde -> Int
lazyLazyInt (TildeTilde ~x _) = x
