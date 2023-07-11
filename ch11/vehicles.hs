module Vehicles where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Infiniti
                  | Nissan
                  | Mercury
                  deriving (Eq, Show)

data Airline = Southwest
              | United
              | Delta
              deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline
             deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

aCar = Car Infiniti (Price 20000)
anotherCar = Car Mercury (Price 12000)
aPlane = Plane Southwest

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "not a car"

-- better
maybeManu :: Vehicle -> Maybe Manufacturer
maybeManu (Car m _) = Just m
maybeManu _ = Nothing
