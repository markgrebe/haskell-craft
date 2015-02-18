module HaskellCraft.Utils where

vector3x :: Num a => (a,a,a) -> a
vector3x (x,y,z) = x

vector3y :: Num a => (a,a,a) -> a
vector3y (x,y,z) = y

vector3z :: Num a => (a,a,a) -> a
vector3z (x,y,z) = z

vector2x :: Num a => (a,a) -> a
vector2x (x,z) = x

vector2z :: Num a => (a,a) -> a
vector2z (x,z) = z

