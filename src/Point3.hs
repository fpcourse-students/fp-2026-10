-- | Трёхмерная точка из ДЗ 9. Все операции обходят координаты в порядке x, y, z.
module Point3 where

data Point3 a = Point3 a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Point3 where
  pure x = Point3 x x x

  Point3 f g h <*> Point3 x y z = Point3 (f x) (g y) (h z)
