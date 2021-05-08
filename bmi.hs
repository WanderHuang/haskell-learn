calcBmi :: (RealFloat a) => a -> a -> String
calcBmi weight height
  | bmi <= skinny = "underweight"
  | bmi <= normal = "healthy"
  | bmi <= fat = "overweight"
  | otherwise = "whale"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25
    fat = 30
