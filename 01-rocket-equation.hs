total_fuel_mass mass = fuel_for_added_fuel [mass]

fuel_for_added_fuel (x:tail) = if current_added_fuel <= 0 then sum (x:tail) else fuel_for_added_fuel (current_added_fuel:x:tail)
  where current_added_fuel = fuel_for_module x



fuel_for_module :: Int -> Int
fuel_for_module mass = (mass `div` 3) - 2


overall_fuel_mass [] acc = acc 
overall_fuel_mass (m:tail) acc =
  let part_fuel = total_fuel_mass $ fuel_for_module m
   in overall_fuel_mass tail (acc + part_fuel)


main = do
  m <- read_modules []
  let mass = overall_fuel_mass m 0
  print mass


read_modules :: [Int] -> IO [Int]
read_modules modules = do
  m <- getLine
  let mod = (read m) :: Int
   in case mod of
        -1 -> return modules
        _ -> read_modules (mod:modules)
