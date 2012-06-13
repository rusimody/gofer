-- Representation of temperatures on both Fahrenheit and Celsius scales
--
-- This program cannot be used on any machine without support for floating
-- point numbers within Gofer (e.g. PCs).

data Temp = Celsius Float | Fahrenheit Float

fahrToCent f = (f-32.0)/1.8

instance Eq Temp where
    Celsius c1    == Celsius c2    = c1==c2
    Celsius c1    == Fahrenheit f2 = c1==fahrToCent f2
    Fahrenheit f1 == Celsius c2    = fahrToCent f1==c2
    Fahrenheit f1 == Fahrenheit f2 = f1==f2

