module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


precioFinal :: Hamburguesa -> Number
precioFinal h = precioBase h + sum( map precioIngrediente (ingredientes h ) )


elegirBase :: [Ingrediente] -> [Ingrediente]
elegirBase cola
  | any (== Carne) cola = [Carne]
  | any (==Pollo) cola = [Pollo]
  | otherwise = []

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [ Pan, Carne, Cheddar, Pan]

agrandar :: Hamburguesa -> Hamburguesa
agrandar h = Hamburguesa (precioBase h) (ingredientes h ++ elegirBase (ingredientes h ))

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingredie h = Hamburguesa (precioBase h) (ingredientes h ++ [ingredie])

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porciento h = Hamburguesa (precioBase h * (1-porciento / 100) ) (ingredientes h)

pdepBurger :: Hamburguesa




