-- Type representations that let you write genreic "maps".
module MapRep
where

type Id a = a



data EPT a b = EP{to :: a -> b, from :: b -> a}
refl :: EPT a a
refl = EP id id

newtype Forall f = Forall { fromForall :: forall b . f b}

data MapRep f a b = RVar (f a b)
                | RInt (EPT a Int)
		| forall g. RForall (forall c. MapRep f c a -> MapRep f (g c) a)
			            (EPT a (Forall g))

