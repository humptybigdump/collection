module ChurchList where

type ChurchList t u = (t -> u -> u) -> u -> u

-- Churchlisten
{-
mapChurch :: (t -> s) -> ((t->u->u)->u->u) -> ((s->u->u)->u->u)
-}
mapChurch :: (t -> s) -> ChurchList t u -> ChurchList s u
mapChurch fts l = \fsuu u -> l (fsuu . fts) u

{-
fts::(t->s)
l::((t->u->u)->u->u)
fsuu::(s -> u -> u)
u'::u'
u::u
fts t :: s
fsuu (fts t) :: u -> u
l (\t u -> fsuu (fts t) u) :: u -> u
-}

mapChurch' :: (t -> s) -> (ChurchList t u) -> (ChurchList s u)
mapChurch' f cl = \c n -> cl (c . f) n

church2list :: ChurchList t [t] -> [t]
church2list cl = cl (:) []

-- 1:2:3:[]=[1,2,3]

list2church :: [t] -> ChurchList t u
list2church l = \c n -> foldr c n l

list2church' :: [t] -> ChurchList t u
list2church' [] = \c n -> n
list2church' (x : xs) = \c n -> c x (list2church' xs c n)

-- Collatz Folge
-- not = λp. p cfalse ctrue
-- isEven = λp. p not ctrue

{-
isEven c_2 = (λp. p not ctrue) (\s. \z. s (s z))
=>(\s. \z. s (s z)) not ctrue
=>(\z. not (not z)) ctrue
=>(not (not ctrue))
=(not ((λp. p cfalse ctrue) (\t. \f. t)))
=>(not ((\t. \f. t) cfalse ctrue))
=>(not ((\f. cfalse) ctrue))
=>(not cfalse)
=>+ctrue
-}

-- collatz' = λcollatz''. λp. (eq p c1) ctrue ((isEven p) (collatz'' (down p)) (collatz'' (up p)))
-- collatz = Y collatz'

-- Zähler Objekte
{-
react n cx inc = (\n. \x. \m. m (n (succ x)) (\y. n (plus x y)) x) n cx inc
=> inc (n (succ cx)) (\y. n (plus cx y)) cx
=> (\i. \a. \g. i) (n (succ cx)) (\y. n (plus cx y)) cx
=> n (succ cx)
=>* n c(x+1)

react n cx add cy = (\n. \x. \m. m (n (succ x)) (\y. n (plus x y)) x) n cx add cy
=> (add (n (succ cx)) (\y. n (plus cx y)) cx) cy
=> ((\i. \a. \g. a) (n (succ cx)) (\y. n (plus cx y)) cx) cy
=> (\y. n (plus cx y)) cy
=> n (plus cx cy) => n c(x+y)

react n cx get => (\n. \x. \m. m (n (succ x)) (\y. n (plus x y)) x) n cx get
=> get (n (succ cx)) (\y. n (plus cx y)) cx
=> (\i. \a. \g. g) (n (succ cx)) (\y. n (plus cx y)) cx
=> cx
-}
