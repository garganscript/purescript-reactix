module ReactixTest where

-- import Prelude
-- import Effect (Effect)
-- import Data.Maybe (Maybe(..))
-- import Reactix.React as R
-- import Reactix.React ( DOM, Root, findRootById, text, render)
-- import Data.Nullable (Nullable, toNullable)
-- import Effect.Ref as Ref
-- import Effect.Uncurried as EU
-- import Data.Function.Uncurried as FU
-- import Reactix.Hooks (useState, useEffect)
-- dom1 :: DOM
-- dom1 = R.createElement "i" {} [text "Hello World"]

-- frag1 :: DOM
-- frag1 = R.createElement R.fragment
--         [ R.createElement "i" {} [ text "Hello World" ]
--         , R.createElement "i" {} [ text "Hello World" ]]

-- foreign import _fakeStatic :: forall p. FU.Fn2 (Ref.Ref Int) { count :: Int | p } DOM

-- staticCounter0 :: ∀ p. Ref.Ref Int -> { count :: Int | p } -> Effect DOM
-- staticCounter0 r p = pure $ FU.runFn2 _fakeStatic r p

-- staticCounter1 :: forall p. Ref.Ref Int -> { count :: Int | p } -> Effect DOM
-- staticCounter1 r x =
--   do y <- useState x.count
--      z <- useState x.count
--      useEffect $ raf (z.setter pure) *> pure Nothing
--      pure $ R.createElement (staticCounter0 r) { count: y.value } []

-- staticCounter2 :: ∀ p.
--      R.Memo {count :: Int} DOM
--   -> { count :: Int | p }
--   -> Effect DOM
-- staticCounter2 c x =
--   do y <- useState x.count
--      z <- useState x.count
--      useEffect $ raf (z.setter pure) *> pure Nothing
--      pure $ R.createElement c { count: y.value } []

-- liveCounterButton :: ((Int -> Effect Int) -> Effect Unit) -> Effect DOM
-- liveCounterButton set = pure $ R.createElement "button" {onClick: (EU.mkEffectFn1 click)} [text "++"]
--   where click :: ∀ b. b -> Effect Unit
--         click _ = set (pure <<< (_+1))

-- showCounter :: forall r. Ref.Ref Int -> { val :: Int | r } -> Effect DOM
-- showCounter r { val: y } =
--    do useEffect (Ref.modify_ (_+1) r *> pure (pure unit))
--       pure $ R.createElement "i" {} [ text (show y) ]

-- lc2 :: forall p. Ref.Ref Int -> R.Memo { val :: Int | p } DOM
-- lc2 r = R.memo' $ showCounter r

-- lc3 :: forall p. Ref.Ref Int -> R.Memo { val :: Int | p } DOM 
-- lc3 r = R.memo (showCounter r) h
--   where h a b = a.val == b.val

-- liveCounter1 :: forall p.
--   Ref.Ref Int
--   -> { count :: Int | p }
--   -> Effect DOM
-- liveCounter1 r x =
--   do y <- useState x.count
--      z <- useState x.count
--      useEffect $ raf (z.setter pure) *> pure Nothing
--      b <- liveCounterButton y.setter
--      d <- R.createElement (showCounter r) { val: y.value } []
--      e <- R.createElement (showCounter r) { val: 999 } []
--      pure $ R.createElement R.fragment [b, d, e]
--   where click :: ∀ b. Int -> (Int -> Effect Unit) -> b -> Effect Unit
--         click orig set _ = set (orig + 1)

-- liveCounter2 :: ∀ p.
--     R.Memo { val :: Int } DOM
--   -> { count :: Int | p }
--   -> Effect DOM
-- liveCounter2 c x =
--   do y <- useState x.count
--      z <- useState x.count
--      useEffect $ raf (z.setter pure) *> pure Nothing
--      b <- liveCounterButton y.setter
--      c' <- R.createElement c { val: y.value } []
--      d <- R.createElement c { val: 999 } []
--      pure $ R.createElement R.fragment [b, c', d]

-- liveCounter3 :: forall p.
--     R.Memo { val :: Int } DOM
--   -> { count :: Int | p }
--   -> Effect DOM
-- liveCounter3 c x =
--   do y <- useState x.count
--      z <- useState x.count
--      useEffect $ raf (z.setter pure) *> pure Nothing
--      b <- liveCounterButton y.setter
--      c' <- R.createElement c { val: y.value } []
--      d <- R.createElement c { val: 999 } []
--      pure $ R.createElement R.fragment [b, c', d]

-- dom1Test :: Partial ⇒ Effect Unit
-- dom1Test = do (Just root) ← findRootById "existing"
--               render dom1 root

-- frag1Test :: Partial ⇒ Effect Unit
-- frag1Test = do (Just root) ← findRootById "existing"
--                render frag1 root

-- staticCounter1Test :: Partial ⇒ Ref.Ref Int -> Effect Unit
-- staticCounter1Test r = do (Just root) ← findRootById "existing"
--                           c <- R.createElement (staticCounter1 r) { count: 1 } []
--                           render c root

-- -- let's squeeze 
-- staticCounter2Test :: Partial ⇒ Ref.Ref Int -> Effect Unit
-- staticCounter2Test r = do (Just root) ← findRootById "existing"
--                           c <- R.createElement cpt { count: 1 } []
--                           render c root
--   where cpt = staticCounter2 $ R.memo' $ staticCounter0 r

-- staticCounter3Test :: Partial ⇒ Ref.Ref Int -> Effect Unit
-- staticCounter3Test r = do (Just root) ← findRootById "existing"
--                           c <- R.createElement cpt { count: 1 } []
--                           render c root
--   where h a b = a.count == b.count
--         cpt = staticCounter2 $ R.memo (staticCounter0 r) h

-- liveCounter1Test :: Partial ⇒ Ref.Ref Int -> Effect Unit
-- liveCounter1Test r = do (Just root) ← findRootById "existing"
--                         frag ← R.createElement (liveCounter1 r) {count: 1} []
--                         render frag root

-- liveCounter2Test :: Partial ⇒ Ref.Ref Int -> Effect Unit
-- liveCounter2Test r = do (Just root) ← findRootById "existing"
--                         let c = lc2 r
--                         frag ← R.createElement (liveCounter2 c) {count: 1} []
--                         render frag root

-- liveCounter3Test :: Partial ⇒ Ref.Ref Int -> Effect Unit
-- liveCounter3Test r = do (Just root) ← findRootById "existing"
--                         let c = lc3 r
--                         frag ← R.createElement (liveCounter3 c) {count: 1} []
--                         render frag root

-- findRoot :: String -> Effect (Nullable Root)
-- findRoot i = toNullable <$> findRootById i

-- mkRef :: Int -> Effect (Ref.Ref Int)
-- mkRef = Ref.new

-- foreign import _raf :: EU.EffectFn1 (Effect Unit) Unit
-- foreign import _st :: EU.EffectFn2 (Effect Unit) Int Unit

-- raf :: Effect Unit -> Effect Unit
-- raf = EU.runEffectFn1 _raf

-- st :: Effect Unit -> Int -> Effect Unit
-- st = EU.runEffectFn2 _st

-- cmp :: { count :: Int } -> { count :: Int } -> Boolean
-- cmp a b = a.count == b.count
