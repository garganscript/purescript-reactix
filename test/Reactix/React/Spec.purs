module Reactix.React.Spec where

import Prelude
import Data.Array as A
import Data.Array ( (!!) )
import Data.Maybe ( Maybe(..) )
import Data.Traversable ( traverse, traverse_, sequence_ )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import Effect ( Effect )
import Effect.Aff ( Aff )
import Effect.Class ( liftEffect )
import Effect.Ref as Ref
import Effect.Uncurried ( EffectFn1, mkEffectFn1, runEffectFn1 )
-- import Effect.Aff (launchAff_)
import Test.Spec ( Spec, describe, it )
import Test.Spec.Assertions ( shouldEqual )
-- import Test.Spec.QuickCheck (quickCheck')
import DOM.Simple as DOM
import DOM.Simple.Document as Document
import DOM.Simple.Element as Element
import DOM.Simple.Event as Event
import Reactix as R
import Reactix.Test as RT
import Reactix.DOM.Raw ( button, div, i, text )
import Reactix.Hooks ( useState, useEffect, useLayoutEffect )

staticTest :: Spec Unit
staticTest =
  describe "Basic DOM rendering" $ do
    it "Simple elements" $ do
      root <- liftEffect $ RT.render elem
      let children = Element.children root.container
      (Element.name <$> children) `shouldEqual` ["I"]
      (Element.innerHTML <$> children) `shouldEqual` ["hello world"]
    it "Fragments" $ do
      root <- liftEffect $ RT.render $ frag
      Element.childCount root.container `shouldEqual` 2
      let children = Element.children root.container
      A.length children `shouldEqual` 2
      (Element.name <$> children) `shouldEqual` ["I", "I"]
      (Element.innerHTML <$> children) `shouldEqual` ["hello","world"]
  where elem = i {} [ text "hello world" ]
        frag = i {} [ text "hello" ] <> i {} [ text "world" ]

type CounterProps = ( count :: Int )

counterCpt :: R.Component CounterProps
counterCpt = R.hooksComponent "Counter" cpt
  where
    cpt {count} _ = do
      y /\ setY <- useState $ \_ -> pure count
      pure $ div { className: "counter" }
        [ button { type: "button", onClick: onclick setY (y + 1) } [ text "++" ]
        , div {} [ text (show y) ] ]
    onclick set to = mkEffectFn1 $ \e -> runEffectFn1 set to

counterTest :: Spec Unit
counterTest =
  describe "Counter" do
    it "Works for plain components" $ do
      let counter = R.createElement counterCpt {count: 0} []
      liftEffect (RT.render counter) >>= test
    it "Works for memoised components" $ do
      let counter = R.createElement (R.memo counterCpt (==)) {count: 0} []
      liftEffect (RT.render counter) >>= test
    it "works for memo'ised components" $ do
      let counter = R.createElement (R.memo' counterCpt) {count: 0} []
      liftEffect (RT.render counter) >>= test
  where
    test root = do
      let children = Element.children root.container
      A.length children `shouldEqual` 1
      let children2 = children >>= Element.children
      A.length children2 `shouldEqual` 2
      (Element.name <$> children2) `shouldEqual` ["BUTTON", "DIV"]
      (Element.innerHTML <$> children2) `shouldEqual` ["++", "0"]
      liftEffect $ traverse_ RT.fireClick (children2 !! 0)
      let children3 = Element.children root.container >>= Element.children
      A.length children3 `shouldEqual` 2
      (Element.innerHTML <$> children3) `shouldEqual` ["++", "1"]
      liftEffect $ traverse_ RT.fireClick (children3 !! 0)
      let children4 = Element.children root.container >>= Element.children
      A.length children4 `shouldEqual` 2
      (Element.innerHTML <$> children4) `shouldEqual` ["++", "2"]
          
data EffectorState = Fresh | Initialised | Done

derive instance eqEffectorState :: Eq EffectorState

instance showEffectorState :: Show EffectorState where
  show Fresh = "fresh"
  show Initialised = "initialised"
  show Done = "done"

type EffectorProps = ( stateRef :: Ref.Ref EffectorState )

effectorCpt :: R.Component EffectorProps
effectorCpt = R.hooksComponent "Effector" cpt
  where cpt {stateRef} _ = do
          useEffect $ \_ -> do
            Ref.write Initialised stateRef
            pure $ \_ -> Ref.write Done stateRef
          pure $ div {} []

-- TODO: test it's firing at the right time
effectorTest :: Spec Unit
effectorTest =
  describe "Effector" do
    it "Works for plain components" $
      test $ effectorCpt
    it "works for memo'ised components" $
      test $ R.memo' effectorCpt
  where
    test :: forall cpt. R.IsComponent cpt EffectorProps (Array R.Element) => cpt -> Aff Unit
    test cpt = do
      ref <- liftEffect $ Ref.new Fresh
      let effector = R.createElement cpt {stateRef: ref} []
      root <- liftEffect (RT.render effector)
      state <- liftEffect $ Ref.read ref
      state `shouldEqual` Initialised
      liftEffect $ RT.cleanup
      state' <- liftEffect $ Ref.read ref
      state' `shouldEqual` Done

layoutEffectorCpt :: R.Component EffectorProps
layoutEffectorCpt = R.hooksComponent "LayoutEffector" cpt
  where cpt {stateRef} _ = do
          useLayoutEffect $ \_ -> do
            Ref.write Initialised stateRef
            pure $ \_ -> Ref.write Done stateRef
          pure $ div {} []

-- TODO: test it's firing at the right time
layoutEffectorTest :: Spec Unit
layoutEffectorTest =
  describe "LayoutEffector" do
    it "Works for plain components" $
      test $ layoutEffectorCpt
    it "works for memo'ised components" $
      test $ R.memo' layoutEffectorCpt
  where
    test :: forall cpt. R.IsComponent cpt EffectorProps (Array R.Element) => cpt -> Aff Unit
    test cpt = do
      ref <- liftEffect $ Ref.new Fresh
      let effector = R.createElement cpt {stateRef: ref} []
      root <- liftEffect (RT.render effector)
      state <- liftEffect $ Ref.read ref
      state `shouldEqual` Initialised
      liftEffect $ RT.cleanup
      state' <- liftEffect $ Ref.read ref
      state' `shouldEqual` Done

type ContextProps = ()

-- contextualCpt :: R.Component ContextProps
-- contextualCpt = R.hooksComponent "Contextual" cpt
--   where cpt {stateRef} _ = do
--           useEffect $ \_ -> do
--             Ref.write Initialised stateRef
--             pure $ \_ -> Ref.write Done stateRef
--           pure $ div {} []

-- contextTest :: Spec Unit
-- contextTest =
--   describe "Context" do
--     it "Works for plain components" $
--       test $ contextualCpt
--   where test cpt = pure unit

-- reducerTest :: Spec Unit
-- memoTest :: Spec Unit
-- refTest :: Spec Unit
-- imperativeHandleTest :: Spec Unit
-- debugValueTest :: Spec Unit

-- listTest :: Spec Unit
-- listTest = pure unit

spec :: Spec Unit
spec = sequence_
  [ staticTest
  , counterTest
  , effectorTest
  , layoutEffectorTest
  ]
  -- , listTest
  -- ]
