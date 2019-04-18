module Reactix.React.Spec where

import Prelude
import Data.Array as A
import Data.Array ( (!!) )
import Data.Maybe ( Maybe(..) )
import Data.Traversable ( traverse, traverse_, sequence_ )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import Effect ( Effect )
import Effect.Class ( liftEffect )
import Effect.Uncurried ( EffectFn1, mkEffectFn1, runEffectFn1 )
-- import Effect.Aff (launchAff_)
import Test.Spec ( Spec, describe, it )
import Test.Spec.Assertions ( shouldEqual )
-- import Test.Spec.QuickCheck (quickCheck')
import DOM.Simple.Console ( log2 )
import DOM.Simple as DOM
import DOM.Simple.Document as Document
import DOM.Simple.Element as Element
import DOM.Simple.Event as Event

import Reactix as R
import Reactix.Test as RT
import Reactix.DOM.Raw ( button, div, i, text )
import Reactix.Hooks ( useState )

staticTest :: Spec Unit
staticTest =
  describe "Basic DOM rendering" do
    it "Simple elements" do
      root <- liftEffect $ RT.render elem
      (Element.name <$> Element.children root.container) `shouldEqual` ["I"]
    it "Fragments" do
      children /\ count <- liftEffect $
        do let root = Document.createElement "div"
           RT.act $ R.render frag root
           pure $ Tuple (Element.children root) (Element.childCount root)
      count `shouldEqual` 2
      A.length children `shouldEqual` 2
      (Element.name <$> children) `shouldEqual` ["I", "I"]
      (Element.innerHTML <$> children) `shouldEqual` ["hello","world"]
  where elem = i {} [ text "hello world" ]
        frag = i {} [ text "hello" ] <> i {} [ text "world" ]

type CounterProps = ( count :: Int )

counterCpt :: R.Component CounterProps
counterCpt = R.hooksLeaf "Counter" cpt
  where
    cpt :: forall m. R.MonadHooks m => Record CounterProps -> m R.Element
    cpt {count} = do
      y /\ setY <- useState $ pure count
      pure $ div { className: "counter" }
        [ button { type: "button", onClick: onclick setY (y + 1) } [ text "++" ]
        , div {} [ text (show y) ] ]
    onclick set to = mkEffectFn1 $ \e -> do
      runEffectFn1 set to

counterTest :: Spec Unit
counterTest =
  describe "Counter" do
    it "Works for plain components" $ do
      let counter = R.createLeaf counterCpt {count: 0}
      liftEffect (RT.render counter) >>= test
    it "Works for memoised components" $ do
      let counter = R.createLeaf (R.memo counterCpt (==)) {count: 0}
      liftEffect (RT.render counter) >>= test
    it "works for memo'ised components" $ do
      let counter = R.createLeaf (R.memo' counterCpt) {count: 0}
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
          
listTest :: Spec Unit
listTest = pure unit

spec :: Spec Unit
spec = sequence_
  [ staticTest
  , counterTest
  , listTest
  ]
