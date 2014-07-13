module Data.DOM where

import Control.Monad.Eff
import Control.Monad.Free
import Control.Reactive

data Attribute = Attribute String String

--
-- ElementData a ~ exists b. (String, [Attribute], Html b, b -> a)
--

data ElementData a = MkElementData (forall r. (forall b. String -> [Attribute] -> Html b -> (b -> a) -> r) -> r)

mkElementData :: forall b a. String -> [Attribute] -> Html b -> (b -> a) -> ElementData a
mkElementData elem attrs children k = MkElementData (\f -> f elem attrs children k)

runElementData :: forall a r. ElementData a -> (forall b. String -> [Attribute] -> Html b -> (b -> a) -> r) -> r
runElementData (MkElementData f) k = f k

instance functorElementData :: Functor ElementData where
  (<$>) f ed = runElementData ed (\elem attrs children k -> mkElementData elem attrs children (f <<< k))

--
-- ForEachData a ~ exists item. (RArray item, [Attribute], item -> RVar Number -> Html Subscription, Subscription -> a)
--

data ForEachData a = MkForEachData (forall r. (forall item. RArray item -> 
                                                            [Attribute] -> 
                                                            (item -> RVar Number -> Html Subscription) -> 
                                                            (Subscription -> a) -> r) -> r)

mkForEachData :: forall item a. RArray item -> 
                                [Attribute] -> 
                                (item -> RVar Number -> Html Subscription) -> 
                                (Subscription -> a) -> 
                                ForEachData a
mkForEachData arr attrs body a = MkForEachData (\f -> f arr attrs body a)

runForEachData :: forall a r. ForEachData a -> 
                              (forall item. RArray item ->
                                            [Attribute] ->                            
                                            (item -> RVar Number -> Html Subscription) ->                            
                                            (Subscription -> a) -> r) -> r                            
runForEachData (MkForEachData f) k = f k

instance functorForEachData :: Functor ForEachData where
  (<$>) f fed = runForEachData fed (\arr attrs body k -> mkForEachData arr attrs body (f <<< k))

--
-- The type of actions, for button clicks etc.
--

type Action = forall eff. Eff (reactive :: Reactive | eff) Unit

--
--
--

data HtmlF a
  = Element (ElementData a)
  | Text String a
  | Label (Computed String) [Attribute] (Subscription -> a)
  | TextBox (RVar String) [Attribute] (Subscription -> a)
  | CheckBox (RVar Boolean) [Attribute] (Subscription -> a)
  | Button String [Attribute] Action (Subscription -> a)
  | ForEach (ForEachData a)

instance functorHtmlF :: Functor HtmlF where
  (<$>) f (Element ed) = Element (f <$> ed) 
  (<$>) f (Text s a) = Text s (f a)
  (<$>) f (Label c attrs k) = Label c attrs (f <<< k)
  (<$>) f (TextBox var attrs k) = TextBox var attrs (f <<< k)
  (<$>) f (CheckBox var attrs k) = CheckBox var attrs (f <<< k)
  (<$>) f (Button text attrs action k) = Button text attrs action (f <<< k)
  (<$>) f (ForEach fed) = ForEach (f <$> fed)

type Html = Free HtmlF

element :: forall b. String -> [Attribute] -> Html b -> Html b
element elem attrs children = liftF $ Element $ mkElementData elem attrs children (\b -> b)

text :: String -> Html Unit
text s = liftF $ Text s unit

label :: Computed String -> [Attribute] -> Html Subscription
label c attrs = liftF $ Label c attrs (\s -> s)

textBox :: RVar String -> [Attribute] -> Html Subscription
textBox var attrs = liftF $ TextBox var attrs (\s -> s)

checkBox :: RVar Boolean -> [Attribute] -> Html Subscription
checkBox var attrs = liftF $ CheckBox var attrs (\s -> s)

forEach :: forall item. RArray item -> [Attribute] -> (item -> RVar Number -> Html Subscription) -> Html Subscription 
forEach arr attrs body = liftF $ ForEach $ mkForEachData arr attrs body (\s -> s)

button :: String -> [Attribute] -> Action -> Html Subscription
button text attrs action = liftF $ Button text attrs action (\s -> s)

