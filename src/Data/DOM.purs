module Data.DOM where

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
--  
--

data HtmlF a
  = Element (ElementData a)
  | Text String a
  | TextBox (RVar String) [Attribute] (Subscription -> a)

instance functorHtmlF :: Functor HtmlF where
  (<$>) f (Element ed) = Element (f <$> ed) 
  (<$>) f (Text s a) = Text s (f a)
  (<$>) f (TextBox var attrs k) = TextBox var attrs (f <<< k)

type Html = Free HtmlF

element :: forall b. String -> [Attribute] -> Html b -> Html b
element elem attrs children = liftF $ Element $ mkElementData elem attrs children (\b -> b)

text :: String -> Html {}
text s = liftF $ Text s {}

textBox :: RVar String -> [Attribute] -> Html Subscription
textBox var attrs = liftF $ TextBox var attrs (\s -> s)

