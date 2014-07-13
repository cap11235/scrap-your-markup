module Data.DOM.Render.JQuery where

import Data.DOM

import Data.Foldable (for_)

import Control.Monad.Free
import Control.Monad.Eff
import qualified Control.Monad.JQuery as JQuery
import qualified Control.Reactive as R
import qualified Control.Reactive.JQuery as RJ

createElement :: forall eff. String -> [Attribute] -> Eff (dom :: JQuery.DOM | eff) JQuery.JQuery
createElement elem attrs = do
  el <- JQuery.create $ "<" ++ elem ++ ">"
  for_ attrs $ \(Attribute key value) ->
    JQuery.setAttr key value el
  return el

renderJQuery :: forall a eff. JQuery.JQuery -> Html a -> Eff (dom :: JQuery.DOM, reactive :: R.Reactive | eff) a
renderJQuery root = iterM go
  where
    go :: forall b eff. HtmlF (Eff (dom :: JQuery.DOM, reactive :: R.Reactive | eff) b)
      -> Eff (dom :: JQuery.DOM, reactive :: R.Reactive | eff) b
    go (Element ed) = runElementData ed (\elem attrs children k -> do
      el <- createElement elem attrs 
      b <- renderJQuery el children
      JQuery.append el root
      k b)
    go (Text s rest) = do
      JQuery.appendText s root
      rest
    go (Label c attrs rest) = do
      el <- createElement "span" attrs
      sub <- RJ.bindTextOneWay c el
      JQuery.append el root
      rest sub
    go (TextBox var attrs rest) = do
      el <- createElement "input" attrs
      sub <- RJ.bindValueTwoWay var el
      JQuery.append el root
      rest sub
    go (CheckBox var attrs rest) = do
      el <- createElement "input" (Attribute "type" "checkbox" : attrs)
      sub <- RJ.bindCheckedTwoWay var el
      JQuery.append el root
      rest sub
    go (Button text attrs action rest) = do
      el <- createElement "button" attrs
      flip (JQuery.on "click") el $ \_ -> action
      JQuery.appendText text el
      JQuery.append el root
      rest Data.Monoid.mempty
    go (ForEach fed) = runForEachData fed (\arr attrs body k -> do
      el <- createElement "div" attrs
      sub <- RJ.bindArray arr el $ \item index -> do
        itemEl <- createElement "div" []
        s <- renderJQuery itemEl (body item index)
        return { el: itemEl, subscription: s }
      JQuery.append el root
      k sub
      )
