{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}


import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe      (readMay)
import Data.Text (pack, unpack, Text)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Text as T
import Control.Lens hiding (view)
import Control.Monad.Fix
import Control.Monad ((<=<), void, liftM2)
import Data.Monoid
import Data.Text.Encoding (encodeUtf8)
import Data.Generics
import           Data.Map (Map)
import qualified Data.Map as Map
import GHCJS.DOM.Types (JSM)




import Switchable


newtype IngredientId = IngredientId Int deriving (Show, Eq, Ord, Read)

data Ingredient t
  = Ingredient { _ingredientName :: Dynamic t Text
               , _ingredientProbability :: Dynamic t Float
               }

data Config t
  = Config { _onNewIngredient :: Event t ()
           , _onRemoveIngredient :: Event t IngredientId
           , _onChangedIngredientName :: Event t (IngredientId, Text)
           , _onChangedIngredientVal :: Event t  (IngredientId, Float)
           }

data Model t
  = Model { _ingredients :: Dynamic t (Map IngredientId (Ingredient t))
          , _nextIngredientId :: Behavior t IngredientId
          }


instance Reflex t => Monoid (Config t) where
  mempty = Config never never never never
  mappend a b = Config { _onNewIngredient = leftmost [_onNewIngredient a, _onNewIngredient b]
                       , _onRemoveIngredient = leftmost [_onRemoveIngredient a, _onRemoveIngredient b]
                       , _onChangedIngredientName = leftmost [_onChangedIngredientName a,  _onChangedIngredientName b]
                       , _onChangedIngredientVal = leftmost [_onChangedIngredientVal a,  _onChangedIngredientVal b]
                       }

instance Reflex t => Switchable t Config where
  doSwitch onConfig
    = Config <$> switchPromptly never (_onNewIngredient         <$> onConfig)
             <*> switchPromptly never (_onRemoveIngredient      <$> onConfig)
             <*> switchPromptly never (_onChangedIngredientName <$> onConfig)
             <*> switchPromptly never (_onChangedIngredientVal  <$> onConfig)

makeModel :: (MonadWidget t m) => Config t -> m (Model t)
makeModel conf = mfix $ \model' -> do
    let onNewIngredient' = pushAlways (const (makeIngredient conf model')) (conf^.onNewIngredient)
    _ingredients <- foldDyn id Map.empty $ mergeWith (.) [ uncurry Map.insert <$> onNewIngredient'
                                                         , Map.delete <$> conf^.onRemoveIngredient
                                                         ]
    _nextIngredientId <- foldp incId (IngredientId 0) $ conf^.onNewIngredient
    pure $ Model {..}
  where
    incId _ (IngredientId a) = IngredientId (a + 1)

makeIngredient :: (Reflex t, MonadSample t m, MonadHold t m) => Config t -> Model t -> m (IngredientId, Ingredient t)
makeIngredient conf model' = do
  ourId <- sample $ model'^.nextIngredientId
  _ingredientName <- holdDyn "" . fmap snd . ffilter ((== ourId) . fst) $ conf^.onChangedIngredientName
  _ingredientProbability <- holdDyn 0.0 . fmap snd . ffilter ((== ourId) . fst) $ conf^.onChangedIngredientVal
  pure $ (ourId, Ingredient {..})
{--
<div class="slidecontainer">
  <input type="range" min="1" max="100" value="50" class="slider" id="myRange">
</div>
--}

viewIngredient :: (MonadWidget t m) => IngredientId -> Ingredient t -> m (Config t)
viewIngredient ingId ingredient = elClass "div" "ingredient" $ do
  t <- elClass "span" "ingredient-name" $ do
    initVal <- sample $ current (_ingredientName ingredient)
    textInput $ def & textInputConfig_initialValue .~ initVal
                    & textInputConfig_setValue .~ updated (_ingredientName ingredient)
  elClass "div" "slidecontainer" $ do
    initVal <- sample $ current (_ingredientProbability ingredient)
    r <- rangeInput $ def & rangeInputConfig_initialValue .~ initVal
                          & rangeInputConfig_setValue .~ updated (_ingredientProbability ingredient)
                          & rangeInputConfig_attributes .~ pure ( "type" =: "range" <> "min" =: "0" <> "max" =: "1" <> "class" =: "slider" <> "step" =: "0.02")
    pure $ mempty & onChangedIngredientName .~ ((ingId,) <$> t^.textInput_input)
                  & onChangedIngredientVal  .~ ((ingId,) <$> r^.rangeInput_input)

viewIngredients :: forall t m. MonadWidget t m => Model t -> m (Config t)
viewIngredients model' = do
  elClass "div" "ingredient-picker" $ do
    addIngredient <-
      elClass "div" "ingredients-head" $ do
        el "h1" $ text "Pick your ingredients ..."
        button "Add Ingredient (+)"
    ingredientConfig <-
      elClass "div" "ingredients-body" $ do
        gDyn $ viewIngredients' <$> model'^.ingredients
    pure $ ingredientConfig & onNewIngredient .~ addIngredient
  where
    viewIngredients' :: Map IngredientId (Ingredient t) -> m (Config t)
    viewIngredients' = fmap mconcat . traverse (uncurry viewIngredient)  . Map.toList


view :: (MonadWidget t m) => Model t -> m (Config t)
view = viewIngredients


controller :: forall t m. (MonadWidget t m) => m ()
controller = void . mfix $ view <=< makeModel

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 css) $ el "div" controller

foldp :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Behavior t b)
foldp = accumB . flip

css :: Text
css =
  "\
   \.slidecontainer {\
   \width: 100%; /* Width of the outside container */\
   \}\
   \/* The slider itself */\
   \.slider {\
   \    -webkit-appearance: none;  /* Override default CSS styles */\
   \    appearance: none;\
   \    width: 100%; /* Full-width */\
   \    height: 25px; /* Specified height */\
   \    background: #d3d3d3; /* Grey background */\
   \    outline: none; /* Remove outline */\
   \    opacity: 0.7; /* Set transparency (for mouse-over effects on hover) */\
   \    -webkit-transition: .2s; /* 0.2 seconds transition on hover */\
   \    transition: opacity .2s;\
   \}\
   \/* Mouse-over effects */\
   \.slider:hover {\
   \    opacity: 1; /* Fully shown on mouse-over */\
   \}\
   \/* The slider handle (use webkit (Chrome, Opera, Safari, Edge) and moz (Firefox) to override default look) */\
   \.slider::-webkit-slider-thumb {\
   \    -webkit-appearance: none; /* Override default look */\
   \    appearance: none;\
   \    width: 25px; /* Set a specific slider handle width */\
   \    height: 25px; /* Slider handle height */\
   \    background: #4CAF50; /* Green background */\
   \    cursor: pointer; /* Cursor on hover */\
   \}\
   \.slider::-moz-range-thumb {\
   \    width: 25px; /* Set a specific slider handle width */\
   \    height: 25px; /* Slider handle height */\
   \    background: #4CAF50; /* Green background */\
   \    cursor: pointer; /* Cursor on hover */\
   \}"

-- Lenses for Ingredient t:

ingredientName :: Lens' (Ingredient t) (Dynamic t Text)
ingredientName f ingredient' = (\ingredientName' -> ingredient' { _ingredientName = ingredientName' }) <$> f (_ingredientName ingredient')

ingredientProbability :: Lens' (Ingredient t) (Dynamic t Float)
ingredientProbability f ingredient' = (\ingredientProbability' -> ingredient' { _ingredientProbability = ingredientProbability' }) <$> f (_ingredientProbability ingredient')


-- Lenses for Config t:

onNewIngredient :: Lens' (Config t) (Event t ())
onNewIngredient f config' = (\onNewIngredient' -> config' { _onNewIngredient = onNewIngredient' }) <$> f (_onNewIngredient config')

onRemoveIngredient :: Lens' (Config t) (Event t IngredientId)
onRemoveIngredient f config' = (\onRemoveIngredient' -> config' { _onRemoveIngredient = onRemoveIngredient' }) <$> f (_onRemoveIngredient config')

onChangedIngredientName :: Lens' (Config t) (Event t (IngredientId, Text))
onChangedIngredientName f config' = (\onChangedIngredientName' -> config' { _onChangedIngredientName = onChangedIngredientName' }) <$> f (_onChangedIngredientName config')

onChangedIngredientVal :: Lens' (Config t) (Event t (IngredientId, Float))
onChangedIngredientVal f config' = (\onChangedIngredientVal' -> config' { _onChangedIngredientVal = onChangedIngredientVal' }) <$> f (_onChangedIngredientVal config')


-- Lenses for Model t:

ingredients :: Lens' (Model t) (Dynamic t (Map IngredientId (Ingredient t)))
ingredients f model' = (\ingredients' -> model' { _ingredients = ingredients' }) <$> f (_ingredients model')

nextIngredientId :: Lens' (Model t) (Behavior t IngredientId)
nextIngredientId f model' = (\nextIngredientId' -> model' { _nextIngredientId = nextIngredientId' }) <$> f (_nextIngredientId model')


