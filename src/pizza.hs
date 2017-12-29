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
import GHC.Generics
import           Data.Map (Map)
import qualified Data.Map as Map
import GHCJS.DOM.Types (JSM)
import System.Random
import Data.List
import Control.Monad.IO.Class
import Data.Foldable
import Data.Aeson



import Switchable


newtype IngredientId = IngredientId Int deriving (Show, Eq, Ord, Read, Generic)

instance FromJSON IngredientId
instance ToJSON IngredientId
instance FromJSONKey IngredientId
instance ToJSONKey IngredientId

data Ingredient t
  = Ingredient { _ingredientName :: Dynamic t Text
               , _ingredientProbability :: Dynamic t Float
               }

data Config t
  = Config { _onNewIngredient :: Event t ()
           , _onRemoveIngredient :: Event t IngredientId
           , _onChangedIngredientName :: Event t (IngredientId, Text)
           , _onChangedIngredientVal :: Event t  (IngredientId, Float)
           , _onChangeIngredientCount :: Event t Int
           }

data Model t
  = Model { _ingredients :: Dynamic t (Map IngredientId (Ingredient t))
          , _ingredientCount :: Dynamic t Int
          , _nextIngredientId :: Behavior t IngredientId
          }

data ModelDump
  = ModelDump { _dumpedIngrediants :: Map IngredientId (Text, Float)
              , _dumpedCount :: Int
              , _dumpedNextId :: IngredientId
              } deriving (Generic)

instance FromJSON ModelDump
instance ToJSON ModelDump


instance Reflex t => Monoid (Config t) where
  mempty = Config never never never never never
  mappend a b = Config { _onNewIngredient = leftmost [_onNewIngredient a, _onNewIngredient b]
                       , _onRemoveIngredient = leftmost [_onRemoveIngredient a, _onRemoveIngredient b]
                       , _onChangedIngredientName = leftmost [_onChangedIngredientName a,  _onChangedIngredientName b]
                       , _onChangedIngredientVal = leftmost [_onChangedIngredientVal a,  _onChangedIngredientVal b]
                       , _onChangeIngredientCount = leftmost [_onChangeIngredientCount a,  _onChangeIngredientCount b]
                       }

instance Reflex t => Switchable t Config where
  doSwitch onConfig
    = Config <$> switchPromptly never (_onNewIngredient         <$> onConfig)
             <*> switchPromptly never (_onRemoveIngredient      <$> onConfig)
             <*> switchPromptly never (_onChangedIngredientName <$> onConfig)
             <*> switchPromptly never (_onChangedIngredientVal  <$> onConfig)
             <*> switchPromptly never (_onChangeIngredientCount <$> onConfig)

-- persistModel :: Model t -> Dynamic t ModelDump

chooseIngredients :: forall t m. MonadWidget t m => Model t -> m (Dynamic t [Dynamic t Text])
chooseIngredients model' = do
    gen <- liftIO $ getStdGen
    let rs = randomRs (0.1, 1.0) gen
    pure $ chooseIngredients' rs model'
  where
    chooseIngredients' :: [Float] -> Model t -> Dynamic t [Dynamic t Text]
    chooseIngredients' rs model' = do
      count <- model'^.ingredientCount
      ingredients' <- model'^.ingredients
      let
        ingredientList = Map.toList ingredients'

        calcedListM :: [(IngredientId, Ingredient t)]
        calcedListM = zipWith calcProb rs ingredientList
      calcedList <- traverse (traverse (^. ingredientProbability)) $ calcedListM
      let orderedIds = map fst . take count . sortBy (\a b -> compare (snd b) (snd a)) $ calcedList
      pure $ map (\iid -> ingredients'^. at iid . _Just . ingredientName) orderedIds


    calcProb :: Float -> (IngredientId, Ingredient t) -> (IngredientId, Ingredient t)
    calcProb r (iId, ingr) = (iId, (ingr & ingredientProbability %~ fmap (*r)))


makeModel :: (MonadWidget t m) => Config t -> m (Model t)
makeModel conf = mfix $ \model' -> do
    let onNewIngredient' = pushAlways (const (makeIngredient conf model')) (conf^.onNewIngredient)
    _ingredients <- foldDyn id Map.empty $ mergeWith (.) [ uncurry Map.insert <$> onNewIngredient'
                                                         , Map.delete <$> conf^.onRemoveIngredient
                                                         ]
    _ingredientCount <- holdDyn 1 $ conf^.onChangeIngredientCount
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
    x <- button "x"
    pure $ mempty & onChangedIngredientName .~ ((ingId,) <$> t^.textInput_input)
                  & onChangedIngredientVal  .~ ((ingId,) <$> r^.rangeInput_input)
                  & onRemoveIngredient .~ (const ingId <$> x)

viewCountPicker :: forall t m. (MonadWidget t m) => Model t -> m (Config t)
viewCountPicker model' = gDyn $ const (viewCountPicker' model') <$> constDyn ()

viewCountPicker' :: (MonadWidget t m) => Model t -> m (Config t)
viewCountPicker' model' = elClass "div" "ingredient-count-chooser" $ do
  t <- elClass "span" "count-name" $ text "Number of ingredients"
  r <- elClass "div" "slidecontainer" $ do
    initVal <- sample $ current (fromIntegral <$> _ingredientCount model')
    rangeInput $ def & rangeInputConfig_initialValue .~ initVal
                          & rangeInputConfig_setValue .~ fmap fromIntegral (updated (_ingredientCount model'))
                          & rangeInputConfig_attributes .~ pure ( "type" =: "range" <> "min" =: "1" <> "max" =: "15" <> "class" =: "slider" <> "step" =: "1")
  elClass "div" "count-chooser-show-val" $ do
    text "You lucky guy, get "
    dynText (T.pack . show <$> model'^.ingredientCount)
    text " ingredients on your pizza!"
  pure $ mempty & onChangeIngredientCount .~ fmap round (r^.rangeInput_input)

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


view :: forall t m. (MonadWidget t m) => Model t -> m (Config t)
view model' = do
  c1 <- viewIngredients model'
  c2 <- do
    el "br" blank
    el "h1" $ text "How many ingredients for your pizza?"
    viewCountPicker model'

  ingredients' <- chooseIngredients model'
  el "div" $ do
    el "h1" $ text "Your pizza ingredients"
  el "ul" $ do
    void $ dyn $ viewAllChosen <$> ingredients'
  pure $ c1 <> c2
  where
    viewAllChosen :: [Dynamic t Text] -> m ()
    viewAllChosen = traverse_ viewChosen

    viewChosen :: Dynamic t Text -> m ()
    viewChosen = el "li" . dynText


controller :: forall t m. (MonadWidget t m) => m ()
controller = void . mfix $ view <=< makeModel

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 css) $ el "div" controller

foldp :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> b) -> b -> Event t a -> m (Behavior t b)
foldp = accumB . flip

css :: Text
css =
  "\
   \h1 { font-size: 14pt;}\
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

onChangeIngredientCount :: Lens' (Config t) (Event t Int)
onChangeIngredientCount f config' = (\onChangeIngredientCount' -> config' { _onChangeIngredientCount = onChangeIngredientCount' }) <$> f (_onChangeIngredientCount config')


-- Lenses for Model t:

ingredients :: Lens' (Model t) (Dynamic t (Map IngredientId (Ingredient t)))
ingredients f model' = (\ingredients' -> model' { _ingredients = ingredients' }) <$> f (_ingredients model')

ingredientCount :: Lens' (Model t) (Dynamic t Int)
ingredientCount f model' = (\ingredientCount' -> model' { _ingredientCount = ingredientCount' }) <$> f (_ingredientCount model')

nextIngredientId :: Lens' (Model t) (Behavior t IngredientId)
nextIngredientId f model' = (\nextIngredientId' -> model' { _nextIngredientId = nextIngredientId' }) <$> f (_nextIngredientId model')


