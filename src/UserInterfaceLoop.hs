module UserInterfaceLoop
  ( mainLoop
  )
where

import qualified Console
import           DinnerDomain
import           DinnerService
import           PricingDomain
import           PricingService

mainLoop :: [Dinner] -> [Pricing] -> IO ()
mainLoop dinners pricings = do
  Console.log "------- Witaj w naszej aplikacji ---------\n"
  Console.log "Znane posiłki :\n"
  logKnownMeals dinners
  Console.log
    "\nPodaj listę posiłków (oddzielonych spacją - np. 1 4 2), lub początki nazw potraw (oddzielone spacją - np. Pał Tort Sała) z których chcesz stworzyć listę zakupów:"
  userInput <- Console.read

  let selectedDinners = findDinnersSelectedByUser userInput dinners
      maxMealLength = findMaxIngredientNameLength selectedDinners
      requiredIngredients = findRequiredIngredients selectedDinners

  Console.log "\nWybrane posiłki:"
  mapM_ (Console.log . meal) selectedDinners
  Console.log "\nNiezbędne składniki:"
  mapM_ (Console.log . (detailedInfo maxMealLength)) requiredIngredients

  Console.log
    $  "\nPrzewidywana cena: "
    ++ (show $ findPricingsForIngredients pricings requiredIngredients)

logKnownMeals :: [Dinner] -> IO ()
logKnownMeals dinners = Console.log $ orderedMeals dinners

findMaxIngredientNameLength :: [Dinner] -> Int
findMaxIngredientNameLength = (+ 1) . maximum . map length . concatMap getIngrediensNames