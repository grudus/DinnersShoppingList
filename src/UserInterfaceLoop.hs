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
  Console.log $ mealsWithIndex dinners
  Console.log
    "\nPodaj listę posiłków (oddzielonych spacją - np. 1 4 2), lub początki nazw potraw (oddzielone spacją - np. Pał Tort Sała) z których chcesz stworzyć listę zakupów:"
  userInput <- Console.read

  let selectedDinners     = findDinnersSelectedByUser userInput dinners
      maxNameLength       = findMaxIngredientNameLength selectedDinners
      requiredIngredients = findRequiredIngredients selectedDinners

  Console.log "\nWybrane posiłki:"
  mapM_ (Console.log . meal) selectedDinners
  Console.log "\nNiezbędne składniki:"
  mapM_ (Console.log . flip detailedInfo maxNameLength) requiredIngredients

  Console.log
    $  "\nPrzewidywana cena: "
    ++ (show $ calculateTotalPrice pricings requiredIngredients)