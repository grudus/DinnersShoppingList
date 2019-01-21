module UserInterfaceLoop
    ( mainLoop
    )
where

import qualified Console as Console
import DinnerDomain
import DinnerService

mainLoop :: [Dinner] -> IO ()
mainLoop dinners = do
    Console.log "------- Witaj w naszej aplikacji ---------\n"
    Console.log "Znane posiłki :\n"
    logKnownMeals dinners
    Console.log "\nPodaj listę posiłków (oddzielonych spacją - np. 1 4 2), z których chcesz stworzyć listę zakupów:"
    userInput <- Console.read

    let selectedDinners = findDinnersSelectedByUser userInput dinners
        in do
        Console.log "\nPodałeś:"
        mapM_ (Console.log . meal) selectedDinners
        Console.log "\nNiezbędne składniki:"
        mapM_ (Console.log . name) $ sumDuplicatedIngredients selectedDinners



logKnownMeals :: [Dinner] -> IO ()
logKnownMeals dinners =
    Console.log $ orderedMeals dinners