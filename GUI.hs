{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{- cabal:
build-depends: base, text, haskell-gi-base, gi-gtk == 3.0.*
-}
module Main where
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified GI.Gtk as Gtk
import Data.GI.Base.Signals
import Data.Text.Internal
import GI.Gtk.Enums (WindowType(..))

-- TYPES --

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data GameResult = PlayerWin State
                | ComputerWin State
                | Tie State
                | InProgress State 
          deriving (Eq, Show)
data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

data Action = Action Int                 -- a move for a player is just an Int
         deriving (Ord,Eq)
type InternalState = ([Action],[Action])   -- (self,other)

main :: IO ()
main = do
  st <- newIORef magicsum_start
  Gtk.init Nothing
  window <- Gtk.windowNew WindowTypeToplevel
  Gtk.windowSetResizable window False
  Gtk.setWindowDefaultWidth window 300
  Gtk.setWindowDefaultHeight window 500
  Gtk.onWidgetDestroy window Gtk.mainQuit
  
  Gtk.setWindowTitle window "Ultimate Tic Tac Toe"

  display <- Gtk.entryNew
  Gtk.set display [ Gtk.entryEditable Gtk.:= False
              , Gtk.entryXalign   Gtk.:= 1 -- makes contents right-aligned
              , Gtk.entryText     Gtk.:= "Make a Move!" ]

  grid <- Gtk.gridNew
  Gtk.gridSetColumnHomogeneous grid True
  Gtk.gridSetRowHomogeneous grid True 
  let attach x y w h item = Gtk.gridAttach grid item x y w h 
      
  button1 <- createButton ""
  button2 <- createButton ""
  button3 <- createButton ""
  button4 <- createButton ""
  button5 <- createButton ""
  button6 <- createButton ""
  button7 <- createButton ""
  button8 <- createButton ""
  button9 <- createButton ""
  button10 <- createButton "Reset Game"
  let updateDisplay = updateTextField display
  let buttons = [button1, button2, button3, button4, button5, button6, button7, button8, button9]
      addAction button action = addClickedAction st (mm_player magicsum) buttons updateDisplay button action

  attach 0 0 3 1 display
  attach 0 1 1 1 button2 
  attach 1 1 1 1 button7
  attach 2 1 1 1 button6
  attach 0 2 1 1 button9
  attach 1 2 1 1 button5
  attach 2 2 1 1 button1
  attach 0 3 1 1 button4
  attach 1 3 1 1 button3
  attach 2 3 1 1 button8
  attach 0 4 3 1 button10

  addAction button2 (makeNewState (Action 2)) 
  addAction button7 (makeNewState (Action 7))
  addAction button6 (makeNewState (Action 6))
  addAction button9 (makeNewState (Action 9))
  addAction button5 (makeNewState (Action 5))
  addAction button1 (makeNewState (Action 1))
  addAction button4 (makeNewState (Action 4))
  addAction button3 (makeNewState (Action 3))
  addAction button8 (makeNewState (Action 8))
  on button10 #clicked 
    (do 
      atomicModifyIORef st $ \x -> let r = magicsum_start in (r, r)
      clearButtons buttons
      Gtk.setEntryText display "Make a move!"
    )

  Gtk.setContainerChild window grid
  -- The final step is to display this newly created widget. Note that this
  -- also allocates the right amount of space to the windows and the button.
  Gtk.widgetShowAll window
  -- All Gtk+ applications must have a main loop. Control ends here
  -- and waits for an event to occur (like a key press or mouse event).
  -- This function returns if the program should finish.
  Gtk.main

createButton :: Data.Text.Internal.Text -> IO Gtk.Button 
createButton name = do
  Gtk.new Gtk.Button [ #label Gtk.:= name ]

addClickedAction :: IORef State -> Player -> [Gtk.Button] -> (GameResult -> IO ()) -> Gtk.Button -> (State -> State) -> IO Data.GI.Base.Signals.SignalHandlerId
addClickedAction st opponent buttons updateDisplay button stateChange = do
  on button #clicked (do
    state <- readIORef st
    let (State (mine, _) _) = state
    let (State (newMine, _) _) = stateChange state
    if length mine == length newMine
      then putStrLn "Button Already Clicked"
    else if not (gameInProgress (getGameResult state)) 
      then putStrLn "Game over"

    else do
      (State (mine, others) avail) <- atomicModifyIORef st $ \x -> let r = stateChange x in (r, r) -- (2)
      Gtk.setButtonLabel button "X"
      if win2 mine || length avail == 0
        then updateDisplay (getGameResult (State (mine, others) avail))
      else do
          -- Guaranteed to get an action
          let (Action num) = opponent (State (others, mine) avail)
          setO buttons (Action (num-1))
          value <- atomicModifyIORef st $ \x -> let r = makeNewStateOpponent (Action num) x in (r, r) -- (2)
          putStrLn ("Debug: "++ show value)
          updateDisplay (getGameResult value)
    )

gameInProgress :: GameResult -> Bool 
gameInProgress (PlayerWin _) = False
gameInProgress (ComputerWin _) = False    
gameInProgress (Tie _) = False    
gameInProgress (InProgress _) = True    


getGameResult :: State -> GameResult
getGameResult (State (player, computer) available)
  | win2 player = PlayerWin (State (player, computer) available)
  | win2 computer = ComputerWin (State (player, computer) available)
  | length available == 0 = Tie (State (player, computer) available)
  | otherwise = InProgress (State (player, computer) available)

updateTextField :: Gtk.Entry -> GameResult -> IO()
updateTextField display (PlayerWin state) = Gtk.setEntryText display "X (Player) Wins!"
updateTextField display (ComputerWin state) = Gtk.setEntryText display "O (CPU) Wins!"
updateTextField display (Tie state) = Gtk.setEntryText display "Tie!"
updateTextField display (InProgress state) = Gtk.setEntryText display "Make A Move! X = Player, O = CPU"

makeNewState :: Action -> State -> State
makeNewState move (State (mine,others) []) = State (mine, others) []
makeNewState move (State (mine,others) available)
  | win2 mine =  State (mine, others) available
  | win2 others = State (mine, others) available
  | move `elem` available = State (move: mine, others) [act | act <- available, act /= move]
  | otherwise = State (mine, others) available

makeNewStateOpponent :: Action -> State -> State
makeNewStateOpponent move (State (mine,others) []) = State (mine, others) []
makeNewStateOpponent move (State (mine,others) available)
  | win2 mine =  State (mine, others) available
  | win2 others = State (mine, others) available
  | move `elem` available = State (mine, move:others) [act | act <- available, act /= move]
  | otherwise = State (mine, others) available

setO :: [Gtk.Button] -> Action -> IO () 
setO (h:r) (Action 0) = do 
  Gtk.setButtonLabel h "O"
setO (h:r) (Action n) = do
  setO r (Action (n-1))

clearButtons :: [Gtk.Button] -> IO ()
clearButtons [] = do putStrLn "All buttons cleared"
clearButtons (h:r) = do
  Gtk.setButtonLabel h ""
  clearButtons r

-- MAGIC SUM --

magicsum :: Game
magicsum move (State (mine,others) available) 
    | win2 (move:mine)                = EndOfGame 1    magicsum_start   -- agent wins
    | available == [move]          = EndOfGame 0  magicsum_start     -- no more moves, tie
    | otherwise                    =
          ContinueGame (State (others,(move:mine))   -- note roles have flipped
                        [act | act <- available, act /= move])

win2 :: [Action] -> Bool
win2 actions = or [x+y+z == 15 | Action x <- actions, Action y <- actions, Action z <- actions, x/=y && y/=z && x/=z]

magicsum_start = State ([],[]) [Action n | n <- [1..9]]

instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]



------- Minimax -------

simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
simple_player (State _ avail) = head [Action e | e <- [5,6,4,2,8,1,3,7,9],
                                               Action e `elem` avail]
minimax:: Game -> State -> (Action, Double)
-- minimax game state   =>  (move,value_to_player)
-- precondition: there are some moves that are available
minimax game st  =
      argmax (valueact game st) avail
      where State _ avail = st

-- valueact game st action  is the value of doing action act in state st for game
valueact:: Game -> State -> Action -> Double
valueact game st act = value game (game act st)

-- value gameminimax magicsum result  = value  for current player after result
value:: Game -> Result -> Double
value _  (EndOfGame val _) = val
value game (ContinueGame st) =  - snd (minimax game st)   -- (action,value) for next player
                 -- value for current player is negative of value of the other player

mm_player:: Game -> Player
mm_player game state = fst ( minimax game state)

-- argmax f lst  = (e, f e) for e <- lsts where f e is maximal
-- Precondition: lst is not empty
--  Note that this does not require the elements of lst to be comparable, only value
-- like  max[(e,f e) <- e in lst] but where only the second elements of pairs are compared in the max.
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t) 
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h
   