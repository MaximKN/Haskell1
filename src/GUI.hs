module GUI where

import Graphics.UI.Gtk
import Expr
import Lit
import Parsing

main :: IO ()      
main = do
  initGUI
  window  <- windowNew
  set window [windowTitle := "Calculator", containerBorderWidth := 5,
              windowDefaultWidth := 400, windowDefaultHeight := 500]
  table   <- tableNew 5 4 True
  containerAdd window table

  txtstack <- labelNew (Nothing::Maybe String)
  tableAttachDefaults table txtstack 0 4 0 1

  clearButton <- buttonNewWithLabel "C"
  onClicked clearButton (clearLabel txtstack)
  tableAttachDefaults table clearButton 1 2 4 5

  equalsButton <- buttonNewWithLabel "="
  onClicked equalsButton (equalsHandler txtstack)
  tableAttachDefaults table equalsButton 2 3 4 5

  -- Digits

  button0 <- buttonNewWithLabel "0"
  onClicked button0 (switchOpNum button0 txtstack)
  tableAttachDefaults table button0 0 1 4 5

  button1 <- buttonNewWithLabel "1"
  onClicked button1 (switchOpNum button1 txtstack)
  tableAttachDefaults table button1 0 1 1 2

  button2 <- buttonNewWithLabel "2"
  onClicked button2 (switchOpNum button2 txtstack)
  tableAttachDefaults table button2 1 2 1 2

  button3 <- buttonNewWithLabel "3"
  onClicked button3 (switchOpNum button3 txtstack)
  tableAttachDefaults table button3 2 3 1 2

  button4 <- buttonNewWithLabel "4"
  onClicked button4 (switchOpNum button4 txtstack)
  tableAttachDefaults table button4 0 1 2 3

  button5 <- buttonNewWithLabel "5"
  onClicked button5 (switchOpNum button5 txtstack)
  tableAttachDefaults table button5 1 2 2 3

  button6 <- buttonNewWithLabel "6"
  onClicked button6 (switchOpNum button6 txtstack)
  tableAttachDefaults table button6 2 3 2 3
  
  button7 <- buttonNewWithLabel "7"
  onClicked button7 (switchOpNum button7 txtstack)
  tableAttachDefaults table button7 0 1 3 4

  button8 <- buttonNewWithLabel "8"
  onClicked button8 (switchOpNum button8 txtstack)
  tableAttachDefaults table button8 1 2 3 4

  button9 <- buttonNewWithLabel "9"
  onClicked button9 (switchOpNum button9 txtstack)
  tableAttachDefaults table button9 2 3 3 4


  -- Operations
  addButton <- buttonNewWithLabel "+"
  onClicked addButton (switchOpNum addButton txtstack)
  tableAttachDefaults table addButton 3 4 1 2

  subtrButton <- buttonNewWithLabel "-"
  onClicked subtrButton (switchOpNum subtrButton txtstack)
  tableAttachDefaults table subtrButton 3 4 2 3

  multButton <- buttonNewWithLabel "*"
  onClicked multButton (switchOpNum multButton txtstack)
  tableAttachDefaults table multButton 3 4 3 4

  divButton <- buttonNewWithLabel "/"
  onClicked divButton (switchOpNum divButton txtstack)
  tableAttachDefaults table divButton 3 4 4 5

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI


-- Event handlers

-- This function takes the text from the button and put it on the label
switchOpNum :: Button -> Label -> IO ()
switchOpNum b l = do label <- get b buttonLabel
                     strin <- labelGetLabel l
                     -- Due to bug in the recent Gtk2Hs version, those labels has to be cased to string
                     labelSetText l ((strin::String) ++ (label::String))  

-- Clears the given label
clearLabel :: Label -> IO ()
clearLabel l = labelSetText l ""

-- Evaluates the expression, converts it to the int, and puts it on the label
equalsHandler :: Label -> IO ()
equalsHandler l = 
  do txt <- labelGetLabel l
     clearLabel l
     case parse pCommand txt of
       [(cmd, "")] -> 
        case cmd of 
          Eval e -> labelSetText l ((txt::String) ++ " = " ++ (showLit $ fromRight $ eval Empty e))
          _ -> labelSetText l ""
















