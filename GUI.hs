import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window  <- windowNew
  set window [windowTitle := "Calculator", containerBorderWidth := 5,
              windowDefaultWidth := 400, windowDefaultHeight := 500]
  table   <- tableNew 6 4 True
  containerAdd window table

  txtstack <- labelNew (Nothing::Maybe String)
  tableAttachDefaults table txtstack 0 4 0 1
  --id <- statusbarGetContextId txtstack "Line"
  --onBufferChanged buf $ do cn <- textBufferInsert buf
  --                         putStrLn (show cn)   


  dotButton <- buttonNewWithLabel "."
  onClicked dotButton (buttonSwitch dotButton txtstack)
  tableAttachDefaults table dotButton 1 2 4 5

  equalsButton <- buttonNewWithLabel "="
  onClicked equalsButton (buttonSwitch equalsButton txtstack)
  tableAttachDefaults table equalsButton 2 3 4 5

  -- Digits

  button0 <- buttonNewWithLabel "0"
  onClicked button0 (buttonSwitch button0 txtstack)
  tableAttachDefaults table button0 0 1 4 5

  button1 <- buttonNewWithLabel "1"
  onClicked button1 (buttonSwitch button1 txtstack)
  tableAttachDefaults table button1 0 1 1 2

  button2 <- buttonNewWithLabel "2"
  onClicked button2 (buttonSwitch button2 txtstack)
  tableAttachDefaults table button2 1 2 1 2

  button3 <- buttonNewWithLabel "3"
  onClicked button3 (buttonSwitch button3 txtstack)
  tableAttachDefaults table button3 2 3 1 2

  button4 <- buttonNewWithLabel "4"
  onClicked button4 (buttonSwitch button4 txtstack)
  tableAttachDefaults table button4 0 1 2 3

  button5 <- buttonNewWithLabel "5"
  onClicked button5 (buttonSwitch button5 txtstack)
  tableAttachDefaults table button5 1 2 2 3

  button6 <- buttonNewWithLabel "6"
  onClicked button6 (buttonSwitch button6 txtstack)
  tableAttachDefaults table button6 2 3 2 3
  
  button7 <- buttonNewWithLabel "7"
  onClicked button7 (buttonSwitch button7 txtstack)
  tableAttachDefaults table button7 0 1 3 4

  button8 <- buttonNewWithLabel "8"
  onClicked button8 (buttonSwitch button8 txtstack)
  tableAttachDefaults table button8 1 2 3 4

  button9 <- buttonNewWithLabel "9"
  onClicked button9 (buttonSwitch button9 txtstack)
  tableAttachDefaults table button9 2 3 3 4


  -- Operations
  addButton <- buttonNewWithLabel "+"
  onClicked addButton (buttonSwitch addButton txtstack)
  tableAttachDefaults table addButton 3 4 1 2

  subtrButton <- buttonNewWithLabel "-"
  onClicked subtrButton (buttonSwitch subtrButton txtstack)
  tableAttachDefaults table subtrButton 3 4 2 3

  multButton <- buttonNewWithLabel "*"
  onClicked multButton (buttonSwitch multButton txtstack)
  tableAttachDefaults table multButton 3 4 3 4

  divButton <- buttonNewWithLabel "/"
  onClicked divButton (buttonSwitch divButton txtstack)
  tableAttachDefaults table divButton 3 4 4 5

  -- Quit
  quitButton <- buttonNewWithLabel "Quit"
  onClicked quitButton mainQuit
  tableAttachDefaults table quitButton 0 2 5 6

  --quitButton <- colorButtonNewWithColor
  --onClicked quitButton mainQuit
  --tableAttachDefaults table quitButton 0 2 4 5

  -- Enter
  enterButton <- buttonNewWithLabel "Enter"
  onClicked enterButton mainQuit
  tableAttachDefaults table enterButton 2 4 5 6

  onDestroy window mainQuit
  widgetShowAll window
  mainGUI



buttonSwitch :: Button -> Label -> IO ()
buttonSwitch b stk = do label <- get b buttonLabel
                        txt1 <- labelGetLabel stk
                        labelSetText stk (label::String) 
                        --putStrLn $ show $ txt1
















