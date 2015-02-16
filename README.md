=====================================
Calculator
Version: 1.0
Release date: 16 Feb 2015
=====================================

The PC application for the School of Computer Science CS2006.
The aim of this project is to develop a calculator program, 
which allows the user to enter arithmetic operations interactively using the keyboard, 
and immediately display the value of such expressions on the screen. 
The application should also provide support for variable assignment, error handling and command history. 
To combine the notion of modeling interactive programs as pure functions with the required side effects, 
we will use Haskell approach of using a new type along with primitives.

=====================================
DESCRIPTION
=====================================

“Calculator” is the interactive Haskell application.
It supports: 1. all arithmetic operations
             2. access command history by "!_numOfCmd"
			 3. variables and functions
			 4. abs, mod and power
			 5. printing, looping anв simplify
			 

=====================================
RUNNING INSTRUCTIONS
=====================================

First of all, you need to initialize sandbox by typing in a command prompt:
	cabal sandbox init
Then, configure this project:
	cabal configure
After that, you need do build it:
	cabal build
This will generate an executable file that can be accessed in:
	./dist/build/Haskell1/Haskell1
The GUI is not included in the configuration file. However, it can be run by GHCI 
	ghci GUI.hs
	
This project uses GUI library called Gtk2Hs.
It should be in the sandbox, BUT
if you have any problems with it, you can download it from:
	http://projects.haskell.org/gtk2hs/
 
=====================================
MINIMUM REQUIREMENTS
=====================================

Processor: Intel Celeron
RAM: 200 Mb
Mouse and keyboard
Haskell platform
Gtk2Hs library

=====================================
VERSION HISTORY
=====================================

[1.0 - Initial release]


