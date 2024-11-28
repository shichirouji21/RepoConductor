module Static (green, yellow, red, reset, cursive, clearScreen,
               header, mainOptions, filterOptions, actionOptions, goodbye) where

green, yellow, red, reset, cursive, clearScreen :: String
green         = "\ESC[32m"
yellow        = "\ESC[33m"
red           = "\ESC[31m"
reset         = "\ESC[0m"
cursive       = "\ESC[3m"
clearScreen   = "\ESC[2J\ESC[H"

header, mainOptions, filterOptions, actionOptions, goodbye :: String
header        = "[RepoConductor] (v0.1.0)"
mainOptions   = "\n(F)ilter, (A)ction, (U)pdate, (T)ag, (Q)uit\n"
filterOptions = "(A)ll, (D)irty, (N)ame, (B)ranch, (T)ag"
actionOptions = "(F)etch, (P)ull, (S)witch, (C)ommit & Push, (L)azygit, (M)anual"
goodbye       = "Thank you for using RepoConductor.\n" 
