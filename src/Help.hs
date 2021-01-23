module Help where




helpMsg = "\
\Convert a quantifier free formula to CNF form\n\
\Usage: ./tocnf [options] [input]\n\
\\n\
\No need to provide the input, if the option '-f [DIR]' is provided\n\
\\n\
\Example usage:\n\
\  ./tocnf \"(p ==> q) <=> (not p or q)\"\n\
\      - read the formula in quotes and display output in the console\n\ 
\\n\
\  ./tocnf -f input.bf -o output.bf\n\
\      - read the formula from the file input.bf and write it in output.bf\n\ 
\\n\
\Input form:\n\
\  Example: (p ==> q) <=> (not p or q)\n\
\\n\
\  Variables can be any unquoted strings beginning with a letter,\n\
\  followed by any combination of letters, digits, and the character '_'\n\
\  reserved words excluded.\n\
\\n\
\  Availible operators:\n\
\    not\n\
\    and\n\
\    or\n\
\    xor\n\
\    ==>\n\
\    <==\n\
\    <=>\n\
\\n\
\Options:\n\
\  -o [DIR]               write output to DIR\n\
\  -f [DIR]               read input from DIR\n\
\\n\
\  --human-friendly     |\n\
\  --human              |\n\
\  --friendly           | display output in human friendly form\n\
\\n\
\  --cadical (default)    display output in a form readable by the\n\
\                         'cadical' sat solver\n\
\\n\
\  --test                 test weather the program is correct\n\
\                         (evaluate and compare input and output\n\
\                         formulas for all possible variable values)\n\
\\n\
\  --help                 display help\n\
\"




