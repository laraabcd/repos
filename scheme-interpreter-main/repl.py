from parser import parser
from interpreter import interp

# the repl was taken from Peter Norvig's Scheme interpreter
def repl(prompt='lis.py> '):
    while True:
        val = interp(parser.parse(input(prompt)))
        if val is not None: 
            print(lispstr(val))

def lispstr(exp):
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')' 
    else:
        return str(exp)

repl()
