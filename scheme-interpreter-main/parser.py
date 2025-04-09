from ply.lex import lex
from ply.yacc import yacc


class Exps:
    "PROGRAM node."
    __match_args__ = ('exps',)
    def __init__(self, exps):
        self.exps = exps

    def __repr__(self):
        return f'(Exps {self.exps})'
    
class If:
    "IF node."
    __match_args__ = ('cnd', 'thn', 'els')
    def __init__(self, cnd, thn, els):
        self.cnd = cnd
        self.thn = thn
        self.els = els

    def __repr__(self):
        return f'(IF (Condition {self.cnd}) (Then {self.thn}) (Else {self.thn}))'

class Application:
    "LIST node."
    __match_args__ = ('exps',)
    def __init__(self, exps):
        self.exps = exps

    def __repr__(self):
        return f'(Application {self.exps})'

class Var:
    "ATOM node."
    __match_args__ = ('var',)
    def __init__(self, var):
        self.var= var

    def __repr__(self):
        return f'(Var {self.var})'

class Op:
    "ATOM node."
    __match_args__ = ('op',)
    def __init__(self, op):
        self.op = op

    def __repr__(self):
        return f'(Op {self.op})'

class Bool:
    "ATOM node."
    __match_args__ = ('b',)
    __match_args__ = ('b')
    def __init__(self, b):
        self.b = b

    def __repr__(self):
        return f'(Bool {self.b})'

class Int:
    "INT node."
    __match_args__ = ('num',)
    def __init__(self, num):
        self.num = num

    def __repr__(self):
        return f'(Int {self.num})'

class Let:
    "LET node."
    __match_args__ = ('bindings', 'body')
    def __init__(self, bindings, body):
        self.bindings = bindings
        self.body = body

    def __repr__(self):
        return f'(Let (Binding {self.bindings}) (Body {self.body}))'

class Begin:
    __match_args__ = ('exps',)
    def __init__(self, exps):
        self.exps = exps

    def __repr__(self):
        return f'(Begin {self.exps} )'

class Binding:
    "BINDING node."
    __match_args__ = ('var', 'exp')
    def __init__(self, var, exp):
        self.var = var
        self.exp = exp 

    def __repr__(self):
        return f'(Binding {self.var} {self.exp})'

class SetBang:
    __match_args__ = ('var', 'exp')
    def __init__(self, var, exp):
        self.var = var
        self.exp = exp
    def __repr__(self):
        return f'(SetBang {self.var} {self.exp} )'

class Define:
    "LET node."
    __match_args__ = ('var', 'exp')
    def __init__(self, var, exp):
        self.var = var
        self.exp = exp

    def __repr__(self):
        return f'(Define (Binding {self.var}) (Body {self.exp}))'

class Lambda:
    "LET node."
    __match_args__ = ('params', 'body')
    def __init__(self, params, body):
        self.params = params
        self.body = body

    def __repr__(self):
        return f'(lambda ({self.params}) (Body {self.body}))'

class Prim:
    __match_args__ = ('op', 'e', 'e2')
    def __init__(self, op, e, e2):
        self.op = op
        self.e = e
        self.e2 = e2
    def __repr__(self):
        return f'(Prim {self.op} {self.e} {self.e2})'
    

reserved = {
    'if' : 'IF',
    'set': 'SET',
    'or' : 'OR',
    'and' : 'AND',
    'let' : 'LET',
    'begin' : 'BEGIN',
    'define': 'DEFINE',
    'lambda': 'LAMBDA'}

# All tokens must be named in advance.
tokens = [ 'PLUS', 'MINUS', 'TIMES', 'LPAREN', 'RPAREN',
           'NAME', 'NUMBER', 'EQ', 'TRUE', 'FALSE', 'LESS', 'GREATER'] + list(reserved.values())

# Ignored characters
t_ignore = ' \t'

# Token matching rules are written as regexs
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_LPAREN = r'\('
t_RPAREN = r'\)'
#t_AND = r'and'
#t_OR = r'or'
t_EQ = r'='
#t_IF = r'if'
t_TRUE = r'\#t'
t_FALSE = r'\#f'
#t_LET = r'let'
#t_SET = r'set'
#t_BEGIN = r'begin'
#t_DEFINE = r'define'
#t_LAMBDA = r'lambda'
#
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'NAME')
    return t

# A function can be used if there is an associated action.
# Write the matching regex in the docstring.
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Ignored token with an action associated with it
def t_ignore_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')

# Error handler for illegal characters
def t_error(t):
    print(f'Illegal character {t.value[0]!r}')
    t.lexer.skip(1)

# Build the lexer object
lexer = lex()

def p_expressions(p):
    '''
    expressions : expressions expression
                | expression 
    '''
    if len(p) == 2:  
        p[0] = Exps([p[1]])
    else:  
        p[0] = Exps(p[1].exps + [p[2]])
        
# --- Parser
def p_expression(p):
    '''
    expression : Prim
               | Bool
               | IfExp
               | Int
               | Var
               | LetExp
               | SetExp
               | BeginExp
               | DefineExp
               | LambdaExp
               | Application
    '''
    p[0] = p[1]

def p_prim(p):
    '''
    Prim : LPAREN Op expression expression RPAREN
    '''
    p[0] = Prim(p[2], p[3], p[4])

def p_set(p):
    '''
    SetExp : LPAREN SET expression expression RPAREN
    '''
    p[0] = SetBang(p[3], p[4])
    
def p_bool(p):
    '''
    Bool : TRUE
         | FALSE
    '''
    p[0] = Bool(p[1])

def p_if_exp(p):
    '''
    IfExp : LPAREN IF expression expression expression RPAREN
    '''
    p[0] = If(p[3], p[4], p[5])

def p_int(p):
    '''
    Int : NUMBER
    '''
    p[0] = Int(p[1])

def p_var(p):
    '''
    Var : NAME
    '''
    p[0] = Var(p[1])

def p_let_exp(p):
    '''
    LetExp : LPAREN LET LPAREN binding RPAREN expression RPAREN
    '''
    p[0] = Let(p[4], p[6])

def p_binding(p):
    '''
    binding : LPAREN Var expression RPAREN
    '''
    p[0] = Binding(p[2], p[3])

def p_op(p):
    '''
    Op : AND
       | OR
       | PLUS
       | MINUS
       | TIMES
       | EQ
    '''
    p[0] = Op(p[1])

def p_begin(p):
    '''
    BeginExp : LPAREN BEGIN expressions RPAREN
    '''
    p[0] = Begin(p[3])

def p_lambda(p):
    '''
    LambdaExp : LPAREN LAMBDA LPAREN params RPAREN expression RPAREN
    '''
    p[0] = Lambda(p[4], p[6])

def p_params(p):
    '''
    params : Var params
           | Var 
    '''
    if len(p) == 3:
        params = list()
        if isinstance(p[2], list):
            p[0] = [p[1]] + p[2]
        else:
            params.append(p[1])
            params.append(p[2])
            p[0] = params 
    else:
        params = list()
        params.append(p[1])
        p[0] = params
        

def p_define(p):
    '''
    DefineExp : LPAREN DEFINE expression expression RPAREN
    '''
    p[0] = Define(p[3], p[4])

def p_application(p):
    '''
    Application : LPAREN expressions RPAREN
    '''
    p[0] = Application(p[2])
    
def p_error(p):
    print(f'Syntax error at {p.value!r}')

parser = yacc()


