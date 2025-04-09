theory Exp imports Main begin
type_synonym vname = string
datatype exp = 
   Num int 
   | Var vname
   | If exp exp exp
   | If exp exp
   | Let vname exp exp
   | Bool True
   | Bool False
   | And exp exp
   | Or exp exp
   | When exp exp
   | Unless exp exp
   | Eqv exp exp
   | Lambda vname exp 
   | Quote exp

fun eval :: exp \<rightarrow> state \<rightarrow> exp
eval (Num n) s = n |
eval (Var v) s = s v |
eval (If cnd thn els) s =
if (eval cnd s) = Bool True 
  then (eval thn s) 
  else (eval els s)
eval (Bool b) s = b
eval (And (Bool True) (Bool True)) s = (Bool True)
eval (And (Bool _) (Bool _)) s = (Bool False)
eval (And e e2) s =
if (eval e s) = (Bool True) then
    if (eval e2 s) = True 
  then Bool True else (Bool False)
else (Bool False)
eval (When e e2) s =
if (eval e s) = Bool True then eval e2 s else else Bool False
eval (Unless e e2) s = if (eval e s) = Bool False then eval e2 s else (Bool False)
eval (Eqv e e2) s = if (eval e s) = (eval e2 s) then Bool True else Bool False
eval (Quote e) s = e