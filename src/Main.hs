module Main where

-- AST definition
data Exp = Var String
         | Lam String Exp
         | Ap Exp Exp

-- Helper function: add surrounding parens on a string
param :: String -> String
param str = "(" ++ str ++ ")"
space str str' = str ++ " " ++ str'

-- Pretty-print an expression
toString :: Exp -> String
toString (Var x) = x
toString (Lam x exp) = "λ" ++ x ++ ". " ++ (toString exp)

toString (Ap (Var x) (Var y)) = x `space` (toString (Var y))
toString (Ap (Var x) (Lam y exp)) = x `space` (param (toString (Lam y exp)))
toString (Ap (Var x) (Ap fun arg)) = x `space` (param (toString (Ap fun arg)))

toString (Ap (Lam x exp) (Var y)) = (param (toString (Lam x exp))) `space` (toString (Var y))
toString (Ap (Lam x exp) (Lam x' exp')) = (param (toString (Lam x exp))) `space` (param (toString (Lam x' exp')))
toString (Ap (Lam x exp) (Ap fun arg)) = (param (toString (Lam x exp))) `space` (param (toString (Ap fun arg)))

toString (Ap (Ap fun arg) (Var x)) = (toString (Ap fun arg)) `space` (toString (Var x))
toString (Ap (Ap fun arg) (Lam x exp)) = (toString (Ap fun arg)) `space` (param (toString (Lam x exp)))
toString (Ap (Ap fun arg) (Ap fun' arg')) = (toString (Ap fun arg)) `space` (param (toString (Ap fun' arg')))

-- checkFV vars exp - check whether every variable in `exp` is from `vars`
checkFV :: [String] -> Exp -> Bool
checkFV vars (Var x) = elem x vars
checkFV vars (Lam x exp) = checkFV (x : vars) exp
checkFV vars (Ap fun arg) = (checkFV vars fun) && (checkFV vars arg)

-- Substitution: subst varName exp target = [exp / varName] target
-- Requirement: exp doesn't have any free variable.
subst :: String -> Exp -> Exp -> Exp
subst varName exp (Var x) =
    if varName == x
    then exp
    else (Var x)

subst varName exp (Lam argName body) =
    if varName == argName
    then (Lam argName body)
    else (Lam argName (subst varName exp body))

subst varName exp (Ap fun arg) =
    (Ap (subst varName exp fun) (subst varName exp arg))

-- Evaluate an expression
eval :: Exp -> Maybe Exp
eval (Var x) = Nothing
eval (Lam x exp) = if checkFV [] (Lam x exp)
                   then Just (Lam x exp)
                   else Nothing
eval (Ap fun arg) =
    if (checkFV [] fun) && (checkFV [] arg)
    then let funMaybe = eval fun
             argMaybe = eval arg
         in case (funMaybe, argMaybe) of
                (Just (Lam x exp), Just arg') -> eval (subst x arg' exp)
                _ -> Nothing
    else Nothing

-- evalAndPrint program - print the program and the evaluation
evalAndPrint :: Exp -> IO ()
evalAndPrint program = do
    putStr (toString program)
    putStr " |-> "
    case eval program of
        Just p -> putStrLn (toString p)
        _ -> putStrLn "error"

-- Example program: λx. λy. x y
program = Lam "x" (Lam "y" (Ap (Var "x") (Var "y")))

-- Example program: (λy. (λx. x) y) ((λu. u) (λv. v))
program' = (Ap (Lam "y" (Ap (Lam "x" (Var "x"))
                            (Var "y")))
               (Ap (Lam "u" (Var "u"))
                   (Lam "v" (Var "v"))))

-- Example program: λx. x y
program'' = Lam "x" (Ap (Var "x") (Var "y"))

main :: IO ()
main = do
    evalAndPrint program
    evalAndPrint program'
    evalAndPrint program''
