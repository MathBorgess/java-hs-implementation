type Id = String
type Numero = Double

data Termo = Var Id
           | Lit Numero
           | Som Termo Termo
           | Lam Id Termo
           | Apl Termo Termo
           | Atr Id Termo
           | Seq Termo Termo

{-termo1 = (Apl (Lam "x" (Som (Var "x") (Lit 2))) (Lit 3))

termo2 = (Apl (Lam "x" (Som (Var "x") (Var "y"))) (Lit 3))

termo3 = (Seq (Atr "y" termo2) termo2)

sq1 = (Seq (Atr "y" (Lit 3)) termo2)

sq2 = (Seq (Atr "y" (Lit 3)) termo3)

sq3 = (Seq (Atr "y" (Som (Atr "z" (Lit 5)) (Var "z"))) termo3)-}

data Valor = Num Double
           | Fun (Valor -> Estado -> (Valor,Estado))
           | Erro

type Ambiente = [(Id,Valor)]
type Estado = [(Id,Valor)]

int :: Ambiente -> Termo -> Estado -> (Valor, Estado)

int a (Var x) e = (search x (a ++ e), e)

int a (Lit n) e = (Num n, e)

int a (Som t u) e = (somaVal v1 v2, e2)
                    where (v1,e1) = int a t e
                          (v2,e2) = int a u e1

int a (Lam x t) e = (Fun (\v -> int ((x,v):a) t), e)

int a (Apl t u) e = app v1 v2 e2
                    where (v1,e1) = int a t e
                          (v2,e2) = int a u e1

int a (Atr x t) e = (v1, wr (x,v1) e1)
                    where (v1,e1) = int a t e

int a (Seq t u) e = int a u e1
                    where (_,e1) = int a t e

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

somaVal (Num x) (Num y) = Num (x+y)
somaVal _ _ = Erro

app (Fun f) v e = f v e
app _ _ e = (Erro, e)

wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)

at t = int [] t []

instance Show Valor where
   show (Num x) = show x
   show Erro = "Erro"
   show (Fun f) = "Função"
