type Id = String
type Numero = Double

-- Termo: representação das expressões
data Termo = Var Id
           | Lit Numero
           | Som Termo Termo
           | Lam Id Termo
           | Apl Termo Termo
           | Atr Id Termo
           | Seq Termo Termo
           | Skip                      -- Comando vazio
           | Bol Bool                  -- Literal booleano
           | Ig Termo Termo            -- Igualdade (==)
           | Menor Termo Termo         -- Comparação (<)
           | And Termo Termo           -- Operador AND (&&)
           | Not Termo                 -- Operador NOT (!)
           | Iff Termo Termo Termo     -- If-Else: cond, then, else
           | Whi Termo Termo           -- While: cond, corpo
           | Cla Id Termo              -- Declaração de classe
           | NewC Id                   -- Instanciação (new)
           | Acc Termo Id              -- Acesso a campo (obj.campo)
           | Set Termo Id Termo        -- Atribuição a campo (obj.campo = valor)

-- Valor: resultados da avaliação
data Valor = Num Double
           | Bol Bool
           | Fun (Valor -> Estado -> (Valor, Estado))
           | Unit
           | Obj Estado                -- Objeto: estado de campos
           | ClaEnv Termo              -- Classe: corpo de inicialização
           | Erro

type Ambiente = [(Id, Valor)]          -- Variáveis locais/globais
type Estado = [(Id, Valor)]            -- Estado mutável (armazenamento)
type ClassEnv = [(Id, Termo)]          -- Ambiente de classes

-- Função de interpretação principal
int :: ClassEnv -> Ambiente -> Termo -> Estado -> (Valor, Estado)
int _ _ (Lit n) e = (Num n, e)
int _ _ (Bol b) e = (Bol b, e)
int _ _ Skip e = (Unit, e)

int c a (Var x) e = (search x (a ++ e), e)

int c a (Som t u) e = (somaVal v1 v2, e2)
  where (v1, e1) = int c a t e
        (v2, e2) = int c a u e1

int c a (Lam x t) e = (Fun (\v -> int c ((x, v) : a) t), e)

int c a (Apl t u) e = app v1 v2 e2
  where (v1, e1) = int c a t e
        (v2, e2) = int c a u e1

int c a (Atr x t) e = (v1, wr (x, v1) e1)
  where (v1, e1) = int c a t e

int c a (Seq t u) e = int c a u e1
  where (_, e1) = int c a t e

-- Novos operadores e estruturas
int c a (Ig t u) e =
  let (v1, e1) = int c a t e
      (v2, e2) = int c a u e1
  in case (v1, v2) of
       (Num x, Num y)  -> (Bol (x == y), e2)
       (Bol x, Bol y)  -> (Bol (x == y), e2)
       (Unit, Unit)    -> (Bol True, e2)
       _               -> (Erro, e2)

int c a (Menor t u) e =
  let (v1, e1) = int c a t e
      (v2, e2) = int c a u e1
  in case (v1, v2) of
       (Num x, Num y) -> (Bol (x < y), e2)
       _               -> (Erro, e2)

int c a (And t u) e =
  let (v1, e1) = int c a t e
  in case v1 of
       Bol False -> (Bol False, e1)
       Bol True  -> 
         let (v2, e2) = int c a u e1
         in case v2 of
              Bol b -> (Bol b, e2)
              _     -> (Erro, e2)
       _ -> (Erro, e1)

int c a (Not t) e =
  let (v, e1) = int c a t e
  in case v of
       Bol b -> (Bol (not b), e1)
       _     -> (Erro, e1)

int c a (Iff cond t1 t2) e =
  let (v, e1) = int c a cond e
  in case v of
       Bol True  -> int c a t1 e1
       Bol False -> int c a t2 e1
       _         -> (Erro, e1)

int c a (Whi cond body) e =
  let (v, e1) = int c a cond e
  in case v of
       Bol True  -> 
         let (_, e2) = int c a body e1
         in int c a (Whi cond body) e2
       Bol False -> (Unit, e1)
       _         -> (Erro, e1)

-- Classes e objetos
int c a (Cla nome corpo) e = (Unit, e)  -- Apenas adiciona ao ClassEnv (tratado externamente)

int c a (NewC nome) e =
  case lookup nome c of
    Just corpo -> 
      let novoObjeto = []  -- Estado inicial vazio
          (_, estadoObj) = int c [("this", Obj novoObjeto)] corpo novoObjeto
      in (Obj estadoObj, e)
    Nothing -> (Erro, e)

int c a (Acc obj campo) e =
  let (v, e1) = int c a obj e
  in case v of
       Obj campos -> (search campo campos, e1)
       _          -> (Erro, e1)

int c a (Set obj campo valor) e =
  let (vObj, e1) = int c a obj e
      (vVal, e2) = int c a valor e1
  in case vObj of
       Obj campos -> 
         let novosCampos = wr (campo, vVal) campos
         in (vVal, e2)  -- Retorna o valor atribuído
       _ -> (Erro, e2)

-- Funções auxiliares
search :: Id -> [(Id, Valor)] -> Valor
search i [] = Erro
search i ((j, v) : l) = if i == j then v else search i l

somaVal :: Valor -> Valor -> Valor
somaVal (Num x) (Num y) = Num (x + y)
somaVal _ _ = Erro

app :: Valor -> Valor -> Estado -> (Valor, Estado)
app (Fun f) v e = f v e
app _ _ e = (Erro, e)

wr :: (Id, Valor) -> Estado -> Estado
wr (i, v) [] = [(i, v)]
wr (i, v) ((j, u) : l) = 
  if i == j 
    then (j, v) : l 
    else (j, u) : wr (i, v) l

-- Função para executar um termo (adiciona suporte a ClassEnv)
at :: ClassEnv -> Termo -> (Valor, Estado)
at c t = int c [] t []

-- Instância de Show para Valor
instance Show Valor where
  show (Num x) = show x
  show (Bol True) = "true"
  show (Bol False) = "false"
  show (Fun _) = "Funcao"
  show Unit = "()"
  show (Obj campos) = "Objeto" ++ show campos
  show (ClaEnv _) = "Classe"
  show Erro = "Erro"
