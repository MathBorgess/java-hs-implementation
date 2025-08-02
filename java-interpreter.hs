type Id = String
type Numero = Double

-- Termo: representação das expressões e comandos
data Termo
    = Var Id
    | Lit Numero
    | Som Termo Termo
    | Mul Termo Termo
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
    | Iff Termo Termo Termo     -- If-Else
    | Whi Termo Termo           -- While
    | Class Id [Id] [Id]        -- Classe: nome, atributos, métodos (simplificado)
    | New Id                    -- Cria instância da classe

-- Valor: resultados da avaliação
data Valor
    = Num Double
    | Bol Bool
    | Fun (Valor -> Estado -> (Valor, Estado))
    | Unit
    | Erro
    | Null
    | ClaDef [Id] [Id]               -- Definição de classe
    | Obj Estado                     -- Objeto (não usado no retorno final)

type Ambiente = [(Id, Valor)]        -- Variáveis e definições
type Estado = [(Id, Valor)]          -- Estado mutável
type Heap = [(Id, (Id, Estado))]     -- (objID, (nomeClasse, atributosDaInstancia))

-- Função principal de interpretação
int :: Heap -> Ambiente -> Termo -> Estado -> (Valor, Estado, Heap)

-- Literais e Skip
int h _ (Lit n) e = (Num n, e, h)
int h _ (Bol b) e = (Bol b, e, h)
int h _ Skip e    = (Unit, e, h)

-- Variáveis
int h a (Var x) e = (search x (a ++ e), e, h)

-- Soma
int h a (Som t u) e =
    let (v1, e1, h1) = int h a t e
        (v2, e2, h2) = int h1 a u e1
    in (somaVal v1 v2, e2, h2)

-- Multiplicação
int h a (Mul t u) e =
    let (v1, e1, h1) = int h a t e
        (v2, e2, h2) = int h1 a u e1
    in (multiplica v1 v2, e2, h2)

-- Lambda
int h a (Lam x t) e = (Fun (\v st -> let (res, st2, _) = int h ((x,v):a) t st in (res, st2)), e, h)

-- Aplicação
int h a (Apl t u) e =
    let (v1, e1, h1) = int h a t e
        (v2, e2, h2) = int h1 a u e1
    in app v1 v2 e2 h2

-- Atribuição
int h a (Atr x t) e =
    let (v1, e1, h1) = int h a t e
    in (v1, wr (x, v1) e1, h1)

-- Sequência
int h a (Seq t u) e =
    let (_, e1, h1) = int h a t e
    in int h1 a u e1

-- Igualdade
int h a (Ig t u) e =
    let (v1, e1, h1) = int h a t e
        (v2, e2, h2) = int h1 a u e1
    in case (v1, v2) of
        (Num x, Num y) -> (Bol (x == y), e2, h2)
        (Bol x, Bol y) -> (Bol (x == y), e2, h2)
        (Unit, Unit)   -> (Bol True, e2, h2)
        _              -> (Erro, e2, h2)

-- Menor
int h a (Menor t u) e =
    let (v1, e1, h1) = int h a t e
        (v2, e2, h2) = int h1 a u e1
    in case (v1, v2) of
        (Num x, Num y) -> (Bol (x < y), e2, h2)
        _              -> (Erro, e2, h2)

-- AND
int h a (And t u) e =
    let (v1, e1, h1) = int h a t e
    in case v1 of
        Bol False -> (Bol False, e1, h1)
        Bol True ->
            let (v2, e2, h2) = int h1 a u e1
            in case v2 of
                Bol b -> (Bol b, e2, h2)
                _     -> (Erro, e2, h2)
        _ -> (Erro, e1, h1)

-- NOT
int h a (Not t) e =
    let (v, e1, h1) = int h a t e
    in case v of
        Bol b -> (Bol (not b), e1, h1)
        _     -> (Erro, e1, h1)

-- IF
int h a (Iff cond t1 t2) e =
    let (v, e1, h1) = int h a cond e
    in case v of
        Bol True  -> int h1 a t1 e1
        Bol False -> int h1 a t2 e1
        _         -> (Erro, e1, h1)

-- WHILE
int h a (Whi cond body) e =
    let (v, e1, h1) = int h a cond e
    in case v of
        Bol True ->
            let (_, e2, h2) = int h1 a body e1
            in int h2 a (Whi cond body) e2
        Bol False -> (Unit, e1, h1)
        _         -> (Erro, e1, h1)

-- Definição de classe
int h a (Class nome attrs mets) e =
    (Unit, e, h)  -- só adiciona ao ambiente
  where _a1 = (nome, ClaDef attrs mets) : a

-- Instanciação de classe
int h a (New nomeClasse) e =
    case search nomeClasse a of
        ClaDef attrs _ ->
            let objID = show (length h + 1)
                instAttrs = inicializaAtributos attrs
                novaHeap = (objID, (nomeClasse, instAttrs)) : h
            in (Num (read objID), e, novaHeap)
        _ -> (Erro, e, h)

-- Funções auxiliares
search :: Id -> [(Id, Valor)] -> Valor
search i [] = Erro
search i ((j, v) : l) = if i == j then v else search i l

somaVal :: Valor -> Valor -> Valor
somaVal (Num x) (Num y) = Num (x + y)
somaVal _ _ = Erro

multiplica :: Valor -> Valor -> Valor
multiplica (Num x) (Num y) = Num (x * y)
multiplica _ _ = Erro

app :: Valor -> Valor -> Estado -> Heap -> (Valor, Estado, Heap)
app (Fun f) v e h =
    let (res, e2) = f v e
    in (res, e2, h)
app _ _ e h = (Erro, e, h)

wr :: (Id, Valor) -> Estado -> Estado
wr (i, v) [] = [(i, v)]
wr (i, v) ((j, u) : l) =
    if i == j
        then (j, v) : l
        else (j, u) : wr (i, v) l

inicializaAtributos :: [Id] -> Estado
inicializaAtributos [] = []
inicializaAtributos (x:xs) = (x, Null) : inicializaAtributos xs

-- Executar um termo
at :: Termo -> (Valor, Estado, Heap)
at t = int [] [] t []

-- Show
instance Show Valor where
    show (Num x) = show x
    show (Bol True) = "true"
    show (Bol False) = "false"
    show (Fun _) = "Funcao"
    show Unit = "()"
    show (ClaDef a m) = "Classe(" ++ show a ++ ")"
    show (Obj campos) = "Objeto" ++ show campos
    show Erro = "Erro"
    show Null = "Null"
