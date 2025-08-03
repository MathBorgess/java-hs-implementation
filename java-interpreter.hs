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
    | Class Id [Id] [Termo]     -- nome, atributos, métodos
    | New Id                    -- instanciar classe
    -- deriving Show

type Programa = [Definicao]
data Definicao = Def Id Termo

-- Valor: resultados da avaliação
data Valor
    = Num Double
    | Bol Bool
    | Fun (Valor -> Estado -> (Valor, Estado))
    | Unit
    | Erro
    | Null
    | ClaDef [Id] [Termo]               -- Definição de classe
    -- deriving Eq

type Estado = [(Id, Valor)]          -- Estado mutável
type Ambiente = [(Id, Valor)]        -- Variáveis e definições
type Heap = [(Id, (Id, Estado))]     -- (objID, (nomeClasse, atributosDaInstancia))


-- Executa um programa

intPrograma :: Ambiente -> Programa -> Estado -> Heap -> (Valor, Estado, Heap)

intPrograma a [] estado heap = (Erro, estado, heap)
intPrograma a [Def i t] estado heap = evaluate heap a t estado
intPrograma a (Def i t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class _ attrs mets -> (i, ClaDef attrs mets) : a
                _                  -> (i, v) : a
    in intPrograma a1 ds estado1 heap1

-- Função principal de interpretação
evaluate :: Heap -> Ambiente -> Termo -> Estado -> (Valor, Estado, Heap)

-- Literais e Skip
evaluate heap _ (Lit n) e = (Num n, e, heap)
evaluate heap _ (Bol b) e = (Bol b, e, heap)
evaluate heap _ Skip e    = (Unit, e, heap)

-- Variáveis
evaluate heap amb (Var x) e = (search x (amb ++ e), e, heap)

-- Soma
evaluate heap amb (Som t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in (somaVal v1 v2, e2, h2)

-- Multiplicação
evaluate heap amb (Mul t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in (multiplica v1 v2, e2, h2)

-- Lambda
evaluate heap amb (Lam x t) e = (Fun (\v st -> let (res, st2, _) = evaluate heap ((x,v):amb) t st in (res, st2)), e, heap)

-- Aplicação
evaluate heap amb (Apl t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in app v1 v2 e2 h2

-- Atribuição
evaluate heap amb (Atr x t) e =
    let (v1, e1, h1) = evaluate heap amb t e
    in (v1, wr (x, v1) e1, h1)

-- Sequência
evaluate heap amb (Seq t u) e =
    let (_, e1, h1) = evaluate heap amb t e
    in evaluate h1 amb u e1

-- Igualdade
evaluate heap amb (Ig t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in case (v1, v2) of
        (Num x, Num y) -> (Bol (x == y), e2, h2)
        (Bol x, Bol y) -> (Bol (x == y), e2, h2)
        (Unit, Unit)   -> (Bol True, e2, h2)
        _              -> (Erro, e2, h2)

-- Menor
evaluate heap amb (Menor t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in case (v1, v2) of
        (Num x, Num y) -> (Bol (x < y), e2, h2)
        _              -> (Erro, e2, h2)

-- AND
evaluate heap amb (And t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
    in case v1 of
        Bol False -> (Bol False, e1, h1)
        Bol True ->
            let (v2, e2, h2) = evaluate h1 amb u e1
            in case v2 of
                Bol b -> (Bol b, e2, h2)
                _     -> (Erro, e2, h2)
        _ -> (Erro, e1, h1)

-- NOT
evaluate heap amb (Not t) e =
    let (v, e1, h1) = evaluate heap amb t e
    in case v of
        Bol b -> (Bol (not b), e1, h1)
        _     -> (Erro, e1, h1)

-- IF
evaluate heap amb (Iff cond t1 t2) e =
    let (v, e1, h1) = evaluate heap amb cond e
    in case v of
        Bol True  -> evaluate h1 amb t1 e1
        Bol False -> evaluate h1 amb t2 e1
        _         -> (Erro, e1, h1)

-- WHILE
evaluate heap amb (Whi cond body) e =
    let (v, e1, h1) = evaluate heap amb cond e
    in case v of
        Bol True ->
            let (_, e2, h2) = evaluate h1 amb body e1
            in evaluate h2 amb (Whi cond body) e2
        Bol False -> (Unit, e1, h1)
        _         -> (Erro, e1, h1)

-- Definição de classe
evaluate heap ambiente (Class nome attrs _) estado =
    (Unit, estado, heap)  -- Ambiente será atualizado por intPrograma


-- Instanciação de classe
evaluate heap ambiente (New nomeClasse) estado =
    case search nomeClasse ambiente of
        ClaDef attrs _ ->
            let objID = show (length heap + 1)
                instanciaAtr = [(x, Null) | x <- attrs]
                novaHeap = (objID, (nomeClasse, instanciaAtr)) : heap
            in (Num (read objID), estado, novaHeap)
        _ -> (Erro, estado, heap)


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

-- Executar um termo
at :: Termo -> (Valor, Estado, Heap)
at t = evaluate [] [] t []

-- Show
instance Show Valor where
    show (Num x) =  show x
    show (Bol True) = "true"
    show (Bol False) = "false"
    show (Fun _) = "Funcao"
    show Unit = "()"
    show Erro = "Erro"
    show Null = "Null"
    show (ClaDef attrs _) = "<classe com atributos: " ++ show attrs ++ ">"
