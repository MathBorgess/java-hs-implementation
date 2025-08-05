module JavaInterpreter where

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
    | Skip                          -- Comando vazio
    | Bol Bool                      -- Literal booleano
    | Ig Termo Termo                -- Igualdade (==)
    | Menor Termo Termo             -- Comparação (<)
    | And Termo Termo               -- Operador AND (&&)
    | Not Termo                     -- Operador NOT (!)
    | Iff Termo Termo Termo         -- If-Else
    | While Termo Termo               -- While
    | Class Id [Id] [Termo]         -- nome, atributos, métodos
    | New Id                        -- instanciar classe
    | Interface Id [Id] [Termo]     -- nome, atributos, métodos
    | ClassAbstrata Id [Id] [Termo] -- nome, atributos, métodos
    | For Termo Termo Termo Termo   -- For loop completo (init, cond, increment, body)
    | InstanceOf Termo Id
    -- deriving Show

type Programa = [Definicao]
data Definicao = Def Id Termo

-- Valor: resultados da avaliação
data Valor
    = Num Double
    | BoolVal Bool
    | Fun (Valor -> Estado -> (Valor, Estado))
    | Unit
    | Erro
    | Null
    | ClaDef [Id] [Termo]               -- Definição de classe
    -- Interface
    | IntDef [Id] [Termo]               -- Definição de interface
    -- Classe Abstrata
    | ClaAbstrataDef [Id] [Termo]       -- Definição de classe abstrata
    -- deriving Eq

type Estado = [(Id, Valor)]          -- Estado mutável
type Ambiente = [(Id, Valor)]        -- Variáveis e definições
type Heap = [(Id, (Id, Estado))]     -- (objID, (nomeClasse, atributosDaInstancia))


-- Executa um programa

testPrograma :: Ambiente -> Programa -> Estado -> Heap -> ((Valor, Estado, Heap), Ambiente)
testPrograma a [] estado heap = ((Erro, estado, heap), a)
testPrograma a [Def i t] estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class _ attrs mets -> (i, ClaDef attrs mets) : a
                Interface _ attrs mets -> (i, IntDef attrs mets) : a
                ClassAbstrata _ attrs mets -> (i, ClaAbstrataDef attrs mets) : a
                _                  ->  a
        in ((v, estado1, heap1), a1)
testPrograma a (Def i t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class _ attrs mets -> (i, ClaDef attrs mets) : a
                Interface _ attrs mets -> (i, IntDef attrs mets) : a
                ClassAbstrata _ attrs mets -> (i, ClaAbstrataDef attrs mets) : a
                _                  ->  a
    in testPrograma a1 ds estado1 heap1

intPrograma :: Ambiente -> Programa -> Estado -> Heap -> (Valor, Estado, Heap)

intPrograma a [] estado heap = (Erro, estado, heap)
intPrograma a [Def i t] estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class _ attrs mets -> (i, ClaDef attrs mets) : a
                Interface _ attrs mets -> (i, IntDef attrs mets) : a
                ClassAbstrata _ attrs mets -> (i, ClaAbstrataDef attrs mets) : a
                _                  -> a
    in (v, estado1, heap1)
intPrograma a (Def i t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class _ attrs mets -> (i, ClaDef attrs mets) : a
                Interface _ attrs mets -> (i, IntDef attrs mets) : a
                ClassAbstrata _ attrs mets -> (i, ClaAbstrataDef attrs mets) : a
                _                  -> a
    in intPrograma a1 ds estado1 heap1

-- Função principal de interpretação
evaluate :: Heap -> Ambiente -> Termo -> Estado -> (Valor, Estado, Heap)

-- Literais e Skip
evaluate heap _ (Lit n) e = (Num n, e, heap)
evaluate heap _ (Bol b) e = (BoolVal b, e, heap)
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
        (Num x, Num y) -> (BoolVal (x == y), e2, h2)
        (BoolVal x, BoolVal y) -> (BoolVal (x == y), e2, h2)
        (Unit, Unit)   -> (BoolVal True, e2, h2)
        _              -> (Erro, e2, h2)

-- Menor
evaluate heap amb (Menor t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in case (v1, v2) of
        (Num x, Num y) -> (BoolVal (x < y), e2, h2)
        _              -> (Erro, e2, h2)

-- AND
evaluate heap amb (And t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
    in case v1 of
        BoolVal False -> (BoolVal False, e1, h1)
        BoolVal True ->
            let (v2, e2, h2) = evaluate h1 amb u e1
            in case v2 of
                BoolVal b -> (BoolVal b, e2, h2)
                _     -> (Erro, e2, h2)
        _ -> (Erro, e1, h1)

-- NOT
evaluate heap amb (Not t) e =
    let (v, e1, h1) = evaluate heap amb t e
    in case v of
        BoolVal b -> (BoolVal (not b), e1, h1)
        _     -> (Erro, e1, h1)

-- IF
evaluate heap amb (Iff cond t1 t2) e =
    let (v, e1, h1) = evaluate heap amb cond e
    in case v of
        BoolVal True  -> evaluate h1 amb t1 e1
        BoolVal False -> evaluate h1 amb t2 e1
        _         -> (Erro, e1, h1)

-- WHILE
evaluate heap amb (While cond body) e =
    let (v, e1, h1) = evaluate heap amb cond e
    in case v of
        BoolVal True ->
            let (_, e2, h2) = evaluate h1 amb body e1
            in evaluate h2 amb (While cond body) e2
        BoolVal False -> (Unit, e1, h1)
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
        -- Erro de instanciação de classe abstrata
        IntDef _ _ -> 
            (Erro, estado, heap)  -- Não é possível instanciar uma interface
        ClaAbstrataDef _ _ ->
            (Erro, estado, heap)  -- Não é possível instanciar uma classe abstrata
        -- Outros casos
        _ -> (Erro, estado, heap)

-- Instaceof
evaluate heap amb (InstanceOf objExpr className) estado =
    let (vObj, e1, h1) = evaluate heap amb objExpr estado
    in case vObj of
        Num objID ->
            case lookup (show objID) h1 of
                Just (objClass, _) ->
                    (BoolVal (objClass == className), e1, h1)
                Nothing -> (Erro, e1, h1)
        _ -> (Erro, e1, h1)

-- Interface 
evaluate heap ambiente (Interface nome attrs _) estado =
    (Unit, estado, heap)  -- Ambiente será atualizado por intPrograma

-- Classe Abstrata
evaluate heap ambiente (ClassAbstrata nome attrs _) estado =
    (Unit, estado, heap)  -- Ambiente será atualizado por intPrograma

-- For - FOR loop completo com incremento
evaluate heap ambiente (For init cond increment body) estado =
    let (_, estado1, heap1) = evaluate heap ambiente init estado
    in forLoop heap1 ambiente cond increment body estado1

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

-- For - implementa o loop FOR correto
forLoop :: Heap -> Ambiente -> Termo -> Termo -> Termo -> Estado -> (Valor, Estado, Heap)
forLoop heap amb cond increment body estado =
    let (v_cond, e1, h1) = evaluate heap amb cond estado
    in case v_cond of
        BoolVal True ->
            let (_, e2, h2) = evaluate h1 amb body e1           -- Executa corpo
                (_, e3, h3) = evaluate h2 amb increment e2      -- Executa incremento
            in forLoop h3 amb cond increment body e3            -- Recursão
        BoolVal False -> (Unit, e1, h1)                         -- Condição falsa, sai
        _ -> (Erro, e1, h1)                                     -- Erro: condição não booleana


-- Executar um termo
at :: Termo -> (Valor, Estado, Heap)
at t = evaluate [] [] t []

-- Show
instance Show Valor where
    show (Num x) =  show x
    show (BoolVal True) = "true"
    show (BoolVal False) = "false"
    show (Fun _) = "Funcao"
    show Unit = "()"
    show Erro = "Erro"
    show Null = "Null"
    show (ClaDef attrs _) = "<classe com atributos: " ++ show attrs ++ ">"
