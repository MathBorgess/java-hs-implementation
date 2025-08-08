module JavaInterpreter where

type Id = String 
type Numero = Double


-- Termo: representação das expressões e comandos
data Termo
    = Var Id
    | Lit Numero
    | LitStr String                 -- Literal de string
    | Som Termo Termo
    | Mul Termo Termo
    | Lam Id Termo
    | Apl Termo Termo
    | Atr Termo Termo               -- Atribuição: pode ser Var Id ou AttrAccess
    | AttrAccess Termo Id           -- Acesso a atributo (com encadeamento)
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
    | This
    | MethodCall Termo Id [Termo]   -- Chamada de método: objeto.metodo(args)
    | Metodo Id [Id] Termo          -- Método: nome, parâmetros, corpo
    | Function Id [Id] Termo        -- Função independente: nome, parâmetros, corpo
    | FunctionCall Id [Termo]       -- Chamada de função: nomeFuncao(args)
    -- deriving Show

type Programa = [Definicao]
type Definicao = Termo

-- Valor: resultados da avaliação
data Valor
    = Num Double
    | Str String                        -- Valor string
    | BoolVal Bool
    | Fun (Valor -> Estado -> (Valor, Estado))
    | Unit
    | Erro
    | Null
    | ClaDef [Id] [Termo]               -- Definição de classe
    | IntDef [Id] [Termo]               -- Definição de interface
    | ClaAbstrataDef [Id] [Termo]       -- Definição de classe abstrata
    | FunDef [Id] Termo                 -- Definição de função independente: parâmetros, corpo
    -- deriving Eq

type Estado = [(Id, Valor)]          -- Estado mutável
type Ambiente = [(Id, Valor)]        -- Variáveis e definições
type Heap = [(Id, (Id, Estado))]     -- (objID, (nomeClasse, atributosDaInstancia))


-- Executa um programa
testPrograma :: Ambiente -> Programa -> Estado -> Heap -> ((Valor, Estado, Heap), Ambiente)
testPrograma a [] estado heap = ((Erro, estado, heap), a)
testPrograma a [t] estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                Interface name attrs mets -> (name, IntDef attrs mets) : a
                ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _                  ->  a
        in ((v, estado1, heap1), a1)
testPrograma a (t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                Interface name attrs mets -> (name, IntDef attrs mets) : a
                ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _                  ->  a
    in testPrograma a1 ds estado1 heap1

intPrograma :: Ambiente -> Programa -> Estado -> Heap -> (Valor, Estado, Heap)

intPrograma a [] estado heap = (Erro, estado, heap)
intPrograma a [t] estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                Interface name attrs mets -> (name, IntDef attrs mets) : a
                ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _                  -> a
    in (v, estado1, heap1)
intPrograma a (t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                Interface name attrs mets -> (name, IntDef attrs mets) : a
                ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _                  -> a
    in intPrograma a1 ds estado1 heap1

-- Função principal de interpretação
evaluate :: Heap -> Ambiente -> Termo -> Estado -> (Valor, Estado, Heap)


-- ============================================================================
-- VALORES LITERAIS E VARIÁVEIS
-- ============================================================================

-- Literais numéricos
evaluate heap _ (Lit n) e = (Num n, e, heap)
-- Literais de string
evaluate heap _ (LitStr s) e = (Str s, e, heap)
-- Literais booleanos
evaluate heap _ (Bol b) e = (BoolVal b, e, heap)
-- Comando vazio: não faz nada, retorna Unit
evaluate heap _ Skip e    = (Unit, e, heap)

-- Variáveis: busca o valor no ambiente (global) + estado (local)
-- Prioridade: estado local sobrescreve ambiente global
evaluate heap amb (Var x) e = (search x (amb ++ e), e, heap)


-- ============================================================================
-- OPERAÇÕES ARITMÉTICAS E LÓGICAS
-- ============================================================================

-- Soma
evaluate heap amb (Som t u) e =
    let (v1, e1, _) = evaluate heap amb t e
        (v2, e2, _) = evaluate heap amb u e1
    in (somaVal v1 v2, e2, heap)

-- Multiplicação
evaluate heap amb (Mul t u) e =
    let (v1, e1, _) = evaluate heap amb t e
        (v2, e2, _) = evaluate heap amb u e1
    in (multiplica v1 v2, e2, heap)

-- Igualdade
evaluate heap amb (Ig t u) e =
    let (v1, e1, _) = evaluate heap amb t e      
        (v2, e2, _) = evaluate heap amb u e1     
    in case (v1, v2) of
        (Num x, Num y)         -> (BoolVal (x == y), e2, heap)
        (Str x, Str y)         -> (BoolVal (x == y), e2, heap)
        (BoolVal x, BoolVal y) -> (BoolVal (x == y), e2, heap)
        (Unit, Unit)           -> (BoolVal True, e2, heap)
        _                      -> (Erro, e2, heap)

-- Comparação menor que
evaluate heap amb (Menor t u) e =
    let (v1, e1, _) = evaluate heap amb t e      
        (v2, e2, _) = evaluate heap amb u e1     
    in case (v1, v2) of
        (Num x, Num y) -> (BoolVal (x < y), e2, heap)
        _              -> (Erro, e2, heap)

-- AND
evaluate heap amb (And t u) e =
    let (v1, e1, _) = evaluate heap amb t e      
    in case v1 of
        BoolVal False -> (BoolVal False, e1, heap)    
        BoolVal True ->
            let (v2, e2, _) = evaluate heap amb u e1  
            in case v2 of
                BoolVal b -> (BoolVal b, e2, heap)
                _         -> (Erro, e2, heap)
        _ -> (Erro, e1, heap)

-- NOT 
evaluate heap amb (Not t) e =
    let (v, e1, _) = evaluate heap amb t e      
    in case v of
        BoolVal b -> (BoolVal (not b), e1, heap)
        _         -> (Erro, e1, heap)


-- ============================================================================
-- FUNÇÕES: LAMBDA, APLICAÇÃO E FUNÇÕES INDEPENDENTES
-- ============================================================================

-- Lambda
evaluate heap amb (Lam x t) e = (Fun (\v st -> let (res, st2, _) = evaluate heap ((x,v):amb) t st in (res, st2)), e, heap)

-- Aplicação
evaluate heap amb (Apl t u) e =
    let (v1, e1, h1) = evaluate heap amb t e
        (v2, e2, h2) = evaluate h1 amb u e1
    in app v1 v2 e2 h2

-- Função independente
evaluate heap ambiente (Function nome params corpo) estado =
    (Unit, estado, heap)

-- Chamada função independente
evaluate heap amb (FunctionCall nomeFuncao args) estado =
    case search nomeFuncao amb of
        FunDef params corpo ->
            let (valsArgs, estadoFinal, heapFinal) = avaliarArgs args amb estado heap
            in if length valsArgs == length params
                then
                    -- Cria estado local apenas com parâmetros (sem __this__)
                    let estadoLocal = zip params valsArgs
                        -- Combina com estado atual para acesso a variáveis globais
                        estadoCombinado = estadoLocal ++ estadoFinal
                        (resultado, _, heapResultado) = evaluate heapFinal amb corpo estadoCombinado
                    in (resultado, estadoFinal, heapResultado)
                else (Erro, estadoFinal, heapFinal)
        _ -> (Erro, estado, heap)  -- Função não encontrada


-- ============================================================================
-- ATRIBUIÇÕES E MUTAÇÃO
-- ============================================================================

-- Atribuição: var = valor ou obj.attr = valor
evaluate heap amb (Atr target t) e =
    let (v1, e1, h1) = evaluate heap amb t e
    in case target of
        Var x -> (v1, wr (x, v1) e1, h1) -- Atribuição a variável
        AttrAccess objTerm attr -> 
            let (objVal, e2, h2) = evaluate h1 amb objTerm e1
            in case objVal of
                Num objID -> 
                    let h3 = setAttr (show (round objID)) attr v1 h2
                    in (v1, e2, h3)
                _ -> (Erro, e2, h2)
        _ -> (Erro, e1, h1)


-- ============================================================================
-- ESTRUTURAS DE CONTROLE DE FLUXO
-- ============================================================================

-- If-Else
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

-- For: loop completo com inicialização, condição, incremento e corpo
evaluate heap ambiente (For init cond increment body) estado =
    let (_, estado1, heap1) = evaluate heap ambiente init estado
    in forLoop heap1 ambiente cond increment body estado1

-- Sequência
evaluate heap amb (Seq t u) e =
    let (_, e1, h1) = evaluate heap amb t e
    in evaluate h1 amb u e1


-- ============================================================================
-- ORIENTAÇÃO A OBJETOS
-- ============================================================================

-- Definição de classe: registra no ambiente (feito por intPrograma)
evaluate heap ambiente (Class nome attrs _) estado =
    (Unit, estado, heap)  

-- Instanciação de Objeto
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

-- Acesso a atributo: obj.attr (suporta encadeamento: obj1.obj2.attr)
evaluate heap amb (AttrAccess objTerm attr) e =
    let (objVal, e1, h1) = evaluate heap amb objTerm e
    in case objVal of
        Num objID -> 
            let val = getAttr (show (round objID)) attr h1
            in (val, e1, h1)
        _ -> (Erro, e1, h1)

-- Instaceof
evaluate heap amb (InstanceOf objExpr className) estado =
    let (vObj, e1, h1) = evaluate heap amb objExpr estado
    in case vObj of
        Num objID ->
            case lookup (show (round objID)) h1 of
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

-- This: retorna referência ao objeto atual
evaluate heap amb This estado = 
    case lookup "__this__" estado of
        Just val -> (val, estado, heap)
        Nothing -> (Erro, estado, heap)  -- This fora de contexto de método

-- Chamada de método: obj.metodo(args)
evaluate heap amb (MethodCall objTerm metodoNome args) estado =
    let (objVal, e1, h1) = evaluate heap amb objTerm estado  -- Avalia objeto
    in case objVal of
        Num objID ->
            case lookup (show (round objID)) h1 of  -- Busca objeto na heap
                Just (nomeClasse, _) ->
                    case search nomeClasse amb of  -- Busca definição da classe
                        ClaDef _ metodos ->
                            case buscarMetodo metodoNome metodos of  -- Busca método
                                Just metodo -> executarMetodo (show (round objID)) metodo args amb e1 h1
                                Nothing -> (Erro, e1, h1)  -- Método não encontrado
                        _ -> (Erro, e1, h1)  -- Classe não encontrada
                Nothing -> (Erro, e1, h1)  -- Objeto não encontrado na heap
        _ -> (Erro, e1, h1)  -- Não é um objeto

-- Definição de método: retorna Unit (métodos são registrados na classe)
evaluate heap ambiente (Metodo nome params corpo) estado =
    (Unit, estado, heap)

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

-- Busca valor em lista de associações (ambiente ou estado)
search :: Id -> [(Id, Valor)] -> Valor
search i [] = Erro
search i ((j, v) : l) = if i == j then v else search i l

-- Operações aritméticas
somaVal :: Valor -> Valor -> Valor
somaVal (Num x) (Num y) = Num (x + y)
somaVal (Str x) (Str y) = Str (x ++ y)
somaVal (Str x) (Num y) = Str (x ++ show y)
somaVal (Num x) (Str y) = Str (show x ++ y)
somaVal _ _ = Erro

multiplica :: Valor -> Valor -> Valor
multiplica (Num x) (Num y) = Num (x * y)
multiplica _ _ = Erro

-- Aplicação de função
app :: Valor -> Valor -> Estado -> Heap -> (Valor, Estado, Heap)
app (Fun f) v e h =
    let (res, e2) = f v e
    in (res, e2, h)
app _ _ e h = (Erro, e, h)

-- Atualização de estado (variáveis)
wr :: (Id, Valor) -> Estado -> Estado
wr (i, v) [] = [(i, v)]
wr (i, v) ((j, u) : l) =
    if i == j
        then (j, v) : l
        else (j, u) : wr (i, v) l

-- Manipulação de atributos na heap
setAttr :: Id -> Id -> Valor -> Heap -> Heap
setAttr objID attrName val [] = []
setAttr objID attrName val ((id, (className, attrs)) : rest) =
    if objID == id
        then (id, (className, wr (attrName, val) attrs)) : rest
        else (id, (className, attrs)) : setAttr objID attrName val rest

getAttr :: Id -> Id -> Heap -> Valor
getAttr objID attrName [] = Erro
getAttr objID attrName ((id, (className, attrs)) : rest) =
    if objID == id
        then search attrName attrs
        else getAttr objID attrName rest

-- Implementação do loop FOR
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

-- Buscar método na lista de métodos de uma classe
buscarMetodo :: Id -> [Termo] -> Maybe Termo
buscarMetodo _ [] = Nothing
buscarMetodo nome (Metodo nomeMetodo params corpo : rest) =
    if nome == nomeMetodo then Just (Metodo nomeMetodo params corpo)
    else buscarMetodo nome rest
buscarMetodo nome (_ : rest) = buscarMetodo nome rest  -- Ignora elementos que não são métodos

-- Executar método com contexto de objeto
executarMetodo :: Id -> Termo -> [Termo] -> Ambiente -> Estado -> Heap -> (Valor, Estado, Heap)
executarMetodo objID (Metodo _ params corpo) args amb estado heap =
    -- Avaliar argumentos
    let (valsArgs, estadoFinal, heapFinal) = avaliarArgs args amb estado heap
    in if length valsArgs == length params
        then
            --  estado local isolado
            let estadoLocal = ("__this__", Num (read objID)) : zip params valsArgs
                -- Combinar para permitir acesso a variáveis globais
                estadoCombinado = estadoLocal ++ estadoFinal
                (resultado, _, heapResultado) = evaluate heapFinal amb corpo estadoCombinado
                -- Retornar estado original inalterado
            in (resultado, estadoFinal, heapResultado)
        else (Erro, estadoFinal, heapFinal)
executarMetodo _ _ _ _ estado heap = (Erro, estado, heap)

-- Avaliar lista de argumentos
avaliarArgs :: [Termo] -> Ambiente -> Estado -> Heap -> ([Valor], Estado, Heap)
avaliarArgs [] _ estado heap = ([], estado, heap)
avaliarArgs (arg:rest) amb estado heap =
    let (val, e1, h1) = evaluate heap amb arg estado
        (vals, e2, h2) = avaliarArgs rest amb e1 h1
    in (val:vals, e2, h2)


-- Executar um termo
at :: Termo -> (Valor, Estado, Heap)
at t = evaluate [] [] t []

instance Show Valor where
    show (Num x) =  show x
    show (Str s) = "\"" ++ s ++ "\""
    show (BoolVal True) = "true"
    show (BoolVal False) = "false"
    show (Fun _) = "Funcao"
    show Unit = "()"
    show Erro = "Erro"
    show Null = "Null"
    show (ClaDef attrs _) = "<classe com atributos: " ++ show attrs ++ ">"
    show (FunDef params _) = "<funcao com parametros: " ++ show params ++ ">"

