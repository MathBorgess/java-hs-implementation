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


type Definicao = Termo
type Programa = [Definicao]

-- Valor: resultados da avaliação
data Valor
    = Num Double
    | Str String                        -- Valor string
    | BoolVal Bool
    | FunLamb (Valor -> Estado -> (Valor, Estado))
    | Void                              -- Retorno vazio de uma execução bem-sucedida
    | Erro
    | Null
    | ClaDef [Id] [Termo]               -- Definição de classe
    -- | IntDef [Id] [Termo]               -- Definição de interface
    -- | ClaAbstrataDef [Id] [Termo]       -- Definição de classe abstrata
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
                -- Interface name attrs mets -> (name, IntDef attrs mets) : a
                -- ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _                  ->  a
        in ((v, estado1, heap1), a1)
testPrograma a (t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                -- Interface name attrs mets -> (name, IntDef attrs mets) : a
                -- ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _                  ->  a
    in testPrograma a1 ds estado1 heap1

intPrograma :: Ambiente -> Programa -> Estado -> Heap -> (Valor, Estado, Heap)

intPrograma a [] estado heap = (Erro, estado, heap)
intPrograma a [t] estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                -- Interface name attrs mets -> (name, IntDef attrs mets) : a
                -- ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _   -> a
    in (v, estado1, heap1)
intPrograma a (t : ds) estado heap =
    let (v, estado1, heap1) = evaluate heap a t estado
        a1 = case t of
                Class name attrs mets -> (name, ClaDef attrs mets) : a
                -- Interface name attrs mets -> (name, IntDef attrs mets) : a
                -- ClassAbstrata name attrs mets -> (name, ClaAbstrataDef attrs mets) : a
                Function name params corpo -> (name, FunDef params corpo) : a
                _   -> a
    in intPrograma a1 ds estado1 heap1

-- Função principal de interpretação
evaluate :: Heap -> Ambiente -> Termo -> Estado -> (Valor, Estado, Heap)


-- ============================================================================
-- VALORES LITERAIS E VARIÁVEIS
-- ============================================================================

-- Literais numéricos
evaluate heap _ (Lit n) estado = (Num n, estado, heap)
-- Literais de string
evaluate heap _ (LitStr s) estado = (Str s, estado, heap)
-- Literais booleanos
evaluate heap _ (Bol b) estado = (BoolVal b, estado, heap)
-- Comando vazio: não faz nada, retorna Void
evaluate heap _ Skip estado    = (Void, estado, heap)

-- Variáveis: busca o valor no  estado (local)  ++  ambiente (global)
evaluate heap ambiente (Var x) estado = (search x (estado ++ ambiente), estado, heap)


-- ============================================================================
-- OPERAÇÕES ARITMÉTICAS E LÓGICAS
-- ============================================================================

-- Soma
evaluate heap ambiente (Som t u) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
        (v2, estado2, h2) = evaluate h1 ambiente u estado1
    in (somaVal v1 v2, estado2, h2)

-- Multiplicação
evaluate heap ambiente (Mul t u) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
        (v2, estado2, h2) = evaluate h1 ambiente u estado1
    in (multiplica v1 v2, estado2, h2)

-- Igualdade
evaluate heap ambiente (Ig t u) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
        (v2, estado2, h2) = evaluate h1 ambiente u estado1
    in case (v1, v2) of
        (Num x, Num y)         -> (BoolVal (x == y), estado2, h2)
        (Str x, Str y)         -> (BoolVal (x == y), estado2, h2)
        (BoolVal x, BoolVal y) -> (BoolVal (x == y), estado2, h2)
        (Void, Void)           -> (BoolVal True, estado2, h2)
        _                      -> (Erro, estado2, h2)

-- Comparação menor que
evaluate heap ambiente (Menor t u) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
        (v2, estado2, h2) = evaluate h1 ambiente u estado1
    in case (v1, v2) of
        (Num x, Num y) -> (BoolVal (x < y), estado2, h2)
        _              -> (Erro, estado2, h2)

-- AND
evaluate heap ambiente (And t u) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
    in case v1 of
        BoolVal False -> (BoolVal False, estado1, h1)
        BoolVal True ->
            let (v2, estado2, h2) = evaluate h1 ambiente u estado1
            in case v2 of
                BoolVal b -> (BoolVal b, estado2, h2)
                _         -> (Erro, estado2, h2)
        _ -> (Erro, estado1, h1)

-- NOT 
evaluate heap ambiente (Not t) estado =
    let (v, estado1, h1) = evaluate heap ambiente t estado
    in case v of
        BoolVal b -> (BoolVal (not b), estado1, h1)
        _         -> (Erro, estado1, h1)


-- ============================================================================
-- FUNÇÕES: LAMBDA, APLICAÇÃO E FUNÇÕES INDEPENDENTES
-- ============================================================================

-- Lambda
evaluate heap ambiente (Lam x t) estado = (FunLamb (\v st -> let (res, st2, _) = evaluate heap ((x,v):ambiente) t st in (res, st2)), estado, heap)

-- Aplicação
evaluate heap ambiente (Apl t u) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
        (v2, estado2, h2) = evaluate h1 ambiente u estado1
    in app v1 v2 estado2 h2

-- Função independente: registra no ambiente (feito por intPrograma)
evaluate heap ambiente (Function nome params corpo) estado =
    (Void, estado, heap)

-- Chamada função independente
evaluate heap ambiente (FunctionCall nomeFuncao args) estado =
    case search nomeFuncao ambiente of
        FunDef params corpo ->
            let (valsArgs, estadoFinal, heapFinal) = avaliarArgs args ambiente estado heap
            in if length valsArgs == length params
                then
                    -- Cria estado local apenas com parâmetros (sem __this__)
                    let estadoLocal = zip params valsArgs
                        -- Combina com estado atual para acesso a variáveis globais
                        estadoCombinado = estadoLocal ++ estadoFinal
                        (resultado, _, heapResultado) = evaluate heapFinal ambiente corpo estadoCombinado
                    in (resultado, estadoFinal, heapResultado)
                else (Erro, estadoFinal, heapFinal)
        _ -> (Erro, estado, heap)  -- Função não encontrada


-- ============================================================================
-- ATRIBUIÇÕES E MUTAÇÃO
-- ============================================================================

-- Atribuição: var = valor ou obj.attr = valor
evaluate heap ambiente (Atr target t) estado =
    let (v1, estado1, h1) = evaluate heap ambiente t estado
    in case target of
        Var x -> (v1, wr (x, v1) estado1, h1) -- Atribuição a variável
        AttrAccess objTerm attr -> 
            let (objVal, estado2, h2) = evaluate h1 ambiente objTerm estado1
            in case objVal of
                Num objID -> 
                    let h3 = setAttr (show objID) attr v1 h2
                    in (v1, estado2, h3)
                _ -> (Erro, estado2, h2)
        _ -> (Erro, estado1, h1)


-- ============================================================================
-- ESTRUTURAS DE CONTROLE DE FLUXO
-- ============================================================================

-- If-Else
evaluate heap ambiente (Iff cond t1 t2) estado =
    let (v, estado1, h1) = evaluate heap ambiente cond estado
    in case v of
        BoolVal True  -> evaluate h1 ambiente t1 estado1
        BoolVal False -> evaluate h1 ambiente t2 estado1
        _         -> (Erro, estado1, h1)

-- WHILE
evaluate heap ambiente (While cond body) estado =
    let (v, estado1, h1) = evaluate heap ambiente cond estado
    in case v of
        BoolVal True ->
            let (_, estado2, h2) = evaluate h1 ambiente body estado1
            in evaluate h2 ambiente (While cond body) estado2
        BoolVal False -> (Void, estado1, h1)
        _         -> (Erro, estado1, h1)

-- For: loop completo com inicialização, condição, incremento e corpo
evaluate heap ambiente (For init cond increment body) estado =
    let (_, estado1, heap1) = evaluate heap ambiente init estado
    in forLoop heap1 ambiente cond increment body estado1

-- Sequência
evaluate heap ambiente (Seq t u) estado =
    let (_, estado1, h1) = evaluate heap ambiente t estado
    in evaluate h1 ambiente u estado1


-- ============================================================================
-- ORIENTAÇÃO A OBJETOS
-- ============================================================================

-- Definição de classe: registra no ambiente (feito por intPrograma)
evaluate heap ambiente (Class nome attrs _) estado =
    (Void, estado, heap)  

-- Instanciação de Objeto
evaluate heap ambiente (New nomeClasse) estado =
    case search nomeClasse ambiente of
        ClaDef attrs _ ->
            let objID = show (length heap + 1)
                instanciaAtr = [(x, Null) | x <- attrs]
                novaHeap = (objID, (nomeClasse, instanciaAtr)) : heap
            in (Num (read objID), estado, novaHeap)
        -- Erro de instanciação de classe abstrata
        -- IntDef _ _ -> 
        --     (Erro, estado, heap)  -- Não é possível instanciar uma interface
        -- ClaAbstrataDef _ _ ->
        --     (Erro, estado, heap)  -- Não é possível instanciar uma classe abstrata
        -- Outros casos
        _ -> (Erro, estado, heap)

-- Acesso a atributo: obj->attr (suporta encadeamento: obj1->...->objN->attr)
evaluate heap ambiente (AttrAccess objTerm attr) estado =
    let (objVal, estado1, h1) = evaluate heap ambiente objTerm estado
    in case objVal of
        Num objID -> 
            let val = getAttr (show objID) attr h1
            in (val, estado1, h1)
        _ -> (Erro, estado1, h1)

-- Instaceof
evaluate heap ambiente (InstanceOf objExpr className) estado =
    let (vObj, estado1, h1) = evaluate heap ambiente objExpr estado
    in case vObj of
        Num objID ->
            case lookup (show objID) h1 of
                Just (objClass, _) ->
                    (BoolVal (objClass == className), estado1, h1)
                Nothing -> (Erro, estado1, h1)
        _ -> (Erro, estado1, h1)

-- Interface 
-- evaluate heap ambiente (Interface nome attrs _) estado =
--     (Void, estado, heap)  -- Ambiente será atualizado por intPrograma

-- -- Classe Abstrata
-- evaluate heap ambiente (ClassAbstrata nome attrs _) estado =
--     (Void, estado, heap)  -- Ambiente será atualizado por intPrograma

-- This: retorna referência ao objeto atual
evaluate heap ambiente This estado = 
    case lookup "__this__" estado of
        Just val -> (val, estado, heap)
        Nothing -> (Erro, estado, heap)  -- This fora de contexto de método

-- Chamada de método: obj.metodo(args)
evaluate heap ambiente (MethodCall objTerm metodoNome args) estado =
    let (objVal, estado1, h1) = evaluate heap ambiente objTerm estado  -- Avalia objeto
    in case objVal of
        Num objID ->
            case lookup (show objID) h1 of  -- Busca objeto na heap
                Just (nomeClasse, _) ->
                    case search nomeClasse ambiente of  -- Busca definição da classe
                        ClaDef _ metodos ->
                            case buscarMetodo metodoNome metodos of  -- Busca método
                                Just metodo -> executarMetodo (show objID) metodo args ambiente estado1 h1
                                Nothing -> (Erro, estado1, h1)  -- Método não encontrado
                        _ -> (Erro, estado1, h1)  -- Classe não encontrada
                Nothing -> (Erro, estado1, h1)  -- Objeto não encontrado na heap
        _ -> (Erro, estado1, h1)  -- Não é um objeto

-- Definição de método: retorna Void (métodos são registrados na classe)
evaluate heap ambiente (Metodo nome params corpo) estado =
    (Void, estado, heap)

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

-- Busca valor em lista de associações (ambiente ou estado)
search :: Id -> [(Id, Valor)] -> Valor
search i [] = Erro
search i ((j, v) : l) = if i == j then v else search i l

-- Operações aritméticas (permite )
somaVal :: Valor -> Valor -> Valor
somaVal (Num x) (Num y) = Num (x + y)
somaVal (Str x) (Str y) = Str (x ++ y)
somaVal _ _ = Erro

multiplica :: Valor -> Valor -> Valor
multiplica (Num x) (Num y) = Num (x * y)
multiplica _ _ = Erro

-- Aplicação de função
app :: Valor -> Valor -> Estado -> Heap -> (Valor, Estado, Heap)
app (FunLamb f) v estado h =
    let (res, estado2) = f v estado
    in (res, estado2, h)
app _ _ estado h = (Erro, estado, h)

-- Atualização de estado (variáveis)
wr :: (Id, Valor) -> Estado -> Estado
wr (i, v) [] = [(i, v)]
wr (i, v) ((j, u) : l) =
    if i == j
        then (j, v) : l
        else (j, u) : wr (i, v) l

-- Manipulação de atributos nos objetos da heap
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
forLoop heap ambiente cond increment body estado =
    let (v_cond, estado1, h1) = evaluate heap ambiente cond estado
    in case v_cond of
        BoolVal True ->
            let (_, estado2, h2) = evaluate h1 ambiente body estado1           -- Executa corpo
                (_, estado3, h3) = evaluate h2 ambiente increment estado2      -- Executa incremento
            in forLoop h3 ambiente cond increment body estado3            -- Recursão
        BoolVal False -> (Void, estado1, h1)                         -- Condição falsa, sai
        _ -> (Erro, estado1, h1)                                     -- Erro: condição não booleana

-- Buscar método na lista de métodos de uma classe
buscarMetodo :: Id -> [Termo] -> Maybe Termo
buscarMetodo _ [] = Nothing
buscarMetodo nome (Metodo nomeMetodo params corpo : rest) =
    if nome == nomeMetodo then Just (Metodo nomeMetodo params corpo)
    else buscarMetodo nome rest
buscarMetodo nome (_ : rest) = buscarMetodo nome rest  -- Ignora elementos que não são métodos

-- Executar método com contexto de objeto
executarMetodo :: Id -> Termo -> [Termo] -> Ambiente -> Estado -> Heap -> (Valor, Estado, Heap)
executarMetodo objID (Metodo _ params corpo) args ambiente estado heap =
    -- Avaliar argumentos
    let (valsArgs, estadoFinal, heapFinal) = avaliarArgs args ambiente estado heap
    in if length valsArgs == length params
        then
            --  estado local isolado
            let estadoLocal = ("__this__", Num (read objID)) : zip params valsArgs
                -- Combinar para permitir acesso a variáveis globais
                estadoCombinado = estadoLocal ++ estadoFinal
                (resultado, _, heapResultado) = evaluate heapFinal ambiente corpo estadoCombinado
                -- Retornar estado original inalterado
            in (resultado, estadoFinal, heapResultado)
        else (Erro, estadoFinal, heapFinal)
executarMetodo _ _ _ _ estado heap = (Erro, estado, heap)

-- Avaliar lista de argumentos
avaliarArgs :: [Termo] -> Ambiente -> Estado -> Heap -> ([Valor], Estado, Heap)
avaliarArgs [] _ estado heap = ([], estado, heap)
avaliarArgs (arg:rest) ambiente estado heap =
    let (val, estado1, h1) = evaluate heap ambiente arg estado
        (vals, estado2, h2) = avaliarArgs rest ambiente estado1 h1
    in (val:vals, estado2, h2)


-- Executar um termo
at :: Termo -> (Valor, Estado, Heap)
at t = evaluate [] [] t []

instance Show Valor where
    show (Num x) =  show x
    show (Str s) = "\"" ++ s ++ "\""
    show (BoolVal True) = "true"
    show (BoolVal False) = "false"
    show (FunLamb _) = "Funcao Lambda"
    show Void = "()"
    show Erro = "Erro"
    show Null = "Null"
    show (ClaDef attrs _) = "<classe com atributos: " ++ show attrs ++ ">"
    -- show (IntDef attrs _) = "<interface com atributos: " ++ show attrs ++ ">"           
    -- show (ClaAbstrataDef attrs _) = "<classe abstrata com atributos: " ++ show attrs ++ ">"  
    show (FunDef params _) = "<funcao com parametros: " ++ show params ++ ">"

