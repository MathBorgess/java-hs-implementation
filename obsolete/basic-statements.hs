module BasicStatements where

-- Implementação das declarações básicas do Java em Haskell
-- Baseado na estrutura do interpretador funcional do base-line.hs
-- Este módulo implementa: IF, ELSE, WHILE, FOR, DO-WHILE, BREAK, CONTINUE

type Id = String
type Numero = Double

-- Definição de tipos de dados para representar declarações básicas do Java
data Expression = Var Id                           -- Variável: x, y, counter
                | Lit Numero                       -- Literal numérico: 1, 2.5, 42
                | Add Expression Expression        -- Adição: x + y
                | Sub Expression Expression        -- Subtração: x - y
                | Mul Expression Expression        -- Multiplicação: x * y
                | Div Expression Expression        -- Divisão: x / y
                | Eq Expression Expression         -- Igualdade: x == y
                | Lt Expression Expression         -- Menor que: x < y
                | Gt Expression Expression         -- Maior que: x > y
                | Le Expression Expression         -- Menor ou igual: x <= y
                | Ge Expression Expression         -- Maior ou igual: x >= y
                | And Expression Expression        -- E lógico: x && y
                | Or Expression Expression         -- Ou lógico: x || y
                | Not Expression                   -- Negação: !x
                deriving (Show, Eq)

-- Definição das declarações (statements) básicas do Java
data Statement = Assignment Id Expression          -- Atribuição: x = 10;
               | Block [Statement]                 -- Bloco: { stmt1; stmt2; }
               | If Expression Statement Statement -- If-else: if (cond) stmt1 else stmt2
               | IfOnly Expression Statement       -- If sem else: if (cond) stmt
               | While Expression Statement        -- While: while (cond) stmt
               | For Statement Expression Statement Statement  -- For: for(init; cond; update) body
               | DoWhile Statement Expression      -- Do-while: do stmt while (cond)
               | Break                             -- Break: break;
               | Continue                          -- Continue: continue;
               | Return Expression                 -- Return: return expr;
               | ExprStmt Expression               -- Statement de expressão: x++;
               | Skip                              -- Statement vazio: ;
               deriving (Show, Eq)

-- Valores que podem ser produzidos durante a interpretação
data Value = NumVal Double                         -- Valor numérico
           | BoolVal Bool                          -- Valor booleano
           | Error String                          -- Erro com mensagem
           deriving (Show, Eq)

-- Estado do programa: mapeamento de variáveis para valores
type State = [(Id, Value)]

-- Resultado da execução de uma declaração
-- Pode ser normal, break, continue, ou return
data ExecResult = Normal State                     -- Execução normal
                | BreakResult State                -- Encontrou break
                | ContinueResult State             -- Encontrou continue
                | ReturnResult Value State         -- Encontrou return
                | ErrorResult String               -- Erro na execução
                deriving (Show, Eq)

-- Função para avaliar expressões
-- Recebe o estado atual e a expressão, retorna o valor
evalExpr :: State -> Expression -> Value

-- Avaliação de variável: busca no estado
evalExpr state (Var id) = case lookup id state of
    Just value -> value
    Nothing -> Error ("Variável não definida: " ++ id)

-- Avaliação de literal: retorna o valor numérico
evalExpr _ (Lit n) = NumVal n

-- Operações aritméticas
evalExpr state (Add e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> NumVal (x + y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Operação de adição requer valores numéricos"

evalExpr state (Sub e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> NumVal (x - y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Operação de subtração requer valores numéricos"

evalExpr state (Mul e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> NumVal (x * y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Operação de multiplicação requer valores numéricos"

evalExpr state (Div e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal 0) -> Error "Divisão por zero"
        (NumVal x, NumVal y) -> NumVal (x / y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Operação de divisão requer valores numéricos"

-- Operações de comparação
evalExpr state (Eq e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> BoolVal (x == y)
        (BoolVal x, BoolVal y) -> BoolVal (x == y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Comparação de igualdade entre tipos incompatíveis"

evalExpr state (Lt e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> BoolVal (x < y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Comparação menor que requer valores numéricos"

evalExpr state (Gt e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> BoolVal (x > y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Comparação maior que requer valores numéricos"

evalExpr state (Le e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> BoolVal (x <= y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Comparação menor ou igual requer valores numéricos"

evalExpr state (Ge e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (NumVal x, NumVal y) -> BoolVal (x >= y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Comparação maior ou igual requer valores numéricos"

-- Operações lógicas
evalExpr state (And e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (BoolVal x, BoolVal y) -> BoolVal (x && y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Operação AND requer valores booleanos"

evalExpr state (Or e1 e2) = 
    case (evalExpr state e1, evalExpr state e2) of
        (BoolVal x, BoolVal y) -> BoolVal (x || y)
        (Error msg, _) -> Error msg
        (_, Error msg) -> Error msg
        _ -> Error "Operação OR requer valores booleanos"

evalExpr state (Not e) = 
    case evalExpr state e of
        (BoolVal x) -> BoolVal (not x)
        (Error msg) -> Error msg
        _ -> Error "Operação NOT requer valor booleano"

-- Função para executar declarações
-- Recebe o estado atual e a declaração, retorna o resultado da execução
execStmt :: State -> Statement -> ExecResult

-- Atribuição: x = expr;
-- Avalia a expressão e atualiza o estado com o novo valor
execStmt state (Assignment id expr) = 
    case evalExpr state expr of
        Error msg -> ErrorResult msg
        value -> Normal (updateState id value state)

-- Bloco de declarações: { stmt1; stmt2; ... }
-- Executa sequencialmente todas as declarações no bloco
execStmt state (Block []) = Normal state
execStmt state (Block (stmt:stmts)) = 
    case execStmt state stmt of
        Normal newState -> execStmt newState (Block stmts)
        BreakResult s -> BreakResult s      -- Propaga break
        ContinueResult s -> ContinueResult s -- Propaga continue
        ReturnResult v s -> ReturnResult v s -- Propaga return
        ErrorResult msg -> ErrorResult msg   -- Propaga erro

-- If-else: if (condition) thenStmt else elseStmt
-- Avalia a condição e executa a declaração apropriada
execStmt state (If condition thenStmt elseStmt) = 
    case evalExpr state condition of
        BoolVal True -> execStmt state thenStmt
        BoolVal False -> execStmt state elseStmt
        Error msg -> ErrorResult msg
        _ -> ErrorResult "Condição IF deve ser booleana"

-- If sem else: if (condition) stmt
-- Similar ao if-else, mas não faz nada se a condição for falsa
execStmt state (IfOnly condition stmt) = 
    case evalExpr state condition of
        BoolVal True -> execStmt state stmt
        BoolVal False -> Normal state
        Error msg -> ErrorResult msg
        _ -> ErrorResult "Condição IF deve ser booleana"

-- While: while (condition) body
-- Executa o corpo enquanto a condição for verdadeira
execStmt state (While condition body) = 
    case evalExpr state condition of
        BoolVal False -> Normal state
        BoolVal True -> 
            case execStmt state body of
                Normal newState -> execStmt newState (While condition body)
                BreakResult s -> Normal s           -- Break sai do loop
                ContinueResult s -> execStmt s (While condition body) -- Continue volta ao início
                ReturnResult v s -> ReturnResult v s -- Return sai da função
                ErrorResult msg -> ErrorResult msg
        Error msg -> ErrorResult msg
        _ -> ErrorResult "Condição WHILE deve ser booleana"

-- For: for(init; condition; update) body
-- Executa init, depois while(condition) { body; update; }
execStmt state (For init condition update body) = 
    case execStmt state init of
        Normal initState -> execForLoop initState condition update body
        ErrorResult msg -> ErrorResult msg
        _ -> ErrorResult "Inicialização do FOR não pode conter break/continue/return"

-- Do-while: do body while (condition)
-- Executa o corpo pelo menos uma vez, depois while
execStmt state (DoWhile body condition) = 
    case execStmt state body of
        Normal newState -> 
            case evalExpr newState condition of
                BoolVal True -> execStmt newState (DoWhile body condition)
                BoolVal False -> Normal newState
                Error msg -> ErrorResult msg
                _ -> ErrorResult "Condição DO-WHILE deve ser booleana"
        BreakResult s -> Normal s           -- Break sai do loop
        ContinueResult s ->                 -- Continue vai para a condição
            case evalExpr s condition of
                BoolVal True -> execStmt s (DoWhile body condition)
                BoolVal False -> Normal s
                Error msg -> ErrorResult msg
                _ -> ErrorResult "Condição DO-WHILE deve ser booleana"
        ReturnResult v s -> ReturnResult v s
        ErrorResult msg -> ErrorResult msg

-- Break: sai do loop mais próximo
execStmt state Break = BreakResult state

-- Continue: vai para a próxima iteração do loop mais próximo
execStmt state Continue = ContinueResult state

-- Return: retorna um valor e sai da função
execStmt state (Return expr) = 
    case evalExpr state expr of
        Error msg -> ErrorResult msg
        value -> ReturnResult value state

-- Statement de expressão: apenas avalia a expressão
execStmt state (ExprStmt expr) = 
    case evalExpr state expr of
        Error msg -> ErrorResult msg
        _ -> Normal state

-- Statement vazio: não faz nada
execStmt state Skip = Normal state

-- Função auxiliar para executar o loop do FOR
execForLoop :: State -> Expression -> Statement -> Statement -> ExecResult
execForLoop state condition update body = 
    case evalExpr state condition of
        BoolVal False -> Normal state
        BoolVal True -> 
            case execStmt state body of
                Normal bodyState -> 
                    case execStmt bodyState update of
                        Normal updateState -> execForLoop updateState condition update body
                        ErrorResult msg -> ErrorResult msg
                        _ -> ErrorResult "Update do FOR não pode conter break/continue/return"
                BreakResult s -> Normal s
                ContinueResult s -> 
                    case execStmt s update of
                        Normal updateState -> execForLoop updateState condition update body
                        ErrorResult msg -> ErrorResult msg
                        _ -> ErrorResult "Update do FOR não pode conter break/continue/return"
                ReturnResult v s -> ReturnResult v s
                ErrorResult msg -> ErrorResult msg
        Error msg -> ErrorResult msg
        _ -> ErrorResult "Condição FOR deve ser booleana"

-- Função auxiliar para atualizar o estado com uma nova variável/valor
updateState :: Id -> Value -> State -> State
updateState id value [] = [(id, value)]
updateState id value ((varId, varValue):rest) 
    | id == varId = (id, value):rest
    | otherwise = (varId, varValue):(updateState id value rest)

-- Função para executar um programa (lista de declarações)
execProgram :: [Statement] -> ExecResult
execProgram stmts = execStmt [] (Block stmts)

-- Exemplos de uso das declarações implementadas

-- Exemplo 1: Contador simples com while
-- int counter = 0;
-- while (counter < 5) {
--     counter = counter + 1;
-- }
exemploWhile :: Statement
exemploWhile = Block [
    Assignment "counter" (Lit 0),
    While (Lt (Var "counter") (Lit 5)) 
          (Assignment "counter" (Add (Var "counter") (Lit 1)))
    ]

-- Exemplo 2: Uso de if-else
-- int x = 10;
-- int y;
-- if (x > 5) {
--     y = x * 2;
-- } else {
--     y = x + 1;
-- }
exemploIfElse :: Statement
exemploIfElse = Block [
    Assignment "x" (Lit 10),
    If (Gt (Var "x") (Lit 5))
       (Assignment "y" (Mul (Var "x") (Lit 2)))
       (Assignment "y" (Add (Var "x") (Lit 1)))
    ]

-- Exemplo 3: Loop for
-- int sum = 0;
-- for (int i = 1; i <= 10; i = i + 1) {
--     sum = sum + i;
-- }
exemploFor :: Statement
exemploFor = Block [
    Assignment "sum" (Lit 0),
    For (Assignment "i" (Lit 1))
        (Le (Var "i") (Lit 10))
        (Assignment "i" (Add (Var "i") (Lit 1)))
        (Assignment "sum" (Add (Var "sum") (Var "i")))
    ]

-- Exemplo 4: Do-while
-- int num = 1;
-- do {
--     num = num * 2;
-- } while (num < 100);
exemploDoWhile :: Statement
exemploDoWhile = Block [
    Assignment "num" (Lit 1),
    DoWhile (Assignment "num" (Mul (Var "num") (Lit 2)))
            (Lt (Var "num") (Lit 100))
    ]

-- Exemplo 5: Break e Continue em loop
-- int i = 0;
-- int found = 0;
-- while (i < 20) {
--     i = i + 1;
--     if (i % 2 == 0) continue;
--     if (i > 15) break;
--     found = i;
-- }
exemploBreakContinue :: Statement
exemploBreakContinue = Block [
    Assignment "i" (Lit 0),
    Assignment "found" (Lit 0),
    While (Lt (Var "i") (Lit 20)) (Block [
        Assignment "i" (Add (Var "i") (Lit 1)),
        IfOnly (Eq (Var "i") (Mul (Div (Var "i") (Lit 2)) (Lit 2))) Continue,
        IfOnly (Gt (Var "i") (Lit 15)) Break,
        Assignment "found" (Var "i")
        ])
    ]

-- Função para testar os exemplos
testarExemplos :: IO ()
testarExemplos = do
    putStrLn "=== Testando Declarações Básicas do Java em Haskell ===\n"
    
    putStrLn "1. Exemplo While (contador de 0 a 5):"
    print (execProgram [exemploWhile])
    putStrLn ""
    
    putStrLn "2. Exemplo If-Else (x = 10, y = x > 5 ? x*2 : x+1):"
    print (execProgram [exemploIfElse])
    putStrLn ""
    
    putStrLn "3. Exemplo For (soma de 1 a 10):"
    print (execProgram [exemploFor])
    putStrLn ""
    
    putStrLn "4. Exemplo Do-While (num *= 2 até >= 100):"
    print (execProgram [exemploDoWhile])
    putStrLn ""
    
    putStrLn "5. Exemplo Break/Continue:"
    print (execProgram [exemploBreakContinue])
    putStrLn ""