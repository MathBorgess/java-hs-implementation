-- Testes Manuais para o Interpretador Java em Haskell
-- ============================================================================
-- Para usar: ghci JavaInterpreter.hs e depois :l test-manual.hs

import qualified JavaInterpreter as JI

-- ============================================================================
-- TESTES INDIVIDUAIS COM RESULTADOS ESPERADOS
-- ============================================================================

-- TESTE 1: Operações Aritméticas
teste1_Aritmetica :: IO ()
teste1_Aritmetica = do
    putStrLn "=== TESTE 1: Operações Aritméticas ==="
    let resultado1 = JI.at (JI.Som (JI.Lit 5) (JI.Lit 3))              -- 5 + 3
        resultado2 = JI.at (JI.Mul (JI.Lit 4) (JI.Lit 6))              -- 4 * 6
        resultado3 = JI.at (JI.Mul (JI.Som (JI.Lit 2) (JI.Lit 3)) (JI.Lit 4)) -- (2 + 3) * 4
    putStrLn "Programa 1: 5 + 3"
    putStrLn $ "Resultado: " ++ show resultado1
    putStrLn "Esperado: (8.0,[],[])"
    putStrLn ""
    putStrLn "Programa 2: 4 * 6"
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn "Esperado: (24.0,[],[])"
    putStrLn ""
    putStrLn "Programa 3: (2 + 3) * 4"
    putStrLn $ "Resultado: " ++ show resultado3
    putStrLn "Esperado: (20.0,[],[])"
    putStrLn ""

-- TESTE 2: Variáveis e Atribuições
teste2_Variaveis :: IO ()
teste2_Variaveis = do
    putStrLn "=== TESTE 2: Variáveis e Atribuições ==="
    let resultado1 = JI.at (JI.Seq (JI.Atr (JI.Var "x") (JI.Lit 10)) (JI.Var "x"))  -- x = 10; x
        resultado2 = JI.at (JI.Seq (JI.Atr (JI.Var "x") (JI.Lit 5)) 
                           (JI.Seq (JI.Atr (JI.Var "y") (JI.Lit 3)) 
                           (JI.Som (JI.Var "x") (JI.Var "y"))))         -- x = 5; y = 3; x + y
    putStrLn "Programa 1: x = 10; x"
    putStrLn $ "Resultado: " ++ show resultado1
    putStrLn "Esperado: (10.0,[(\"x\",10.0)],[])"
    putStrLn ""
    putStrLn "Programa 2: x = 5; y = 3; x + y"
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn "Esperado: (8.0,[(\"y\",3.0),(\"x\",5.0)],[])"
    putStrLn ""

-- TESTE 3: Operações Booleanas
teste3_Booleanas :: IO ()
teste3_Booleanas = do
    putStrLn "=== TESTE 3: Operações Booleanas ==="
    let resultado1 = JI.at (JI.Ig (JI.Lit 5) (JI.Lit 5))               -- 5 == 5
        resultado2 = JI.at (JI.Menor (JI.Lit 3) (JI.Lit 7))            -- 3 < 7
        resultado3 = JI.at (JI.And (JI.Bol True) (JI.Bol False))       -- true && false
        resultado4 = JI.at (JI.Not (JI.Bol True))                      -- !true
    putStrLn "Programa 1: 5 == 5"
    putStrLn $ "Resultado: " ++ show resultado1
    putStrLn "Esperado: (true,[],[])"
    putStrLn ""
    putStrLn "Programa 2: 3 < 7"
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn "Esperado: (true,[],[])"
    putStrLn ""
    putStrLn "Programa 3: true && false"
    putStrLn $ "Resultado: " ++ show resultado3
    putStrLn "Esperado: (false,[],[])"
    putStrLn ""
    putStrLn "Programa 4: !true"
    putStrLn $ "Resultado: " ++ show resultado4
    putStrLn "Esperado: (false,[],[])"
    putStrLn ""

-- TESTE 4: Estruturas de Controle
teste4_Controle :: IO ()
teste4_Controle = do
    putStrLn "=== TESTE 4: Estruturas de Controle ==="
    let resultado1 = JI.at (JI.Iff (JI.Menor (JI.Lit 3) (JI.Lit 5)) (JI.Lit 10) (JI.Lit 20)) -- if 3 < 5 then 10 else 20
        resultado2 = JI.at (JI.Seq (JI.Atr (JI.Var "x") (JI.Lit 8))
                           (JI.Iff (JI.Menor (JI.Lit 5) (JI.Var "x"))
                            (JI.Mul (JI.Var "x") (JI.Lit 2))
                            (JI.Som (JI.Var "x") (JI.Lit 1))))        -- x = 8; if x > 5 then x*2 else x+1
        resultado3 = JI.at (JI.Seq (JI.Atr (JI.Var "counter") (JI.Lit 0))
                           (JI.Seq (JI.While (JI.Menor (JI.Var "counter") (JI.Lit 3))
                                    (JI.Atr (JI.Var "counter") (JI.Som (JI.Var "counter") (JI.Lit 1))))
                            (JI.Var "counter")))                      -- counter = 0; while counter < 3 do counter++; counter
    putStrLn "Programa 1: if (3 < 5) then 10 else 20"
    putStrLn $ "Resultado: " ++ show resultado1
    putStrLn "Esperado: (10.0,[],[])"
    putStrLn ""
    putStrLn "Programa 2: x = 8; if (x > 5) then x*2 else x+1"
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn "Esperado: (16.0,[(\"x\",8.0)],[])"
    putStrLn ""
    putStrLn "Programa 3: counter = 0; while (counter < 3) counter++; counter"
    putStrLn $ "Resultado: " ++ show resultado3
    putStrLn "Esperado: (3.0,[(\"counter\",3.0)],[])"
    putStrLn ""

-- TESTE 5: Funções Lambda
teste5_Lambda :: IO ()
teste5_Lambda = do
    putStrLn "=== TESTE 5: Funções Lambda ==="
    let resultado1 = JI.at (JI.Apl (JI.Lam "x" (JI.Som (JI.Var "x") (JI.Lit 1))) (JI.Lit 5)) -- (lambda x -> x + 1) 5
        resultado2 = JI.at (JI.Apl (JI.Apl (JI.Lam "x" (JI.Lam "y" (JI.Som (JI.Var "x") (JI.Var "y")))) (JI.Lit 3)) (JI.Lit 4))
    putStrLn "Programa 1: (lambda x -> x + 1) 5"
    putStrLn $ "Resultado: " ++ show resultado1
    putStrLn "Esperado: (6.0,[],[])"
    putStrLn ""
    putStrLn "Programa 2: (lambda x -> lambda y -> x + y) 3 4"
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn "Esperado: (7.0,[],[])"
    putStrLn ""

-- TESTE 6: Programas Completos
teste6_Programas :: IO ()
teste6_Programas = do
    putStrLn "=== TESTE 6: Programas Completos ==="
    let programa1 = [JI.Function "x" [] (JI.Lit 10), JI.Function "y" [] (JI.Lit 20), JI.Function "resultado" [] (JI.Som (JI.FunctionCall "x" []) (JI.FunctionCall "y" []))]
        resultado1 = JI.testPrograma [] programa1 [] []
        programa2 = [JI.Class "Pessoa" ["nome", "idade"] [], JI.Atr (JI.Var "p1") (JI.New "Pessoa")]
        resultado2 = JI.testPrograma [] programa2 [] []
    putStrLn "Programa 1: definições de funções x=10, y=20, resultado=x+y"
    putStrLn $ "Resultado: " ++ show resultado1
    putStrLn "Esperado: resultado com soma das funções"
    putStrLn ""
    putStrLn "Programa 2: class Pessoa {nome, idade}; p1 = new Pessoa"
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn "Esperado: objeto criado na heap com atributos nulos"
    putStrLn ""

-- TESTE 7: Casos de Erro
teste7_Erros :: IO ()
teste7_Erros = do
    putStrLn "=== TESTE 7: Casos de Erro ==="
    let erro1 = JI.at (JI.Var "variavel_inexistente")   -- Erro: variável não existe
        erro2 = JI.at (JI.Som (JI.Lit 5) (JI.Bol True))       -- Erro: soma inválida
        erro3 = JI.at (JI.Iff (JI.Lit 5) (JI.Lit 10) (JI.Lit 20)) -- Erro: condição não booleana
    putStrLn "Erro 1: Variável inexistente"
    putStrLn $ "Resultado: " ++ show erro1
    putStrLn "Esperado: (Erro,[],[])"
    putStrLn ""
    putStrLn "Erro 2: Soma inválida (número + booleano)"
    putStrLn $ "Resultado: " ++ show erro2
    putStrLn "Esperado: (Erro,[],[])"
    putStrLn ""
    putStrLn "Erro 3: Condição IF não booleana"
    putStrLn $ "Resultado: " ++ show erro3
    putStrLn "Esperado: (Erro,[],[])"
    putStrLn ""

-- ============================================================================
-- FUNÇÃO PRINCIPAL PARA EXECUTAR TODOS OS TESTES
-- ============================================================================

-- Executa todos os testes de forma organizada
main :: IO ()
main = runTests
runTests :: IO ()
runTests = do
    putStrLn "          SUITE DE TESTES MANUAIS - INTERPRETADOR JAVA-HASKELL"
    putStrLn ""
    
    teste1_Aritmetica
    putStrLn "----------------------------------------------------------------------"
    
    teste2_Variaveis  
    putStrLn "----------------------------------------------------------------------"
    
    teste3_Booleanas
    putStrLn "----------------------------------------------------------------------"
    
    teste4_Controle
    putStrLn "----------------------------------------------------------------------"
    
    teste5_Lambda
    putStrLn "----------------------------------------------------------------------"
    
    teste6_Programas
    putStrLn "----------------------------------------------------------------------"
    
    teste7_Erros
    putStrLn "----------------------------------------------------------------------"
    
    putStrLn "                           TESTES CONCLUÍDOS"
