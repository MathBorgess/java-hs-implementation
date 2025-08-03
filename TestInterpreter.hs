-- Arquivo de teste para o interpretador Java em Haskell
-- Este arquivo testa todas as funcionalidades implementadas

import qualified JavaInterpreter as JI

-- Testes básicos de expressões aritméticas

-- Teste 1: Operações aritméticas básicas
-- 5 + 3 = 8
teste1 :: JI.Termo
teste1 = JI.Som (JI.Lit 5) (JI.Lit 3)

-- Teste 2: Multiplicação
-- 4 * 6 = 24
teste2 :: JI.Termo
teste2 = JI.Mul (JI.Lit 4) (JI.Lit 6)

-- Teste 3: Expressão composta
-- (2 + 3) * 4 = 20
teste3 :: JI.Termo
teste3 = JI.Mul (JI.Som (JI.Lit 2) (JI.Lit 3)) (JI.Lit 4)

-- Testes de variáveis e atribuições

-- Teste 4: Atribuição simples
-- x = 10; x
teste4 :: JI.Termo
teste4 = JI.Seq (JI.Atr "x" (JI.Lit 10)) (JI.Var "x")

-- Teste 5: Múltiplas atribuições
-- x = 5; y = 3; x + y
teste5 :: JI.Termo
teste5 = JI.Seq (JI.Atr "x" (JI.Lit 5)) 
         (JI.Seq (JI.Atr "y" (JI.Lit 3)) 
         (JI.Som (JI.Var "x") (JI.Var "y")))

-- Testes de operações booleanas

-- Teste 6: Comparação de igualdade
-- 5 == 5 (deve retornar true)
teste6 :: JI.Termo
teste6 = JI.Ig (JI.Lit 5) (JI.Lit 5)

-- Teste 7: Comparação menor que
-- 3 < 7 (deve retornar true)
teste7 :: JI.Termo
teste7 = JI.Menor (JI.Lit 3) (JI.Lit 7)

-- Teste 8: Operação AND
-- true && false (deve retornar false)
teste8 :: JI.Termo
teste8 = JI.And (JI.Bol True) (JI.Bol False)

-- Teste 9: Operação NOT
-- !true (deve retornar false)
teste9 :: JI.Termo
teste9 = JI.Not (JI.Bol True)

-- Testes de estruturas de controle

-- Teste 10: If-Else simples
-- if (5 > 3) then 10 else 20 (deve retornar 10)
teste10 :: JI.Termo
teste10 = JI.Iff (JI.Menor (JI.Lit 3) (JI.Lit 5)) (JI.Lit 10) (JI.Lit 20)

-- Teste 11: If-Else com variáveis
-- x = 8; if (x > 5) then x * 2 else x + 1
teste11 :: JI.Termo
teste11 = JI.Seq (JI.Atr "x" (JI.Lit 8))
          (JI.Iff (JI.Menor (JI.Lit 5) (JI.Var "x"))
           (JI.Mul (JI.Var "x") (JI.Lit 2))
           (JI.Som (JI.Var "x") (JI.Lit 1)))

-- Teste 12: While loop simples
-- counter = 0; while (counter < 3) { counter = counter + 1 }; counter
teste12 :: JI.Termo
teste12 = JI.Seq (JI.Atr "counter" (JI.Lit 0))
          (JI.Seq (JI.Whi (JI.Menor (JI.Var "counter") (JI.Lit 3))
                   (JI.Atr "counter" (JI.Som (JI.Var "counter") (JI.Lit 1))))
           (JI.Var "counter"))

-- Testes de funções lambda

-- Teste 13: Lambda simples
-- (lambda x -> x + 1) 5 (deve retornar 6)
teste13 :: JI.Termo
teste13 = JI.Apl (JI.Lam "x" (JI.Som (JI.Var "x") (JI.Lit 1))) (JI.Lit 5)

-- Teste 14: Lambda com múltiplos parâmetros (currying)
-- (lambda x -> lambda y -> x + y) 3 4 (deve retornar 7)
teste14 :: JI.Termo
teste14 = JI.Apl (JI.Apl (JI.Lam "x" (JI.Lam "y" (JI.Som (JI.Var "x") (JI.Var "y")))) 
                  (JI.Lit 3)) 
                  (JI.Lit 4)

-- Testes de classes (orientação a objetos básica)

-- Teste 15: Definição e instanciação de classe
programa1 :: JI.Programa
programa1 = [JI.Def "Pessoa" (JI.Class "Pessoa" ["nome", "idade"] []),
             JI.Def "p1" (JI.New "Pessoa")]

-- Teste 16: Programa com múltiplas definições
programa2 :: JI.Programa
programa2 = [JI.Def "x" (JI.Lit 10),
             JI.Def "y" (JI.Lit 20),
             JI.Def "resultado" (JI.Som (JI.Var "x") (JI.Var "y"))]

-- Função para executar todos os testes
executarTestes :: IO ()
executarTestes = do
    putStrLn "=== TESTES DO INTERPRETADOR JAVA EM HASKELL ==="
    putStrLn ""
    
    putStrLn "--- Testes de Expressões Aritméticas ---"
    putStrLn $ "Teste 1 (5 + 3): " ++ show (JI.at teste1)
    putStrLn $ "Teste 2 (4 * 6): " ++ show (JI.at teste2)
    putStrLn $ "Teste 3 ((2 + 3) * 4): " ++ show (JI.at teste3)
    putStrLn ""
    
    putStrLn "--- Testes de Variáveis e Atribuições ---"
    putStrLn $ "Teste 4 (x = 10; x): " ++ show (JI.at teste4)
    putStrLn $ "Teste 5 (x = 5; y = 3; x + y): " ++ show (JI.at teste5)
    putStrLn ""
    
    putStrLn "--- Testes de Operações Booleanas ---"
    putStrLn $ "Teste 6 (5 == 5): " ++ show (JI.at teste6)
    putStrLn $ "Teste 7 (3 < 7): " ++ show (JI.at teste7)
    putStrLn $ "Teste 8 (true && false): " ++ show (JI.at teste8)
    putStrLn $ "Teste 9 (!true): " ++ show (JI.at teste9)
    putStrLn ""
    
    putStrLn "--- Testes de Estruturas de Controle ---"
    putStrLn $ "Teste 10 (if 5 > 3 then 10 else 20): " ++ show (JI.at teste10)
    putStrLn $ "Teste 11 (if-else com variáveis): " ++ show (JI.at teste11)
    putStrLn $ "Teste 12 (while loop): " ++ show (JI.at teste12)
    putStrLn ""
    
    putStrLn "--- Testes de Funções Lambda ---"
    putStrLn $ "Teste 13 (lambda x -> x + 1) 5: " ++ show (JI.at teste13)
    putStrLn $ "Teste 14 (currying): " ++ show (JI.at teste14)
    putStrLn ""
    
    putStrLn "--- Testes de Programas ---"
    putStrLn $ "Programa 1 (classe): " ++ show (JI.intPrograma [] programa1 [] [])
    putStrLn $ "Programa 2 (múltiplas def): " ++ show (JI.intPrograma [] programa2 [] [])
    putStrLn ""

-- Testes de casos extremos e erros

testeErro1 :: JI.Termo
testeErro1 = JI.Var "variavel_inexistente"  -- Deve retornar erro

testeErro2 :: JI.Termo
testeErro2 = JI.Som (JI.Lit 5) (JI.Bol True)  -- Deve retornar erro (soma inválida)

testeErro3 :: JI.Termo
testeErro3 = JI.Iff (JI.Lit 5) (JI.Lit 10) (JI.Lit 20)  -- Condição não booleana

executarTestesErro :: IO ()
executarTestesErro = do
    putStrLn "--- Testes de Casos de Erro ---"
    putStrLn $ "Erro 1 (variável inexistente): " ++ show (JI.at testeErro1)
    putStrLn $ "Erro 2 (soma inválida): " ++ show (JI.at testeErro2)
    putStrLn $ "Erro 3 (condição não booleana): " ++ show (JI.at testeErro3)
    putStrLn ""

-- Função principal para executar todos os testes
main :: IO ()
main = do
    executarTestes
    executarTestesErro
    putStrLn "=== TESTES CONCLUÍDOS ==="
