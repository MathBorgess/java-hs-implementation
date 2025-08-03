-- Testes simples para o interpretador Java em Haskell
-- Para usar: ghci java-interpreter.hs e depois copiar e colar os testes

-- Carrega o arquivo e testa funcionalidades básicas

-- Teste 1: Operações aritméticas
teste1 = at (Som (Lit 5) (Lit 3))              -- 5 + 3 = 8
teste2 = at (Mul (Lit 4) (Lit 6))              -- 4 * 6 = 24
teste3 = at (Mul (Som (Lit 2) (Lit 3)) (Lit 4)) -- (2 + 3) * 4 = 20

-- Teste 4: Atribuição e variáveis
teste4 = at (Seq (Atr "x" (Lit 10)) (Var "x"))  -- x = 10; x

-- Teste 5: Múltiplas atribuições
teste5 = at (Seq (Atr "x" (Lit 5)) 
            (Seq (Atr "y" (Lit 3)) 
            (Som (Var "x") (Var "y"))))         -- x = 5; y = 3; x + y

-- Teste 6: Operações booleanas
teste6 = at (Ig (Lit 5) (Lit 5))               -- 5 == 5 (true)
teste7 = at (Menor (Lit 3) (Lit 7))            -- 3 < 7 (true)
teste8 = at (And (Bol True) (Bol False))       -- true && false (false)
teste9 = at (Not (Bol True))                   -- !true (false)

-- Teste 10: If-Else
teste10 = at (Iff (Menor (Lit 3) (Lit 5)) (Lit 10) (Lit 20)) -- if 3 < 5 then 10 else 20

-- Teste 11: If-Else com variáveis
teste11 = at (Seq (Atr "x" (Lit 8))
             (Iff (Menor (Lit 5) (Var "x"))
              (Mul (Var "x") (Lit 2))
              (Som (Var "x") (Lit 1))))        -- x = 8; if x > 5 then x*2 else x+1

-- Teste 12: While loop
teste12 = at (Seq (Atr "counter" (Lit 0))
             (Seq (Whi (Menor (Var "counter") (Lit 3))
                      (Atr "counter" (Som (Var "counter") (Lit 1))))
              (Var "counter")))                -- counter = 0; while counter < 3 do counter++; counter

-- Teste 13: Lambda
teste13 = at (Apl (Lam "x" (Som (Var "x") (Lit 1))) (Lit 5)) -- (lambda x -> x + 1) 5

-- Teste 14: Lambda com currying
teste14 = at (Apl (Apl (Lam "x" (Lam "y" (Som (Var "x") (Var "y")))) (Lit 3)) (Lit 4))

-- Teste 15: Programa com definições
programa1 = [Def "x" (Lit 10), Def "y" (Lit 20), Def "resultado" (Som (Var "x") (Var "y"))]
teste15 = testPrograma [] programa1 [] []

-- Teste 16: Classe e instanciação
programa2 = [Def "Pessoa" (Class "Pessoa" ["nome", "idade"] []), Def "p1" (New "Pessoa")]
teste16 = testPrograma [] programa2 [] []

-- Testes de casos de erro
testeErro1 = at (Var "variavel_inexistente")   -- Erro: variável não existe
testeErro2 = at (Som (Lit 5) (Bol True))       -- Erro: soma inválida
testeErro3 = at (Iff (Lit 5) (Lit 10) (Lit 20)) -- Erro: condição não booleana

-- Para rodar todos os testes, use:
-- Primeiro carregue: :l java-interpreter.hs
-- Depois carregue: :l test-manual.hs
-- Então execute: runTests

runTests = do
    putStrLn "=== Testes do Interpretador Java em Haskell ==="
    putStrLn ""
    putStrLn "Testes Aritméticos:"
    putStrLn $ "5 + 3 = " ++ show teste1
    putStrLn $ "4 * 6 = " ++ show teste2  
    putStrLn $ "(2 + 3) * 4 = " ++ show teste3
    putStrLn ""
    putStrLn "Testes de Variáveis:"
    putStrLn $ "x = 10; x = " ++ show teste4
    putStrLn $ "x = 5; y = 3; x + y = " ++ show teste5
    putStrLn ""
    putStrLn "Testes Booleanos:"
    putStrLn $ "5 == 5 = " ++ show teste6
    putStrLn $ "3 < 7 = " ++ show teste7
    putStrLn $ "true && false = " ++ show teste8
    putStrLn $ "!true = " ++ show teste9
    putStrLn ""
    putStrLn "Testes de Controle:"
    putStrLn $ "if 3 < 5 then 10 else 20 = " ++ show teste10
    putStrLn $ "if-else com variáveis = " ++ show teste11
    putStrLn $ "while loop = " ++ show teste12
    putStrLn ""
    putStrLn "Testes de Funções:"
    putStrLn $ "lambda x -> x + 1 aplicada a 5 = " ++ show teste13
    putStrLn $ "currying = " ++ show teste14
    putStrLn ""
    putStrLn "Testes de Programas:"
    putStrLn $ "programa com definições = " ++ show teste15
    putStrLn $ "classe e instanciação = " ++ show teste16
    putStrLn ""
    putStrLn "Testes de Erro:"
    putStrLn $ "variável inexistente = " ++ show testeErro1
    putStrLn $ "soma inválida = " ++ show testeErro2
    putStrLn $ "condição não booleana = " ++ show testeErro3
