import JavaInterpreter

-- ============================================================================
-- TESTES PARA FUNCOES GLOBAIS
-- ============================================================================

main :: IO ()
main = do
    putStrLn "####### TESTANDO FUNCOES GLOBAIS #######"
    putStrLn ""
    
    -- Teste 1: Funcao dobrar
    putStrLn "=== Teste 1: Funcao Dobrar ==="
    let testeDobrar = [
            Function "dobrar" ["x"] (Som (Var "x") (Var "x")),
            FunctionCall "dobrar" [Lit 5]
            ]
    let resultadoDobrar = testPrograma [] testeDobrar [] []
    putStrLn "Programa: function dobrar(x) = x + x; dobrar(5)"
    putStrLn $ "Resultado: " ++ show resultadoDobrar
    putStrLn "Esperado: ((10.0,[],[]),[...])"
    putStrLn ""
    
    -- Teste 2: Funcao somar
    putStrLn "=== Teste 2: Funcao Somar ==="
    let testeSomar = [
            Function "somar" ["a", "b"] (Som (Var "a") (Var "b")),
            FunctionCall "somar" [Lit 10, Lit 20]
            ]
    let resultadoSomar = testPrograma [] testeSomar [] []
    putStrLn "Programa: function somar(a, b) = a + b; somar(10, 20)"
    putStrLn $ "Resultado: " ++ show resultadoSomar
    putStrLn "Esperado: ((30.0,[],[]),[...])"
    putStrLn ""
    
    -- Teste 3: Funcao saudar
    putStrLn "=== Teste 3: Funcao Saudar ==="
    let testeSaudar = [
            Function "saudar" ["nome"] (Som (LitStr "Ola, ") (Var "nome")),
            FunctionCall "saudar" [LitStr "Mundo"]
            ]
    let resultadoSaudar = testPrograma [] testeSaudar [] []
    putStrLn "Programa: function saudar(nome) = \"Ola, \" + nome; saudar(\"Mundo\")"
    putStrLn $ "Resultado: " ++ show resultadoSaudar
    putStrLn "Esperado: ((\"Ola, Mundo\",[],[]),[...])"
    putStrLn ""
    
    -- Teste 4: Funcao soma de quadrados (funcao chamando funcao)
    putStrLn "=== Teste 4: Soma de Quadrados (Funcao Chamando Funcao) ==="
    let testeSomaQuadrados = [
            Function "quadrado" ["x"] (Mul (Var "x") (Var "x")),
            Function "somaQuadrados" ["a", "b"] (
                Som (FunctionCall "quadrado" [Var "a"]) 
                    (FunctionCall "quadrado" [Var "b"])
            ),
            FunctionCall "somaQuadrados" [Lit 3, Lit 4]
            ]
    let resultadoSomaQuadrados = testPrograma [] testeSomaQuadrados [] []
    putStrLn "Programa:"
    putStrLn "  function quadrado(x) = x * x;"
    putStrLn "  function somaQuadrados(a, b) = quadrado(a) + quadrado(b);"
    putStrLn "  somaQuadrados(3, 4)"
    putStrLn $ "Resultado: " ++ show resultadoSomaQuadrados
    putStrLn "Esperado: ((25.0,[],[]),[...]) // 3^2 + 4^2 = 9 + 16 = 25"
    putStrLn ""
    
    -- Teste 5: Funcao com condicional
    putStrLn "=== Teste 5: Funcao com Condicional ==="
    let testeMaior = [
            Function "maior" ["a", "b"] (
                Iff (Menor (Var "a") (Var "b")) 
                    (Var "b") 
                    (Var "a")
            ),
            FunctionCall "maior" [Lit 15, Lit 8]
            ]
    let resultadoMaior = testPrograma [] testeMaior [] []
    putStrLn "Programa: function maior(a, b) = if (a < b) then b else a; maior(15, 8)"
    putStrLn $ "Resultado: " ++ show resultadoMaior
    putStrLn "Esperado: ((15.0,[],[]),[...])"
    putStrLn ""

    -- Teste 7: Funcao sem parametros (constante)
    putStrLn "=== Teste 7: Funcao Constante ==="
    let testeConstante = [
            Function "pi" [] (Lit 3.14159),
            FunctionCall "pi" []
            ]
    let resultadoConstante = testPrograma [] testeConstante [] []
    putStrLn "Programa: function pi() = 3.14159; pi()"
    putStrLn $ "Resultado: " ++ show resultadoConstante
    putStrLn "Esperado: ((3.14159,[],[]),[...])"
    putStrLn ""

    -- Teste 8: Recursao
    putStrLn "=== Teste 8: Recursao ==="
    let testeRecursao = [
            Function "fatorial" ["n"] (
                Iff (Ig (Var "n") (Lit 0))
                    (Lit 1)
                    (Mul (Var "n") (FunctionCall "fatorial" [Som (Var "n") (Lit (-1))]))
            ),
            FunctionCall "fatorial" [Lit 5]
            ]
    let resultadoRecursao = testPrograma [] testeRecursao [] []
    putStrLn "Programa: function fatorial(n) = if (n == 0) then 1 else n * fatorial(n - 1); fatorial(5)"
    putStrLn $ "Resultado: " ++ show resultadoRecursao
    putStrLn "Esperado: ((120.0,[],[]),[...])"
    putStrLn ""