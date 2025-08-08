import JavaInterpreter

-- ============================================================================
-- TESTES PARA FUNÇÕES INDEPENDENTES
-- ============================================================================

main :: IO ()
main = do
    putStrLn "####### TESTANDO FUNÇÕES INDEPENDENTES #######"
    putStrLn ""
    
    -- Teste 1: Função dobrar
    putStrLn "=== Teste 1: Função Dobrar ==="
    let testeDobrar = [
            Function "dobrar" ["x"] (Som (Var "x") (Var "x")),
            FunctionCall "dobrar" [Lit 5]
            ]
    let resultadoDobrar = testPrograma [] testeDobrar [] []
    putStrLn "Programa: function dobrar(x) = x + x; dobrar(5)"
    putStrLn $ "Resultado: " ++ show resultadoDobrar
    putStrLn "Esperado: ((10.0,[],[]),[...])"
    putStrLn ""
    
    -- Teste 2: Função somar
    putStrLn "=== Teste 2: Função Somar ==="
    let testeSomar = [
            Function "somar" ["a", "b"] (Som (Var "a") (Var "b")),
            FunctionCall "somar" [Lit 10, Lit 20]
            ]
    let resultadoSomar = testPrograma [] testeSomar [] []
    putStrLn "Programa: function somar(a, b) = a + b; somar(10, 20)"
    putStrLn $ "Resultado: " ++ show resultadoSomar
    putStrLn "Esperado: ((30.0,[],[]),[...])"
    putStrLn ""
    
    -- Teste 3: Função saudar
    putStrLn "=== Teste 3: Função Saudar ==="
    let testeSaudar = [
            Function "saudar" ["nome"] (Som (LitStr "Ola, ") (Var "nome")),
            FunctionCall "saudar" [LitStr "Gabriel"]
            ]
    let resultadoSaudar = testPrograma [] testeSaudar [] []
    putStrLn "Programa: function saudar(nome) = \"Ola, \" + nome; saudar(\"Gabriel\")"
    putStrLn $ "Resultado: " ++ show resultadoSaudar
    putStrLn "Esperado: ((\"Ola, Gabriel\",[],[]),[...])"
    putStrLn ""
    
    -- Teste 4: Função soma de quadrados (função chamando função)
    putStrLn "=== Teste 4: Soma de Quadrados (Função Chamando Função) ==="
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
    putStrLn "Esperado: ((25.0,[],[]),[...]) // 3² + 4² = 9 + 16 = 25"
    putStrLn ""
    
    -- Teste 5: Função com condicional
    putStrLn "=== Teste 5: Função com Condicional ==="
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
    
    -- Teste 6: Função que concatena string com número
    putStrLn "=== Teste 6: String + Número ==="
    let testeStringNum = [
            Function "mostrarIdade" ["nome", "idade"] (
                Som (Som (LitStr "Nome: ") (Var "nome"))
                    (Som (LitStr ", Idade: ") (Var "idade"))
            ),
            FunctionCall "mostrarIdade" [LitStr "Ana", Lit 25]
            ]
    let resultadoStringNum = testPrograma [] testeStringNum [] []
    putStrLn "Programa: function mostrarIdade(nome, idade) = \"Nome: \" + nome + \", Idade: \" + idade"
    putStrLn "          mostrarIdade(\"Ana\", 25)"
    putStrLn $ "Resultado: " ++ show resultadoStringNum
    putStrLn "Esperado: string concatenada com nome e idade"
    putStrLn ""
    
    -- Teste 7: Função sem parâmetros (constante)
    putStrLn "=== Teste 7: Função Constante ==="
    let testeConstante = [
            Function "pi" [] (Lit 3.14159),
            FunctionCall "pi" []
            ]
    let resultadoConstante = testPrograma [] testeConstante [] []
    putStrLn "Programa: function pi() = 3.14159; pi()"
    putStrLn $ "Resultado: " ++ show resultadoConstante
    putStrLn "Esperado: ((3.14159,[],[]),[...])"
    putStrLn ""

