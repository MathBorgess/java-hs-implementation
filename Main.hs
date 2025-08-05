import JavaInterpreter

main :: IO ()
main = do
    putStrLn "=== Testando Interpretador Java-Haskell ==="
    
    -- Teste 1: Atribuicao simples a variavel
    putStrLn "\n1. Teste de atribuicao a variavel:"
    let teste1 = at (Seq (Atr (Var "x") (Lit 42)) (Var "x"))
    putStrLn $ "   at (Seq (Atr (Var \"x\") (Lit 42)) (Var \"x\"))"
    putStrLn $ "   Resultado: " ++ show teste1
    
    -- Teste 2: Atribuicao basica sem sequencia  
    putStrLn "\n2. Teste de atribuicao basica:"
    let teste2 = at (Atr (Var "y") (Lit 10))
    putStrLn $ "   at (Atr (Var \"y\") (Lit 10))"
    putStrLn $ "   Resultado: " ++ show teste2
    
    -- Teste 3: Programa com classe
    putStrLn "\n3. Teste de definicao de classe:"
    let programa1 = [Class "Pessoa" ["nome", "idade"] []]
    let teste3 = testPrograma [] programa1 [] []
    putStrLn $ "   testPrograma [] [Class \"Pessoa\" [\"nome\", \"idade\"] []] [] []"
    putStrLn $ "   Resultado: " ++ show teste3
    
    -- Teste 4: Instanciacao de classe
    putStrLn "\n4. Teste de instanciacao de classe:"
    let ambiente1 = [("Pessoa", ClaDef ["nome", "idade"] [])]
    let teste4 = evaluate [] ambiente1 (New "Pessoa") []
    putStrLn $ "   evaluate [] ambiente (New \"Pessoa\") []"
    putStrLn $ "   Resultado: " ++ show teste4
    
    -- Teste 5: Programa completo - classe + instanciacao + atribuicao a atributo
    putStrLn "\n5. Teste completo com atributo:"
    let programa2 = [
            Class "Pessoa" ["nome"] [],
            Seq (Atr (Var "p") (New "Pessoa"))
                (Atr (AttrAccess (Var "p") "nome") (Lit 100))
            ]
    let teste5 = testPrograma [] programa2 [] []
    putStrLn $ "   Programa: define classe, instancia, atribui a atributo"
    putStrLn $ "   Resultado: " ++ show teste5
