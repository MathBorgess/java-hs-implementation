import JavaInterpreter

main :: IO ()
main = do
    putStrLn "=== Testando Interpretador Java-Haskell ==="
    
    -- Teste 1: classe + instanciacao + atribuicao a atributo
    putStrLn "\n1. Teste completo com atributo:"
    let programa2 = [
            Class "Pessoa" ["nome"] [],
            Seq (Atr (Var "p") (New "Pessoa"))
                (Atr (AttrAccess (Var "p") "nome") (Lit 100))
            ]
    let teste5 = testPrograma [] programa2 [] []
    putStrLn $ "   Programa: define classe, instancia, atribui a atributo"
    putStrLn $ "   Resultado: " ++ show teste5


    -- Teste 2: Elementos principais da linguagem
    putStrLn "\n2. Teste de elementos principais da linguagem:"
    let programa2 = [
            Function "fibonacci" ["n"] (
                Iff (Menor (Var "n") (Lit 0))
                    (Lit 0)
                (Iff (Menor (Var "n") (Lit 2))
                    (Var "n")
                (Som    (FunctionCall "fibonacci" [Som (Var "n") (Lit (-1))])
                        (FunctionCall "fibonacci" [Som (Var "n") (Lit (-2))])
                        )
                    )
            ),
            
            Class "Calculator" [] [
                Metodo "calcFib" ["n"] (
                    FunctionCall "fibonacci" [Var "n"]
                ),
                Metodo "fatorial" ["n"] (
                    Iff (Menor (Var "n") (Lit 0))
                        (Lit 0)
                    (Iff (Menor (Var "n") (Lit 2))
                        (Lit 1)
                    (Mul (Var "n") (MethodCall This "fatorial" [Som (Var "n") (Lit (-1))]) )
                        )
                    )
            ],

            Seq (Atr (Var "calc") (New "Calculator"))
            (Seq (Atr (Var "resultFib") (MethodCall (Var "calc") "calcFib" [Lit 10]))
                (Atr (Var "resultFat") (MethodCall (Var "calc") "fatorial" [Lit 6]))
            )
            ]
    let ((resultado2, estado2, heap2), ambiente2) = testPrograma [] programa2 [] []
    putStrLn $ "Resultado: " ++ show resultado2
    putStrLn $ "Estado: " ++ show estado2
    putStrLn $ "Heap: " ++ show heap2
    putStrLn "Esperado: resultadoFib=55.0, resultadoFat=720.0"


    -- Teste 3: Lambda + While
    putStrLn "\n3. Teste combinado: Lambda + While:"
    let programa3 = [
            Class "Contador" ["valor"] [
                Metodo "somarAte" ["limite"] (
                    Seq (Atr (AttrAccess This "valor") (Lit 0))
                    (Seq (Atr (Var "i") (Lit 1))
                        (While (Menor (Var "i") (Som (Var "limite") (Lit 1)))
                            (Seq (Atr (AttrAccess This "valor") 
                                     (Som (AttrAccess This "valor") (Var "i")))
                                 (Atr (Var "i") (Som (Var "i") (Lit 1)))
                            )
                        )
                    )
                )
            ],
            
            Seq (Atr (Var "contador") (New "Contador"))
            
            (Seq (Atr (Var "quadrado") (Lam "x" (Mul (Var "x") (Var "x"))))
            
            (Seq (MethodCall (Var "contador") "somarAte" [Lit 5])
                 (Atr (Var "resultado") (Apl (Var "quadrado") (AttrAccess (Var "contador") "valor")))
            ))
            ]
    let ((resultado3, estado3, heap3), ambiente3) = testPrograma [] programa3 [] []
    putStrLn "Programa:"
    putStrLn "  Classe Contador com metodo somarAte(limite) usando while"
    putStrLn "  Lambda quadrado = \\x -> x * x"
    putStrLn "  contador.somarAte(5) // soma 1+2+3+4+5 = 15"
    putStrLn "  resultado = quadrado(contador.valor) // 15 * 15 = 225"
    putStrLn $ "Resultado: " ++ show resultado3
    putStrLn $ "Estado: " ++ show estado3
    putStrLn $ "Heap: " ++ show heap3
    putStrLn "Esperado: resultado=225.0, contador.valor=15.0 (esse muda só na Heap)"

    