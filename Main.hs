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
    let programa3 = [
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
    let ((resultado6, estado6, heap6), ambiente6) = testPrograma [] programa3 [] []
    putStrLn $ "Resultado: " ++ show resultado6
    putStrLn $ "Estado: " ++ show estado6
    putStrLn $ "Heap: " ++ show heap6
    putStrLn "Esperado: resultadoFib=55.0, resultadoFat=720.0"

    