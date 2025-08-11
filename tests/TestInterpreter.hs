-- Arquivo de teste para o interpretador Java em Haskell
-- Este arquivo testa todas as funcionalidades implementadas

import qualified JavaInterpreter as JI

-- Testes básicos de expressões aritméticas

teste1_Aritmetica :: IO ()
teste1_Aritmetica = do
    putStrLn "=== Teste 1: 5 + 3 ==="
    let resultado = JI.at (JI.Som (JI.Lit 5) (JI.Lit 3))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (8.0,[],[])\n"

teste2_Multiplicacao :: IO ()
teste2_Multiplicacao = do
    putStrLn "=== Teste 2: 4 * 6 ==="
    let resultado = JI.at (JI.Mul (JI.Lit 4) (JI.Lit 6))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (24.0,[],[])\n"

teste3_ExpressaoComposta :: IO ()
teste3_ExpressaoComposta = do
    putStrLn "=== Teste 3: (2 + 3) * 4 ==="
    let resultado = JI.at (JI.Mul (JI.Som (JI.Lit 2) (JI.Lit 3)) (JI.Lit 4))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (20.0,[],[])\n"

-- Testes de variáveis e atribuições

teste4_AtribuicaoSimples :: IO ()
teste4_AtribuicaoSimples = do
    putStrLn "=== Teste 4: x = 10; x ==="
    let resultado = JI.at (JI.Seq (JI.Atr (JI.Var "x") (JI.Lit 10)) (JI.Var "x"))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (10.0,[(\"x\",10.0)],[])\n"

teste5_MultiplasAtribuicoes :: IO ()
teste5_MultiplasAtribuicoes = do
    putStrLn "=== Teste 5: x = 5; y = 3; x + y ==="
    let resultado = JI.at (JI.Seq (JI.Atr (JI.Var "x") (JI.Lit 5))
                          (JI.Seq (JI.Atr (JI.Var "y") (JI.Lit 3))
                          (JI.Som (JI.Var "x") (JI.Var "y"))))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (8.0,[(\"x\",5.0),(\"y\",3.0)],[])\n"

-- Testes de operações booleanas
teste6_ComparacaoIgualdade :: IO ()
teste6_ComparacaoIgualdade = do
    putStrLn "=== Teste 6: 5 == 5 ==="
    let resultado = JI.at (JI.Ig (JI.Lit 5) (JI.Lit 5))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (true,[],[])\n"

teste7_MenorQue :: IO ()
teste7_MenorQue = do
    putStrLn "=== Teste 7: 3 < 7 ==="
    let resultado = JI.at (JI.Menor (JI.Lit 3) (JI.Lit 7))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (true,[],[])\n"

teste8_And :: IO ()
teste8_And = do
    putStrLn "=== Teste 8: true && false ==="
    let resultado = JI.at (JI.And (JI.Bol True) (JI.Bol False))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (false,[],[])\n"

teste9_Not :: IO ()
teste9_Not = do
    putStrLn "=== Teste 9: !true ==="
    let resultado = JI.at (JI.Not (JI.Bol True))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (false,[],[])\n"

-- Testes de estruturas de controle

teste10_IfElse :: IO ()
teste10_IfElse = do
    putStrLn "=== Teste 10: if (3 < 5) then 10 else 20 ==="
    let resultado = JI.at (JI.Iff (JI.Menor (JI.Lit 3) (JI.Lit 5)) (JI.Lit 10) (JI.Lit 20))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (10.0,[],[])\n"

teste11_IfElseComVariaveis :: IO ()
teste11_IfElseComVariaveis = do
    putStrLn "=== Teste 11: x = 8; if (5 < x) then x * 2 else x + 1 ==="
    let resultado = JI.at (JI.Seq (JI.Atr (JI.Var "x") (JI.Lit 8))
                          (JI.Iff (JI.Menor (JI.Lit 5) (JI.Var "x"))
                           (JI.Mul (JI.Var "x") (JI.Lit 2))
                           (JI.Som (JI.Var "x") (JI.Lit 1))))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (16.0,[(\"x\",8.0)],[])\n"

teste12_While :: IO ()
teste12_While = do
    putStrLn "=== Teste 12: counter = 0; while (counter < 3) { counter = counter + 1 }; counter ==="
    let resultado = JI.at (JI.Seq (JI.Atr (JI.Var "counter") (JI.Lit 0))
                          (JI.Seq (JI.While (JI.Menor (JI.Var "counter") (JI.Lit 3))
                                   (JI.Atr (JI.Var "counter") (JI.Som (JI.Var "counter") (JI.Lit 1))))
                           (JI.Var "counter")))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (3.0,[(\"counter\",3.0)],[])\n"

-- Testes de funções lambda

teste13_Lambda :: IO ()
teste13_Lambda = do
    putStrLn "=== Teste 13: (lambda x -> x + 1) 5 ==="
    let resultado = JI.at (JI.Apl (JI.Lam "x" (JI.Som (JI.Var "x") (JI.Lit 1))) (JI.Lit 5))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (6.0,[],[])\n"

teste14_Currying :: IO ()
teste14_Currying = do
    putStrLn "=== Teste 14: (lambda x -> lambda y -> x + y) 3 4 ==="
    let resultado = JI.at (JI.Apl (JI.Apl (JI.Lam "x" (JI.Lam "y" (JI.Som (JI.Var "x") (JI.Var "y"))))
                             (JI.Lit 3)) (JI.Lit 4))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (7.0,[],[])\n"

-- Testes de casos extremos e erros

testeErro1 :: IO ()
testeErro1 = do
    putStrLn "=== Erro 1: variável inexistente ==="
    let resultado = JI.at (JI.Var "variavel_inexistente")
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (Erro,[],[])\n"

testeErro2 :: IO ()
testeErro2 = do
    putStrLn "=== Erro 2: soma inválida (número + booleano) ==="
    let resultado = JI.at (JI.Som (JI.Lit 5) (JI.Bol True))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (Erro,[],[])\n"

testeErro3 :: IO ()
testeErro3 = do
    putStrLn "=== Erro 3: condição não booleana no if ==="
    let resultado = JI.at (JI.Iff (JI.Lit 5) (JI.Lit 10) (JI.Lit 20))
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn "Esperado: (Erro,[],[])\n"

main :: IO ()
main = do
    putStrLn "=== TESTES DO INTERPRETADOR JAVA EM HASKELL ===\n"
    teste1_Aritmetica
    teste2_Multiplicacao
    teste3_ExpressaoComposta
    teste4_AtribuicaoSimples
    teste5_MultiplasAtribuicoes
    teste6_ComparacaoIgualdade
    teste7_MenorQue
    teste8_And
    teste9_Not
    teste10_IfElse
    teste11_IfElseComVariaveis
    teste12_While
    teste13_Lambda
    teste14_Currying
    testeErro1
    testeErro2
    testeErro3
    putStrLn "=== TESTES CONCLUÍDOS ==="