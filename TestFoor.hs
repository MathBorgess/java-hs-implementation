-- TESTES COMPLETOS PARA OS LOOPS FOR
-- Para executar: ghc -o TestFoor TestFoor.hs && .\TestFoor.exe

module Main where

import qualified JavaInterpreter as JI

-- ==============================================================================
-- TESTES DO FOR LOOP COMPLETO (For)
-- ==============================================================================

-- Teste 3: For básico - contador de 0 a 3
testeFor1 = JI.For (JI.Atr (JI.Var "i") (JI.Lit 0))                    -- init: i = 0
                   (JI.Menor (JI.Var "i") (JI.Lit 3))                  -- cond: i < 3
                   (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))    -- incr: i = i + 1
                   JI.Skip                                              -- body: nada

-- Teste 4: For com soma - somar números de 1 a 5
testeFor2 = JI.Seq (JI.Atr (JI.Var "sum") (JI.Lit 0))                      -- sum = 0
                   (JI.For (JI.Atr (JI.Var "i") (JI.Lit 1))                 -- init: i = 1
                           (JI.Menor (JI.Var "i") (JI.Lit 6))                -- cond: i < 6 (para i ir até 5)
                           (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))   -- incr: i = i + 1
                           (JI.Atr (JI.Var "sum") (JI.Som (JI.Var "sum") (JI.Var "i")))) -- body: sum = sum + i

-- Teste 5: For com condição falsa desde o início
testeFor3 = JI.For (JI.Atr (JI.Var "i") (JI.Lit 10))
                   (JI.Menor (JI.Var "i") (JI.Lit 5))
                   (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))
                   JI.Skip

-- Teste 6: For com múltiplas variáveis
testeFor4 = JI.Seq (JI.Atr (JI.Var "produto") (JI.Lit 1))
                   (JI.For (JI.Atr (JI.Var "i") (JI.Lit 1))
                           (JI.Menor (JI.Var "i") (JI.Lit 4))           -- i vai de 1 a 3
                           (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))
                           (JI.Atr (JI.Var "produto") (JI.Mul (JI.Var "produto") (JI.Var "i"))))

-- ==============================================================================
-- FUNÇÃO PARA EXECUTAR TODOS OS TESTES
-- ==============================================================================

executarTestesForCompletos :: IO ()
executarTestesForCompletos = do
    putStrLn "=== TESTES COMPLETOS DOS LOOPS FOR ==="
    putStrLn ""
    
    putStrLn "1. FOR COMPLETO (For):"
    putStrLn ""
    
    putStrLn "Teste 1 - Contador básico de 0 a 3:"
    putStrLn "for(i=0; i<3; i++) skip"
    print (JI.at (JI.Seq testeFor1 (JI.Var "i")))
    putStrLn "Esperado: (3.0, [(\"i\", 3.0)], [])"
    putStrLn ""
    
    putStrLn "Teste 2 - Soma de 1 a 5:"
    putStrLn "sum=0; for(i=1; i<6; i++) sum+=i"
    print (JI.at (JI.Seq testeFor2 (JI.Var "sum")))
    putStrLn "Esperado: (15.0, [(\"sum\", 15.0), (\"i\", 6.0)], [])"
    putStrLn ""
    
    putStrLn "Teste 3 - Condição falsa desde início:"
    putStrLn "for(i=10; i<5; i++) skip"
    print (JI.at (JI.Seq testeFor3 (JI.Var "i")))
    putStrLn "Esperado: (10.0, [(\"i\", 10.0)], [])"
    putStrLn ""
    
    putStrLn "Teste 4 - Produto de 1 a 3 (fatorial de 3):"
    putStrLn "produto=1; for(i=1; i<4; i++) produto*=i"
    print (JI.at (JI.Seq testeFor4 (JI.Var "produto")))
    putStrLn "Esperado: (6.0, [(\"produto\", 6.0), (\"i\", 4.0)], [])"
    putStrLn ""

-- ==============================================================================
-- TESTES DE CASOS ESPECIAIS E ERROS
-- ==============================================================================

-- Teste de erro: condição não booleana no For
testeForErro1 = JI.For (JI.Atr (JI.Var "i") (JI.Lit 0))
                       (JI.Lit 5)                           -- Erro: condição não é booleana
                       (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))
                       JI.Skip

-- Teste de erro: incremento que causa erro
testeForErro2 = JI.For (JI.Atr (JI.Var "i") (JI.Lit 0))
                       (JI.Menor (JI.Var "i") (JI.Lit 2))
                       (JI.Som (JI.Var "i") (JI.Bol True))       -- Erro: soma inválida
                       JI.Skip

executarTestesErroFor :: IO ()
executarTestesErroFor = do
    putStrLn "2. TESTES DE CASOS DE ERRO:"
    putStrLn ""
    
    putStrLn "Erro 1 - Condição não booleana:"
    print (JI.at testeForErro1)
    putStrLn "Esperado: (Erro, [(\"i\", 0.0)], [])"
    putStrLn ""
    
    putStrLn "Erro 2 - Incremento inválido:"
    print (JI.at testeForErro2)
    putStrLn "Esperado: (Erro, [...], [])"
    putStrLn ""

-- ==============================================================================
-- COMPARAÇÃO COM WHILE EQUIVALENTE
-- ==============================================================================

-- Equivalência: For vs While manual
-- For(i=0; i<3; i++) body  ≡  i=0; while(i<3) { body; i++; }

forFullEquivalente = JI.For (JI.Atr (JI.Var "i") (JI.Lit 0))
                            (JI.Menor (JI.Var "i") (JI.Lit 3))
                            (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))
                            JI.Skip

whileEquivalente = JI.Seq (JI.Atr (JI.Var "i") (JI.Lit 0))
                          (JI.While (JI.Menor (JI.Var "i") (JI.Lit 3))
                                    (JI.Seq JI.Skip 
                                            (JI.Atr (JI.Var "i") (JI.Som (JI.Var "i") (JI.Lit 1)))))

testarEquivalencia :: IO ()
testarEquivalencia = do
    putStrLn "3. EQUIVALÊNCIA FOR vs WHILE:"
    putStrLn ""
    
    putStrLn "For(i=0; i<3; i++) skip:"
    print (JI.at (JI.Seq forFullEquivalente (JI.Var "i")))
    putStrLn ""
    
    putStrLn "i=0; while(i<3) { skip; i++; }:"
    print (JI.at (JI.Seq whileEquivalente (JI.Var "i")))
    putStrLn ""
    
    putStrLn "Ambos devem retornar: (3.0, [(\"i\", 3.0)], [])"

-- ==============================================================================
-- FUNÇÃO PRINCIPAL PARA EXECUTAR TODOS OS TESTES
-- ==============================================================================

main :: IO ()
main = do
    executarTestesForCompletos
    executarTestesErroFor
    testarEquivalencia
    putStrLn "=== TESTES CONCLUÍDOS ==="

-- Para testar rapidamente no GHCi:
-- :l JavaInterpreter.hs
-- :l TestFoor.hs
-- main
