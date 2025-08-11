-- Arquivo de Testes para This, Métodos e Isolamento de Escopo
-- ============================================================================
module Main where

import qualified JavaInterpreter as JI
import Data.List (intercalate)

-- ============================================================================
-- FUNcÕES AUXILIARES DE EXIBIcÃO
-- ============================================================================

showEstado :: JI.Estado -> String
showEstado estado = "[" ++ intercalate ", " (map showTupla estado) ++ "]"
  where showTupla (id, val) = id ++ " = " ++ show val

showHeap :: JI.Heap -> String
showHeap heap = "[\n" ++ intercalate ",\n" (map showObj heap) ++ "\n]"
  where showObj (id, (className, attrs)) =
          "  Obj " ++ id ++ " (Classe: " ++ className ++ ") -> {" ++ showEstado attrs ++ "}"

showAmbiente :: JI.Ambiente -> String
showAmbiente amb = "[" ++ intercalate ", " (map showDefClass amb) ++ "]"
  where 
    showDefClass (nome, JI.ClaDef attrs _) = nome ++ " (Classe)"
    showDefClass (nome, JI.FunDef params _) = nome ++ " (Funcão)"
    showDefClass (nome, _) = nome ++ " (?)"

-- ============================================================================
-- 8 TESTES ESSENCIAIS
-- ============================================================================

-- TESTE 1: This fora de contexto (deve dar erro)
teste1_ThisSemContexto :: IO ()
teste1_ThisSemContexto = do
    putStrLn "=== TESTE 1: This sem contexto (deve dar ERRO) ==="
    let (resultado, estado, heap) = JI.at JI.This
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ " Esperado: Erro\n"

-- TESTE 2: Criacão de objeto e acesso básico
teste2_CriacaoObjeto :: IO ()
teste2_CriacaoObjeto = do
    putStrLn "=== TESTE 2: Criacao de objeto simples ==="
    let programa = [
            JI.Class "Pessoa" ["nome", "idade"] [],
            JI.Atr (JI.Var "p1") (JI.New "Pessoa"),
            JI.Atr (JI.AttrAccess (JI.Var "p1") "nome") (JI.Lit 100),
            JI.AttrAccess (JI.Var "p1") "nome"
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ " Esperado: Resultado=100.0, Estado=[p1=1.0], Heap com obj 1\n"

-- TESTE 3: Método que retorna This
teste3_MetodoRetornaThis :: IO ()
teste3_MetodoRetornaThis = do
    putStrLn "=== TESTE 3: Método que retorna This ==="
    let programa = [
            JI.Class "Contador" ["valor"] [
                JI.Metodo "getSelf" [] JI.This
            ],
            JI.Atr (JI.Var "c") (JI.New "Contador"),
            JI.MethodCall (JI.Var "c") "getSelf" []
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ " Esperado: Resultado=1.0 (ID do objeto), Estado=[c=1.0]\n"

-- TESTE 4: Método que acessa atributo via This
teste4_MetodoAcessaAtributo :: IO ()
teste4_MetodoAcessaAtributo = do
    putStrLn "=== TESTE 4: Método acessa atributo via This ==="
    let programa = [
            JI.Class "Calculadora" ["resultado"] [
                JI.Metodo "getResultado" [] (JI.AttrAccess JI.This "resultado")
            ],
            JI.Atr (JI.Var "calc") (JI.New "Calculadora"),
            JI.Atr (JI.AttrAccess (JI.Var "calc") "resultado") (JI.Lit 42),
            JI.MethodCall (JI.Var "calc") "getResultado" []
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ " Esperado: Resultado=42.0, Estado=[calc=1.0]\n"

-- TESTE 5: Método que modifica atributo via This
teste5_MetodoModificaAtributo :: IO ()
teste5_MetodoModificaAtributo = do
    putStrLn "=== TESTE 5: Método modifica atributo via This ==="
    let programa = [
            JI.Class "Banco" ["saldo"] [
                JI.Metodo "depositar" ["valor"] (
                    JI.Atr (JI.AttrAccess JI.This "saldo") 
                           (JI.Som (JI.AttrAccess JI.This "saldo") (JI.Var "valor"))
                ),
                JI.Metodo "getSaldo" [] (JI.AttrAccess JI.This "saldo")
            ],
            JI.Atr (JI.Var "conta") (JI.New "Banco"),
            JI.Atr (JI.AttrAccess (JI.Var "conta") "saldo") (JI.Lit 100),
            JI.MethodCall (JI.Var "conta") "depositar" [JI.Lit 50],
            JI.MethodCall (JI.Var "conta") "getSaldo" []
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ " Esperado: Resultado=150.0, Estado=[conta=1.0]\n"

-- TESTE 6: Isolamento de escopo (sem vazamento de variáveis locais)
teste6_IsolamentoEscopo :: IO ()
teste6_IsolamentoEscopo = do
    putStrLn "=== TESTE 6: Isolamento de escopo (SEM vazamento) ==="
    let programa = [
            JI.Class "Test" ["x"] [
                JI.Metodo "processar" ["param", "temp"] (
                    JI.Seq (JI.Atr (JI.Var "localVar") (JI.Lit 999))
                           (JI.Som (JI.Var "param") (JI.Var "temp"))
                )
            ],
            JI.Atr (JI.Var "globalVar") (JI.Lit 10),
            JI.Atr (JI.Var "obj") (JI.New "Test"),
            JI.MethodCall (JI.Var "obj") "processar" [JI.Lit 5, JI.Lit 3]
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ " Esperado: Resultado=8.0, Estado=[globalVar=10.0, obj=1.0]"
    putStrLn $ " NAO deve conter: __this__, param, temp, localVar\n"

-- TESTE 7: Método chamando outro método via This
teste7_MetodoChamaMetodo :: IO ()
teste7_MetodoChamaMetodo = do
    putStrLn "=== TESTE 7: Método chama outro método via This ==="
    let programa = [
            JI.Class "Matematica" ["numero"] [
                JI.Metodo "dobrar" [] (
                    JI.Mul (JI.AttrAccess JI.This "numero") (JI.Lit 2)
                ),
                JI.Metodo "quadruplicar" [] (
                    JI.MethodCall JI.This "dobrar" []
                )
            ],
            JI.Atr (JI.Var "math") (JI.New "Matematica"),
            JI.Atr (JI.AttrAccess (JI.Var "math") "numero") (JI.Lit 5),
            JI.MethodCall (JI.Var "math") "quadruplicar" []
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ " Esperado: Resultado=10.0 (5*2), Estado=[math=1.0]\n"

-- TESTE 8: Ambiente final com múltiplas classes
teste8_AmbienteCompleto :: IO ()
teste8_AmbienteCompleto = do
    putStrLn "=== TESTE 8: Ambiente final com múltiplas classes ==="
    let programa = [
            JI.Class "Pessoa" ["nome"] [],
            JI.Class "Animal" ["especie"] [],
            JI.Function "calcular" ["x", "y"] (JI.Som (JI.Var "x") (JI.Var "y")),
            JI.Atr (JI.Var "p") (JI.New "Pessoa"),
            JI.Atr (JI.Var "global") (JI.Lit 123)
            ]
        ((resultado, estado, heap), ambiente) = JI.testPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado: " ++ showEstado estado
    putStrLn $ "Heap: " ++ showHeap heap
    putStrLn $ "Ambiente: " ++ showAmbiente ambiente
    putStrLn $ "Esperado: 3 definicoes no ambiente, Estado=[p=1.0, global=123.0]\n"

-- ============================================================================
-- FUNcÃO PRINCIPAL
-- ============================================================================
main :: IO ()
main = do
    putStrLn "###          TESTES DE THIS E ISOLAMENTO DE ESCOPO      ###"
    
    teste1_ThisSemContexto
    teste2_CriacaoObjeto
    teste3_MetodoRetornaThis
    teste4_MetodoAcessaAtributo
    teste5_MetodoModificaAtributo
    teste6_IsolamentoEscopo
    teste7_MetodoChamaMetodo
    teste8_AmbienteCompleto
    
    putStrLn "###                TESTES CONCLUIDOS                    ###"
