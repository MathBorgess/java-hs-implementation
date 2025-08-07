-- Importa o módulo do interpretador com um alias "JI" para facilitar a utilização.
import JavaInterpreter as JI
import Data.List (intercalate)

-- ============================================================================
-- FUNÇÕES AUXILIARES DE EXIBIÇÃO
-- ============================================================================

-- Função para mostrar o estado (lista de variáveis) de forma mais legível.
-- Ex: [var1 = 10.0, obj = 2.0]
showEstado :: JI.Estado -> String
showEstado estado = "[" ++ intercalate ", " (map showTupla estado) ++ "]"
  where showTupla (id, val) = id ++ " = " ++ show val

-- Função para mostrar a heap (lista de objetos) de forma mais legível.
-- Exibe cada objeto com seu ID, classe e estado dos atributos.
showHeap :: JI.Heap -> String
showHeap heap = "[\n" ++ intercalate ",\n" (map showObj heap) ++ "\n]"
  where showObj (id, (className, attrs)) =
          "  Obj " ++ id ++ " (Classe: " ++ className ++ ") -> {" ++ showEstado attrs ++ "}"

-- ============================================================================
-- DEFINIÇÕES DE CLASSES PARA OS TESTES
-- ============================================================================

-- Classe simples apenas com um atributo "valor".
classeSimples :: JI.Termo
classeSimples = JI.Class "MinhaClasse" ["valor"] []

-- Classe com um método "getId" que retorna `This`.
classeComThis :: JI.Termo
classeComThis = JI.Class "ClasseThis" ["id"] [
    JI.Metodo "getId" [] JI.This
    ]

-- Classe com um método que acessa um atributo do próprio objeto via `This`.
classeComAtributo :: JI.Termo
classeComAtributo = JI.Class "ClasseAtributo" ["numero"] [
    JI.Metodo "getNumero" [] (JI.AttrAccess JI.This "numero")
    ]


-- ============================================================================
-- TESTES INDIVIDUAIS
-- ============================================================================

-- Teste 1: Tenta aceder a `This` sem um contexto de objeto (deve dar erro).
testeThis1 :: IO ()
testeThis1 = do
    putStrLn "=== Teste 1: This sem contexto ==="
    let (resultado, _, _) = JI.at JI.This
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn ""

-- Teste 2: Acede a `This` num estado pré-configurado manualmente.
testeThis2 :: IO ()
testeThis2 = do
    putStrLn "=== Teste 2: This com contexto manual ==="
    let estadoComThis = [("__this__", JI.Num 42)]
        (resultado, _, _) = JI.evaluate [] [] JI.This estadoComThis
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn ""

-- Teste 3: Cria uma instância de uma classe simples.
testeObjeto1 :: IO ()
testeObjeto1 = do
    putStrLn "=== Teste 3: Criar objeto simples ==="
    let programa = [
            classeSimples,
            JI.New "MinhaClasse"
            ]
    let (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado (ID do objeto): " ++ show resultado
    putStrLn $ "Estado Final: " ++ showEstado estado
    putStrLn $ "Heap Final: \n" ++ showHeap heap
    putStrLn ""

-- Teste 4: Chama um método que retorna a referência do próprio objeto (`This`).
testeMetodo1 :: IO ()
testeMetodo1 = do
    putStrLn "=== Teste 4: Método que retorna This ==="
    let programa = [
            classeComThis,
            -- Atribui uma nova instância de ClasseThis à variável "obj".
            JI.Atr (JI.Var "obj") (JI.New "ClasseThis"),
            -- Chama o método "getId" no objeto "obj".
            JI.MethodCall (JI.Var "obj") "getId" []
            ]
    let (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado (deve ser o ID do objeto): " ++ show resultado
    putStrLn $ "Estado Final: " ++ showEstado estado
    putStrLn $ "Heap Final: \n" ++ showHeap heap
    putStrLn ""

-- Teste 5: Chama um método que acede a um atributo do objeto.
testeMetodo2 :: IO ()
testeMetodo2 = do
    putStrLn "=== Teste 5: Método que acessa atributo com This ==="
    let programa = [
            classeComAtributo,
            -- 1. Cria o objeto e guarda na variável "obj".
            JI.Atr (JI.Var "obj") (JI.New "ClasseAtributo"),
            -- 2. Atribui o valor 100 ao atributo "numero" do objeto "obj".
            JI.Atr (JI.AttrAccess (JI.Var "obj") "numero") (JI.Lit 100),
            -- 3. Chama o método que retorna o valor do atributo "numero".
            JI.MethodCall (JI.Var "obj") "getNumero" []
            ]
    let  (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Resultado (deve ser 100.0): " ++ show resultado
    putStrLn $ "Estado Final: " ++ showEstado estado
    putStrLn $ "Heap Final: \n" ++ showHeap heap
    putStrLn ""

-- Teste 6: Método altera apenas atributo do objeto, não variável global.
testeAmbiente1 :: IO ()
testeAmbiente1 = do
    putStrLn "=== Teste 6: Ambiente global não é alterado por método ==="
    let classe = JI.Class "C" ["x"] [
            JI.Metodo "setX" ["novo"] (JI.Atr (JI.AttrAccess JI.This "x") (JI.Var "novo"))
            ]
        programa = [
            classe,
            JI.Atr (JI.Var "obj") (JI.New "C"),
            JI.Atr (JI.Var "global") (JI.Lit 999),
            JI.MethodCall (JI.Var "obj") "setX" [JI.Lit 123]
            ]
    let  (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Estado Final: " ++ showEstado estado
    putStrLn $ "Heap Final: \n" ++ showHeap heap
    putStrLn "A variável global deve continuar 999, e o atributo x do objeto deve ser 123."
    putStrLn ""

-- Teste 7: Atribuição direta altera variável global.
testeAmbiente2 :: IO ()
testeAmbiente2 = do
    putStrLn "=== Teste 7: Atribuição direta altera variável global ==="
    let programa = [
            JI.Atr (JI.Var "g") (JI.Lit 1),
            JI.Atr (JI.Var "g") (JI.Lit 2)
            ]
        (resultado, estado, heap) = JI.intPrograma [] programa [] []
    putStrLn $ "Estado Final: " ++ showEstado estado
    putStrLn "A variável global 'g' deve ser 2."
    putStrLn ""



-- Teste específico para verificar isolamento de escopo
testeEscopoCorreto :: IO ()
testeEscopoCorreto = do
    putStrLn "=== Teste: Isolamento de Escopo Correto ==="
    let classe = JI.Class "Test" ["x"] [
            JI.Metodo "metodo" ["param"] (
                JI.Seq (JI.Atr (JI.Var "localVar") (JI.Lit 999))  -- Variável local
                       (JI.Var "param")  -- Retorna parâmetro
            )
            ]
        programa = [
            classe,
            JI.Atr (JI.Var "globalVar") (JI.Lit 123),  -- Variável global
            JI.Atr (JI.Var "obj") (JI.New "Test"),
            JI.MethodCall (JI.Var "obj") "metodo" [JI.Lit 777]
            ]
    let (resultado, estado, heap) = JI.intPrograma [] programa [] []
    
    putStrLn $ "Resultado: " ++ show resultado
    putStrLn $ "Estado Final: " ++ showEstado estado
    putStrLn "Esperado: Apenas [globalVar = 123.0, obj = 1.0]"
    putStrLn "NÃO deve conter: __this__, param, localVar"
    putStrLn ""

-- ============================================================================
-- FUNÇÃO PRINCIPAL PARA EXECUTAR TODOS OS TESTES
-- ============================================================================

main :: IO ()
main = do
    putStrLn "####### INICIANDO TESTES DO INTERPRETADOR JAVA #######"
    putStrLn ""

    testeThis1
    testeThis2
    testeObjeto1
    testeMetodo1
    testeMetodo2
    testeAmbiente1
    testeAmbiente2
    testeEscopoCorreto

    putStrLn "####### TESTES CONCLUÍDOS COM SUCESSO! #######"