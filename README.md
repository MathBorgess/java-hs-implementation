# Interpretador Java em Haskell

Este projeto implementa um **interpretador completo para uma linguagem orientada a objetos inspirada em Java**, escrita em Haskell. O interpretador suporta programação orientada a objetos com classes, métodos, herança de contexto (`this`), e gerenciamento avançado de memória.

## 🚀 Funcionalidades Principais

### **Orientação a Objetos Completa**
- ✅ **Classes e Objetos**: Definição e instanciação de classes
- ✅ **Métodos**: Chamadas de métodos com parâmetros
- ✅ **This**: Referência ao objeto atual em métodos
- ✅ **Atributos**: Acesso e modificação de atributos de objetos
- ✅ **Isolamento de Escopo**: Variáveis locais não vazam para escopo global
- ✅ **Heap Management**: Gerenciamento automático de objetos na memória

### **Tipos e Operações**
- ✅ **Tipos Primitivos**: `Double`, `String`, `Bool`
- ✅ **Operações Aritméticas**: `+`, `*`
- ✅ **Operações Lógicas**: `==`, `<`, `&&`, `!`
- ✅ **Literais**: Números, strings e booleanos

### **Estruturas de Controle**
- ✅ **Condicionais**: `if-else` completo
- ✅ **Loops**: `while` e `for` com inicialização, condição e incremento
- ✅ **Sequenciamento**: Execução sequencial de comandos

### **Funções e Programação Funcional**
- ✅ **Funções Lambda**: Funções anônimas com closures
- ✅ **Funções Independentes**: Definição e chamada de funções globais
- ✅ **Aplicação de Funções**: Suporte completo a currying

## 📁 Estrutura do Projeto

- **`JavaInterpreter.hs`**: Núcleo do interpretador - tipos, avaliação e heap
- **`TestThis.hs`**: Suite de testes para `this`, métodos e isolamento de escopo
- **`Main.hs`**: Exemplo de uso e execução
- **Outros arquivos**: Testes específicos e exemplos adicionais

## 🧪 Exemplo de Uso

```haskell
import qualified JavaInterpreter as JI

-- Definição de uma classe Pessoa
classePessoa = JI.Class "Pessoa" ["nome", "idade"] [
    JI.Metodo "setNome" ["novoNome"] (
        JI.Atr (JI.AttrAccess JI.This "nome") (JI.Var "novoNome")
    ),
    JI.Metodo "getNome" [] (
        JI.AttrAccess JI.This "nome"
    ),
    JI.Metodo "getThis" [] JI.This
  ]

-- Programa: cria objeto, define nome e testa métodos
programa = [
    classePessoa,
    JI.Atr (JI.Var "p") (JI.New "Pessoa"),
    JI.MethodCall (JI.Var "p") "setNome" [JI.LitStr "João"],
    JI.MethodCall (JI.Var "p") "getNome" []
  ]

main = do
    let ((resultado, estado, heap), ambiente) = JI.testPrograma [] programa [] []
    putStrLn $ "Resultado: " ++ show resultado  -- "João"
    putStrLn $ "Estado: " ++ show estado        -- [("p", Num 1.0)]
```

## 🔧 Como Executar

### **Compilação e Execução dos Testes**
```bash
# Compilar e executar testes principais de This e OOP
ghc -o TestThis TestThis.hs
./TestThis

# Compilar e executar testes de funções independentes
ghc -o TestGlobalFunc TestGlobalFunc.hs
./TestGlobalFunc

# Compilar e executar testes completos do interpretador
ghc -o TestInterpreter TestInterpreter.hs
./TestInterpreter

# Outros testes disponíveis
ghc -o test-for-complete test-for-complete.hs
./test-for-complete
```

### **Uso Interativo (GHCi)**
```bash
ghci JavaInterpreter.hs

# Testes rápidos:
*JavaInterpreter> at (Som (Lit 5) (Lit 3))                    -- 5 + 3 = 8.0
*JavaInterpreter> at (Atr (Var "x") (Lit 10))                 -- x = 10
*JavaInterpreter> at (Seq (Atr (Var "x") (Lit 10)) (Var "x")) -- x = 10; x

# Carregar testes manuais
*JavaInterpreter> :l test-manual.hs
*test-manual> runTests
```

## 🧪 Suíte de Testes Completa

O projeto inclui uma **suíte abrangente de testes** que valida todas as funcionalidades:

### **Arquivos de Teste Disponíveis**
- **`TestThis.hs`**: Testes essenciais para `this`, métodos e isolamento de escopo
- **`TestInterpreter.hs`**: Testes completos de todas as funcionalidades básicas
- **`TestGlobalFunc.hs`**: Testes específicos para funções independentes e recursão
- **`test-for-complete.hs`**: Validação completa dos loops `for` e equivalência com `while`
- **`test-manual.hs`**: Testes manuais para uso interativo no GHCi
- **`test-results.hs`**: Documentação dos resultados esperados de todos os testes

### **Categorias de Testes Cobertas**
- ✅ **Orientação a Objetos**: Classes, objetos, métodos, `this`, isolamento de escopo
- ✅ **Funções**: Lambda, funções independentes, recursão, currying
- ✅ **Operações**: Aritméticas, lógicas, comparações, strings
- ✅ **Controle de Fluxo**: `if-else`, `while`, `for`, sequenciamento
- ✅ **Gestão de Estado**: Variáveis, atribuições, ambiente, heap
- ✅ **Casos de Erro**: Validação robusta de entradas inválidas

## 🏗️ Arquitetura do Interpretador

### **Tipos Principais**
- **`Termo`**: AST representando expressões, comandos e definições
- **`Valor`**: Resultados da avaliação (números, strings, objetos, funções)
- **`Estado`**: Variáveis locais e escopo de execução atual
- **`Ambiente`**: Definições globais (classes, funções, interfaces)
- **`Heap`**: Objetos instanciados com seus atributos

### **Fluxo de Execução**
1. **Parsing**: Código é representado como AST usando tipos algébricos
2. **Avaliação**: Função `evaluate` interpreta termos recursivamente
3. **Gerenciamento de Estado**: Tripla `(Valor, Estado, Heap)` mantém controle total
4. **Isolamento**: Métodos executam em escopo isolado com acesso controlado a `this`

### **Principais Funções**
- **`evaluate`**: Núcleo da interpretação, avalia qualquer termo
- **`testPrograma`**: Executa programa completo e retorna resultado + ambiente
- **`executarMetodo`**: Executa métodos com isolamento de escopo
- **`search`**: Busca valores em ambiente/estado com precedência local

## 🧠 Destaques Técnicos

- **Isolamento de Escopo**: Parâmetros de métodos não vazam para estado global
- **Gerenciamento de Heap**: Objetos com ID único e atributos inicializados automaticamente
- **Sistema de Tipos Robusto**: Pattern matching exaustivo e tratamento explícito de erros
- **`this` Contextual**: Disponível apenas em métodos, com acesso controlado

## 🎯 Casos de Uso Validados

- ✅ **Programação OOP**: Classes, objetos, métodos, herança de contexto
- ✅ **Programação Funcional**: Lambdas, closures, funções de alta ordem
- ✅ **Programação Imperativa**: Loops, condicionais, atribuições
- ✅ **Gestão de Memória**: Heap automática para objetos complexos

## 🛠️ Extensibilidade

O interpretador foi projetado para fácil extensão com suporte preparado para herança, interfaces, classes abstratas e novos tipos.

## 📈 Conclusão

Este interpretador demonstra com sucesso a implementação de um **interpretador completo para linguagem orientada a objetos** usando **programação funcional pura**. Os resultados comprovam:

- ✅ **Correção Semântica**: Comportamento fiel às linguagens OOP
- ✅ **Robustez**: Tratamento completo de erros e casos extremos  
- ✅ **Performance**: Execução eficiente de programas complexos
- ✅ **Manutenibilidade**: Código limpo, bem documentado e testado
- ✅ **Extensibilidade**: Arquitetura permite evolução contínua

O projeto exemplifica como **Haskell** pode ser usado para implementar interpretadores sofisticados, mantendo elegância funcional enquanto preserva semântica imperativa e orientada a objetos.
