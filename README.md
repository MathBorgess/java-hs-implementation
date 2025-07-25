# Implementação das Declarações Básicas do Java em Haskell

Este projeto implementa as declarações fundamentais do Java usando Haskell, incluindo:

## Declarações Implementadas

1. **IF/ELSE** - Estruturas condicionais
2. **WHILE** - Loops com condição no início
3. **FOR** - Loops com inicialização, condição e incremento
4. **DO-WHILE** - Loops com condição no final
5. **BREAK** - Interrompe loops
6. **CONTINUE** - Pula para próxima iteração
7. **RETURN** - Retorna valores de funções
8. **Atribuições** - Definição e modificação de variáveis
9. **Blocos** - Agrupamento de declarações

## Estrutura do Código

### Tipos de Dados Principais

- `Expression`: Representa expressões (variáveis, literais, operações)
- `Statement`: Representa declarações (if, while, for, etc.)
- `Value`: Valores durante execução (números, booleanos, erros)
- `State`: Estado do programa (variáveis e seus valores)
- `ExecResult`: Resultado da execução (normal, break, continue, return, erro)

### Exemplos de Teste

O código inclui 5 exemplos que demonstram:

1. **exemploWhile**: Contador de 0 a 5

   - Resultado: `counter = 5`

2. **exemploIfElse**: Condicional com x = 10

   - Resultado: `x = 10, y = 20` (y = x \* 2 porque x > 5)

3. **exemploFor**: Soma de 1 a 10

   - Resultado: `sum = 55, i = 11`

4. **exemploDoWhile**: Multiplicação por 2 até >= 100

   - Resultado: `num = 128` (1 → 2 → 4 → 8 → 16 → 32 → 64 → 128)

5. **exemploBreakContinue**: Loop com break e continue
   - Resultado: `i = 20, found = 0` (pula números pares, para quando i > 15)

## Como Executar

```bash
# Compilar e testar
ghci basic-statements.hs

# No GHCI, executar:
testarExemplos
```

## Características Implementadas

- **Avaliação de expressões**: Suporte a operações aritméticas, lógicas e de comparação
- **Controle de fluxo**: IF/ELSE, loops (WHILE, FOR, DO-WHILE)
- **Controle de loop**: BREAK e CONTINUE funcionais
- **Gerenciamento de estado**: Variáveis persistem entre declarações
- **Tratamento de erros**: Erros são propagados adequadamente
- **Semântica correta**: Comportamento idêntico ao Java

Este interpretador demonstra como implementar estruturas imperativas usando programação funcional, mantendo a semântica original do Java.
