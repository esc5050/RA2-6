# Sistema de Inventário Haskell

Este projeto é um sistema de gerenciamento de inventário interativo via terminal, desenvolvido em Haskell. O objetivo é aplicar conceitos de programação funcional, separando rigorosamente a lógica pura (manipulação de estado) das operações impuras (I/O), e garantir a persistência dos dados e um log de auditoria robusto.

---

## 1. Informações da Equipe 

* **Instituição:** PUCPR
* **Disciplina:** Programação Lógica e Funcional
* **Professor:** Frank Coelho De Alcantara

### Alunos
* **Antonio Bernardo Zilio Tomasi** (GitHub: `Tonhooooo`)
* **Gustavo Lona Grespan** (GitHub: `esc5050`)
* **Julia Machado Kociolek** (GitHub: `JuliaKociolek`)

---

## 2. Link de Execução

O projeto pode ser executado, testado e compilado sem modificações diretamente no GDB Online através do seguinte link:

**(https://onlinegdb.com/OwepNmwlJT)**


---

## 3. Como Compilar e Executar

O projeto consiste em um único arquivo `Main.hs` e não requer bibliotecas externas além das que acompanham o GHC (como `Data.Map` e `Data.Time`).

### Executando no GDB Online
1.  Abra o link fornecido na Seção 2.
2.  Clique no botão **"Run"** (Verde) no topo da tela.
3.  O código será compilado e o programa iniciará no terminal inferior.

### Arquivos Gerados
O programa utiliza dois arquivos para persistência:
* `Inventario.dat`: Armazena o estado atual do inventário. É **sobrescrito** a cada operação de sucesso.
* `Auditoria.log`: Armazena **todas** as tentativas de operação (sucesso ou falha) em modo "append-only".

---

## 4\. Comandos do Sistema

O programa funciona com os seguintes comandos interativos:

### `add`

Inicia o prompt para adicionar um novo item (ID, Nome, Quantidade, Categoria).

**Exemplo:**

```bash
Digite um comando (help para opções): add
ID do item: 5
Nome: Exemplo 01
Quantidade: 15
Categoria: Exemplos 
Item adicionado com sucesso.
```

### `remove`

Inicia o prompt para remover (dar baixa) em uma quantidade de um item existente (por ID).

**Exemplo:**

```bash
Digite um comando (help para opções): remove
ID do item: 5
Quantidade a remover: 10
Remoção aplicada.
```

### `update`

Permite atualizar nome, quantidade e/ou categoria de um item (deixe o campo em branco para manter o valor atual).

**Exemplo:**

```bash
Digite um comando (help para opções): update
ID do item: 5
Novo nome (Enter para manter): Exemplo Alterado
Nova quantidade (Enter para manter): 20
Nova categoria (Enter para manter): 
Item atualizado.
```

### `query`

Consulta e exibe os detalhes de um único item (por ID).

**Exemplo:**

```bash
Digite um comando (help para opções): query
ID do item: 5
Item encontrado: Item {itemID = "5", nome = "Exemplo Alterado", quantidade = 20, categoria = "Exemplos "}
```

### `list`

Lista todos os itens atualmente em memória no inventário.

**Exemplo:**

```bash
Digite um comando (help para opções): list

--- Inventário Atual ---
- 1: Teste 01 | qtd=20 | categoria=Sujeito de Teste
- 2: Teste 02 | qtd=40 | categoria=Sujeito de Teste
- 3: Teste 03 | qtd=80 | categoria=Sujeito de Teste
- 4: Teste 04 | qtd=10 | categoria=Sujeito de Teste
- 5: Exemplo Alterado | qtd=20 | categoria=Exemplos 
-----------------------
```

### `report`

Gera relatórios de auditoria com base no `Auditoria.log` (histórico por item, logs de erro, item mais movimentado).

**Exemplo:**

```bash
Digite um comando (help para opções): report

===== Relatório de Auditoria =====
Total de entradas registradas: 10
Item para histórico detalhado (Enter para pular): 

Falhas registradas: 2
  2025-11-15 00:04:53.110086095 UTC | Remove | Falha: Estoque insuficiente | item=4 :: Tentativa de remover -> Estoque insuficiente
  2025-11-15 00:17:48.457991125 UTC | Add | Falha: Item já existe no inventário (ID duplicado) | item=1 :: Tentativa de adicionar -> Item já existe no inventário (ID duplicado)

Item mais movimentado: 5 (3 operações)
==================================
```

### `help`

Exibe o menu de comandos.

**Exemplo:**

```bash
Digite um comando (help para opções): help

Comandos disponíveis:
    add       - adicionar novo item
    remove    - remover unidades de um item
    update    - atualizar campos de um item
    query     - consultar item
    list      - listar inventário completo
    report    - gerar relatório baseado nos logs
    help      - mostrar este menu
    exit      - sair do sistema
```

### `exit`

Salva o estado atual no `Inventario.dat` e encerra o programa.

**Exemplo:**

```bash
Digite um comando (help para opções): exit
Estado salvo. Encerrando...
```
---

## 5. Dados Mínimos de Teste 

## Testes Manuais

### Cenário 1: Persistência de Estado (Sucesso)

**Objetivo:** Verificar se o sistema salva o inventário e os logs de auditoria em arquivos (`Inventario.dat`, `Auditoria.log`) e os recarrega corretamente ao reiniciar.

**1. Adicionando Itens (Primeira Execução):**
O sistema é iniciado com 0 itens. Três itens são adicionados sequencialmente.

```bash
[1 of 2] Compiling Main           ( main.hs, main.o )
[2 of 2] Linking a.out
Sistema de Inventário Haskell
=================================================
Itens carregados: 0
Nenhum log de auditoria anterior encontrado.

Comandos disponíveis:
...
Digite um comando (help para opções): add
ID do item: 1
Nome: Teste 01
Quantidade: 20
Categoria: Sujeito de Teste
Item adicionado com sucesso.

Digite um comando (help para opções): add
ID do item: 2
Nome: Teste 02
...
Item adicionado com sucesso.

Digite um comando (help para opções): add
ID do item: 3
Nome: Teste 03
...
Item adicionado com sucesso.
```

**2. Verificação dos Arquivos (Após Sair):**
Após finalizar o programa, os arquivos de persistência foram criados:

  * `Auditoria.log`:

    ```log
    LogEntry {timestamp = 2025-11-14 23:58:39.485468905 UTC, acao = Add, detalhes = "item=1 :: Adicionado: Teste 01", status = Sucesso}
    LogEntry {timestamp = 2025-11-14 23:58:54.253775977 UTC, acao = Add, detalhes = "item=2 :: Adicionado: Teste 02", status = Sucesso}
    LogEntry {timestamp = 2025-11-14 23:59:14.617617937 UTC, acao = Add, detalhes = "item=3 :: Adicionado: Teste 03", status = Sucesso}
    ```

  * `Inventario.dat`:

    ```haskell
    fromList [("1",Item {itemID = "1", nome = "Teste 01", quantidade = 20, categoria = "Sujeito de Teste"}),("2",Item {itemID = "2", nome = "Teste 02", quantidade = 40, categoria = "Sujeito de Teste"}),("3",Item {itemID = "3", nome = "Teste 03", quantidade = 80, categoria = "Sujeito de Teste"})]
    ```

**3. Verificação (Após Reiniciar):**
Ao ser iniciado novamente, o sistema carrega os dados salvos e o comando `list` exibe o inventário correto.

```bash
[1 of 2] Compiling Main           ( main.hs, main.o )
[2 of 2] Linking a.out
Sistema de Inventário Haskell (Projeto de Estudo)
=================================================
Itens carregados: 3
Últimos 5 registros (Auditoria.log):
LogEntry {timestamp = 2025-11-14 23:58:39.485468905 UTC, acao = Add, detalhes = "item=1 :: Adicionado: Teste 01", status = Sucesso}
LogEntry {timestamp = 2025-11-14 23:58:54.253775977 UTC, acao = Add, detalhes = "item=2 :: Adicionado: Teste 02", status = Sucesso}
LogEntry {timestamp = 2025-11-14 23:59:14.617617937 UTC, acao = Add, detalhes = "item=3 :: Adicionado: Teste 03", status = Sucesso}

Comandos disponíveis:
...
Digite um comando (help para opções): list

--- Inventário Atual ---
- 1: Teste 01 | qtd=20 | categoria=Sujeito de Teste
- 2: Teste 02 | qtd=40 | categoria=Sujeito de Teste
- 3: Teste 03 | qtd=80 | categoria=Sujeito de Teste
------------------------
```

**Resultado:** **Sucesso.** O estado foi persistido e recarregado corretamente.

-----

### Cenário 2: Tratamento de Erro (Estoque Insuficiente)

**Objetivo:** Verificar se o sistema impede a remoção de uma quantidade maior de itens do que a existente no estoque e se registra essa falha corretamente na auditoria, sem alterar o inventário.

**1. Ações:**
Adicionamos o "Item 4" com **10 unidades** e, em seguida, tentamos remover **15 unidades**.

```bash
Digite um comando (help para opções): add
ID do item: 4
Nome: Teste 04 
Quantidade: 10
Categoria: Sujeito de Teste
Item adicionado com sucesso.

Digite um comando (help para opções): remove
ID do item: 4
Quantidade a remover: 15
Erro: Estoque insuficiente
```

**2. Verificação dos Arquivos:**
Os arquivos de log e inventário devem refletir a tentativa falha.

  * `Auditoria.log` (registra a adição e a falha):

    ```log
    ...
    LogEntry {timestamp = 2025-11-15 00:04:40.5459967 UTC, acao = Add, detalhes = "item=4 :: Adicionado: Teste 04", status = Sucesso}
    LogEntry {timestamp = 2025-11-15 00:04:53.110086095 UTC, acao = Remove, detalhes = "item=4 :: Tentativa de remover -> Estoque insuficiente", status = Falha "Estoque insuficiente"} 
    ```

  * `Inventario.dat` (item 4 permanece com 10 unidades):

    ```haskell
    fromList [..., ("3",Item {itemID = "3", nome = "Teste 03", quantidade = 80, categoria = "Sujeito de Teste"}),("4",Item {itemID = "4", nome = "Teste 04", quantidade = 10, categoria = "Sujeito de Teste"})]
    ```

**Resultado:** **Sucesso.** A operação falhou como esperado, o erro foi logado e o `Inventario.dat` permaneceu íntegro (Item 4 continua com 10 unidades).

-----

### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar se o comando `report` analisa o `Auditoria.log` e exibe corretamente as falhas ocorridas (como a do Cenário 2).

**1. Ação:**
Executamos o comando `report` após a falha do cenário anterior.

```bash
Digite um comando (help para opções): report

===== Relatório de Auditoria =====
Total de entradas registradas: 5
Item para histórico detalhado (Enter para pular): 

Falhas registradas: 1
  2025-11-15 00:04:53.110086095 UTC | Remove | Falha: Estoque insuficiente | item=4 :: Tentativa de remover -> Estoque insuficiente

Item mais movimentado: 4 (2 operações)
==================================
```

**Resultado:** **Sucesso.** O relatório identificou e exibiu corretamente a falha de "Estoque insuficiente" registrada no log de auditoria.
