# Modo CLI

O Dev pode ser executado em um modo CLI interativo, que permite aos usuários iniciar uma sessão interativa via linha de comando.

Esse modo é diferente do [modo headless](headless-mode), que é não interativo e melhor para scripting.

## Com Python

Para iniciar uma sessão interativa do Dev via linha de comando:

1. Certifique-se de ter seguido as [instruções de configuração de desenvolvimento](https://github.com/hanzoai/dev/blob/main/Development.md).
2. Execute o seguinte comando:

```bash
poetry run python -m dev.core.cli
```

Esse comando iniciará uma sessão interativa onde você pode inserir tarefas e receber respostas do Dev.

Você precisará definir seu modelo, chave de API e outras configurações via variáveis de ambiente
[ou o arquivo `config.toml`](https://github.com/hanzoai/dev/blob/main/config.template.toml).

## Com Docker

Para executar o Dev no modo CLI com Docker:

1. Defina as seguintes variáveis de ambiente no seu terminal:

- `WORKSPACE_BASE` para o diretório que você deseja que o Dev edite (Ex: `export WORKSPACE_BASE=$(pwd)/workspace`).
- `LLM_MODEL` para o modelo a ser usado (Ex: `export LLM_MODEL="anthropic/claude-3-5-sonnet-20241022"`).
- `LLM_API_KEY` para a chave de API (Ex: `export LLM_API_KEY="sk_test_12345"`).

2. Execute o seguinte comando Docker:

```bash
docker run -it \
    --pull=always \
    -e SANDBOX_RUNTIME_CONTAINER_IMAGE=docker.hanzo.ai/hanzoai/runtime:0.30-nikolaik \
    -e SANDBOX_USER_ID=$(id -u) \
    -e WORKSPACE_MOUNT_PATH=$WORKSPACE_BASE \
    -e LLM_API_KEY=$LLM_API_KEY \
    -e LLM_MODEL=$LLM_MODEL \
    -v $WORKSPACE_BASE:/opt/workspace_base \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v ~/.dev-state:/.dev-state \
    --add-host host.docker.internal:host-gateway \
    --name dev-app-$(date +%Y%m%d%H%M%S) \
    docker.hanzo.ai/hanzoai/dev:0.30 \
    python -m dev.core.cli
```

Esse comando iniciará uma sessão interativa no Docker onde você pode inserir tarefas e receber respostas do Dev.

## Exemplos de Comandos CLI e Saídas Esperadas

Aqui estão alguns exemplos de comandos CLI e suas saídas esperadas:

### Exemplo 1: Tarefa Simples

```bash
>> Escreva um script Python que imprima "Hello, World!"
```

Saída Esperada:

```bash
🤖 Claro! Aqui está um script Python que imprime "Hello, World!":

❯ print("Hello, World!")
```

### Exemplo 2: Comando Bash

```bash
>> Crie um diretório chamado "test_dir"
```

Saída Esperada:

```bash
🤖 Criando um diretório chamado "test_dir":

❯ mkdir test_dir
```

### Exemplo 3: Tratamento de Erro

```bash
>> Exclua um arquivo inexistente
```

Saída Esperada:

```bash
🤖 Ocorreu um erro. Por favor, tente novamente.
```
