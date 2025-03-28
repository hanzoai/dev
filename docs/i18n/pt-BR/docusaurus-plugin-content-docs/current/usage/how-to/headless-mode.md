# Modo Headless

Você pode executar o Dev com um único comando, sem iniciar a aplicação web.
Isso facilita a escrita de scripts e a automação de tarefas com o Dev.

Isso é diferente do [Modo CLI](cli-mode), que é interativo e melhor para desenvolvimento ativo.

## Com Python

Para executar o Dev no modo headless com Python:
1. Certifique-se de ter seguido as [instruções de configuração de desenvolvimento](https://github.com/hanzoai/dev/blob/main/Development.md).
2. Execute o seguinte comando:
```bash
poetry run python -m dev.core.main -t "escreva um script bash que imprima oi"
```

Você precisará definir seu modelo, chave de API e outras configurações por meio de variáveis de ambiente
[ou do arquivo `config.toml`](https://github.com/hanzoai/dev/blob/main/config.template.toml).

## Com Docker

Para executar o Dev no modo Headless com Docker:

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
    -e LOG_ALL_EVENTS=true \
    -v $WORKSPACE_BASE:/opt/workspace_base \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v ~/.dev-state:/.dev-state \
    --add-host host.docker.internal:host-gateway \
    --name dev-app-$(date +%Y%m%d%H%M%S) \
    docker.hanzo.ai/hanzoai/dev:0.30 \
    python -m dev.core.main -t "escreva um script bash que imprima oi"
```

## Configurações Avançadas do Headless

Para visualizar todas as opções de configuração disponíveis para o modo headless, execute o comando Python com a flag `--help`.

### Logs Adicionais

Para que o modo headless registre todas as ações do agente, execute no terminal: `export LOG_ALL_EVENTS=true`
