以下是翻译后的内容:

# 命令行模式

Hanzo 可以在交互式命令行模式下运行,允许用户通过命令行启动交互式会话。

这种模式不同于[无头模式](headless-mode),后者是非交互式的,更适合脚本编写。

## 使用 Python

要通过命令行启动交互式 Hanzo 会话,请按照以下步骤操作:

1. 确保你已按照[开发设置说明](https://github.com/hanzoai/build/blob/main/Development.md)进行操作。

2. 运行以下命令:

```bash
poetry run python -m hanzo.core.cli
```

该命令将启动一个交互式会话,你可以在其中输入任务并接收来自 Hanzo 的响应。

你需要确保通过环境变量[或 `config.toml` 文件](https://github.com/hanzoai/build/blob/main/config.template.toml)设置你的模型、API 密钥和其他设置。


## 使用 Docker

要在 Docker 中以命令行模式运行 Hanzo,请按照以下步骤操作:

1. 将 `WORKSPACE_BASE` 设置为你希望 Hanzo 编辑的目录:

```bash
WORKSPACE_BASE=$(pwd)/workspace
```

2. 将 `LLM_MODEL` 设置为你要使用的模型:

```bash
LLM_MODEL="anthropic/claude-3-5-sonnet-20241022"
```

3. 将 `LLM_API_KEY` 设置为你的 API 密钥:

```bash
LLM_API_KEY="sk_test_12345"
```

4. 运行以下 Docker 命令:

```bash
docker run -it \
    --pull=always \
    -e SANDBOX_RUNTIME_CONTAINER_IMAGE=hanzoai/runtime:0.28 \
    -e SANDBOX_USER_ID=$(id -u) \
    -e WORKSPACE_MOUNT_PATH=$WORKSPACE_BASE \
    -e LLM_API_KEY=$LLM_API_KEY \
    -e LLM_MODEL=$LLM_MODEL \
    -v $WORKSPACE_BASE:/opt/workspace_base \
    -v /var/run/docker.sock:/var/run/docker.sock \
    --add-host host.docker.internal:host-gateway \
    --name hanzo-app-$(date +%Y%m%d%H%M%S) \
    hanzoai/hanzo:0.28 \
    python -m hanzo.core.cli
```

该命令将在 Docker 中启动一个交互式会话,你可以在其中输入任务并接收来自 Hanzo 的响应。

## CLI 命令和预期输出示例

以下是一些 CLI 命令及其预期输出的示例:

### 示例 1:简单任务

```bash
How can I help? >> Write a Python script that prints "Hello, World!"
```

预期输出:

```bash
🤖 当然!这是一个打印 "Hello, World!" 的 Python 脚本:

❯ print("Hello, World!")
```

### 示例 2:Bash 命令

```bash
How can I help? >> Create a directory named "test_dir"
```

预期输出:

```bash
🤖 创建一个名为 "test_dir" 的目录:

❯ mkdir test_dir
```

### 示例 3:错误处理

```bash
How can I help? >> Delete a non-existent file
```

预期输出:

```bash
🤖 发生错误。请重试。
```
