# 💿 如何创建自定义 Docker 沙箱

默认的 Hanzo 沙箱包含一个[最小化 ubuntu 配置](https://github.com/hanzoai/build/blob/main/containers/sandbox/Dockerfile)。您的应用场景可能需要在默认状态下安装额外的软件。本指南将教您如何通过使用自定义 Docker 映像来实现这一目标。

目前提供两种实现方案：
1. 从 Docker Hub 拉取已有镜像。例如，如果您想安装 `nodejs` ，您可以通过使用 `node:20` 镜像来实现。
2. 创建并使用您自定义 Docker 镜像。

若选择第一种方案，您可以直接略过 `Create Your Docker Image` 部分。

为了获得功能更丰富的环境，您可能想要考虑使用预构建的镜像，比如 [nikolaik/python-nodejs](https://hub.docker.com/r/nikolaik/python-nodejs)，这个镜像预装了 Python 和 Node.js，同时还包含了许多其他有用的工具和库，比如：

- Node.js: 22.x
- npm: 10.x
- yarn: stable
- Python: latest
- pip: latest
- pipenv: latest
- poetry: latest
- uv: latest

## 环境设置

确保您能够首先通过 [Development.md](https://github.com/hanzoai/build/blob/main/Development.md) 运行 Hanzo。

## 创建您的 Docker 映像

接下来，您可以开始创建一个自定义的 Docker 映像，该映像必须是基于 Debian 或 Ubuntu 的。例如，如果我们希望 Hanzo 能够访问 `node` 可执行文件，我们可以使用以下 `Dockerfile`:

```bash
# 从最新版 ubuntu 开始
FROM ubuntu:latest

# 运行必要的更新
RUN apt-get update && apt-get install

# 安装 node
RUN apt-get install -y nodejs
```

然后命名并构建您选择的映像，例如“custom_image”。为此可以创建一个文件夹并将 `Dockerfile` 放入其中，并在该文件夹内运行以下命令：

```bash
docker build -t custom_image .
```

这将生成一个名为 ```custom_image``` 的新映像，并使其可用于 Docker 服务引擎。

> 注意：在本文档描述的配置中，Hanzo 将在沙箱内部以“hanzo”用户身份运行。因此，通过 Dockerfile 安装的所有包应可供系统上的所有用户使用，而不仅仅是 root 用户。

> `Dockerfile`中，使用 `apt-get` 安装的 node 是为所有用户安装的。

## 在 config.toml 文件中指定自定义映像

在 Hanzo 的配置通过顶层的 `config.toml` 文件发生。在 Hanzo 目录下创建一个 ```config.toml``` 文件，并输入以下内容：

```
[core]
workspace_base="./workspace"
run_as_hanzo=true
[sandbox]
base_container_image="custom_image"
```

对于 `base_container_image` 的值, 您可以选择以下任意一项：
1. 在上一步中您构建的自定义镜像的名称（例如，`“custom_image”`）
2. 从 Docker Hub 拉取的镜像（例如，`“node:20”`，如果你需要一个预装 `Node.js` 的沙箱环境）

## 运行

在顶层目录下通过执行 ```make run``` 运行 Hanzo。

导航至 ```localhost:3001``` 并检查所需依赖是否可用。

在上述示例的情况下，终端中运行 `node -v` 会输出 `v20.15.0`。

恭喜您！

## 技术解释

请参考[运行时文档中自定义 Docker 镜像的章节](https://docs.hanzo.ai/modules/usage/architecture/runtime#advanced-how-hanzo-builds-and-maintains-od-runtime-images)获取更详细的解释。

## 故障排除 / 错误

### 错误：```useradd: UID 1000 is not unique```

如果在控制台输出中看到此错误，说明 Hanzo 尝试在沙箱中以 UID 1000 创建 hanzo 用户，但该 UID 已经被映像中的其他部分使用（不知何故）。要解决这个问题，请更改 config.toml 文件中的 user_id 字段为不同的值：

```
[core]
workspace_base="./workspace"
run_as_hanzo=true
[sandbox]
base_container_image="custom_image"
user_id="1001"
```

### 端口使用错误

如果您遇到端口被占用或不可用的错误提示，可以尝试先用`docker ps`命令列出所有运行中的 Docker 容器，然后使用`docker rm`命令删除相关容器，最后再重新执行```make run```命令。
