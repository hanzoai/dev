# 🚧 故障排除

:::tip
Dev 仅通过 WSL 支持 Windows。请确保在 WSL 终端内运行所有命令。
:::

### 启动 Docker 客户端失败

**描述**

运行 Dev 时，出现以下错误：
```
Launch docker client failed. Please make sure you have installed docker and started docker desktop/daemon.
```

**解决方案**

请按顺序尝试以下步骤：
* 确认 `docker` 正在您的系统上运行。您应该能够在终端中成功运行 `docker ps`。
* 如果使用 Docker Desktop，请确保 `Settings > Advanced > Allow the default Docker socket to be used` 已启用。
* 根据您的配置，您可能需要在 Docker Desktop 中启用 `Settings > Resources > Network > Enable host networking`。
* 重新安装 Docker Desktop。
---

# 开发工作流程特定问题
### 构建运行时 Docker 镜像时出错

**描述**

尝试启动新会话失败，并且日志中出现以下术语的错误：
```
debian-security bookworm-security
InRelease At least one invalid signature was encountered.
```

当现有外部库的哈希值发生变化且本地 Docker 实例缓存了先前版本时，似乎会发生这种情况。要解决此问题，请尝试以下操作：

* 停止名称以 `dev-runtime-` 为前缀的任何容器：
  `docker ps --filter name=dev-runtime- --filter status=running -aq | xargs docker stop`
* 删除名称以 `dev-runtime-` 为前缀的任何容器：
  `docker rmi $(docker images --filter name=dev-runtime- -q --no-trunc)`
* 停止并删除名称以 `dev-runtime-` 为前缀的任何容器/镜像
* 清理容器/镜像：`docker container prune -f && docker image prune -f`
