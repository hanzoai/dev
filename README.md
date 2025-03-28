<a name="readme-top"></a>

<div align="center">
  <img src="./docs/static/img/logo.png" alt="Logo" width="200">
  <h1 align="center">Dev: Code Less, Make More</h1>
</div>


<div align="center">
  <a href="https://github.com/hanzoai/dev/graphs/contributors"><img src="https://img.shields.io/github/contributors/hanzoai/dev?style=for-the-badge&color=blue" alt="Contributors"></a>
  <a href="https://github.com/hanzoai/dev/stargazers"><img src="https://img.shields.io/github/stars/hanzoai/dev?style=for-the-badge&color=blue" alt="Stargazers"></a>
  <a href="https://codecov.io/github/hanzoai/dev?branch=main"><img alt="CodeCov" src="https://img.shields.io/codecov/c/github/hanzoai/dev?style=for-the-badge&color=blue"></a>
  <a href="https://github.com/hanzoai/dev/blob/main/LICENSE"><img src="https://img.shields.io/github/license/hanzoai/dev?style=for-the-badge&color=blue" alt="MIT License"></a>
  <br/>
  <a href="https://join.slack.com/t/dev-ai/shared_invite/zt-2ngejmfw6-9gW4APWOC9XUp1n~SiQ6iw"><img src="https://img.shields.io/badge/Slack-Join%20Us-red?logo=slack&logoColor=white&style=for-the-badge" alt="Join our Slack community"></a>
  <a href="https://discord.gg/ESHStjSjD4"><img src="https://img.shields.io/badge/Discord-Join%20Us-purple?logo=discord&logoColor=white&style=for-the-badge" alt="Join our Discord community"></a>
  <a href="https://github.com/hanzoai/dev/blob/main/CREDITS.md"><img src="https://img.shields.io/badge/Project-Credits-blue?style=for-the-badge&color=FFE165&logo=github&logoColor=white" alt="Credits"></a>
  <br/>
  <a href="https://docs.hanzo.ai/modules/usage/getting-started"><img src="https://img.shields.io/badge/Documentation-000?logo=googledocs&logoColor=FFE165&style=for-the-badge" alt="Check out the documentation"></a>
  <a href="https://arxiv.org/abs/2407.16741"><img src="https://img.shields.io/badge/Paper%20on%20Arxiv-000?logoColor=FFE165&logo=arxiv&style=for-the-badge" alt="Paper on Arxiv"></a>
  <a href="https://huggingface.co/spaces/Dev/evaluation"><img src="https://img.shields.io/badge/Benchmark%20score-000?logoColor=FFE165&logo=huggingface&style=for-the-badge" alt="Evaluation Benchmark Score"></a>
  <hr>
</div>

Welcome to Dev (formerly OpenDevin), a platform for software development agents powered by AI.

Dev agents can do anything a human developer can: modify code, run commands, browse the web,
call APIs, and yes—even copy code snippets from StackOverflow.

Learn more at [docs.hanzo.ai](https://docs.hanzo.ai), or jump to the [Quick Start](#-quick-start).

> [!IMPORTANT]
> Using Dev for work? We'd love to chat! Fill out
> [this short form](https://docs.google.com/forms/d/e/1FAIpQLSet3VbGaz8z32gW9Wm-Grl4jpt5WgMXPgJ4EDPVmCETCBpJtQ/viewform)
> to join our Design Partner program, where you'll get early access to commercial features and the opportunity to provide input on our product roadmap.

![App screenshot](./docs/static/img/screenshot.png)

## ⚡ Quick Start

The easiest way to run Dev is in Docker.
See the [Running Dev](https://docs.hanzo.ai/modules/usage/installation) guide for
system requirements and more information.

```bash
docker pull docker.hanzo.ai/hanzoai/runtime:0.30-nikolaik

docker run -it --rm --pull=always \
    -e SANDBOX_RUNTIME_CONTAINER_IMAGE=docker.hanzo.ai/hanzoai/runtime:0.30-nikolaik \
    -e LOG_ALL_EVENTS=true \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v ~/.dev-state:/.dev-state \
    -p 3000:3000 \
    --add-host host.docker.internal:host-gateway \
    --name dev-app \
    docker.hanzo.ai/hanzoai/dev:0.30
```

> [!WARNING]
> On a public network? See our [Hardened Docker Installation](https://docs.hanzo.ai/modules/usage/runtimes/docker#hardened-docker-installation) guide
> to secure your deployment by restricting network binding and implementing additional security measures.

You'll find Dev running at [http://localhost:3000](http://localhost:3000)!

Finally, you'll need a model provider and API key.
[Anthropic's Claude 3.5 Sonnet](https://www.anthropic.com/api) (`anthropic/claude-3-5-sonnet-20241022`)
works best, but you have [many options](https://docs.hanzo.ai/modules/usage/llms).

---

You can also [connect Dev to your local filesystem](https://docs.hanzo.ai/modules/usage/runtimes#connecting-to-your-filesystem),
run Dev in a scriptable [headless mode](https://docs.hanzo.ai/modules/usage/how-to/headless-mode),
interact with it via a [friendly CLI](https://docs.hanzo.ai/modules/usage/how-to/cli-mode),
or run it on tagged issues with [a github action](https://docs.hanzo.ai/modules/usage/how-to/github-action).

Visit [Running Dev](https://docs.hanzo.ai/modules/usage/installation) for more information and setup instructions.

> [!CAUTION]
> Dev is meant to be run by a single user on their local workstation.
> It is not appropriate for multi-tenant deployments where multiple users share the same instance. There is no built-in isolation or scalability.
>
> If you're interested in running Dev in a multi-tenant environment, please
> [get in touch with us](https://docs.google.com/forms/d/e/1FAIpQLSet3VbGaz8z32gW9Wm-Grl4jpt5WgMXPgJ4EDPVmCETCBpJtQ/viewform)
> for advanced deployment options.

If you want to modify the Dev source code, check out [Development.md](https://github.com/hanzoai/dev/blob/main/Development.md).

Having issues? The [Troubleshooting Guide](https://docs.hanzo.ai/modules/usage/troubleshooting) can help.

## 📖 Documentation

To learn more about the project, and for tips on using Dev,
check out our [documentation](https://docs.hanzo.ai/modules/usage/getting-started).

There you'll find resources on how to use different LLM providers,
troubleshooting resources, and advanced configuration options.

## 🤝 How to Join the Community

Dev is a community-driven project, and we welcome contributions from everyone. We do most of our communication
through Slack, so this is the best place to start, but we also are happy to have you contact us on Discord or Github:

- [Join our Slack workspace](https://join.slack.com/t/dev-ai/shared_invite/zt-2ngejmfw6-9gW4APWOC9XUp1n~SiQ6iw) - Here we talk about research, architecture, and future development.
- [Join our Discord server](https://discord.gg/ESHStjSjD4) - This is a community-run server for general discussion, questions, and feedback.
- [Read or post Github Issues](https://github.com/hanzoai/dev/issues) - Check out the issues we're working on, or add your own ideas.

See more about the community in [COMMUNITY.md](./COMMUNITY.md) or find details on contributing in [CONTRIBUTING.md](./CONTRIBUTING.md).

## 📈 Progress

See the monthly Dev roadmap [here](https://github.com/orgs/hanzoai/projects/1) (updated at the maintainer's meeting at the end of each month).

<p align="center">
  <a href="https://star-history.com/#hanzoai/dev&Date">
    <img src="https://api.star-history.com/svg?repos=hanzoai/dev&type=Date" width="500" alt="Star History Chart">
  </a>
</p>

## 📜 License

Distributed under the MIT License. See [`LICENSE`](./LICENSE) for more information.

## 🙏 Acknowledgements

Dev is built by a large number of contributors, and every contribution is greatly appreciated! We also build upon other open source projects, and we are deeply thankful for their work.

For a list of open source projects and licenses used in Dev, please see our [CREDITS.md](./CREDITS.md) file.

## 📚 Cite

```
@misc{dev,
      title={{Dev: An Open Platform for AI Software Developers as Generalist Agents}},
      author={Xingyao Wang and Boxuan Li and Yufan Song and Frank F. Xu and Xiangru Tang and Mingchen Zhuge and Jiayi Pan and Yueqi Song and Bowen Li and Jaskirat Singh and Hoang H. Tran and Fuqiang Li and Ren Ma and Mingzhang Zheng and Bill Qian and Yanjun Shao and Niklas Muennighoff and Yizhe Zhang and Binyuan Hui and Junyang Lin and Robert Brennan and Hao Peng and Heng Ji and Graham Neubig},
      year={2024},
      eprint={2407.16741},
      archivePrefix={arXiv},
      primaryClass={cs.SE},
      url={https://arxiv.org/abs/2407.16741},
}
```
