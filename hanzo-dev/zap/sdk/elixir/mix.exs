defmodule HanzoZap.MixProject do
  use Mix.Project

  @version "0.6.0"
  @source_url "https://github.com/hanzoai/zap"

  def project do
    [
      app: :hanzo_zap,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs(),
      name: "HanzoZap",
      description: "Zero-copy Agent Protocol - 1000x faster than MCP/JSON-RPC",
      source_url: @source_url,
      homepage_url: "https://zap.hanzo.ai"
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      name: "hanzo_zap",
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url,
        "Documentation" => "https://hexdocs.pm/hanzo_zap",
        "Hanzo AI" => "https://hanzo.ai"
      },
      maintainers: ["Hanzo AI"],
      files: ~w(lib .formatter.exs mix.exs README.md LICENSE)
    ]
  end

  defp docs do
    [
      main: "HanzoZap",
      extras: ["README.md", "LICENSE"],
      source_ref: "v#{@version}"
    ]
  end
end
