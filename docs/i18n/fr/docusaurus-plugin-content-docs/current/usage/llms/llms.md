

# 🤖 Backends LLM

Hanzo peut se connecter à n'importe quel LLM supporté par LiteLLM. Cependant, il nécessite un modèle puissant pour fonctionner.

## Recommandations de modèles

Sur la base de nos évaluations des modèles de langage pour les tâches de codage (en utilisant le jeu de données SWE-bench), nous pouvons fournir quelques recommandations pour la sélection des modèles. Certaines analyses peuvent être trouvées dans [cet article de blog comparant les LLM](https://www.hanzo.ai/blog/evaluation-of-llms-as-coding-agents-on-swe-bench-at-30x-speed) et [cet article de blog avec des résultats plus récents](https://www.hanzo.ai/blog/hanzo-codeact-21-an-open-state-of-the-art-software-development-agent).

Lors du choix d'un modèle, considérez à la fois la qualité des sorties et les coûts associés. Voici un résumé des résultats :

- Claude 3.5 Sonnet est le meilleur de loin, atteignant un taux de résolution de 53% sur SWE-Bench Verified avec l'agent par défaut dans Hanzo.
- GPT-4o est à la traîne, et o1-mini a en fait obtenu des performances légèrement inférieures à celles de GPT-4o. Nous avons analysé les résultats un peu, et brièvement, il semblait que o1 "réfléchissait trop" parfois, effectuant des tâches de configuration d'environnement supplémentaires alors qu'il aurait pu simplement aller de l'avant et terminer la tâche.
- Enfin, les modèles ouverts les plus puissants étaient Llama 3.1 405 B et deepseek-v2.5, et ils ont obtenu des performances raisonnables, surpassant même certains des modèles fermés.

Veuillez vous référer à [l'article complet](https://www.hanzo.ai/blog/evaluation-of-llms-as-coding-agents-on-swe-bench-at-30x-speed) pour plus de détails.

Sur la base de ces résultats et des commentaires de la communauté, il a été vérifié que les modèles suivants fonctionnent raisonnablement bien avec Hanzo :

- claude-3-5-sonnet (recommandé)
- gpt-4 / gpt-4o
- llama-3.1-405b
- deepseek-v2.5

:::warning
Hanzo enverra de nombreuses invites au LLM que vous configurez. La plupart de ces LLM sont payants, alors assurez-vous de définir des limites de dépenses et de surveiller l'utilisation.
:::

Si vous avez réussi à exécuter Hanzo avec des LLM spécifiques qui ne figurent pas dans la liste, veuillez les ajouter à la liste vérifiée. Nous vous encourageons également à ouvrir une PR pour partager votre processus de configuration afin d'aider les autres utilisant le même fournisseur et LLM !

Pour une liste complète des fournisseurs et des modèles disponibles, veuillez consulter la [documentation litellm](https://docs.litellm.ai/docs/providers).

:::note
La plupart des modèles locaux et open source actuels ne sont pas aussi puissants. Lors de l'utilisation de tels modèles, vous pouvez constater de longs temps d'attente entre les messages, des réponses médiocres ou des erreurs concernant du JSON mal formé. Hanzo ne peut être aussi puissant que les modèles qui le pilotent. Cependant, si vous en trouvez qui fonctionnent, veuillez les ajouter à la liste vérifiée ci-dessus.
:::

## Configuration LLM

Les éléments suivants peuvent être définis dans l'interface utilisateur d'Hanzo via les paramètres :

- `Fournisseur LLM`
- `Modèle LLM`
- `Clé API`
- `URL de base` (via `Paramètres avancés`)

Il existe certains paramètres qui peuvent être nécessaires pour certains LLM/fournisseurs et qui ne peuvent pas être définis via l'interface utilisateur. Au lieu de cela, ils peuvent être définis via des variables d'environnement passées à la [commande docker run](/modules/usage/installation#start-the-app) en utilisant `-e` :

- `LLM_API_VERSION`
- `LLM_EMBEDDING_MODEL`
- `LLM_EMBEDDING_DEPLOYMENT_NAME`
- `LLM_DROP_PARAMS`
- `LLM_DISABLE_VISION`
- `LLM_CACHING_PROMPT`

Nous avons quelques guides pour exécuter Hanzo avec des fournisseurs de modèles spécifiques :

- [Azure](llms/azure-llms)
- [Google](llms/google-llms)
- [Groq](llms/groq)
- [LiteLLM Proxy](llms/litellm-proxy)
- [OpenAI](llms/openai-llms)
- [OpenRouter](llms/openrouter)

### Nouvelles tentatives d'API et limites de débit

Les fournisseurs de LLM ont généralement des limites de débit, parfois très basses, et peuvent nécessiter de nouvelles tentatives. Hanzo réessaiera automatiquement les requêtes s'il reçoit une erreur de limite de débit (code d'erreur 429), une erreur de connexion API ou d'autres erreurs transitoires.

Vous pouvez personnaliser ces options selon vos besoins pour le fournisseur que vous utilisez. Consultez leur documentation et définissez les variables d'environnement suivantes pour contrôler le nombre de nouvelles tentatives et le temps entre les tentatives :

- `LLM_NUM_RETRIES` (Par défaut 8)
- `LLM_RETRY_MIN_WAIT` (Par défaut 15 secondes)
- `LLM_RETRY_MAX_WAIT` (Par défaut 120 secondes)
- `LLM_RETRY_MULTIPLIER` (Par défaut 2)

Si vous exécutez Hanzo en mode développement, vous pouvez également définir ces options dans le fichier `config.toml` :

```toml
[llm]
num_retries = 8
retry_min_wait = 15
retry_max_wait = 120
retry_multiplier = 2
```
