

# Utilisation de l'Action GitHub Dev

Ce guide explique comment utiliser l'Action GitHub Dev, à la fois dans le dépôt Dev et dans vos propres projets.

## Utilisation de l'Action dans le dépôt Dev

Pour utiliser l'Action GitHub Dev dans un dépôt, vous pouvez :

1. Créer un ticket dans le dépôt.
2. Ajouter l'étiquette `fix-me` au ticket ou laisser un commentaire sur le ticket commençant par `@dev-agent`.

L'action se déclenchera automatiquement et tentera de résoudre le ticket.

## Installation de l'Action dans un nouveau dépôt

Pour installer l'Action GitHub Dev dans votre propre dépôt, suivez le [README pour le Resolver Dev](https://github.com/hanzoai/dev/blob/main/dev/resolver/README.md).

## Conseils d'utilisation

### Résolution itérative

1. Créez un ticket dans le dépôt.
2. Ajoutez l'étiquette `fix-me` au ticket, ou laissez un commentaire commençant par `@dev-agent`
3. Examinez la tentative de résolution du ticket en vérifiant la pull request
4. Faites un suivi avec des commentaires via des commentaires généraux, des commentaires de revue ou des commentaires de fil en ligne
5. Ajoutez l'étiquette `fix-me` à la pull request, ou adressez un commentaire spécifique en commençant par `@dev-agent`

### Étiquette versus Macro

- Étiquette (`fix-me`) : Demande à Dev de traiter le ticket ou la pull request dans son **intégralité**.
- Macro (`@dev-agent`) : Demande à Dev de ne considérer que la description du ticket/de la pull request et **le commentaire spécifique**.

## Paramètres avancés

### Ajouter des paramètres de dépôt personnalisés

Vous pouvez fournir des instructions personnalisées pour Dev en suivant le [README pour le resolver](https://github.com/hanzoai/dev/blob/main/dev/resolver/README.md#providing-custom-instructions).

### Configurations personnalisées

Le resolver Github vérifiera automatiquement les [secrets de dépôt](https://docs.github.com/en/actions/security-for-github-actions/security-guides/using-secrets-in-github-actions?tool=webui#creating-secrets-for-a-repository) ou les [variables de dépôt](https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/store-information-in-variables#creating-configuration-variables-for-a-repository) valides pour personnaliser son comportement.
Les options de personnalisation que vous pouvez définir sont :

| **Nom de l'attribut**            | **Type** | **Objectif**                                                                                                | **Exemple**                                          |
|----------------------------------| -------- |-------------------------------------------------------------------------------------------------------------|------------------------------------------------------|
| `LLM_MODEL`                      | Variable | Définir le LLM à utiliser avec Dev                                                                    | `LLM_MODEL="anthropic/claude-3-5-sonnet-20241022"`   |
| `DEV_MAX_ITER`             | Variable | Définir la limite maximale pour les itérations de l'agent                                                   | `DEV_MAX_ITER=10`                              |
| `DEV_MACRO`                | Variable | Personnaliser la macro par défaut pour invoquer le resolver                                                 | `DEV_MACRO=@resolveit`                         |
| `DEV_BASE_CONTAINER_IMAGE` | Variable | Sandbox personnalisé ([en savoir plus](https://docs.hanzo.ai/modules/usage/how-to/custom-sandbox-guide))| `DEV_BASE_CONTAINER_IMAGE="custom_image"`      |
