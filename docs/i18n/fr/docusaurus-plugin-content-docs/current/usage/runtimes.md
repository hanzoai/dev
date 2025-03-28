

# Configuration d'exécution

Un Runtime est un environnement où l'agent Dev peut modifier des fichiers et exécuter des commandes.

Par défaut, Dev utilise un runtime basé sur Docker, s'exécutant sur votre ordinateur local. Cela signifie que vous n'avez à payer que pour le LLM que vous utilisez, et votre code n'est envoyé qu'au LLM.

Nous prenons également en charge les runtimes "distants", qui sont généralement gérés par des tiers. Ils peuvent simplifier la configuration et la rendre plus évolutive, en particulier si vous exécutez de nombreuses conversations Dev en parallèle (par exemple pour faire de l'évaluation).

## Runtime Docker
C'est le Runtime par défaut qui est utilisé lorsque vous démarrez Dev. Vous remarquerez peut-être que certains flags sont passés à `docker run` pour rendre cela possible :

```
docker run # ...
    -e SANDBOX_RUNTIME_CONTAINER_IMAGE=docker.all-hands.dev/hanzoai/runtime:0.30-nikolaik \
    -v /var/run/docker.sock:/var/run/docker.sock \
    # ...
```

Le `SANDBOX_RUNTIME_CONTAINER_IMAGE` de nikolaik est une image de runtime pré-construite qui contient notre serveur Runtime, ainsi que quelques utilitaires de base pour Python et NodeJS. Vous pouvez également [construire votre propre image de runtime](how-to/custom-sandbox-guide).

### Connexion à votre système de fichiers
Une fonctionnalité utile ici est la possibilité de se connecter à votre système de fichiers local.

Pour monter votre système de fichiers dans le runtime, définissez d'abord WORKSPACE_BASE :
```bash
export WORKSPACE_BASE=/chemin/vers/votre/code

# Exemple Linux et Mac
# export WORKSPACE_BASE=$HOME/Dev
# Définira $WORKSPACE_BASE sur /home/<username>/Dev
#
# Exemple WSL sur Windows
# export WORKSPACE_BASE=/mnt/c/dev/Dev
# Définira $WORKSPACE_BASE sur C:\dev\Dev
```

puis ajoutez les options suivantes à la commande `docker run` :

```bash
docker run # ...
    -e SANDBOX_USER_ID=$(id -u) \
    -e WORKSPACE_MOUNT_PATH=$WORKSPACE_BASE \
    -v $WORKSPACE_BASE:/opt/workspace_base \
    # ...
```

Attention ! Rien n'empêche l'agent Dev de supprimer ou de modifier les fichiers montés dans son espace de travail.

Cette configuration peut causer des problèmes de permissions de fichiers (d'où la variable `SANDBOX_USER_ID`) mais semble bien fonctionner sur la plupart des systèmes.

## Runtime Hanzo
Le Runtime Hanzo est actuellement en version bêta. Vous pouvez demander l'accès en rejoignant le canal #remote-runtime-limited-beta sur Slack ([voir le README](https://github.com/hanzoai/dev?tab=readme-ov-file#-join-our-community) pour une invitation).

Pour utiliser le Runtime Hanzo, définissez les variables d'environnement suivantes lors du démarrage d'Dev :

```bash
docker run # ...
    -e RUNTIME=remote \
    -e SANDBOX_REMOTE_RUNTIME_API_URL="https://runtime.app.all-hands.dev" \
    -e SANDBOX_API_KEY="votre-clé-api-all-hands" \
    -e SANDBOX_KEEP_RUNTIME_ALIVE="true" \
    # ...
```

## Runtime Modal
Nos partenaires de [Modal](https://modal.com/) ont également fourni un runtime pour Dev.

Pour utiliser le Runtime Modal, créez un compte, puis [créez une clé API.](https://modal.com/settings)

Vous devrez ensuite définir les variables d'environnement suivantes lors du démarrage d'Dev :
```bash
docker run # ...
    -e RUNTIME=modal \
    -e MODAL_API_TOKEN_ID="votre-id" \
    -e MODAL_API_TOKEN_SECRET="votre-secret" \
```
