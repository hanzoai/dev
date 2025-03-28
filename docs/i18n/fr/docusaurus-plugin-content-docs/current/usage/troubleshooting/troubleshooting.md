

# 🚧 Dépannage

:::tip
Dev ne prend en charge Windows que via WSL. Veuillez vous assurer d'exécuter toutes les commandes dans votre terminal WSL.
:::

### Échec du lancement du client docker

**Description**

Lors de l'exécution d'Dev, l'erreur suivante est observée :
```
Launch docker client failed. Please make sure you have installed docker and started docker desktop/daemon.
```

**Résolution**

Essayez ces étapes dans l'ordre :
* Vérifiez que `docker` est en cours d'exécution sur votre système. Vous devriez pouvoir exécuter `docker ps` dans le terminal avec succès.
* Si vous utilisez Docker Desktop, assurez-vous que `Settings > Advanced > Allow the default Docker socket to be used` est activé.
* Selon votre configuration, vous devrez peut-être activer `Settings > Resources > Network > Enable host networking` dans Docker Desktop.
* Réinstallez Docker Desktop.
---

# Spécifique au flux de travail de développement
### Erreur lors de la construction de l'image docker du runtime

**Description**

Les tentatives de démarrage d'une nouvelle session échouent et des erreurs contenant des termes comme les suivants apparaissent dans les logs :
```
debian-security bookworm-security
InRelease At least one invalid signature was encountered.
```

Cela semble se produire lorsque le hash d'une bibliothèque externe existante change et que votre instance docker locale a
mis en cache une version précédente. Pour contourner ce problème, veuillez essayer ce qui suit :

* Arrêtez tous les conteneurs dont le nom a le préfixe `dev-runtime-` :
  `docker ps --filter name=dev-runtime- --filter status=running -aq | xargs docker stop`
* Supprimez tous les conteneurs dont le nom a le préfixe `dev-runtime-` :
  `docker rmi $(docker images --filter name=dev-runtime- -q --no-trunc)`
* Arrêtez et supprimez tous les conteneurs / images dont le nom a le préfixe `dev-runtime-`
* Nettoyez les conteneurs / images : `docker container prune -f && docker image prune -f`
