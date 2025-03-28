

# 🧠 Agent Principal et Capacités

## CodeActAgent

### Description

Cet agent implémente l'idée de CodeAct ([article](https://arxiv.org/abs/2402.01030), [tweet](https://twitter.com/xingyaow_/status/1754556835703751087)) qui consolide les **act**ions des agents LLM dans un espace d'action de **code** unifié à la fois pour la _simplicité_ et la _performance_.

L'idée conceptuelle est illustrée ci-dessous. À chaque tour, l'agent peut :

1. **Converser** : Communiquer avec les humains en langage naturel pour demander des clarifications, des confirmations, etc.
2. **CodeAct** : Choisir d'effectuer la tâche en exécutant du code

- Exécuter n'importe quelle commande Linux `bash` valide
- Exécuter n'importe quel code `Python` valide avec [un interpréteur Python interactif](https://ipython.org/). Ceci est simulé via une commande `bash`, voir le système de plugin ci-dessous pour plus de détails.

![image](https://github.com/hanzoai/dev/assets/38853559/92b622e3-72ad-4a61-8f41-8c040b6d5fb3)

### Démo

https://github.com/hanzoai/dev/assets/38853559/f592a192-e86c-4f48-ad31-d69282d5f6ac

_Exemple de CodeActAgent avec `gpt-4-turbo-2024-04-09` effectuant une tâche de science des données (régression linéaire)_.
