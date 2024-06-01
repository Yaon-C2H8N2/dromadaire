# Projet OCaml

## Description
Ce projet contient des programmes pour encoder et décoder des données en utilisant la bibliothèque Zarith sous OCaml.

## Installation
Assurez-vous que `ocamlfind` et le package `zarith` sont installés sur votre système. Vous pouvez installer ces dépendances via OPAM:

```bash
opam install ocamlfind zarith
```

## Utilisation

Pour utiliser les programmes encode et decode, suivez ces étapes :

Préparer le fichier de message :
- Insérez le message que vous souhaitez encoder dans **msg.txt**.
- Encodage du message : Pour encoder le message, utilisez la commande suivante :
```bash
./encode
```
- Décodage du message :
Pour décoder un message encodé, exécutez :
```bash
./decode
```
