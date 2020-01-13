# Projet craigslistVehicles
***Ceci est un projet universitaire***

En collaboration avec:
 - [Paul Leydier](https://github.com/pleydier) 
 - [Wenceslas Sanchez](https://github.com/Orlogskapten)
 - Donovan Thaing
 ----
## Présentation
Le but de ce projet est d'appliquer nos connaissances en dataming ET en data viz' pour analyser et construire une problématique autour d'un dataset de [Kaggle](https://www.kaggle.com/austinreese/craigslist-carstrucks-data).

Ce dataset est un sample des voitures de secondes mains que l'on retrouve sur [craiglist](https://www.craigslist.org/about/sites?lang=fr&cc=fr#US).

## Le projet
C'est pourquoi nous avons décidé de construire notre problèmatique d'un point de vue BI, tant sur la construction de nos modèles que sur la partie DataViz.

On a donc construit notre travail à travers le spectre de l'entreprise [Autotrader](https://www.autotrader.com/), site de vente de voiture d'occasion, qui souhaite à la fois, conseiller ses clients sur le prix d'un véhicule lors de sa mise en vente mais aussi pouvoir déterminer quel véhicule devrait être mis en avant sur le site. 

Enfin, on a aussi cherché à classifer nos données selon des typologies géographiques, mais pas uniquement (BU marketing).

## Les fichiers
Vous pourrez ainsi retrouver dans le repository notre projet scindé en 3 parties:
- 1_exploration.Rmd et son homologue en HTML. Il contient l'exploration des données, le traitement des valeurs manquantes et celui des valeurs aberrantes.
- 2_unsupervised_learning. La première étape a été de comprendre notre marché, c'est à dire sa segmentation géographique dans un premier temps, puis de comprendre les caractéristiques principales des véhicules qui contituent notre dataset.
- 3_supervised_learning.
- dashboard_craiglist.twbx. Dans une optique métier, il nous a fallu créer un dahsboard sur l'état du marché des voitures d'occasions pour permettre aux équipes métiers de piloter / comprendre leurs actions. Le dashboard MACRO contient les outils de pilotage nécessairent pour comprendre le marché Américains dans son ensemble, par état et par marque.
