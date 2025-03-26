# Projet Méthodologique

Ce dépôt Git correspond aux travaux projet méthodologique réalisés dans le cadre du projet de fin d'études de la filière Génie Statistique (GS), à l'[ENSAI](https://www.ensai.fr).

Il comprend l'ensemble des codes réalisés en ```R``` et en ```Python``` pour reproduire l'ensemble des graphiques du rapport et du support de présentation du projet. 

## Table des matières
- [Installation de pyHSICLasso](#installation-pyhsiclasso)
- [Remarques pour l'implémentation du NOCCO Lasso](#remarques-pour-limplémentation-du-nocco-lasso-avec-hsiclassovi)
- [Contributeurs](#contributeurs)


## Installation de pyHSICLasso
```bash
pip install pyHSICLasso
```

## Remarques pour l'implémentation du NOCCO Lasso (avec HSICLassoVI)
Le package ```HSICLassoVI``` utilisé n'étant pas à jour, l'importation du fichier préconisée via le fichier ```setup.py``` a été un échec. 

C'est pourquoi nous avons conservé l'arborescence proposée pour ce package (pour éviter les problèmes de dépendance entre les méthodes de différents fichiers), se trouvant dans ```python-code/nocco_lasso/HSICLassoVI```. 

Nos implémentations personnelles pour les jeux de données simulés se trouvent quant à elles dans ```python-code/nocco_lasso/HSICLassoVI/models``` : fichiers ```nocco_lasso.py``` et ```storage_csv_plots.py```.


## Contributeurs
[MAGHAMES Alexandre](https://github.com/AlexandreMaghames)
[LEBOT Marceau](https://github.com/MarceauLB)
[BRAULT Tom](https://github.com/TomBrault) 
