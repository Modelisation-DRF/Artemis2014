## Artemis2014

Simulateur de croissance à l'échelle de l'arbre pour les forêts du Québec.

Auteurs: Hugues Power - Ministère des Ressources naturelles et des Forêts du Québec

Courriel: hugues.power@mrnf.gouv.qc.ca

This R package is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This library is distributed with the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

See the license LGPL-3.0 at http://www.gnu.org/copyleft/lesser.html.

## Introduction
Le simulateur Artémis-2014 prévoit la mortalité, l'accroissement et le recrutement d'arbres marchands dans des
placettes de 400 m2. Il fonctionne par pas de simulation de 10 ans. Le package permet également de remplacer les fonctions de mortalité et d'accroissement de base d'Artemis-2014 par des fonctions sensibles au climat.

## Documentation et références
Non disponibles pour l'instant.

## Dépendences
Ce package dépends des packages suivants.

- OutilsDRF est disponible ici: https://github.com/Modelisation-DRF/OutilsDRF

- ExtractMap est disponible ici: https://github.com/Modelisation-DRF/ExtractMap

- Billonage est disponible ici: https://github.com/Modelisation-DRF/Billonnage

- CWFC-CCFB/J4R est disponible ici: https://github.com/CWFC-CCFB/J4R

- RNCan/BioSimClient_R est disponible ici: https://github.com/RNCan/BioSimClient_R

## Comment obtenir le code source
Taper cette ligne dans une invite de commande pour cloner le dépôt dans un sous-dossier "natura3":

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
git clone https://github.com/Modelisation-DRF/Artemis2014 Artemis2014
```

## Comment installer le package Natura3 dans R

```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
require(remotes)
remotes::install_github("Modelisation-DRF/Artemis2014")
```

## Historique des versions

| Date |  Version  | Features et bugs | Détails |
|:-----|:---------:|:-----------------|:--------|
| 2025-04-25 | 4.3.0 |  | Premiere version stable |

