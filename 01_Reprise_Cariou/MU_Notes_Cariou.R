setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/01_Reprise_Cariou")
## MU R Nantes - Exploration des principales fonctionnalités des packages dplyr et tidyr
## Auteur : Wilfrid Cariou
### Prise de notes
###########################################################################
## Dplyr : une grammaire de manipulation des données - Sélection de colonnes (select), Filtrage de lignes (filter), Groupements (group by), Résumés de données (summarise), Construction de nouvelles variables (mutate), Tri des valeurs (arrange)
library(dplyr)
## Tidyr : mise en ordre simple de données - Transpositions tableaux larges / longs (gather, spread), Division et combinaison de cellules (separate, unique), Gestion des valeurs manquantes (drop_na, fill, replace_na), Combinaisons de colonnes (bind_rows, fonctions de jointure (ex: inner_join)), Combinaisons de lignes (bind_cols)
library(tidyr)
## Readr : permet d'importer facilement des données.
library(readr)
###########################################################################
## %>% (Piping operator): Opérateur majeur de la syntaxe dplyr. L'objet situé à gauche de l'opérateur %>% est utilisé comme premier argument de la fonction située à droite de l'opérateur. On peut également enchainer les opérations sur les données avec cet opérateur.
## Utilisation dans ce MU de : Voyages, The Trans-Atlantic Slave Trade Database (http://slavevoyages.org/)
voyagesALL <- read.csv("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/R_Scraping/Datas/2017_tastdb_meetup/tastdb_105var_utf8.csv")
## 105 variables et 34946 observations
dim(voyagesALL)
colnames(voyagesALL)
## Création d'un sous ensemble de voyageALL contenant 10 observations et 5 variables 
#### https://fr.wikibooks.org/wiki/Programmer_en_R/Manipuler_un_tableau_de_donn%C3%A9es
voyages <-  voyagesALL %>% slice(1:100) %>% select(shipname, ptdepimp, voyageid, yearam, slaximp)
dim(voyages)
### - Navires, pavillons, armateurs (27 var)
### - L'issue des expéditions (outcomes) (5 var)
### - Itinéraire (20 var)
### - Données temporelles (7 var)
### - Equipage (10 var)
### - Esclaves (15 var)
### - Sources (18 var)
###########################################################################
# Sélection de colonnes
## repérée par son nom : 
demo <- voyages %>% select(shipname);head(demo)
## repérée par sa position : 
demo <- voyages %>% select(5)
# Sélections multiples
demo <- voyages %>% select(1, 3, shipname);head(demo)  
# Sélection de colonnes contigues
demo <- voyages %>% select(2:4);head(demo)
# Sélection par exclusion
demo <- voyages %>% select(-2);head(demo)
###########################################################################
# Ré-agencement des colonnes
## Par nom
demo <- voyages %>% select(shipname, ptdepimp, voyageid, yearam, slaximp);head(demo)
## Par numéro de colonne
demo <- voyages %>% select(4, 3, 2, 1, 5);head(demo)
## Mélange des deux :
demo <- voyages %>% select(yearam, 1:5);head(demo)
###########################################################################
# Tri de valeur
## Ordre croissant
demo <- voyages %>% arrange(yearam);head(demo) 
## Ordre décroissant
### Ecriture 1
demo <- voyages %>%  arrange(desc(slaximp));head(demo)
### Ecriture 2
demo <- voyages %>%  arrange(-slaximp);head(demo)
###########################################################################
##Création d'un sous ensemble de voyages qui ont été réalisées sur le bateau Betsey - Juste 20 valeurs
betsey_ship <- voyagesALL %>% filter(shipname == "Betsey") %>% select(voyageid,yearam,shipname,captaina,captainb) %>% slice(1:20);betsey_ship
# Transposition format large / format long - Récupération des données : On souhaite récupérer les noms des capitaines sur l'ensemble betsey_ship
demo <- betsey_ship %>% gather(captaina, captainb, key="statut", value="captain",na.rm = TRUE) %>% arrange(desc(captain));head(demo,30)
###########################################################################
# Sélection de lignes en fonction d'une colonne
voyagesBis <- voyagesALL %>% select(shipname, ptdepimp, voyageid, yearam, slaximp,placcons,yrcons);colnames(voyagesBis);dim(voyagesBis);head(voyagesBis)
## Sélections portant sur des données textuelles
### Sélection selon une chaîne de caractères
demo <- voyagesBis %>% filter (ptdepimp == "Nantes");head(demo)
### Sélection selon plusieurs chaînes de caractères
demo <- voyagesBis %>% filter (ptdepimp == "Nantes" | ptdepimp =="Bordeaux");head(demo)
dim(demo)
sum(demo$ptdepimp == "Bordeaux")
sum(demo$ptdepimp == "Nantes")
### Sélection selon une séquence de caractères
demo <- voyagesBis %>% filter(grepl("Duc", shipname));head(demo)
### Sélection par exclusion d'une chaîne de caractères
demo <- voyagesBis %>% filter(!ptdepimp == "Nantes");dim(demo)
sum(demo$ptdepimp == "Nantes")
sum(demo$ptdepimp != "Nantes")
### Sélection par exclusion d'une séquence de caractères
demo <- voyagesBis %>% filter(!grepl("Bretagne", shipname));dim(demo);head(demo)
### Sélection de lignes en fonction d'un vecteur
chantier <- c("Brest", "Paimboeuf", "Quimper")
liste_chantiers <- voyagesBis %>% filter(placcons %in% chantier);head(liste_chantiers,20)
### Sélection de lignes en fonction de plusieurs colonnes
demo <- voyagesBis %>% filter(slaximp == 105 & ptdepimp == "Bordeaux");head(demo)
#### Ou
demo <- voyagesBis %>% filter(slaximp == 105) %>% filter (ptdepimp == "Bordeaux");head(demo)
###########################################################################
# Sélections portant sur des valeurs numériques
## Sélection selon une valeur numérique
demo <- voyagesBis %>% filter(yearam == 1709);head(demo)
## Sélection selon un intervalle de valeurs
demo <- voyagesBis %>% filter(slaximp >= 100 & slaximp <= 300);head(demo)
###########################################################################
# Résumés de données
## Note : filter(!grepl("^A[cd]",shipname)) permet de Débuter à Africain
## Liste d'expéditions - navires au départ de Dunkerque
demo <- voyagesBis %>% filter(ptdepimp=="Dunkerque") %>% arrange(shipname) %>% select(voyageid, yearam, shipname);demo
## Liste des valeurs uniques - navires au départ de Dunkerque
demo <-  voyagesBis %>% filter(ptdepimp=="Dunkerque") %>% distinct(shipname) %>% arrange(shipname);demo
## Liste des valeurs uniques - navires au départ de Nantes dont la date de construction est identifiée
demo <- voyagesBis %>%  filter(ptdepimp=="Nantes") %>% drop_na(yrcons) %>%  distinct(shipname) %>%  arrange(shipname) %>% filter(!grepl("^A[cd]",shipname));demo 
## Liste de valeurs uniques en fonction de plusieurs colonnes - navires au départ de Nantes dont la date de construction est identifiée
### Utilisation de tidyr
demo <- voyagesBis %>%  filter(ptdepimp=="Nantes") %>%  drop_na(yrcons) %>%  distinct(shipname, yrcons) %>%  arrange(shipname) %>%  filter(!grepl("^A[cd]",shipname));demo
###########################################################################
# Résumés de position et de dispersion
### Note : Attention, fonctionne pas si trop de données d'où le besoin de redéfinir un sous ensemble plus petit
voyagesTris <-  voyagesALL %>% slice(1:5) %>% select(shipname, ptdepimp, voyageid, yearam, slaximp,slamimp);voyagesTris
## Résumer une colonne
demo <- voyagesTris %>% summarise(avg_slaximp = mean(slaximp));demo
## Appliquer un ensemble de résumés à une colonne
demo <- voyagesTris %>% summarise(avg_slaximp = mean(slaximp), sum_slaximp= sum(slaximp));demo
## Appliquer un résumé à un ensemble de colonnes ## Erreur à étudier
demo <- voyagesTris %>% filter(ptdepimp=="Nantes") %>% summarise_each(funs(mean), slaximp, slamimp);demo
## Appliquer un résumé à un ensemble de colonnes ## Erreur à étudier 
demo <- voyagesTris %>%  filter(ptdepimp=="Nantes") %>%  drop_na(slaximp, slamimp) %>% summarise_each(funs(mean), slaximp, slamimp);demo
## Appliquer un résumé à un ensemble de colonnes ## Erreur à étudier 
demo <- voyagesTris %>% filter(ptdepimp=="Nantes") %>% summarise_each(funs(mean(., na.rm = TRUE)), slaximp, slamimp);demo
###########################################################################
# Opération sur des groupes de données
## Tirage aléatoire portant sur des groupes - diplyr
### Basiquement, on définit le nombre de critère pour filtrer (ici 3 : Nantes, Bordeaux, Le Havre), sample_n(x) correpond au nombre de tirage a effectuer pour chacun de ces critères (3 critères, x = 1 => 3 tirages, x = 3 => 9 tirages)
group_demo1 <- voyagesALL %>% select(voyageid, slaximp, yearam, ptdepimp, shipname) %>% filter(ptdepimp =="Nantes"| ptdepimp =="Bordeaux" | ptdepimp =="Le Havre") %>% group_by(ptdepimp) %>% sample_n(3);group_demo1
### Grouper les données
group_demo2 <-voyagesALL %>% filter(ptdepimp =="Nantes"| ptdepimp =="Bordeaux" | ptdepimp =="Le Havre") %>% drop_na(slaximp) %>%  group_by(ptdepimp) %>% summarise(sum_slaximp = sum(slaximp));group_demo2
group_demo3 <-voyagesALL %>% filter(ptdepimp =="Nantes"| ptdepimp =="Bordeaux" | ptdepimp =="Le Havre") %>% count(ptdepimp);group_demo3
###########################################################################
# Création de nouvelles colonnes
## Création de nouvelles colonnes effectifs cumulés croissants d'esclaves embarqués (navires au départ de Dunkerque)
## Tableau croisé