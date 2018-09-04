setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/01_Reprise_Cariou")
library(dplyr)
library(tidyr)
library(readr)
library(plotrix)
## Option pour augmenter le nombre de données transmises par paste
options(max.print=100)
all_voyages <- read.csv("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/R_Scraping/Datas/2017_tastdb_meetup/tastdb_105var_utf8.csv")
###########################################################################
# Triez chronologiquement les expéditions de traite dans un tableau de données appelé chrono_voyages
chrono_voyages <- all_voyages %>% arrange(yearam) %>% select(voyageid,yearam,shipname,natinimp,ptdepimp)
paste(chrono_voyages$voyageid,"-",chrono_voyages$yearam, "-",chrono_voyages$shipname,"-", chrono_voyages$natinimp,"-",chrono_voyages$ptdepimp)
## Tableau récapitulatif - Nombre de bateau par nationalité.
table(chrono_voyages$natinimp)
## Affichage du nombre de bateaux par annnée
barplot(table(chrono_voyages$yearam))
## Affichage d'un graphe sectoriel de la part de chaque natinalité
pie3D(table(chrono_voyages$natinimp),explode=0.1)
## Tri par ordre chronologique décroissant : 
chrono_voyages_dec <- all_voyages %>% arrange(desc(yearam))
paste(chrono_voyages_dec$voyageid,"-",chrono_voyages_dec$yearam, "-",chrono_voyages_dec$shipname,"-", chrono_voyages_dec$natinimp,"-",chrono_voyages_dec$ptdepimp)
barplot(table(chrono_voyages_dec$yearam))
## Tri par nationnalité
natio_voyages <- voyages %>% arrange(natinimp)
paste(chrono_voyages$voyageid,"-",chrono_voyages$yearam, "-",chrono_voyages$shipname,"-", chrono_voyages$natinimp,"-",chrono_voyages$ptdepimp)
pie(table(chrono_voyages$natinimp),explode=0.1)
pie3D(table(chrono_voyages$natinimp),explode=0.1)
## Récupération des bateaux de nationnalité française et tri par ordre croissant d'année de départ
fr_natio_voyages <- all_voyages %>% filter(natinimp =="France") %>% arrange(yearam)
paste(fr_natio_voyages$voyageid,"-",fr_natio_voyages$yearam, "-",fr_natio_voyages$shipname,"-", fr_natio_voyages$natinimp,"-",fr_natio_voyages$ptdepimp)
barplot(table(fr_natio_voyages$yearam))
