setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/01_Reprise_Cariou")
library(dplyr)
library(tidyr)
library(readr)
## Option pour augmenter le nombre de données transmises par paste
options(max.print=70000)
all_voyages <- read.csv("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/R_Scraping/Datas/2017_tastdb_meetup/tastdb_105var_utf8.csv")
###########################################################################
# Créez le tableau de données sm contenant la liste des expéditions au départ de Saint-Malo.
sm <- all_voyages %>% filter(ptdepimp == "Saint-Malo");nrow(sm)
paste(sm$voyageid,"-",sm$shipname,"-",sm$ptdepimp)
###########################################################################
# Créez le tableau de données cies_sm1 listant les compagnies armant depuis le port de Saint-Malo (ownera). 
# La variable ownera figurera en seconde position de votre tableau.
cies_sm1 <- sm %>% filter(grepl("Compagnie",ownera)) %>% select(voyageid,ownera,shipname,ptdepimp,natinimp);nrow(cies_sm1)
head(cies_sm1)
###########################################################################
# Créez le tableau de données cies_sm2 listant les compagnies armant depuis le port de Saint-Malo (ownera et ownerb).
# Les variables ownera et ownerb seront situées en seconde et troisième positions de votre tableau.
cies_sm2 <- sm %>% filter(grepl("Compagnie",ownera) | grepl("Compagnie", ownerb)) %>% select(voyageid,ownera,ownerb,shipname,natinimp,ptdepimp);nrow(cies_sm2)
head(cies_sm2)
###########################################################################
# Pour chaque période ci-dessous, créez un tableau de données :
#   * La période postérieure à l’abandon de la traite au Royaume-Uni (var abolition_ru) ;
abolition_ru = 1807
#   * La période de la guerre de Trente ans (var trente_ans) ;
trente_ans_inf = 1618 
trente_ans_sup = 1648
#   * La période de la guerre d’indépendance des États-Unis (var indp_usa) ;
guerre_indep_inf = 1775 
guerre_indep_sup = 1783
#   * La période d’activité de la Compagnie française des Indes occidentale (var cie_indes_occ).
cie_fr_ind_inf = 1664 
cie_fr_ind_sup = 1674.
####################################
## Tableau 1
boat_1 <- all_voyages %>% filter(yearam >= abolition_ru)
paste(boat_1$voyageid,"-",boat_1$shipname,"-",boat_1$yearam, "-", boat_1$natinimp,"-",boat_1$ptdepimp)
## Tableau 2
boat_2 <- all_voyages %>% filter (yearam >= trente_ans_inf & yearam <= trente_ans_sup)
paste(boat_2$voyageid,"-",boat_2$shipname,"-",boat_2$yearam, "-", boat_2$natinimp,"-",boat_2$ptdepimp)
## Tableau 3
boat_3 <- all_voyages %>% filter (yearam >= guerre_indep_inf & yearam <= guerre_indep_sup)
paste(boat_3$voyageid,"-",boat_3$shipname,"-",boat_3$yearam, "-", boat_3$natinimp,"-",boat_3$ptdepimp)
## Tableau 4
boat_4 <- all_voyages %>% filter(yearam >= cie_fr_ind_inf & yearam <= cie_fr_ind_sup)
paste(boat_4$voyageid,"-",boat_4$shipname,"-",boat_4$yearam, "-", boat_4$natinimp,"-",boat_4$ptdepimp)
