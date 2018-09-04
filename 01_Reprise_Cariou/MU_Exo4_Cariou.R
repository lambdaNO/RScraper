setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/01_Reprise_Cariou")
library(dplyr)
library(tidyr)
library(readr)
## Option pour augmenter le nombre de données transmises par paste
options(max.print=100)
all_voyages <- read.csv("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/R_Scraping/Datas/2017_tastdb_meetup/tastdb_105var_utf8.csv")
###########################################################################
# Calculez le nombre total d’esclaves embarqués par les navires ayant pour port de départ Liverpool
## ### drop_na() : Drop rows containing missing values
nb_from_lvp = all_voyages %>%   drop_na(slaximp) %>%  filter(ptdepimp == "Liverpool") %>% summarise(total_escl <- sum(slaximp));nb_from_lvp
# Concevez le tableau de distribution des expéditions de traite par années au départ du port de Liverpool
cpt_lvp <- all_voyages %>% filter(ptdepimp=="Liverpool") %>% count(yearam);cpt_lvp
# Etablissez la liste alphabétique des ports ayant pratiqué la traite atlantique en omettant les désignations imprécises.
L_all <- all_voyages %>% distinct(ptdepimp) %>% filter(!grepl("unspecified",ptdepimp)) %>% arrange(ptdepimp);L_all
# Reproduisez le tableau ci-dessous
##### Source: local data frame [13 x 3]
##### Groups: yearam [12]
##### # A tibble: 13 x 3
##### yearam          natinimp     n
##### <int>           <chr>        <int>
## Traduction : Nombre de bateau par nationnalité et par année
nbship_pnat_pyear <- all_voyages %>% drop_na(natinimp) %>% group_by(yearam,natinimp)  %>% count(yearam,natinimp);head(nbship_pnat_pyear,15)
# Créez un tableau de données appelé voyages2 reproduisant le tableau ci-dessous.
## Groupage de variables par périodisation 
## Calculer des periodes adéquates 
## Grouper ensuite le nombre d'expédition en fonction des périodes définies 
voyages2 <- all_voyages %>% 
  ### cut divides the range of x into intervals and codes the values in x according to which interval they fall.
  group_by(intervalles_chrono = cut(yearam, breaks= seq(1500, 1875, by = 25),
  ### x = yearam : a numeric vector which is to be converted to a factor by cutting.
  ### breaks	: either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut
  include.lowest = TRUE,
  ### include.lowest	: logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.
  right = FALSE,  
  ### right	: logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
  dig.lab = 4)) %>%
  ### dig.lab	: integer which is used when labels are not given. It determines the number of digits used in formatting the break numbers.
  summarise(n_expeditions= n()) 
voyages2
