setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/01_Reprise_Cariou")
library(dplyr)
library(tidyr)
library(readr)
## Option pour augmenter le nombre de données transmises par paste
options(max.print=100)
all_voyages <- read.csv("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/R_Scraping/Datas/2017_tastdb_meetup/tastdb_105var_utf8.csv")
###########################################################################
# Lorque cela est possible, calculez la durée de navigation des navires nantais jusqu’au principal lieu d’achat d’esclaves.
# Votre tableau de données devra reproduire le tableau ci-dessous.
duree_nav <- all_voyages %>% 
            ## Récupération des données au départ de Nantes
            filter(ptdepimp == "Nantes") %>%
            drop_na(datebuy) %>% drop_na(datedep) %>%
            select(voyageid,shipname,datedep,datebuy) %>%
            mutate(duree_navigation = as.Date.(datebuy) - as.Date.numeric(datedep)) %>%
            arrange(duree_navigation)
duree_nav

class(all_voyages$datebuy)
head(all_voyages$datebuy)
all_voyages$datebuy
as.Date(all_voyages$datebuy,tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

