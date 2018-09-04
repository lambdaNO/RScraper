setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/01_Reprise_Cariou")
library(dplyr)
library(tidyr)
library(readr)
## Option pour augmenter le nombre de données transmises par paste
options(max.print=70000)
all_voyages <- read.csv("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/R_Scraping/Datas/2017_tastdb_meetup/tastdb_105var_utf8.csv")
###########################################################################
# Créer la tableau voyage2 contenant les colonnes 
#   * voyageid
#   * yearam
#   * ptdepimp
#   * shipname
voyage2 <- all_voyages %>% select(voyageid,yearam,ptdepimp,shipname);head(voyage2);dim(voyage2)
colnames(voyage2)
which(colnames(voyage2) == "voyageid")
which(colnames(voyage2) == "ptdepimp")
which(colnames(voyage2) == "yearam")
which(colnames(voyage2) == "shipname")
###########################################################################
# Créez un tableau de données comportant les colonnes suivantes :
#   * id d’expédition
#   * nom du navire
#   * armateurs (owner[a:p]) et capitaines(captain[a:c])
voyage3 <- all_voyages %>% select(voyageid,shipname,ownera:ownerp,captaina:captainc);head(voyage3,2);dim(voyage3)
colnames(voyage3)
V <- grep("^owner",colnames(voyage3));V ## Quels sont les colonnes dont le nom commence par owner
X <- grep("^captain",colnames(voyage3));X ## Quels sont les colonnes dont l nom commence par captain
###########################################################################
# Réagencez les colonnes de votre tableau voyages2 selon l’ordre de colonnes suivant : 
#   1. id d’expédition ; "voyageid"
#   2. nom du navire ; "shipname"
#   3. port de départ ; "ptdepimp"
#   4. année d’arrivée au port de débarquement des esclaves. "shipname"
which(colnames(voyage2)=="voyageid")
which(colnames(voyage2)=="shipname")
which(colnames(voyage2)=="ptdepimp")
which(colnames(voyage2)=="yearam")
voyage4 <- voyage2 %>% select(which(colnames(voyage2)=="voyageid"),which(colnames(voyage2)=="shipname"),which(colnames(voyage2)=="ptdepimp"),which(colnames(voyage2)=="yearam"));head(voyage4)
voyage4bis <- voyage2 %>% select(voyageid,shipname,ptdepimp,yearam);head(voyage4bis)

