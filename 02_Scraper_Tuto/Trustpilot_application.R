setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/02_Scraper_Tuto/")
source('Trustpilot_scraper.R')
###############################################################
## Etude de cas 
## Analyse des métadonnées des commentaires (Notations et heure).
## On suppose deux compagnies qui revendiquent leurs scores sur Trustpilot comme argument de vente
## Lien : https://www.trustpilot.com/categories
## Lien : http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
###############################################################
library(xts) # Manipulation de séries temporelles
library(infer) # Test d'hypothèse
###############################################################
## Déclaration des urls
url_cie_A <- 'https://www.trustpilot.com/review/www.google.com'
url_cie_B <- 'https://www.trustpilot.com/review/www.apple.com'
###############################################################
## Scraping des données et générations des fichiers tsv
scrape_write_table(url_cie_A,'cie_A')
scrape_write_table(url_cie_B,'cie_B')
###############################################################
## Lecture des données depuis les fichiers tsv
data_cie_A <- read_tsv('cie_A.tsv')
data_cie_B <- read_tsv('cie_B.tsv')
###############################################################
## Synthèse des données - Ré aggencement. (nb de commentaires et moyenne)
full_data <- rbind(data_cie_A,data_cie_B)
full_data %>% 
  group_by(company) %>%
  summarise(count =n(), mean_rating = mean(rating))
###############################################################
## Analyse des séries temporelles - Analyse des performances mensuelles
cie_A_ts <- xts(data_cie_A$rating, data_cie_A$date);head(cie_A_ts)
colnames(cie_A_ts) <- 'rating';head(cie_A_ts)
cie_B_ts <- xts(data_cie_B$rating, data_cie_B$date);head(cie_B_ts)
colnames(cie_B_ts) <- 'rating';head(cie_B_ts)
### Déclaration d'une borne inf d'intervalle d'analyse 
open_ended_interval <- '2011-01-01/'
### Et création des séries temporelles
cie_A_sts <- cie_A_ts[open_ended_interval];head(cie_A_sts)
cie_B_sts <- cie_B_ts[open_ended_interval];head(cie_B_sts)
###############################################################
## Calcul des notes mensuelles moyennes (apply.monthly) - Calcul du nombre de commentaires par mois et de la note mensuelle
### Note : dans le cas où un mois contient plusieurs indices, on prendra la moyenne de ces derniers (à l'aide de colMeans)
### Ensuite, n'oubliez pas de passer en longueur à l'argument FUN pour récupérer les comptages mensuels. C'est parce que les séries chronologiques peuvent être considérées comme un vecteur. Chaque revue augmente la longueur de ce vecteur de un et la fonction de longueur compte essentiellement les revues.
cie_A_month_avg <-  apply.monthly(cie_A_sts, colMeans, na.rm = T);cie_A_month_avg
cie_A_month_count  <-  apply.monthly(cie_A_sts, FUN = length);cie_A_month_count
cie_B_month_avg <-  apply.monthly(cie_B_sts, colMeans, na.rm = T);cie_B_month_avg
cie_B_month_count  <-  apply.monthly(cie_B_sts, FUN = length);head(cie_B_month_count)
## http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
## Reformater les données si l'on souhaite utilser ggplot.
dev.new()
par(mfrow=c(2,2))
plot(x = cie_A_month_count$rating,screens =1,col="red",main="Cie A monthly rating count")
plot(x = cie_B_month_count$rating,screens =1,col="blue",main="Cie B monthly rating count")
plot(x = cie_A_month_avg$rating,screens =1,col="red",main="Cie A monthly rating count")
plot(x = cie_B_month_avg$rating,screens =1,col="blue",main="Cie B monthly rating count")
par(mfrow=c(1,1))
###############################################################
## Analyse de l'activité journalière 
start_date <- open_ended_interval
full_data <- full_data %>%
              group_by(company) %>%
              filter(date >= start_date) %>%
              mutate(weekday = weekdays(date,abbreviate = T), hour = hour(date))
full_data$weekday <- factor(full_data$weekday, levels = c('Lun','Mar','Mer','Jeu','Ven','Sam','Dim'))

full_data_cie_A <- full_data %>% 
                  filter(company == "cie_A")
full_data_cie_B <- full_data %>% 
                  filter(company == "cie_B")

dev.new()
par(mfrow=c(1,2))
plot(full_data_cie_A$weekday,col=rainbow(10), main="Review by weekdays - Cie A")
plot(full_data_cie_B$weekday,col=rainbow(10), main="Review by weekdays - Cie B")
par(mfrow=c(1,1))
## Note, l'analyse de full_data_cie_X$hour n'est pas pertinente dans le cas présent
###############################################################
## Analyse de l'hypothès nulle : 
## Ces tendances semblent indiquer qu'il se passe quelque chose de louche dans l'entreprise B. 
## Peut-être que certaines des critiques ne sont pas écrites par des utilisateurs, mais plutôt par des professionnels. 
## On s'attendrait à ce que ces critiques soient, en moyenne, meilleures que celles qui sont rédigées par des gens ordinaires. 
## Étant donné que l'activité d'examen de la société B est beaucoup plus importante en semaine, il semble probable que les professionnels écriraient leurs examens un de ces jours-là. 
## Vous pouvez maintenant formuler une hypothèse nulle que vous pouvez essayer de réfuter en utilisant les preuves des données.
## H_{0} : Il n'y a pas de différence systématique entre les examens écrits un jour ouvrable et les examens écrits la fin de semaine.
## => Pour tester cela, vous divisez les évaluations de la société B en celles qui sont écrites les jours de semaine et celles qui sont écrites les week-ends, et vous vérifiez leurs notes moyennes.
hypothesis_data <- full_data %>% 
  mutate(is_weekend = ifelse(weekday %in% c('Sam', 'Dim'), 1, 0)) %>% 
  select(company, is_weekend, rating)
table(hypothesis_data$is_weekend)

hypothesis_data %>% 
  group_by(company, is_weekend) %>% 
  summarise(avg_rating = mean(rating)) %>% 
  spread(key = is_weekend, value = avg_rating) %>% 
  rename(weekday = '0', weekend = '1')
## Extraction des différence entre moyenne 
weekend_rating <- hypothesis_data %>% 
  filter(company == 'cie_B') %>% 
  filter(is_weekend == 1) %>% 
  summarise(mean(rating)) %>% 
  pull()
workday_rating <- hypothesis_data %>% 
  filter(company == 'cie_B') %>% 
  filter(is_weekend == 0) %>% 
  summarise(mean(rating)) %>% 
  pull()
weekend_rating
workday_rating
diff_work_we <- workday_rating - weekend_rating;diff_work_we

hypothesis_data$company
## S'il n'y avait pas vraiment de différence entre les jours ouvrables et les week-ends, alors vous pourriez simplement permuter l'étiquette is_weekend et la différence 
## entre les moyennes devrait être du même ordre de grandeur. 
## Un moyen pratique de faire ce genre de'Statistiques sur les pirates informatiques' est fourni par le package "infer" :
## Le calcul doit obligatoirement se faire sur des facteurs, si on est pas sû, on force 
hypothesis_data$is_weekend <- factor(hypothesis_data$is_weekend)
hypothesis_data$company <- factor(hypothesis_data$company)
hypothesis_data$rating <- factor(hypothesis_data$rating)
str(hypothesis_data)
permutation_tests <- hypothesis_data %>% 
  filter(company == 'cie_B') %>% 
  specify(rating ~ is_weekend ) %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 10000, type = 'permute') %>% 
  calculate(stat = 'diff in means', order = c(0,1))
head(permutation_tests)
permutation_tests %>% 
  summarise(p = mean(abs(stat)>= diff_work_we))
## En effet, la probabilité que l'effet observé soit purement fortuit est extrêmement faible. Cela ne prouve pas qu'il y a eu méfait, mais c'est très suspect.
## Le p-value trouvé est > 0.05.
## Au risque de 5%, on peut invalider l'hypothèse nulle et donc supposer qu'il 'y a une différence systématique entre les examens écrits un jour ouvrable et les examens écrits la fin de semaine.
###############################################################
## Ouverture : https://www.theguardian.com/money/2013/jan/26/fake-reviews-plague-consumer-websites
###############################################################