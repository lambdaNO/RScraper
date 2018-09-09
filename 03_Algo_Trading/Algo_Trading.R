## Algorithmic Trading in R Tutorial
## source : https://www.datacamp.com/community/tutorials/r-trading-tutorial
## Objectif : Utilisation d'une API financière pour générer des séries temporelles interactives et réaliser une stratégie de trading algorithmique.
## Nous allons collecter les données sur loyal3. Loyal3 fait payer les sociétés pour qu'elles s'inscrivent sur leur plateforme. La prémisse est que les gens qui aiment le service d'une entreprise achèteraient aussi les actions et, ce faisant, deviendraient de solides défenseurs de la marque.
## Rendre la plateforme plus attrayante, c'est que vous pouvez acheter des fractions d'actions. Ainsi, vous pouvez entrer dans ce stock de 800 $ amazon pour seulement 10 $ et acheter une autre fraction de 10 $ chaque fois que vous avez un peu d'argent supplémentaire à la fin du mois.
## Note : Entre temps, loyal3 est devenu foliofirst. On va essayer de scraper ce nouveau site.
setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/03_Algo_Trading")
##############################################################
## Instalation des librairies 
library(dplyr)
library(tidyquant)
library(rvest) ## Package pour le scraping (CF Tuto sur Trustpilot)
library(pbapply) ## Une librairie pour des barres de progression - Peut être utile quand on va scrapper des pages (toujours mieux que mon bdouillage)
library(TTR) ## Une librairie utilisée pour effectuer des calculs techniques de Trading. Note : On va développer notre propre algorithme mais il faut savoir que TTR peut effectuer des calculs beaucoups plus sophistiqués
library(dygraphs) ## Une librarie sympahique pour créer des graphiques dynamiques avec R (principe : créer un fichier HTML pour rendre le graphique dynamique et non une image)
library(lubridate) ## Librairie pour simplifier la manipulation des objets de types Date (Déjà utilisé dans le tuto sur Trustpilot)
library(stringr)
##############################################################
## Collecte des données : 
## Nous allons tenter de scraper l'ensemble des données présentes sur la page de foliofist/Browse investments
## Note : Version alternative scraping du site gurufocus.com
## ATTENTION : c'est long (> 2h)
url <- 'https://www.gurufocus.com/stock_list.php?&n='
tmp_stocks <- read_html(url)
nb_stocks <- tmp_stocks %>%
            html_nodes('.page_links') %>%
            html_nodes("strong") %>%
            html_text()    %>%
            unique()
nb_stocks
new_url <- paste(url,nb_stocks,sep="");new_url
#new_url <- 'https://www.gurufocus.com/stock_list.php?&n=10'
stocks <- read_html(new_url)
get_data_from_url <- function(url){
  tbls <- html_nodes(read_html(new_url), "table") %>%
    html_table(header = TRUE)
  tbls
}
tbls <- get_data_from_url(stocks);tbls
Fin_Tbls <- data_frame(tbls[[2]]$Symbol,tbls[[2]]$Company, tbls[[2]]$Price)
colnames(Fin_Tbls) <- c("Symboles","Nom","Prix")
head(Fin_Tbls);nrow(Fin_Tbls)
write_csv(Fin_Tbls,"Scraping_stock_data.csv")
##############################################################
##############################################################
##############################################################
## Définition d'un intervalle d'étude - Les trois dernières années dépuis ce jour
start.date<-Sys.Date();start.date
end.date<-Sys.Date()-years(3);end.date
##############################################################
## Pour obtenir les données financières de Yahoo, l'objet date doit être changé en simple objet caractère sans tiret. 
## L'utilisation de la fonction de substitution globale gsub() à la fois sur start.date et end.date changera la classe et supprimera simultanément les tirets.
## Dans gsub(), passez le motif de caractère à rechercher, puis les caractères de remplacement. Dans ce cas, le motif de remplacement est un caractère vide entre guillemets. 
## Le dernier paramètre est l'objet auquel gsub() sera appliqué.
##############################################################
import_data <- read_csv('Scraping_stock_data.csv',col_names = TRUE)
head(import_data)
stock_names <- import_data$Symboles
stock_names
##############################################################
## Vérification (on obtint bien les actifs que l'on souhaites étudier)
index_AMC <- which(stock_names=="AMC");index_AMC
stock_names[index_AMC]
index_TWTR <- which(stock_names == "TWTR");index_TWTR
stock_names[index_TWTR]
##############################################################
sel_stock <- "TWTR"
#sel_stock <- "APPL"
#sel_stock <- "AMZN"
sel_stock
## La fonction renvoie une DF qui contient des informations sur les séries temporelles. 
## Chaque ligne est une date et les colonnes contiennent des informations telles que le cours "Ouvert", "Haut", "Bas" et "Clôture" d'une action.
## Attention : Fonction prochainement dépréciée
stocks_ts <- getYahooData(sel_stock,start = gsub('-','', end.date),end = gsub('-','', start.date));head(stocks_ts)
##############################################################
## Visualisation des données obtenues - Plot des ts
dev.new()
par(mfrow=c(2,2))
plot(stocks_ts$Close, main =paste(sel_stock,"$Close",sep=""),col="red")
plot(stocks_ts$Open, main =paste(sel_stock,"$Open",sep=""),col="blue")
plot(stocks_ts$High, main =paste(sel_stock,"$High",sep=""),col="red")
plot(stocks_ts$Low, main =paste(sel_stock,"$Low",sep=""),col="blue")
par(mfrow=c(1,1))
##############################################################
## Création un dygraphe se référant à l'action Twitter, $TWTR, puis la colonne que vous voulez tracer, $Close. 
## Dans le dygraphe, main ajoute un titre qui est spécifié entre guillemets. En utilisant le "%>%", l'objet entier est transmis à la 
## fonction suivante dyRangeSelector(). Vous pouvez spécifier une plage de dates par défaut en utilisant c() avec une chaîne de dates de 
## début et de fin. L'objet HTML qui en résulte est une série chronologique dynamique pour le stock de Twitter avec un curseur de date en bas.
combi_data_OC <- cbind(stocks_ts$Close,stocks_ts$Open)
dygraph(combi_dataOC,main = paste(sel_stock,"Stock Price",sep=" ")) %>% dyOptions(stackedGraph = TRUE)
combi_data_LH <- cbind(stocks_ts$Low,stocks_ts$High)
dygraph(combi_data_LH,main = paste(sel_stock,"Stock Price",sep=" ")) %>% dyRangeSelector() %>% dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"),stackedGraph = TRUE)
## Pour télécharger ces merveilles, cliquer simplement sur export > webpage
##############################################################
## Application d'un modèle de trading simple 
## Pour plus d'informations sur le high frequency trading : www.quantopian.com et www.Investopedia.com (plus simple)
## Méthode de trading simple : 
## Pour une action donnée
##      - Calcul d'une moyenne mobile sur 200 jours \mu_{T}
##      - Calcul d'une moyenne mobile sur 50 jours \mu_{p}
## Si un jour donné, \mu_{p} \geq \mu_{T}, alors il faut acheter de cette action OU conserver sa position actuelle.
## Les jours où \mu_{T} \gep \mu_{p}, il faut vendre ses actions
## Cette stratégie s'appelle le suivi des tendances (trend following strategy). La nature positive ou négative entre les deux moyennes temporelles représente la dynamique du titre.
##
## Le package TTR propose une fonction permettant le calcul simple des moyennes mobiles.
## Il s'agit d'un vecteur unique des cours de clôture de l'action TWTR. Le deuxième paramètre est un entier représentant le nombre d'observations pour la moyenne mobile. 
## Sans utiliser head() la fonction SMA() retournera toutes les valeurs.
head(SMA(stocks_ts$Close, 200),100)
head(SMA(stocks_ts$Close, 50),100)
## Maintenant que vous avez examiné en détail la fonction de moyenne mobile, vous devez appliquer à chacun des 70 stocks. stocks.ts est une liste de 70 cadres de données contenant des données individuelles sur les stocks. La quatrième colonne de chaque trame de données contient le cours de clôture que nous voulons utiliser pour les moyennes mobiles.
## La fonction personnalisée mov.avgs() accepte une seule trame de données d'actions pour calculer les moyennes mobiles. La première ligne sélectionne les cours de clôture parce qu'elle indexe [,4] pour créer stock.close. Ensuite, la fonction utilise ifelse pour vérifier le nombre de lignes dans le bloc de données. Spécifiquement, si la ligne dans la trame de données est inférieure à (2*260), alors la fonction créera une trame de données de moyennes mobiles avec "NA".
##" J'ai choisi ce numéro parce qu'il y a environ 250 jours de bourse par an, ce qui permet de vérifier que la série chronologique est d'environ 2 ans ou plus. Loyal3 peut parfois avoir accès aux introductions en bourse et si l'action est nouvellement cotée, il n'y aura pas assez de données pour une moyenne mobile de 200 jours. Cependant, si la valeur du rang est supérieure à 2*260, la fonction créera une trame de données avec les données d'origine et les moyennes mobiles de 200 et 50 jours comme nouvelles colonnes. En utilisant colnames, je déclare les noms des colonnes. La dernière partie de la fonction utilise les cas complets pour vérifier les valeurs dans la colonne Moyenne mobile sur 200 jours. Toutes les lignes qui n'ont pas de valeur sont supprimées dans le résultat final".
head(stocks_ts[,4])

mov.avgs<-function(stock.df){
  stock.close<-stock.df[,4]
  ifelse((nrow(stock.df)<(2*260)),
         x<-data.frame(stock.df, 'NA', 'NA'),
         x<-data.frame(stock.df, SMA(stock.close, 200), SMA(stock.close, 50)))
  colnames(x)<-c(names(stock.df), 'sma_200','sma_50')
  x<-x[complete.cases(x$sma_200),]
  return(x)
}
stocks_ts<-mov.avgs(stocks_ts);head(stocks_ts)
## Utilisez le code ci-dessous pour visualiser les moyennes mobiles d'un titre à l'aide d'un dygraphe. 
## Encore une fois, ce code utilise l'opérateur "%>%" pour faire suivre les objets. La fonction dygraph() accepte la trame de données 
##stocks_ts sur FOX. 
## Plus précisément, la trame de données est indexée par nom de colonne avec c('sma_200','sma_50'). Cet objet est passé à dySeries() dans les 2 lignes suivantes. Vous pouvez vous référer à une colonne par son nom, donc dySeries() trace une ligne pour les valeurs "sma_50" et "sma_200" sur les lignes 2 et 3. 
## Cet objet est redirigé vers dyRangeSelector() pour régler la hauteur du sélecteur. Enfin, j'ai ajouté des ombres pour définir les périodes où vous auriez voulu acheter ou détenir les actions et une période où vous auriez dû vendre vos actions ou rester à l'écart en fonction de votre position.

dygraph(stocks_ts[,c('sma_200','sma_50')],main = paste(sel_stock,'Moving Averages',sep=" ")) %>%
  dySeries(stocks_ts$sma_50, label = 'sma 50G',drawPoints = TRUE) %>%
  dySeries(stocks_ts$sma_200, label = 'sma 200',drawPoints = TRUE) %>%
  dyRangeSelector(height = 30) %>% dyOptions(stackedGraph = TRUE)#%>%
  #dyShading(from = '2016-4-28', to = '2016-7-27', color = '#CCEBD6') %>%
  #dyShading(from = '2016-7-28', to = '2016-12-30', color = '#FFE6E6')

## Conclusion
## As a budding algorithmic trader, you do not need to plot all 70 shares. Instead, you would want to run the code every day and add a programmatic way to identify stocks that fit the rule based method, “buy if the 50 day moving average is above the 200 day moving average”. As you review the preceding chart, the green section is a time in which you would buy the FOX equity. The red section represents the time to sell your shares and not reenter.
## Since the graph is interactive, you can use the slider to resize the visual. Based on this simple algo trading approach, now may be a good time to buy FOX! December 30, 2016 was a trading day where the 50 day moving average moved $0.01 higher than the 200 day moving average!
  

  