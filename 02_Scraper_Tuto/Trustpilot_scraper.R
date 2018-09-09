setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/02_Scraper_Tuto/")
## Travaux autour des avis clients (Trustpilot) : Extraction, pré-traitement et analyse.
## Utilsation des packages : tidyverse (ggplot2, dplyr,xts).
## Définition : Trustpilot - brève description du service, un classement 5 étoiles, un nom d'utilisateur et l'heure à laquelle le message a été publié.
## Objectif : Développer une fonction générique permettant de réaliser une extraction depuis n'importe quel site d'entreprise.
###############################################################
### Déclaration des packages
library(tidyverse)  # General-purpose data wrangling
library(rvest) # Parser de documents HTML/XML
library(stringr) # Manipulation des chaines de caractères
library(rebus) # Utilisation d'expression régulières
library(lubridate) # Permet de manipuler plus facilement des données de type Date
###############################################################
## Note : Ultra important, aller inspecter le code source avant de se lancer !!
## Par exemple, le site internet avait changé l'ensemble de ses balises HTML; On ne pouvait donc plus rien scraper
###############################################################
### Déclaration de l'URL - Travaille sur le site Trustpilot.com rassemblant des avis sur un multitudes d'entreprises
### Le tuto se base sur l'analyse de la cie Amazon. Aujourd'hui 189 pages de commentaires
## url <-'http://www.trustpilot.com/review/www.amazon.com'
### Objectif : Créer un programme qui
###   1) Trouve le maximum de page à interroger
###   2) Génère l'ensemble des sous pages (i.e. les 189 pages) associée à une entreprise
###   3) Récupère l'information voulu sur ces sous pages
###   4) Synthétise les données
### 1) Récupération du nombre de page. Idée : Se baser sur la structure du code source de la page HTML.
### Structure standard d'une donnée HTML :
###     < Tag  Attribute_1 = Value_1 Attribute_2 = Value_2 ...>
###       The tagged data
###     <\Tag>
### Utilisation des fonctions du package rvest
###     read_html() : converti le code HTML en code XML
###     html_nodes() : dont l'argument est le descripteur de classe (Ne pas oublier le point pour spécifier qu'il s'agit d'une classe)
###     html_text() : retourne les données contenues dans la balise
###     html_attrs() : retourne les attribus de la balise
### Note : On récupère l'ensemble de ces données en ouvrant un inspecteur de page depuis n'importe quel navigteur.
### - Récupération du nombre de page
###   En ouvrant l'inspeceur sur le bouton "page suivante", on trouve que la classe associé à cet objet est : 'pagination-page'
###   On peut donc définir une fonction permettant de récupérer la valeur maximale.
get_last_page <- function(html){
  pages_data <- html %>%
                ## Spécification de la classe recherchée
                html_nodes('.pagination-page') %>%
                ## Récupération du texte brut contenu dans l'objet associé à cette classe
                html_text()
  ## Attention, on est en algorithmique donc on indexe les valeurs à partir de 0
  pages_data[length(pages_data)-1] %>%
                ## Remove the names or dimnames attribute of an R object. - En gros, on récupère le texte brut
                unname() %>%
                ## On s'assure de bien retourner un format numérique
                as.numeric()
}
### Test de la fonction get_last_page
## first_page <- read_html(url)
## latest_page_numb <- get_last_page(first_page)
###############################################################
### 2) Génèrer l'ensemble des sous pages (i.e. les 189 pages) associée à une entreprise
### Maintenant que l'on connait ce paramètre, on peut générer une structure pour contenir l'ensemble des sous pages
### list_of_page est un vecteur contenant l'ensemble des urls qu'il faudra visiter
## list_of_pages <- str_c(url,'?page=',1:latest_page_numb);head(list_of_pages,5)
###############################################################
### 3) Extraire les informations sur les pages : Analyse de la classe star-rating, review body et récupération des datetime
### On souhaite extraire le texte de la critique, la note, le nom de l'auteur et l'heure de soumission de toutes les critiques sur une sous page
### On va donc écrire une fonction d'extraction pour chaque type de données
### Pour les critiques (.review-body) (une div)
get_reviews <- function(html){
  html %>%
      html_nodes('.review-info__body__text') %>%
      html_text() %>%
      ## str_trim() removes whitespace from start and end of string;
      str_trim() %>%
      ## Given a list structure x, unlist simplifies it to produce a vector which contains all the atomic components which occur in x.
      unlist()
}
### Pour les auteurs des critiques (.user-review-name-link) (une div)
get_reviewer_names <- function(html){
  html %>%
      html_nodes('.consumer-info__details__name') %>%
      html_text() %>%
      str_trim() %>%
      unlist()
}
### Pour les date (pas une div). Cette données est stockée dans une balise de type time en tant qu'attribut
### Idée de base : On va collecter l'ensemble des valeurs contenues dans cette balise et filtrer ce qui nous intéresse
#### <time datetime="2018-07-29T20:44:38.000+00:00" class="ndate" title="dimanche 29 juillet 2018 - 20:44:38">
####    Publié
####    dimanche 29 juillet 2018
#### <span title="2018-07-29T20:44:38.000+00:00"></span>
#### </time>
get_review_dates <- function(html){
  status <-html %>%
        html_nodes('time') %>%
        ## On ne s'intéresse plus au texte mais aux valeurs des atributs.
        html_attrs() %>%
        ## On souhaite récupérer le second attribut
        map(2) %>%
        unlist()
  dates <- html %>%
        html_nodes('time') %>%
        html_attrs() %>%
        ## On souhaite recupérer le premier attribut
        map(1) %>%
        unlist()
  ### Maintenant, on peut empacter le tout dans un dataframe (e.g. un tibble : Tibbles are a modern take on data frames. They keep the features that have stood the test of time, and drop the features that used to be convenient but are now frustrating (i.e. converting character vectors to factors).)
  return_dates <- tibble(status = status, dates = dates) %>%
    ## Visiblement la classe ndate permet de spécifier si une review est actuelle
    filter(status == "ndate") %>%
    pull(dates) %>%
    ## Convert DateTimes to POSIX objects (standardisation des interfaces de programmation des logiciels destinés à fonctionner sur les variantes du système d'exploitation UNIX - Les quatre premières lettres forment l’acronyme de Portable Operating System Interface (interface portable de système d'exploitation), et le X exprime l'héritage UNIX. )
    as.POSIXct(origin = '1970-01-01 00:00:00')
  ### Note : Il est possible que certaines données ne "s'alignent" pas. On va imposer une taille dès maintenant. Cela peut causer des imperfection/incohérences dans le tibble
  length_reviews <- length(get_reviews(html))
  return_reviews <- if (length(return_dates)> length_reviews){
    return_dates[1:length_reviews]
  } else{
    return_dates
  }
  return_reviews
}
### Pour les notes (.star-rating), on va devoir définir une expression régulière pour s'en sortir.
### Le score est placé comme attribut de la balise. De plus, la balise est de la forme : count-x où x \in |[1,5]| est le nombre d'étoile attribué par cette critique.
### Pour manipuler les regexp avec R, il existe le package rebus qui permet de décomposer une expression en sous motifs à l'aide de l'opérateur %R% pour ensuite composer des regexp plus complexes.
get_star_rating <-function(html){
  ## Définitin du modèle de regexp.
  pattern = 'star-rating-' %R% capture(DIGIT)
  ratings <- html %>%
          html_nodes('.star-rating') %>%
          html_attrs() %>%
          ## Vectorised over string and pattern. For str_match, a character matrix. First column is the complete match, followed by one column for each capture group.
          map(str_match,pattern = pattern) %>%
          ## str_match[1] is the fully matched string, the second entry is the part you extract with the capture in your pattern
          ## On veut donc récupérer le second membre
          map(2) %>%
          unlist()
  ## Ici, la structure du site (et l'utilisation de la classe .star_rating) ajoute un élément supplémentaire au vecteur (l'élément 0 (qui provient de l'échelle invitant l'utilisateur à saisir une note)).
  ## On doit donc le supprimer. Mais visblement, dans la fonction ça semble étrangement assez compliqué.
  ## On va donc faire ça de manière assez moche et le faire à l'exterieur (ici : *)
  ratings[2:length(ratings)]
}
## Test des fonctions définies
### simple_url <- list_of_pages[1]
### first_page <- read_html(simple_url)
### T1 <- get_reviews(first_page);T1
### T2 <- get_reviewer_names(first_page);T2
### T3 <- get_review_dates(first_page);T3
### T4 <- get_star_rating(first_page);T4
###############################################################
### 4) Synthèse des données - Ajout d'un champ : "nom de la compagnie"
get_data_table <- function(html,cie_name){
  prgs <- ""
  ## Extaction des données avec les fonctions définies plus haut
  reviews <- get_reviews(html)
  reviewer_names <- get_reviewer_names(html)
  ## On se rend compte en pratique qu'il est possible d'avoir un nombre de rédacteurs supérieur à celui du nombre de critique ce qui génère des vecteurs de tailles différentes (et donc une impossibilité de générer un tibble)
  ## Une solution pour résoudre rapidement ce problème (assez sale je l'avoue) est de supprimer la dernière valeur du nom des rédacteurs.
  ## Pour le moment, seul le cas d'une différence d'un nom s'est présenté; la fonction devra être retravaillé pour permettre de gérer plus de cas posant problème ... (e.g. calculer la différence et compléter cette dernière par des NA dans les vecteur)
  if(length(reviewer_names) != length(reviews)){
    prgs <- c(prgs, "!")
    reviewer_names <- reviewer_names[-length(reviewer_names)]
  }
  # print(c("2 - ",length(reviewer_names)))

  dates <- get_review_dates(html)
  ratings <- get_star_rating(html)[-1] ## (ici : *)
  prgs <- c(prgs, "#")
  ## Permet surtout de vérifier si ça n'a pas crashé
  cat(prgs)
  ## Création du tibble
  combined_data <- tibble(reviewer = reviewer_names,
                   date = dates,
                   rating = ratings,
                   review = reviews
                   )
  combined_data %>%
                mutate(company = cie_name) %>%
                select(company, reviewer,date,rating,review)
}
## exExtrac <- get_data_table(first_page,'amazon');exExtrac
## remove(exExtrac)
### Et plus rapidement
get_data_from_url <- function(url, company_name){
      html <- read_html(url)
      get_data_table(html, company_name)
}
## L <- get_data_from_url(url, 'amazon');L
###############################################################
### On va maintenant appliquer la même méthode de récupération depuis un URL mais cette fois ci en explorant toutes les sous pages que l'on a définit plus tôt dans list_of_pages
### Plutôt que de faire bêtement une boucle for, on va appliquer la fonction map() du package purrr
scrape_write_table <- function(url,cie_name){
    ## Initialisation : lecture de la première page.
    first_page <- read_html(url)
    ## Récupération du nombre de sous page
    latest_page_number <- get_last_page(first_page)
    list_of_pages <- str_c(url, '?page=', 1:latest_page_number)
    # Apply the extraction and bind the individual results back into one table,
    # which is then written as a tsv file into the working directory
    list_of_pages %>%
      # Apply to all URLs
      map(get_data_from_url, cie_name) %>%
      # Combine the tibbles into one tibble
      bind_rows() %>%
      # Write a tab-separated file
      write_tsv(str_c(cie_name,'.tsv'))
}
### Note : Tab-separated values (TSV) est un format texte ouvert représentant des données tabulaires sous forme de « valeurs séparées par des tabulations ».

### Bon maintenant, on a besoin de renseigner :
###   - L'URL de la page d'accueil : url <-'http://www.trustpilot.com/review/www.amazon.com'
###   - Le nom de la compagnie qu'on souhaite scraper 'amazon'
### Et on obtient un tibble complet avec les données pour chaque pages de critiques
## urlFR <- 'https://fr.trustpilot.com/review/www.amazon.com?languages=en'
## scrape_write_table(urlFR,'amazon')
## amz_tbl <- read_tsv('amazon.tsv');head(amz_tbl)
