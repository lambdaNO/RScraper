setwd("~/Desktop/DIVERS_TEMPLATES/R_Scraping/02_Scraper_Tuto/")
source('Trustpilot_scraper.R')
###############################################################
## Fonctions de tests
###############################################################
url <-'http://www.trustpilot.com/review/www.amazon.com'
first_page <- read_html(url)
latest_page_numb <- get_last_page(first_page);latest_page_numb
list_of_pages <- str_c(url,'?page=',1:latest_page_numb);head(list_of_pages,5)
exExtrac <- get_data_table(first_page,'amazon');exExtrac
remove(exExtrac)
L <- get_data_from_url(url, 'amazon');L
remove(L)
urlFR <- 'https://fr.trustpilot.com/review/www.amazon.com?languages=en'
scrape_write_table(urlFR,'amazon')
amz_tbl <- read_tsv('amazon.tsv');head(amz_tbl)




