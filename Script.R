install.packages('selectr')
install.packages('xml2')
install.packages('rvest')
install.packages('jsonlite')
install.packages('stringr')
install.packages('wrapr')
install.packages('data.table')
install.packages('dplyr')
install.packages('Rcpp')
install.packages('tibble')
install.packages('xlsReadWrite')
library('selectr')
library('xml2')
library('rvest')
library('jsonlite')
library('stringr')
library('wrapr')
library('data.table')
library('dplyr')
library('tidyr')
library('tidyverse')
library('tibble')

Scrapetest <- function(website){
  website %>%
    read_html %>%
    rvest::html_nodes('.auflistung tr') %>%
    rvest::html_text() %>%
    str_replace_all('[\n]', '') %>%
    str_replace_all('[\t]', '') %>%
    str_replace_all('  ', '') %>%
    as_tibble()}

Scrapetestliga <- function(website){
  website %>%
    read_html %>%
    rvest::html_nodes('.mediumpunkt a') %>%
    rvest::html_text() %>%
    str_replace_all('[\n]', '') %>%
    str_replace_all('[\t]', '') %>%
    str_replace_all('  ', '') %>%
    as_tibble()}

website<-'https://www.transfermarkt.com/neymar/profil/spieler/17286'
#scrape title of the product
Scrape <- function(website){
  website %>%
     read_html %>%
     rvest::html_nodes('.auflistung tr') %>%
     rvest::html_text() %>%
     str_replace_all('[\n]', '') %>%
     str_replace_all('[\t]', '') %>%
     str_replace_all('  ', '') %>%
  as_tibble()%>%
  separate(value, c('attribute','data'), ':')%>%
  add_row(attribute='Name', data=website%>%
                                                  read_html %>%
                                                  rvest::html_nodes('h1') %>%
                                                  rvest::html_text() %>%
                                                  str_replace_all('[\n]', '') %>%
                                                  str_replace_all('[\t]', '') %>%
                                                  str_replace_all('  ', '')) %>%
  add_row(attribute='Marktwert', data=(if(identical(character(0),website%>%
                                                    read_html %>%
                                                    rvest::html_nodes('div.right-td') %>% ##.dataMarktwert a
                                                    rvest::html_text() %>%
                                                    str_replace_all('[\n]', '') %>%
                                                    str_replace_all('[\t]', '') %>%
                                                    str_replace_all('  ', '')) == TRUE) {'NA'} else {website%>%
                                                        read_html %>%
                                                        rvest::html_nodes('div.right-td') %>%
                                                        rvest::html_text() %>%
                                                        str_replace_all('[\n]', '') %>%
                                                        str_replace_all('[\t]', '') %>%
                                                        str_replace_all('  ', '') %>%
                                                        first()})) %>%
  add_row(attribute='Liga', data=(if(identical(character(0),website%>%
                                               read_html %>%
                                               rvest::html_nodes('.mediumpunkt a') %>%
                                               rvest::html_text() %>%
                                               str_replace_all('[\n]', '') %>%
                                               str_replace_all('[\t]', '') %>%
                                               str_replace_all('  ', '')) == TRUE) {'NA'} else {website%>%
                                                   read_html %>%
                                                   rvest::html_nodes('.mediumpunkt a') %>%
                                                   rvest::html_text() %>%
                                                   str_replace_all('[\n]', '') %>%
                                                   str_replace_all('[\t]', '') %>%
                                                   str_replace_all('  ', '')}))}


# LOOP
#Test Scraper("https://www.transfermarkt.com/noam-baumann/profil/spieler/17286")
'https://www.transfermarkt.ch/manuel-neuer/profil/spieler/17259'
'https://www.transfermarkt.com/neymar/profil/spieler/68290'
'https://www.transfermarkt.com/neymar/profil/spieler/68290'
'https://www.transfermarkt.com/neymar/profil/spieler/17286'

df_total1<-tibble(data = 'test', attribute = 'test')

##Liegen<-c('Bundesliga','2. Bundesliga', 'Premier League','Serie A', 'Ligue 1', 'LaLiga','Liga NOS','Egyptian Premier League','Superliga','Armenian Premier League', 'Premyer Liqasi', 'Hyundai A-League', 'Jupiler Pro League', 'Campeonato Brasileiro A', 'Campeonato AFP Planvital','Chinese Super League', 'Primera Division', '3F Superliga', 'LigaPro Banco Pichincha', 'Primera Division', 'Championship',  'A.LeCoq Premium Liiga', 'Veikkausliiga','Ligue 2','Erovnuli Liga', 'Super League 1', 'Liga Nacional', 'Indian Super League', 'Persian Gulf Pro League', 'Airtricity League', 'Pepsi-Deildin','Serie B','J-League', 'Canadian Premier League','Stars League','Superliga','Liga BetPlay Dimayor','1. HNL','Virsliga','A-Lyga','BGL Ligue','','','','','','','','','','','','','','','','','',)

for (i in 17288:195750){
  Sys.sleep(15)
  if (((isTRUE(Scrapetest(paste0('https://www.transfermarkt.com/neymar/profil/spieler/',i))[1,1]=="Position:AllGoalkeeperDefendersMidfieldersForwards"))
       == FALSE)) 
  {
    Sys.sleep(15)
  df_total1 <- full_join(df_total1,Scrape(paste0('https://www.transfermarkt.com/neymar/profil/spieler/',i)), by ='attribute')
  print(df_total1)} else {
  print(i)}
}

df
df_total
df_total<-t(df_total[-1,-1])

colnames(df_total) = df_total[1, ]
df_total = df_total[-1, ]          



save(df_total, file = "/Users/davidkaufmann/Documents/Projects/df_total.Hyper")




