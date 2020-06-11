library(tibble)
library(xml2)
library(stringr)
library(dplyr)
library(tidyr)
library(base)

###This Scrape creates an output containing several data from football players.
###This data will be used later on to analyse it in a Tableau-Dashboard

##Create Functions that will be used in the Scrape-Loop later on:

shortScrape <- function(html_info){
 html_info %>%
    rvest::html_nodes('.auflistung tr') %>%
    rvest::html_text() %>%
    str_replace_all('[\n]', '') %>%
    str_replace_all('[\t]', '') %>%
    str_replace_all('  ', '') %>%
    as_tibble()}

fullScrape <- function(html_info){
  html_info %>%
     rvest::html_nodes('.auflistung tr') %>%
     rvest::html_text() %>%
     str_replace_all('[\n]', '') %>%
     str_replace_all('[\t]', '') %>%
     str_replace_all('  ', '') %>%
  as_tibble()%>%
  separate(value, c('attribute','data'), ':')%>%
  add_row(attribute='Name', data=html_info %>%
                                                  rvest::html_nodes('h1') %>%
                                                  rvest::html_text() %>%
                                                  str_replace_all('[\n]', '') %>%
                                                  str_replace_all('[\t]', '') %>%
                                                  str_replace_all('  ', '')) %>%
  add_row(attribute='Marktwert', data=(if(identical(character(0),html_info %>%
                                                    rvest::html_nodes('div.right-td') %>% ##.dataMarktwert a
                                                    rvest::html_text() %>%
                                                    str_replace_all('[\n]', '') %>%
                                                    str_replace_all('[\t]', '') %>%
                                                    str_replace_all('  ', '')) == TRUE) {'NA'} else {html_info %>%
                                                        rvest::html_nodes('div.right-td') %>%
                                                        rvest::html_text() %>%
                                                        str_replace_all('[\n]', '') %>%
                                                        str_replace_all('[\t]', '') %>%
                                                        str_replace_all('  ', '') %>%
                                                        first()})) %>%
  add_row(attribute='Liga', data=(if(identical(character(0),html_info %>%
                                               rvest::html_nodes('.mediumpunkt a') %>%
                                               rvest::html_text() %>%
                                               str_replace_all('[\n]', '') %>%
                                               str_replace_all('[\t]', '') %>%
                                               str_replace_all('  ', '')) == TRUE) {'NA'} else {html_info %>%
                                                   rvest::html_nodes('.mediumpunkt a') %>%
                                                   rvest::html_text() %>%
                                                   str_replace_all('[\n]', '') %>%
                                                   str_replace_all('[\t]', '') %>%
                                                   str_replace_all('  ', '')}))}


#Execute Loop through different transfermarkt.ch pages

df_total<-tibble(data = 'test', attribute = 'test')

for (i in 17288:195750){
  Sys.sleep(15)
  html_info<-paste0('https://www.transfermarkt.com/neymar/profil/spieler/',i) %>% read_html
  if (isTRUE(((shortScrape(html_info)[1,1]=="Position:AllGoalkeeperDefendersMidfieldersForwards"))
       == FALSE))
  {
  df_total <- full_join(df_total,fullScrape(html_info), by ='attribute')
  print(df_total)} else {
  print(i)}
}


#Transform data ,to make it ready to save
df_total<-t(df_total[-1,-1])
colnames(df_total) = df_total[1, ]
df_total = df_total[-1, ]          
df_total<-as.data.frame(df_total)

write_xlsx(df_total,"/Users/davidkaufmann/Documents/Projects/df_total.xlsx")



