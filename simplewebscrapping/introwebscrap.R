#install.packages(c('tidyverse'))
#working_path <- '~/intro-to-web-scraping-201803'
#dir.create(file.path(working_path), showWarnings = FALSE)
#setwd(working_path)

# select all department names
#.table150 a:nth-child(3) , .table150 a:nth-child(1)

library(rvest)
library(dplyr)
library(stringr)

depts_url <- "http://houstontx.gov/departments.html"

depts_html <- depts_url %>% read_html() 

#depts_html %>% 
#  html_nodes(".table150 a:nth-child(3) , .table150 a:nth-child(1)")  %>%  
#  #html_text()
#  html_attr("href")


emails <- depts_html %>% 
  html_nodes(".table150 a:first-of-type")  %>%  
  html_attr("href")  %>%
  gsub("mailto:","", .) %>%
  gsub("\\?.*","", .)

name <- depts_html %>% 
  html_nodes(".table150 a:nth-child(3) , .table150 a:nth-child(1)")  %>%  
  html_text("href")  %>%
  gsub("\\n|\\s{2,}"," ", .) %>%
  trimws()

dept <- depts_html %>% 
  html_nodes("br~ br+ a")  %>%  
  html_text()  %>%
  gsub("\\n|\\s{2,}"," ", .) %>%
  trimws()

website <- depts_html %>%
  html_nodes("br~ br+ a")  %>%
  html_attr("href")

#phone_number <- depts_url %>%
#  read_html() %>%
#  html_nodes(".table150 p")  %>%
#  html_text()  %>%
#  stringr::str_extract("[0-9]{3}\\.[0-9]{3}\\.[0-9]{4}") %>%
#  unlist() %>%
#  !is.na(.)


df <- data.frame(Name =name,
                # Phone = phone_number,
                 Email = emails,
                 Dept= dept,
                 Website = website)