
#Theoretical Background:

##  Basic Introduction to Programming with dplyr: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
##  Hands-on Tutorial for writing function in tidyverse: http://jonthegeek.com/2018/06/04/writing-custom-tidyverse-functions/

### get_data ###

rm(list = ls())

# load packages
library(dplyr)
library(httr)
library(purrr)
library(jsonlite)
library(magrittr)
library(tidyr)
library(stringr)
library(rlang)

##Run the show_data function from the first function

setwd("/Users/lukas/CitiesRopen/R")
source("show_data_phil.R")

url_example <- "https://offenedaten-konstanz.de/sites/default/files/Abfallplaner_M%C3%BCllabfuhrtermine_2020.csv"

##Seting up an example list of URLs

function_return2 <- function_return[1:2,]

function_return %>%
  select(url,title, format) -> lst

lst = lst[1:2,]

## Setting up Function

get_data2 <- function(data){
  for (i in 1:nrow(data)){
    url <- data$url[i]
    name <- data$title[i]
    utils::download.file(url, "temp.csv")
    file.rename(from = "temp.csv", to = paste(i,name,"csv", sep = "." ))
  }}

##Checking
lst %>%
  get_data2()

