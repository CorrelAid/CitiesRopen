
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

##Seting up an example list of URLs, which should be provided by the show_data()-Function!

function_return2 <- function_return[1:2,]

function_return %>%
  select(url,title, format) -> lst

lst = lst[1:2,]

basename(lst$url[1])

## Setting up Function

get_data <- function(data){
  for (i in 1:nrow(data)){
    url <- data$url[i]
    name <- basename(lst$url[1]) ##Alternatively, one could also use the respective title in the "title" column
    utils::download.file(url, "temp.csv")
    file.rename(from = "temp.csv", to = paste(i,name, sep = "." ))
  }}

##Checking
lst %>%
  get_data()

