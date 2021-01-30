
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
library(data.table)

##Run the show_data function from the first function

setwd("/Users/lukas/CitiesRopen/R")
source("show_data.R")

##Seting up an example list of URLs, which should be provided by the show_data()-Function!

function_return2 <- function_return[1:2,]

function_return %>%
  select(url,title, name, format) -> lst

lst = lst[4:10,]

## Setting up Function

get_data <- function(data){
  for (i in 1:nrow(data)){
    url <- data$url[i]
  if (data$format[i] == "csv"){
    basename <- data.table::fread(url) %>% ##For CSV Files https://www.rdocumentation.org/packages/data.table/versions/1.13.6/topics/fread
    assign(paste(data$name[i]),.,envir = .GlobalEnv)}

    #else if(data$format[i] == "xx")
    #(message("Oh no"))
  }
  message("\n","A total number of ", nrow(data), " files were added to your environment")
}

##Getting data
lst %>%
  get_data() #


