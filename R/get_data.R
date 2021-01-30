
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

rm(list = ls())

setwd("/Users/lukas/CitiesRopen/R")

source("show_data.R")

##Seting up an example list of URLs, which should be provided by the show_data()-Function!

function_return2 <- function_return[1:2,]

function_return %>%
  select(url,title, name, format) -> lst

lst = lst[4:10,]

## Setting up Function

get_data <- function(data, download = "Environment"){
  answer <- readline(prompt=message("If you continue, a total of ",nrow(data)," files are downloaded.\nDo you want to proceed? (Y/N)? \nPlease type the correct letter into the console and execute!"
                                    ))
  if (answer == "Y" | answer == "y" | answer == "Yes" | answer == "yes"){
    for (i in 1:nrow(data)){
      url <- data$url[i]
      if (data$format[i] == "csv"){
        if (download == "Environment"){
          basename <- data.table::fread(url) %>%  ##For CSV Files https://www.rdocumentation.org/packages/data.table/versions/1.13.6/topics/fread
          assign(paste(lst$name[i]),.,envir = .GlobalEnv)
        }
        if (download == "Local"){
          dir.create("Open_Data_Konstanz")
          utils::download.file(url = url, destfile = '~/Open_Data_Konstanz', quiet = T)
        }
      }
    }
  }
  else message("You have aborted the download. Please come back later or change the settings")
}

##Testing Function

lst %>%
  get_data() #

lst %>%
  get_data(download = "Local") #

### Erstellen von LIste

