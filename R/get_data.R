
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

source("show_data.R")

##Seting up an example list of URLs, which should be provided by the show_data()-Function!

function_return2 <- function_return[1:2,]

function_return %>%
  select(url,title, name, format) -> lst

lst = lst[4:10,]

get_data <- function(data, download = "Environment"){
  answer <- readline(prompt=message("If you continue, a total of ",nrow(data)," files are downloaded.\nDo you want to proceed? (Y/N)? \nPlease type the correct letter into the console and execute!"))
  if (answer == "Y" | answer == "y" | answer == "Yes" | answer == "yes"){
    if (download == "Environment"){
      purrr::pwalk(.l = data,.f = function(...){
        current <- tibble(...)
        if (current$format == "csv"){
          basename <- data.table::fread(current$url) %>%  ##For CSV Files https://www.rdocumentation.org/packages/data.table/versions/1.13.6/topics/fread
            assign(paste(current$name),.,envir = .GlobalEnv)
        }
        ###Placeholder for subsequent File Formats, e.g.
        # if (current$format == "txt"){
        #   ...
        # }
      })
    }
    else if (download == "Local" | download == "local"){
        dir.create("Open_Data_Konstanz")
        purrr::pwalk(.l = data,.f = function(...){
        current <- tibble(...)
        utils::download.file(url = current$url, destfile = paste('./Open_Data_Konstanz/',current$name,'.',current$format, sep = ''), quiet = T)
      })
      } else (message("You have not specified a proper setting for download. Please come back later or change the settings"))
  } else message("You have aborted the download. Please come back later or change the settings")
}

lst %>%
  get_data()
