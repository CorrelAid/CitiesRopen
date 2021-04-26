
#Theoretical Background:

##  Basic Introduction to Programming with dplyr: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
##  Hands-on Tutorial for writing function in tidyverse: http://jonthegeek.com/2018/06/04/writing-custom-tidyverse-functions/

# ### get_data ###
#
# # load packages
# library(dplyr)
# library(httr)
# library(purrr)
# library(jsonlite)
# library(magrittr)
# library(tidyr)
# library(stringr)
# library(rlang)
# library(data.table)
#
# ##Run the show_data function from the first function
#
# rm(list = ls())
#
# source("show_data.R")
#
# ##Seting up an example list of URLs, which should be provided by the show_data()-Function!
#
# function_return %>%
#   select(url,title, name, format) %>%
#   .[28:38,]-> lst


#' Get Data Function (no final description)
#'
#' @param data The data specified in the show_data function
#' @param download Specifies the way, how the data is downloaded. Choose from c("Environment", "Local")
#'
#' @return Returns the data from the
#' @export
#'
#' @examples show_date(tag = 'XX') %>% get_data()
get_data <- function(data, download = "Environment"){
  answer <- readline(prompt=message("If you continue, a total of ",nrow(data)," files will be downloaded.\nDo you want to proceed? (Y/N)? \nPlease type the correct letter into the console and execute!"))
  # 1. Step: Stop, if there is no permission to download
  if (answer == "N" | answer == "n" | answer == "No" | answer == "no"){
    stop("You have aborted the download. Please run the function again!")}
  # 2. Step: Evaluate download option and start downloading
  if (download == "Environment" | download == "E"){
      assign("List_Open_Data",list(), envir = .GlobalEnv)
      purrr::pwalk(.l = data,.f = function(...){
        current <- tibble(...)
        #CSV: Define and Apply Fetcher
        if (current$format == "csv"){
          List_Open_Data <<- append(List_Open_Data, list(data.table::fread(current$url)))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        }
        #JSON: Define and Apply Fetcher
        else if (current$format == "json") {
          List_Open_Data <<- append(List_Open_Data, list (jsonlite::fromJSON(txt = current$url, flatten = TRUE)))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        }
        #Others: Append URL for further analysis
        else {
          List_Open_Data <<- append(List_Open_Data, list(current$url))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        }
      })
    }
  else if (download == "Local" | download == "L"){
    dir.create("Open_Data_Konstanz")
    purrr::pwalk(.l = data,.f = function(...){
      current <- tibble(...)
      utils::download.file(url = current$url, destfile = paste('./Open_Data_Konstanz/',current$name,'.',current$format, sep = ''), quiet = T)
    })
  }
}


### Working version

##Testing
# lst %>% get_data()







