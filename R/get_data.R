
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

source("show_data_phil.R")

url_example <- "https://offenedaten-konstanz.de/sites/default/files/Abfallplaner_M%C3%BCllabfuhrtermine_2020.csv"

# get package list with resources

get_data <- function(.data) {
    for (i in function_return2$url) {
      resp <- httr::GET(function_return2$url)

      if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }

      parsed <- httr::content(resp, "text") %>%
      jsonlite::fromJSON(simplifyVector = FALSE, flatten = TRUE) %>%
      assign(liste$title,.,envir = .GlobalEnv)
  }
}

function_return2 <- function_return[1:2,]

function_return2 %>%
 get_data()

function_return %>%
  select(url,title) -> lst
    for (i in lst$url) {
      print(lst$url)
}

for (i in lenght(r)) {
   return(print(r$url))
}


resp <- httr::GET(!!rlang::ensym(.data))
if (http_type(resp) != "application/json") {
  stop("API did not return json", call. = FALSE)
}

parsed <- httr::content(resp, "text") %>%
  jsonlite::fromJSON(simplifyVector = FALSE, flatten = TRUE)


list(function_return2) -> r
