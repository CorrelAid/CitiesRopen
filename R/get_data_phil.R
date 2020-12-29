### get_data ###


# load packages
library(dplyr)
library(httr)
library(purrr)
library(jsonlite)
library(magrittr)

# define API endpoint



# get package list with resources

show_data <- function() {
  url <- "https://offenedaten-konstanz.de/api/3/action/current_package_list_with_resources"

  resp <- httr::GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- httr::content(resp, "text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  return(parsed)

}

package_list <- show_data()

package_list %>%
  chuck("result", 1, 1, "id")

package_list %>%
  chuck("result", 1) %>%
  map_dfr(extract, c("id", "title"))

