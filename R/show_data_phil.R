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
    jsonlite::fromJSON(simplifyVector = FALSE, flatten = TRUE)

  parsed %>%
    chuck("result", 1) -> package_list

  package_list %>%
    map_dfr(magrittr::extract, c("id","title")) -> macro_data

  package_list %>%
    map(chuck, "resources") -> only_ressources

  temp_list <- vector("list", length = length(only_ressources))

  for (i in seq_along(only_ressources)) {

    only_ressources %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("url","format","resource_group_id"),
                     .id = "no_ressource") %>%
      dplyr::bind_cols(tibble(datasource = i)) -> temp_list[[i]]
  }

  dplyr::bind_rows(temp_list) -> ressource_df

  return(list(macro_data, ressource_df))

}




# tryout_stuff - not relevant for function call ---------------------------


function_return <- show_data() # now the function returns two lists in which two dfs are stored


