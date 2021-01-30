### get_data ###


# load packages
library(dplyr)
library(httr)
library(purrr)
library(jsonlite)
library(magrittr)
library(tidyr)
library(stringr)

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
    map_dfr(magrittr::extract, c("id","title"),
            .id = "datasource") %>%
    mutate(datasource = as.integer(datasource)) -> macro_data

  package_list %>%
    map(chuck, "resources") -> only_ressources

  temp_list_ressources <- vector("list", length = length(only_ressources))

  for (i in seq_along(only_ressources)) {

    only_ressources %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("url","name","format","resource_group_id"),
                     .id = "no_ressource") %>%
      dplyr::bind_cols(tibble(datasource = i)) -> temp_list_ressources[[i]]
  }

  dplyr::bind_rows(temp_list_ressources) -> ressource_df

  package_list %>%
    map(chuck, "tags") -> only_tags

  temp_list_tags <- vector("list", length = length(only_tags))

  for (i in seq_along(only_tags)) {

    only_tags %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("name"),
                     .id = "no_tag") %>%
      dplyr::bind_cols(tibble(datasource = i)) -> temp_list_tags[[i]]
  }

  dplyr::bind_rows(temp_list_tags) -> tag_df

  tag_df %>%
    dplyr::mutate(no_tag = stringr::str_c("tag_no", "_",  no_tag)) %>%
    tidyr::pivot_wider(names_from = no_tag, values_from = name) -> tag_df_merge

  ressource_df %>%
    dplyr::left_join(macro_data) %>%
    dplyr::left_join(tag_df_merge) -> global_df

  message("There are in total ", nrow(macro_data), " different datasets available.\n",
          "These datasets belong to ", nrow(distinct(tag_df, name)), " groups. These groups are:\n",
          distinct(tag_df, name))

  return(global_df)

}




# tryout_stuff - not relevant for function call ---------------------------


function_return <- show_data() # now the function returns two lists in which two dfs are stored



tag_df %>%
  dplyr::mutate(no_tag = stringr::str_c("tag_no", "_",  no_tag))


function_return %>%
  group_by(datasource) %>%
  tally()
