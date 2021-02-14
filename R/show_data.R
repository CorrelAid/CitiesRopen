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

show_data <- function(tag = NULL) {
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

  
  ##Filter out datasets with their tag
  if (is.null(tag)) {  # Check if there is a filter
    message("There are in total ", nrow(macro_data), " different datasets available.\n",
            "These datasets belong to ", nrow(distinct(tag_df, name)), " groups. These groups are:\n",
            distinct(tag_df, name))
    return(global_df) # If there is no filter, the function returns all the datasets
  } else {
    global_df %>%
      dplyr::filter(tag_no_1 %in% tag | tag_no_2 %in% tag | tag_no_3 %in% tag) -> filtered_global #Checks the specified filter in all levels of tags
      message("You have used the filter(s) ", paste(tag, collapse = " and "), ".") #Indicate whether one or more filters have been specified. 
      for (i in 1:length(tag)){
        message("There are in total ", nrow(filter(filtered_global, tag_no_1 == tag [i]| tag_no_2 %in% tag [i] | tag_no_3 %in% tag[i])), 
                " datasets under the category ", tag[i], ".")
      }
    return(filtered_global) # Returns only the datasets from the filter
  }     
}





# tryout_stuff - not relevant for function call ---------------------------
function_return <- show_data() # now the function returns two lists in which two dfs are stored


#Test whether the function returns all datasets when no filter is specified
test_nofilter <- show_data()
#Test with one filter
test_onefilter <- show_data ("Soziales")
#If I want several filters, I can store my filters in a list
filters <- c("Soziales", "Umwelt und Klima")
test_multifilter <- show_data(filters)












tag_df %>%
  dplyr::mutate(no_tag = stringr::str_c("tag_no", "_",  no_tag))


function_return %>%
  group_by(datasource) %>%
  tally()
