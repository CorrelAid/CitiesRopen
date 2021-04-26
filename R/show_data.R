### show_data ###


# load packages
library(dplyr)
library(httr)
library(purrr)
library(jsonlite)
library(magrittr)
library(tidyr)
library(stringr)
library(urltools)


# define API endpoint



# get package list with resources


show_data <- function(external = TRUE, overview = TRUE, tag = NULL, format_filter = NULL) {

  # define base url
  url <- "https://offenedaten-konstanz.de/api/3/action/current_package_list_with_resources"

  # get json file with ressources

  resp <- httr::GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }


  # parse json file to r list object
  parsed <- httr::content(resp, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE, flatten = TRUE)

  # extract list element of interest
  parsed %>%
    chuck("result", 1) -> package_list

  # extract title and id of available datasets and save as df

  package_list %>%
    map_dfr(magrittr::extract, c("id","title"),
            .id = "datasource") %>%
    mutate(datasource = as.integer(datasource)) -> macro_data


  # extract list element with ressources of different data sets
  package_list %>%
    map(chuck, "resources") -> only_ressources

  # create empty list with lenght of ressources list
  temp_list_ressources <- vector("list", length = length(only_ressources))

  # iterate over ressources list and store in df

  for (i in seq_along(only_ressources)) {

    only_ressources %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("url","name","format","resource_group_id"),
                     .id = "no_ressource") %>%
      dplyr::bind_cols(tibble(datasource = i)) -> temp_list_ressources[[i]]
  }

  dplyr::bind_rows(temp_list_ressources) -> ressource_df

  package_list %>%
    map(purrr::pluck, "tags") -> only_tags



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
    dplyr::left_join(macro_data, by = "datasource") %>%
    dplyr::left_join(tag_df_merge, by = "datasource") -> global_df


  ##Filter out datasets with their tag
  if (!is.null(format_filter)) {
    global_df %>%
      dplyr::filter(format %in% format_filter) -> global_df
  }

  if(!is.null(tag)) {
    global_df %>%
      dplyr::filter(if_any(starts_with("tag_no"), ~. %in% tag)) -> global_df
  }

  #Add the messages
  if (is.null(tag) & !is.null(format_filter)) {
    global_df %>%
      distinct(datasource) %>%
      nrow() -> nb_filtered_datasets

    message("You have used the format filter(s) ", paste(format_filter, collapse = " and "), ".")

    for (i in 1:length(format_filter)){
      global_df %>%
        filter(format %in% format_filter [i]) -> byformat_df
      message("There are in total ", nrow(byformat_df),
              " resources in the format ", format_filter [i], ".")
    }
  } else if (!is.null(tag) & is.null(format_filter)) {
    message("You have used the category filter(s) ", paste(tag, collapse = " and "), ".") #Indicate whether one or more filters have been specified.

    for (i in 1:length(tag)){
      global_df %>%
        filter(if_any(starts_with("tag_no"), ~. %in% tag [i])) -> bytag_df
      message("There are in total ", nrow(distinct(bytag_df,datasource)),
              " datasets under the category ", tag[i], ".")
    }

  } else if (!is.null(tag) & !is.null(format_filter)) {
    message("You have used the category filter(s) ", paste(tag, collapse = " and "), " and the format filter ", paste(format_filter, collapse = " and "), ".") #Indicate whether one or more filters have been specified.
    for (i in 1:length(tag)){
      global_df %>%
        filter(if_any(starts_with("tag_no"), ~. %in% tag [i])) -> bytag_df
      message("There are in total ", nrow(distinct(bytag_df, datasource)),
              " datasets under the category ", tag[i], ".")
      for (i in 1:length(format_filter)) {
        bytag_df %>%
          filter(format %in% format_filter[i]) -> bytag_byformat_df
        message("In this category, there are ", nrow(bytag_byformat_df),
                " resources with the format ", format_filter[i], ".")
      }
    }

  } else {
    message("There are in total ", nrow(macro_data), " different datasets available.\n",
            "These datasets belong to ", nrow(distinct(tag_df, name)), " categories. These categories are:\n",
            distinct(tag_df, name))
  }

  # check for external hosted datasets
  if(external == FALSE){
    urltools::url_parse(global_df$url) %>%
      select(domain) %>%
      bind_cols(global_df) %>%
      filter(domain %in% "offenedaten-konstanz.de") -> global_df
  }

  if(overview == TRUE){
    message("There are in total ", nrow(macro_data), " different datasets available.\n",
          "These datasets belong to ", nrow(distinct(tag_df, name)), " categories. These categories are:\n",
          distinct(tag_df, name))

  invisible(global_df)

}
  invisible(global_df)
}










  #### überlegen ob nicht output als df sinnvoll ist
#
#
#
#
#
#
# system.time(function_return <- show_data(overview = F))
#
#
#
#
# # tryout_stuff - not relevant for function call ---------------------------
#
# #Test whether the function returns all datasets when no filter is specified
# test_nofilter <- show_data(overview = FALSE)
# #Test with one filter
# test_onefilter <- show_data (tag = "Soziales", overview = FALSE)
# #If I want several filters, I can store my filters in a list
# tag_filters <- c("Soziales", "Umwelt und Klima")
# test_multifilter <- show_data(tag = tag_filters, format_filter = c("json", "csv"), overview = FALSE)
#
# #If I want only datasets with csv format
# test_format <- show_data(format_filter ="csv", overview = FALSE)




