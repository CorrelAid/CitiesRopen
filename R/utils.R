# Helpers show_data ----

#' Create Global DF for show_data
#'
#' @noRd
#' @return returns a full list of ressources in the Open Data Portal
#'
#' @examples
#'

# Make piping operator available for package users
usethis::use_pipe(export = TRUE)

helper_create_global_df <- function(){

  # define base url
  url <- "https://offenedaten-konstanz.de/api/3/action/current_package_list_with_resources"

  # get json file with ressources
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)}

  # parse json file to r list object
  parsed <- httr::content(resp, "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE, flatten = TRUE)

  # extract list element of interest
  parsed %>%
    purrr::chuck("result", 1) -> package_list

  # extract title and id of available datasets and save as df
  package_list %>%
    purrr::map_dfr(magrittr::extract, c("id","title"),
                   .id = "datasource") %>%
    dplyr::mutate(datasource = as.integer(datasource)) -> macro_data

  # extract list element with ressources of different data sets
  package_list %>%
    purrr::map(purrr::pluck, "resources") -> only_ressources

  # create empty list with lenght of ressources list
  temp_list_ressources <- vector("list", length = length(only_ressources))

  # iterate over resources list and store in df
  for (i in seq_along(only_ressources)) {

    only_ressources %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("url","name","format","resource_group_id"),
                     .id = "no_ressource") %>%
      dplyr::bind_cols(dplyr::tibble(datasource = i)) -> temp_list_ressources[[i]]
  }

  # Binding rows
  dplyr::bind_rows(temp_list_ressources) -> ressource_df

  package_list %>%
    purrr::map(purrr::pluck, "tags") -> only_tags

  temp_list_tags <- vector("list", length = length(only_tags))

  for (i in seq_along(only_tags)) {

    only_tags %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("name"),
                     .id = "no_tag") %>%
      dplyr::bind_cols(dplyr::tibble(datasource = i)) -> temp_list_tags[[i]]
  }

  dplyr::bind_rows(temp_list_tags) -> tag_df

  tag_df %>%
    dplyr::mutate(no_tag = stringr::str_c("tag_no", "_",  no_tag)) %>%
    tidyr::pivot_wider(names_from = no_tag, values_from = name) -> tag_df_merge

  ressource_df %>%
    dplyr::left_join(macro_data, by = "datasource") %>%
    dplyr::left_join(tag_df_merge, by = "datasource") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(.=="", NA, as.character(.)))) -> global_df
}


#' Helper: Filter global df for external data sources (show_data)
#'
#' @param global_df
#'
#' @noRd
#' @return global_df without external data sources
#'
#' @examples
helper_filter_external <- function(global_df) {
  urltools::url_parse(global_df$url) %>%
    dplyr::select(domain) %>%
    dplyr::bind_cols(global_df) %>%
    dplyr::filter(domain %in% "offenedaten-konstanz.de")
}

#' Helper: Matching Filters Format
#'
#' @param format_filter
#' @param possible_filters
#'
#' @return a match
#' @noRd
#'
#' @examples
helper_matching_function_format <- function(format_filter,possible_filters){
  results <- stringdist::stringdist(format_filter, possible_filters, method = "jw")
  best <- which(results == min(results))[1]
  return(possible_filters[best])
}

#' Helper: Suggest Matching Filters Format
#'
#' @param format_filter
#' @param possible_filters
#'
#' @return
#' @export
#'
#' @examples
helper_suggest_filter_format <- function(format_filter, possible_filters){
  purrr::map(format_filter, helper_matching_function_format, possible_filters)
}

#' Helper: Matching Filters Tags
#'
#' @param tag_filter
#' @param possible_filters
#'
#' @return a match
#' @noRd
#'
#' @examples
helper_matching_function_tags <- function(tag_filter,possible_filters){
  results <- stringdist::stringdist(tag_filter, possible_filters, method = "jw")
  best <- which(results == min(results))[1]
  return(possible_filters[best])
}

#' Helper: Suggest Matching Filters Tags
#'
#' @param tag_filter
#' @param possible_filters
#'
#' @return
#' @export
#'
#' @examples
helper_suggest_filter_tags <- function(tag_filter, possible_filters){
  purrr::map(tag_filter, helper_matching_function_tags, possible_filters)
}



# Helpers get_data ----

#' Stopping the function without throwing an error messages
#'
#' @param ...
#' @noRd
#' @return Stops the execution of an script without printing an error messsage
#'
#' @examples
helper_stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
}

url <-
helper_request_handler <- function(url) {
  req <- tryCatch({ httr::content(httr::GET(url))},
                  error = function(e) bad_urls <- list.append(bad_urls, urls))
  return(req)
}