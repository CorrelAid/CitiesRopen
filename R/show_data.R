#' show data function
#'
#' @param external boolean to include/exclude external datasets (not hosted by OpenData Konstanz)
#' @param tag vector of strings to filter for specific tags/groups
#' @param format vector of strings to filter for specific data formats
#' @param message boolean to get description of returned data set
#'
#' @return dataframe/tibble with links and information to available data sets
#' @export
#'
#' @examples
show_data <- function(external = TRUE, tag = NULL, format = NULL, message = TRUE) {


  global_df <- create_global_df()

  ### create global containers for tags and formats
  n_datasources <- dplyr::n_distinct(global_df$datasource)
  n_datasets <- nrow(global_df)

  global_df %>%
    dplyr::select(dplyr::starts_with("tag")) %>%
    as.matrix() %>%
    as.vector() %>%
    unique() %>%
    purrr::discard(is.na) -> tag_list

  global_df %>%
    dplyr::select(format) %>%
    as.matrix() %>%
    as.vector() %>%
    unique() %>%
    purrr::discard(is.na) -> format_list


  # check for external hosted datasets
  if(external == FALSE){

    global_df <- filter_external(global_df)

  }


  #For the tag function
  #Check if tag filter matches the existing categories

  #Store the possible tags

  if(!is.null(tag) & isFALSE(all(tag %in% tag_list))){
    wrong_tag <- tag[!tag %in% tag_list]

    stop("Your category filter(s) ", paste(wrong_tag, collapse = ","), " does not match the existing categories. Is this what you meant?\n",
         paste(purrr::map_chr(wrong_tag, ~ getBestMatch(.x, tag_list)), collapse = ", "), ".",
         "\n\nAll possible category filters are:\n",
         paste(tag_list, collapse = ", "), ". \nPlease check the filter(s) you entered.")
  }

  ##Filter out datasets with their tag
  if(!is.null(tag)) {
    global_df %>%
      dplyr::filter(dplyr::if_any(dplyr::starts_with("tag_no"), ~. %in% tag)) -> global_df
  }



  ##for the format function
  #First check that the filter is correct

  if (!is.null(format) & isFALSE(all(format %in% format_list))) {
    #Identify and store the filter(s) that is wrong
    wrong_format <- format[!format %in% format_list]


    stop("Your format filter(s) ", paste(wrong_format, collapse = " and "), " does not match the existing formats.Is this what you meant?\n",
         paste(purrr::map_chr(wrong_format, ~ getBestMatch(.x, format_list)), collapse = ", "),
         "\n\nAll possible formats are:\n",
         paste(format_list, collapse = ", "), ".")
  }


  #Then if everything is correct
  if (!is.null(format)) {

    input_format <- format

    global_df %>%
      dplyr::filter(format %in% input_format) -> global_df
  }


  #Add the messages
  if (message == TRUE) {
    message_function(global_df)
  }

  invisible(global_df)
}



### helper function
create_global_df <- function(){

  # define base url
  url <- "https://offenedaten-konstanz.de/api/3/action/current_package_list_with_resources"

  # get json file with ressources

  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }


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

  # iterate over ressources list and store in df

  for (i in seq_along(only_ressources)) {

    only_ressources %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("url","name","format","resource_group_id"),
                     .id = "no_ressource") %>%
      dplyr::bind_cols(dplyr::tibble(datasource = i)) -> temp_list_ressources[[i]]
  }

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


### helper function
filter_external <- function(global_df) {
  urltools::url_parse(global_df$url) %>%
    dplyr::select(domain) %>%
    dplyr::bind_cols(global_df) %>%
    dplyr::filter(domain %in% "offenedaten-konstanz.de")
}



### helper function

message_function <- function(global_df){

  ### create  containers for tags and formats
  n_datasources_message <- dplyr::n_distinct(global_df$datasource)
  n_datasets_message <- nrow(global_df)

  global_df %>%
    dplyr::select(dplyr::starts_with("tag")) %>%
    as.matrix() %>%
    as.vector() %>%
    unique() %>%
    purrr::discard(is.na) -> tag_list_message

  global_df %>%
    dplyr::select(format) %>%
    as.matrix() %>%
    as.vector() %>%
    unique() %>%
    purrr::discard(is.na) -> format_list_message



  cli::cli({
    cli::cli_h1("Query result")
    cli::cli_alert_info("Your query resulted in {n_datasources_message} data sourc{?e/es}.")
    cli::cli_h2("Amount of data sets")
    cli::cli_alert_info("In total there {?is/are} {n_datasets_message} data se{?t/ts} associated with these data sources.")
    cli::cli_h2("Different groups/tags")
    cli::cli_alert_info("{?This/These} {n_datasets_message} data se{?t/ts} belon{?gs/g} to the following groups and can be filtered accordingly:")
    cli::cli_ul(tag_list_message)
    cli::cli_h2("File format of data sets")
    cli::cli_alert_info("{?This/These} {n_datasets_message} data se{?t/ts} include the following file formats and can be filtered accordingly:")
    cli::cli_ul(format_list_message)
  })
}




### helper function

getBestMatch <- function(user_filter, possible_filters){
  purrr::map_dbl(possible_filters, ~RecordLinkage::jarowinkler(user_filter, .x)) %>%
    magrittr::set_names(possible_filters) %>%
    which.max %>%
    names
}






