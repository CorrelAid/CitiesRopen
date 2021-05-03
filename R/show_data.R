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
    purrr::map(chuck, "resources") -> only_ressources

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
    purrr::map(purrr::pluck, "tags") -> only_tags



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



  # check for external hosted datasets
  if(external == FALSE){
    urltools::url_parse(global_df$url) %>%
      dplyr::select(domain) %>%
      dplyr::bind_cols(global_df) %>%
      dplyr::filter(domain %in% "offenedaten-konstanz.de") -> global_df
  }

  ##for the format function
  #First check that the filter is correct

  if (!is.null(format) & isFALSE(all(format %in% global_df$format))) {
    #Identify and store the filter(s) that is wrong
    wrong_format <- format[!format %in% global_df$format]
    #Store the possible formats
    possible_formats <- dplyr::distinct(global_df,format)
    possible_formats <- as.list(possible_formats$format)


    stop("Your format filter(s) ", paste(wrong_format, collapse = " and "), " does not match the existing formats.Is this what you meant?\n",
         paste(purrr::map_chr(wrong_format, ~ getBestMatch(.x, possible_formats)), collapse = ", "),
         "\n\nAll possible formats are:\n",
         paste(possible_formats, collapse = ", "), ".")
  }


  #Then if everything is correct
  if (!is.null(format)) {
    global_df %>%
      dplyr::filter(format %in% format) -> global_df
  }



  #For the tag function
  #Check if tag filter matches the existing categories

  #Store the possible tags
  possible_tags <- dplyr::distinct(tag_df,name)
  possible_tags <- as.list(possible_tags$name)

  if(!is.null(tag) & isFALSE(all(tag %in% tag_df$name))){
    wrong_tag <- tag[!tag %in% tag_df$name]

    stop("Your category filter(s) ", paste(wrong_tag, collapse = ","), " does not match the existing categories. Is this what you meant?\n",
         paste(purrr::map_chr(wrong_tag, ~ getBestMatch(.x, possible_tags)), collapse = ", "), ".",
         "\n\nAll possible category filters are:\n",
         paste(possible_tags, collapse = ", "), ". \nPlease check the filter(s) you entered.")
  }
  ##Filter out datasets with their tag
  if(!is.null(tag)) {
    global_df %>%
      dplyr::filter(dplyr::if_any(dplyr::starts_with("tag_no"), ~. %in% tag)) -> global_df
  }

  #Add the messages
  if (message == TRUE) {
    if (!is.null(format)){
      for (i in 1:length(format)){
        global_df %>%
          dplyr::filter(format %in% format [i]) -> byformat_df
        message("There are in total ", nrow(byformat_df),
                " resources in the format ", format [i], ".")
      }
    }

    if (!is.null(tag)) {
      for (i in 1:length(tag)){
        global_df %>%
          dplyr::filter(if_any(starts_with("tag_no"), ~. %in% tag [i])) -> bytag_df
        message("There are in total ", nrow(distinct(bytag_df,datasource)),
                " datasets under the category ", tag[i], ".")
      }
    }

    if (is.null(format) & is.null(tag)){
      message("There are in total ", nrow(macro_data), " different datasets available.\n",
              "These datasets belong to ", nrow(distinct(tag_df, name)), " categories. These categories are:\n",
              paste(possible_tags, collapse =", "), ".")

    }
  }

  invisible(global_df)
}







#A function to create suggest another filter if there is an error
#' Title
#'
#' @param user_filter
#' @param possible_filters
#'
#' @return
#'
#'
#' @examples
getBestMatch <- function(user_filter, possible_filters){
  purrr::map_dbl(possible_filters, ~RecordLinkage::jarowinkler(user_filter, .x)) %>%
    magrittr::set_names(possible_filters) %>%
    which.max %>%
    names
}







