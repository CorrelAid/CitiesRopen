#' show data function
#'
#' @param external boolean to include/exclude external datasets (not hosted by OpenData Konstanz)
#' @param category vector of strings to filter for specific category/groups
#' @param format vector of strings to filter for specific data formats
#' @param message boolean to get description of returned data set
#'
#' @return dataframe/tibble with links and information to available data sets
#' @export
#'
#' @examples
library(magrittr)
show_data <- function(external = TRUE, category = NULL, format = NULL, message = TRUE) {


  global_df <- helper_create_global_df()

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

    global_df <- helper_filter_external(global_df)

  }


  #Check if category filter matches the existing categories
  if(!is.null(category) & isFALSE(all(category %in% tag_list))){
    wrong_tag <- category[!category %in% tag_list]
    suggested_tag_filters <- helper_suggest_filter_tags(wrong_tag, tag_list)

    helper_message_matching_tag(wrong_tag, suggested_tag_filters, tag_list)
    helper_stopQuietly()
  }

  #Filter out data sets with tags
  if(!is.null(category)) {
    global_df %>%
      dplyr::filter(dplyr::if_any(dplyr::starts_with("tag_no"), ~. %in% category)) -> global_df
  }

  #Check if format filters are correct
  if (!is.null(format) & isFALSE(all(format %in% format_list))) {
    #Identify and store the filter(s) that is wrong
    wrong_format <- format[!format %in% format_list]
    suggested_format_filters <- helper_suggest_filter_format(wrong_format, format_list)

    helper_message_matching_filter(wrong_format, suggested_format_filters, format_list)
    helper_stopQuietly()
    }


  #Filter out data sets with formats
  if (!is.null(format)) {

    input_format <- format

    global_df %>%
      dplyr::filter(format %in% input_format) -> global_df
  }

  #Fetching fails if URL is NA
  global_df <- global_df[which(!is.na(global_df$url)),]

  #global_df$format unreliable in some cases, rely on URL to fix
  for(i in 1:length(global_df$url)){
    if (grepl("csv", global_df$url[i], ignore.case= TRUE)){
      global_df$format[i] <- "csv"
    }
    else if (grepl(".xls", global_df$url[i], ignore.case= TRUE)) {
      global_df$format[i] <- "xls"
    }
  }

  #Add the messages
  if (message == TRUE) {
    helper_message_show_data(global_df)
  }
  # #Need to extract urls from faulty url patterns
  urls_to_extract <- !stringr::str_starts(global_df$url, stringr::fixed('http'))
  pat <- stringr::regex('(http|ftp|https):\\/\\/([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:\\/~+#-]*[\\w@?^=%&\\/~+#-])')
  global_df$url[urls_to_extract] <- stringr::str_extract(global_df$url[urls_to_extract], pat)

  invisible(global_df)
}








