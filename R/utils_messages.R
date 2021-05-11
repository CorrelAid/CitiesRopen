# Helpers show_data ----

#' Message: Generating the overview_message for show_data
#'
#' @param global_df
#'
#' @return returns a cli-message
#' @noRd
#'
#' @examples
helper_message_show_data <- function(global_df){

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

#' Message: Matching Error Message for Tags
#'
#' @param wrong_tag
#' @param suggested_tag_filters
#' @param tag_list
#'
#' @return An Error message
#' @noRd
#'
#' @examples
helper_message_matching_tag <- function(wrong_tag, suggested_tag_filters, tag_list){
  cli::cli({
    cli::cli_h1("Error")
    cli::cli_text("")
    cli::cli_text("Your category {?filter/filters} {.wrong_tag {wrong_tag}} {?does/do} not match the existing categories")
    cli::cli_alert_info("Is this what you meant? {.var {suggested_tag_filters}}")
    cli::cli_text("")
    cli::cli_alert_info("All possible category filters are: {tag_list}")
    cli::cli("Please check the filter(s) you entered.")
  })
}

#' Message: Matching Error Message for Format
#'
#' @param wrong_format
#' @param suggested_format_filters
#' @param format_list
#'
#' @return An Error message
#' @noRd
#'
#' @examples
helper_message_matching_filter <- function(wrong_format, suggested_format_filters, format_list){
  cli::cli({
    cli::cli_h1("Error")
    cli::cli_text("")
    cli::cli_text("Your category {?filter/filters} {.wrong_format {wrong_format}} {?does/do} not match the existing categories.")
    cli::cli_alert_info("Is this what you meant? {.var {suggested_format_filters}}")
    cli::cli_text("")
    cli::cli_alert_info("All possible format filters are: {format_list}")
    cli::cli("Please check the filter(s) you entered.")
  })
}

# Helpers get_data ----

#' Message: Authorizing Download Message
#'
#' @param data
#'
#' @return A message
#' @noRd
#'
#' @examples
helper_message_authorization <- function(data){
  cli::cli({
    cli::cli_h1("Download Authorization")
    cli::cli_text("")
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("You have selected a total of {nrow(data)} data sets!")
    cli::cli_alert("{.emph Do you want to start the download (Y/N)?}")
    cli::cli_text("Please type your answer into the console and press Enter!")
  })
}

#' Message: Message to start download
#'
#'
#' @return A message
#' @noRd
#'
#' @examples
helper_message_start_download <- function(...){
  cli::cli({
    cli::cli_alert_info("About to start downloads.")
    cli::cli_text("")})
}

#' Message: Unknown Format
#'
#' @param name
#' @param format
#'
#' @noRd
#' @return A message
#'
#' @examples
helper_message_unknown_format <- function(name, format){
  cli::cli_alert_warning("Not able to download {name} of type {.emph {format}}")
}

#' Message: Ending of Download
#'
#' @param ...
#'
#' @return A message
#' @noRd
#'
#' @examples
helper_message_end_download <- function(...){
  cli::cli({
    cli::cli_h1("Done")
    cli::cli_alert_success("Congratulations, your download has been successfully completed!")
    cli::cli_text("For more information, please refer to the documentation.")
  })
}



