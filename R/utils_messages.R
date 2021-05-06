### Helper Functions for Messages

## show_data

#' Helper Function Overview Messages
#'
#' @noRd
#' @param n_datasources_message internal parameters
#' @param n_datasets_message internal parameters
#' @param tag_list_message internal parameters
#' @param format_list_message internal parameters
message_overview_show_data <- function(n_datasources_message,
                                       n_datasets_message,
                                       tag_list_message,
                                       format_list_message){
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

## get_data

#' Helper Function Overview Messages
#' @noRd
#' @param data
#'
message_authorization <- function(data){
  cli::cli({
    cli::cli_h1("Download Authorization")
    cli::cli_text("")
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("You have selected a total of {nrow(data)} data sets!")
    cli::cli_alert("{.emph Do you want to start the download (Y/N)?}")
    cli::cli_text("Please type your answer into the console and press Enter!")
  })
}

message_start_download <- function(...){
  cli::cli({
    cli_alert_info("About to start downloads.")
    cli::cli_text("")})
}

message_unknown_format <- function(name, format){
  cli::cli_alert_warning("Not able to download {name} of type {.emph {format}}")
}


message_end_download <- function(...){
  cli::cli({
    cli::cli_h1("Done")
    cli::cli_alert_success("Congratulations, your download has been successfully completed!")
    cli::cli_text("For more information, please refer to the documentation.")
  })
}

