#' Get Data Function (no final description)
#'
#' @param data The data specified in the show_data function
#' @param download Specifies the way, how the data is downloaded. Choose from c("Environment", "Local")
#'
#' @return Returns the data from the
#' @export
#'
#' @examples show_data(tag = 'Umwelt und Klima') %>% get_data()
get_data <- function(data, download = "Environment"){
  cli::cli({
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_h1("Download Authorization")
    cli::cli_text("You have selected a total of {nrow(data)} data sets!")
    cli::cli_alert("{.emph Do you want to start the download (Y/N)?}")
    cli::cli_text("Please type your answer into the console and press Enter!")
  })
  answer <- readline()
  # 1. Step: Stop, if there is no permission to download
  if (answer == "N" | answer == "n" | answer == "No" | answer == "no"){
  cli::cli_alert_danger("You have aborted the download. Please run the function again!")
  CitiesRopen:::stopQuietly()}
  # 2. Step: Evaluate download option and start downloading
  if (download == "Environment" | download == "E"){
      assign("List_Open_Data",list(), envir = .GlobalEnv)
      purrr::pwalk(.l = data,.f = function(...){
        current <- tibble::tibble(...)
        #CSV: Define and Apply Fetcher
        if (current$format == "csv"){
          List_Open_Data <<- append(List_Open_Data, list(data.table::fread(current$url)))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
          cli::cli({
            cli::cli_text("Downloading {current$name} of type {current$format}")
            cli::cli_alert_success("Done")
          })
        }
        #JSON: Define and Apply Fetcher
        else if (current$format == "json") {
          List_Open_Data <<- append(List_Open_Data, list (jsonlite::fromJSON(txt = current$url, flatten = TRUE)))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
          cli::cli({
          cli::cli_text("Downloading {current$name} of type {current$format}")
          cli::cli_alert_success("Done")
          })
        }
        #Others: Append URL for further analysis
        else {
          List_Open_Data <<- append(List_Open_Data, list(current$url))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
          cli::cli_alert_warning("Not able to download {current$name} of type {.emph {current$format}}")
        }
      })
    }
  else if (download == "Local" | download == "L"){
    dir.create("Open_Data_Konstanz")
    purrr::pwalk(.l = data,.f = function(...){
      current <- tibble::tibble(...)
      utils::download.file(url = current$url, destfile = paste('./Open_Data_Konstanz/',current$name,'.',current$format, sep = ''), quiet = T)
    })
  }
  cli::cli({
    cli::cli_h1("Done")
    cli::cli_alert_success("Congratulations, your download has been successfully completed!")
    cli::cli_text("For more information, please refer to the documentation.")
  })
}

