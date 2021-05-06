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

  #Asking for permission to download
  cli::cli({message_authorization(data)})
  answer <- readline()

  # 1. Step: Stop, if there is no permission to download
  if (answer == "N" | answer == "n" | answer == "No" | answer == "no"){
  cli::cli_alert_danger("You have aborted the download. Please run the function again!")
  CitiesRopen:::stopQuietly()}
  # 2. Step: Evaluate download option and start downloading
  if (download == "Environment" | download == "E"){

    assign("List_Open_Data",list(), envir = .GlobalEnv)

    message_start_download()
    id <- cli::cli_status("")

    purrr::pwalk(.l = data,.f = function(...){
      current <- tibble::tibble(...)
      #CSV: Define and Apply Fetcher
      if (current$format == "csv"){
        cli::cli_status_update(id,"Downloading {current$name} of type {.emph {current$format}}")
        List_Open_Data <<- append(List_Open_Data, list(data.table::fread(current$url, showProgress = F, encoding = "UTF-8",)))
        names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        cli::cli_status_update(id, cli::cli_alert_success("{current$name}: {symbol$tick} Done"))
      }
      #JSON: Define and Apply Fetcher
      else if (current$format == "json") {
        cli::cli_status_update(id,"Downloading {current$name} of type {emph. {current$format}}")
        List_Open_Data <<- append(List_Open_Data, list (jsonlite::fromJSON(txt = current$url, flatten = TRUE)))
        names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        cli::cli_status_update(id, cli::cli_alert_success("{current$name}:Done"))
      }
      #Others: Append URL for further analysis
        else {
          List_Open_Data <<- append(List_Open_Data, list(current$url))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
          #message_unknown_format(current$name, current$format)
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
  cli::cli_status_clear(id)
  message_end_download()
}




