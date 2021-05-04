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
  cli({
    cli_h2("Security Check")
      cli_text("If you continue, a total of ",nrow(data), " Files will be downloaded")
      cli_alert_warning("Do you want to proceed? (Y/N)?")
      cli_text("Please type the correct letter into the console and press Enter!")
  })
  answer <- readline()
  # 1. Step: Stop, if there is no permission to download
  if (answer == "N" | answer == "n" | answer == "No" | answer == "no"){
  cli_alert_danger("You have aborted the download. Please run the function again!")
  stopQuietly()}
  # 2. Step: Evaluate download option and start downloading
  if (download == "Environment" | download == "E"){
      assign("List_Open_Data",list(), envir = .GlobalEnv)
      purrr::pwalk(.l = data,.f = function(...){
        current <- tibble::tibble(...)
        #CSV: Define and Apply Fetcher
        if (current$format == "csv"){
          List_Open_Data <<- append(List_Open_Data, list(data.table::fread(current$url)))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        }
        #JSON: Define and Apply Fetcher
        else if (current$format == "json") {
          List_Open_Data <<- append(List_Open_Data, list (jsonlite::fromJSON(txt = current$url, flatten = TRUE)))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
        }
        #Others: Append URL for further analysis
        else {
          List_Open_Data <<- append(List_Open_Data, list(current$url))
          names(List_Open_Data)[length(List_Open_Data)] <<- current$name
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
}

