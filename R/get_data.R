#' Get Data Function (no final description)
#'
#' @param data The data specified in the show_data function
#' @param download Specifies the way, how the data is downloaded. Choose from c("Environment", "Local")
#'
#' @return Returns the data from the
#' @export
#'
#' @examples show_data(tag = 'Umwelt und Klima') %>% get_data()

get_data <- function(data, download = "Environment")  {

  #Asking for permission to download
  cli::cli({helper_message_authorization(data)})
  answer <- readline()
  
  # 1. Step: Stop, if there is no permission to download
  if (answer == "N" | answer == "n" | answer == "No" | answer == "no"){
  cli::cli_alert_danger("You have aborted the download. Please run the function again!")
  helper_stopQuietly()}

  
  #May include some info about excluded urls
  #Need for documentation
  id <- cli::cli_status("")

  # cli::cli_alert_info("We have exluded {x} datasets because of invalid URLs or unknown fomats")

  cli::cli_alert_info("We have exluded {x} datasets because of invalid URLs or unknown fomats")

  
  
  # 4. Step: Evaluate download option and start downloading
  if (download == "Environment" | download == "E"){
    bad_urls <- list()
    assign("List_Open_Data", list(), envir = .GlobalEnv)


    assign("Remaining_Data", list(), envir = .GlobalEnv)

    assign("List_Open_Data",list(), envir = .GlobalEnv)
    assign("remaining_data",list(), envir = .GlobalEnv)

    
    helper_message_start_download()
    #id <- cli::cli_status("")


    purrr::pwalk(.l = data,.f = function(...){
      current <- tibble::tibble(...)
      #CSV: Define and Apply Fetcher
      if (current$format == "csv"){
        cli::cli_status_update(id,"Downloading {current$name} of type {.emph {current$format}}")

        tryCatch({ List_Open_Data <<- append(List_Open_Data, list(data.table::fread(current$url, showProgress = F, encoding = "UTF-8")))
                  cli::cli_status_update(id, cli::cli_alert_success("{current$name}: {cli::symbol$tick} Done"))
                  },
                  warning = function(w) {
                    suppressWarnings(w)
                  },
                  error = function(e){
                    List_Open_Data <<- append(List_Open_Data, list(paste(current$name,' - ', 'failed')))
                    cli::cli_status_update(id, cli::cli_alert("{current$name}: {cli::symbol$cross} failed"))}
                  )

        List_Open_Data <<- append(List_Open_Data, list(data.table::fread(current$url, showProgress = F, encoding = "UTF-8")))

        names(List_Open_Data)[length(List_Open_Data)] <<- current$name
      }
       #JSON: Define and Apply Fetcher
      else if (current$format == "json" | current$format == "geojson") {
      cli::cli_status_update(id,"Downloading {current$name} of type {.emph {current$format}}")
      tryCatch({ List_Open_Data <<- append(List_Open_Data, list (jsonlite::fromJSON(txt = current$url, flatten = TRUE)))
        cli::cli_status_update(id, cli::cli_alert_success("{current$name}: {cli::symbol$tick} Done"))
      },
        warning = function(w) {
          suppressWarnings(w)
        },
        error = function(e){
          List_Open_Data <<- append(List_Open_Data, list(paste(current$name,' - ', 'failed')))
          cli::cli_status_update(id, cli::cli_alert("{current$name}: {cli::symbol$cross} failed"))}
      )
      names(List_Open_Data)[length(List_Open_Data)] <<- current$name
      }

      #Excel: Define and Apply Fetcher

      else if (current$format == "xls" | current$format == "xlsx") {
      tryCatch({ List_Open_Data <<- append(List_Open_Data, list (rio::import(current$url)))
        cli::cli_status_update(id, cli::cli_alert_success("{current$name}: {cli::symbol$tick} Done"))
      },
        warning = function(w) {
          suppressWarnings(w)
        },
        error = function(e){
          List_Open_Data <<- append(List_Open_Data, list(paste(current$name,' - ', 'failed')))
          cli::cli_status_update(id, cli::cli_alert("{current$name}: {cli::symbol$cross} failed"))}
      )
      }
      #XML: Define and Apply Fetcher
      else if (current$format == "xml") {
      tryCatch({ List_Open_Data <<- append(List_Open_Data, list(XML::xmlTreeParse("{current$url}")))
        cli::cli_status_update(id, cli::cli_alert_success("{current$name}: {cli::symbol$tick} Done"))
      },
        warning = function(w) {
          suppressWarnings(w)
        },
        error = function(e){
          List_Open_Data <<- append(List_Open_Data, list(paste(current$name,' - ', 'failed')))
          cli::cli_status_update(id, cli::cli_alert("{current$name}: {cli::symbol$cross} failed"))}
      )}

      #Others: Append URL for further analysis
      else {
        Remaining_Data  <<- append(Remaining_Data, list(current))
        names(Remaining_Data)[length(Remaining_Data)] <<- current$name
        return(current)
        }
    })
    
    if (length(Remaining_Data)>0){

      cli::cli_alert_info("The remaining {length(Remaining_Data)} datasets cannot be loaded in the R Environment. Do you want to save them locally? (Y/N)")
      answer <- readline()
      if ("Y" %in% answer | "y" %in% answer) {
        if(!dir.exists("./Open_Data_Remaining")) {
          dir.create("Open_Data_Remaining")
        }
        purrr::map(.x = Remaining_Data,.f = function(x){
          current <- as.data.frame(x)
          utils::download.file(url = current$url, destfile = paste0('./Open_Data_Remaining/',current$name,'.',current$format), quiet = T)

        # remaining_data <- dplyr::bind_rows(remaining_data, temp)
        # return(remaining_data)
        remaining_data <<- append(remaining_data, list(c(current$url, current$format, current$name)))
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
  helper_message_end_download()
    }
  }
}



