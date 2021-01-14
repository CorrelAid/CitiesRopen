#Before creating the main calling functions, we’ll create some utilitary functions that will run some tests: is the
# internet connexion running? Does the {httr} result return the right http code?
# For this, we’ll create a file called utils.R, save it in the R/ folder, and put into it:
# https://colinfay.me/build-api-wrapper-package-r/

#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

#' @importFrom httr status_code
check_status <- function(res){
  stop_if_not(.x = status_code(res),
              .p = ~ .x == 200,
              msg = "The API returned an error")
}

base_url <- "https://www.offenedaten-konstanz.de/api/3/action/"
check_internet()
check_status()
