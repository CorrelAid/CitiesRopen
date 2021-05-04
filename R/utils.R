#' Create Logo
#' @noRd
#' @return Creates the package logo
#'
#' @examples
create_logo <- function(...){
  imgurl <- "https://cdn.pixabay.com/photo/2013/07/12/16/51/mexico-city-151380_1280.png"
  hexSticker::sticker(imgurl,
          package="CitiesRopen",
          p_fontface = "bold",
          p_size=7.25,
          p_y = 1.35,
          s_x=1,
          s_y=0.8,
          s_width=0.8,
          s_height = 0.8,
          h_fill="white",
          h_size = 1.5,
          asp = 1.8,
          p_color = "#3863a2",
          h_color = "#f04451",
          filename="man/figures/logo_package.png")
}

#' Stopping a function without throwing an error messages
#'
#' @param ...
#' @noRd
#' @return Stops the execution of an script without printing an error messsage
#'
#' @examples
stopQuietly <- function(...) {
  blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
}
