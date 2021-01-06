### First Function
### Access Metadata from all files in Open Data Portal


####Source: https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
###general built up of an URL: https://skorks.com/2010/05/what-every-developer-should-know-about-urls/
### <scheme>://<username>:<password>@<host>:<port>/<path>;<parameters>?<query>#<fragment>

rm(list = ls())

###Loading Packages
require("httr")
require("jsonlite")
require("magrittr")
require("tidyverse")

###Defining Arguments Dataset API (Sample)

show_data <- function(meta = FALSE, group = NULL, external = NULL) {
  resp <- httr::GET("https://www.offenedaten-konstanz.de/api/3/action/current_package_list_with_resources") #get URL
  resp_list <- fromJSON(httr::content(resp, 'text'), flatten = TRUE)       #save JSON-data in a list
  metadata <- as.data.frame(resp_list)               #generate data Frame
  if (!meta)  return (metadata %>%
                             select(c('result.title','result.tags','result.resources','result.groups')) %>%
                             as.data.frame() %>%
                             assign("Konstanz_df",.,envir = .GlobalEnv) %>%
                             View()

  )
  if (meta)  return (View(metadata %>%
                            result <- show_data() %>%
                            View()))
}


show_data()  #When there is no argument "Simplified = FALSE", the function only shows the most basic variables
show_data(meta = TRUE)

##This is for unnesting dataframes within dataframes
DF_ressources <- unnest(Konstanz_df, cols = c(result.resources))
DF_tags <- unnest(Konstanz_df, cols = c(result.tags))
DF_groups <- unnest(Konstanz_df, cols = c(result.groups))






