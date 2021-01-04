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

###Defining Arguments Dataset API (Sample)

show_data <- function(meta = FALSE, group = NULL, external = NULL) {
  resp <- httr::GET("https://www.offenedaten-konstanz.de/api/3/action/current_package_list_with_resources") #get URL
  resp_list <- fromJSON(httr::content(resp, 'text'), flatten = TRUE)       #save JSON-data in a list
  metadata <- as.data.frame(resp_list)               #generate data Frame
  if (!meta)  return (metadata %>%
                             select(c('result.title','result.tags','result.resources','result.groups')) %>%
                             as.data.frame() %>%
                             View()
  )
  if (meta)  return (View(metadata))
}

<<<<<<< HEAD
### Note; The current result is the basic information when running the show_data() function. However, the most important information
###       are in the sub-datasets within the variables 'result.tags','result.resources','result.groups'.
###       Having to leave now, I will next continue to iterate over those data frames to extract the most important
###       information, namely category, URL to access the data and data provider.
=======
### Note; The current result is the basic information like in df_temp. However, the most important information
###       are in the sub-datasets within the variables 'result.tags','result.resources','result.groups'.
###       Having to leave now, I will next continue to iterate over those data frames to extract the most important
###       information, namely category, URL to access the data and possibly origin

>>>>>>> cc83dc68825ad03d73e10bc01908c062e9615844


#Testing most basic function show_data
show_data()  #When there is no argument "Simplified = FALSE", the function only shows the most basic variables
show_data(meta = TRUE)



