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

show_data <- function(Simplified = TRUE, Parameter = NULL) {
  resp <- GET("https://www.offenedaten-konstanz.de/api/3/action/current_package_list_with_resources") #get URL
  resp_list <- fromJSON(content(resp, 'text'), flatten = TRUE)       #save JSON-data in a list
  metadata <- as.data.frame(resp_list)               #generate data Frame
  if (Simplified)  return (metadata %>%
                             select(c('result.title','result.url')) %>%
                             as.data.frame() %>%
                             View()
  )
  if (!Simplified)  return (View(metadata))
}

#Testing most basic function show_data
show_data()  #When there is no argument "Simplified = FALSE", the function only shows the most basic variables
show_data(Simplified = FALSE)



