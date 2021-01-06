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
require('tidyverse')

###Basic Show-Data Function

show_data <- function(meta = FALSE, group = NULL, external = NULL) {
  resp <- httr::GET("https://www.offenedaten-konstanz.de/api/3/action/current_package_list_with_resources") #get URL
  resp_list <- fromJSON(httr::content(resp, 'text'), flatten = TRUE)       #save JSON-data in a list
  metadata <- as.data.frame(resp_list)               #generate data Frame
  if (!meta)  return (metadata %>%
                             assign("Metadata",.,envir = .GlobalEnv) %>%
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

##Analysing Nested Data Frames and trying to unnest them
DF_ressources <- unnest(Konstanz_df, cols = c(result.resources))
DF_tags <- unnest(Konstanz_df, cols = c(result.tags))
DF_groups <- unnest(Konstanz_df, cols = c(result.groups))

DF_ressources %>%
  select(id,format,state,size) %>%
  as.data.frame() %>%
  assign("DF_ressources",.,envir = .GlobalEnv)

DF_tags %>%
  select(id,name) %>%
  as.data.frame() %>%
  assign("DF_tags",.,envir = .GlobalEnv)


DF_groups %>%
  select(description, id, image_display_url) %>%
  as.data.frame() %>%
  assign("DF_groups",.,envir = .GlobalEnv)


MyMerge <- function(x, y){
  df <- merge(x, y, by= "id", all.x= TRUE, all.y= TRUE)
  return(df)
}

new.df <- Reduce(MyMerge, list(DF_ressources, DF_tags, DF_groups))
View(new.df[256,])

xx <- MyMerge(DF_ressources,DF_tags)

xx %>%
  filter(format == 'csv') %>%
  View()



df <- join_all(list(DF_ressources,DF_tags,DF_groups), by = 'id', type = 'full')




