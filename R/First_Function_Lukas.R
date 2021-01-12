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

###Basic Show-Data Function

show_data <- function(meta = FALSE, group = NULL, external = NULL) {
  resp <- httr::GET("https://www.offenedaten-konstanz.de/api/3/action/current_package_list_with_resources") #get URL
  resp_list <- jsonlite::fromJSON(httr::content(resp, 'text'), flatten = FALSE)       #save JSON-data in a list
  Konstanz_df <- as.data.frame(resp_list)               #generate data Frame

  for(i in names(Konstanz_df)){
    if (i == 'result.resources'){
      Konstanz_df  %>%
        tidyr::unnest(cols = result.resources, keep_empty = TRUE) -> ressources}
    else if (i == 'result.tags'){
      Konstanz_df  %>%
        tidyr::unnest(result.tags) -> tags}
    else if ( i == 'result.groups'){
      Konstanz_df  %>%
        tidyr::unnest(result.groups) -> groups}
    else{}
  }
  MyMerge <- function(x, y){
    df <- merge(x, y, by= "result.id", all.x= TRUE, all.y= TRUE)
    return(df)
  }
  Meta_df <- Reduce(MyMerge, list(Konstanz_df, ressources, tags, groups))

  if (!meta)  return (Meta_df %>%
                        dplyr::select('result.id','result.title.x','name.y','result.notes.x','result.url.x','url','format','mimetype') %>%
                        tibble::as_tibble() %>%
                        View())

  if (meta)  return (View(Meta_df))

}

##Checking

show_data()  #When there is no argument "Simplified = FALSE", the function only shows the most basic variables
show_data(meta = TRUE)
