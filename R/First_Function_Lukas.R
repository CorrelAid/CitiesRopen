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

  #Calling the API

  resp <- httr::GET("https://www.offenedaten-konstanz.de/api/3/action/current_package_list_with_resources")

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
}

  #Putting the response into a list

  resp_list <- jsonlite::fromJSON(httr::content(resp, 'text'), flatten = TRUE)

  #Saving the list as Dataframe

  Konstanz_df <- as.data.frame(resp_list)

  #The List/Dataframe comes with a nested structure. There are three variables (result.resources,result.tags,result.groups),
  #which contain further information on the single files.
  #The loop extracts the variables of interest and assigns them to a temporal variables

  for(i in names(Konstanz_df)){
    if (i == 'result.resources'){
      Konstanz_df  %>%
        tidyr::unnest(cols = result.resources, keep_empty = TRUE) %>%
        dplyr::select('result.id','id','result.id','result.name','result.notes','result.url','format','mimetype','size')-> ressources}
    else if (i == 'result.tags'){
      Konstanz_df  %>%
        tidyr::unnest(result.tags) %>%
        dplyr::select('result.id','id','name')-> tags}
    else if ( i == 'result.groups'){
      Konstanz_df  %>%
        tidyr::unnest(result.groups) %>%
        dplyr::select('result.id','id','image_display_url', 'name')-> groups}
    else{}
  }

  #Creating a function to merge all variables of interest via the 'id_variable'

  MyMerge <- function(x, y){
    df <- merge(x, y, by= "result.id", all.x= TRUE, all.y= TRUE)
    return(df)
  }

  #Merging the dfs into the Output 'Meta_df', containing all relevant information

  Meta_df <- purrr::reduce(list(ressources, tags, groups), MyMerge)

  #Argument: Meta

  if (!meta)  return (Meta_df %>%
                        dplyr::distinct(result.id, .keep_all = TRUE) %>%
                        dplyr::select(result.id, result.name, name.x))

  if (meta)  return (Meta_df)


}

###############Checking################################

system.time(function_return1 <-show_data(meta = FALSE))
system.time(function_return2 <-show_data(meta = TRUE))
