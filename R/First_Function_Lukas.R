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

  #Saving the list as Dataframe and dropping the first level

  resp_list %>%
  chuck("result", 1) -> Konstanz_df

  #The List/Dataframe comes with a nested structure. There are three variables (result.resources,result.tags,result.groups),
  #which contain further information on the single files.
  #The loop extracts the variables of interest and assigns them to a temporal variables

  for(i in names(Konstanz_df)){
    if (i == 'resources'){
        Konstanz_df  %>%
        select('id','resources') %>%
        drop_na()  %>%
        dplyr::mutate(resources = map(resources, ~.x %>% #Source: https://stackoverflow.com/questions/56048124/how-to-change-colname-of-nested-dataframe
                            rename_at(1, ~ "id_resources"))) %>%
        tidyr::unnest(cols = resources, keep_empty = TRUE, names_repair = 'unique') %>%
        dplyr::select('id','id_resources','name','url','format','mimetype','size')-> ressources}
    else if (i == 'tags'){
      Konstanz_df  %>%
        select('id','tags') %>%
        drop_na()  %>%
        dplyr::mutate(tags = map(tags, ~.x %>%
                                        rename_at(1, ~ "id_tags"))) %>%
        tidyr::unnest(cols = tags, keep_empty = TRUE, names_repair = 'unique') %>%
        dplyr::select('id','id_tags','name')-> tags}
    else if ( i == 'groups'){
      Konstanz_df  %>%
        select('id','groups') %>%
        drop_na()  %>%
        dplyr::mutate(groups = map(groups, ~.x %>%
                                        rename_at(3, ~ "id_groups"))) %>%
        tidyr::unnest(cols = groups, keep_empty = TRUE, names_repair = 'unique') %>%
        dplyr::select('id','id_groups','title','name') -> groups}
    else{}
  }

  #Creating a function to merge all variables of interest via the 'id_variable'

  MyMerge <- function(x, y){
    df <- merge(x, y, by= "id", all.x= TRUE, all.y= TRUE)
    return(df)
  }

  #Merging the dfs into the Output 'Meta_df', containing all relevant information

  Meta_df <- purrr::reduce(list(ressources, tags, groups), MyMerge)

  #Argument: Meta

  if (!meta)  return (Meta_df %>%
                        dplyr::distinct(id, .keep_all = TRUE) %>%
                        dplyr::select(id, name.x, name.y, title) %>%
                        dplyr::arrange(desc(name.y)))

  if (meta)  return (Meta_df %>%
                       dplyr::arrange(desc(name.x)) )

}

###############Checking################################

system.time(function_return1 <-show_data(meta = FALSE))
system.time(function_return2 <-show_data(meta = TRUE))

