### get_data ###


# load packages
library(dplyr)
library(httr)
library(purrr)
library(jsonlite)
library(magrittr)
library(tidyr)
library(stringr)

# define API endpoint



# get package list with resources

show_data <- function(tag = NULL, filter_format = NULL) {
  url <- "https://offenedaten-konstanz.de/api/3/action/current_package_list_with_resources"
  
  resp <- httr::GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- httr::content(resp, "text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE, flatten = TRUE)
  
  parsed %>%
    chuck("result", 1) -> package_list
  
  package_list %>%
    map_dfr(magrittr::extract, c("id","title"),
            .id = "datasource") %>%
    mutate(datasource = as.integer(datasource)) -> macro_data
  
  package_list %>%
    map(chuck, "resources") -> only_ressources
  
  temp_list_ressources <- vector("list", length = length(only_ressources))
  
  for (i in seq_along(only_ressources)) {
    
    only_ressources %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("url","name","format","resource_group_id"),
                     .id = "no_ressource") %>%
      dplyr::bind_cols(tibble(datasource = i)) -> temp_list_ressources[[i]]
  }
  
  dplyr::bind_rows(temp_list_ressources) -> ressource_df
  
  package_list %>%
    map(pluck, "tags") -> only_tags
  
  temp_list_tags <- vector("list", length = length(only_tags))
  
  for (i in seq_along(only_tags)) {
    
    only_tags %>%
      purrr::chuck(i) %>%
      purrr::map_dfr(magrittr::extract, c("name"),
                     .id = "no_tag") %>%
      dplyr::bind_cols(tibble(datasource = i)) -> temp_list_tags[[i]]
  }
  
  dplyr::bind_rows(temp_list_tags) -> tag_df
  
  tag_df %>%
    dplyr::mutate(no_tag = stringr::str_c("tag_no", "_",  no_tag)) %>%
    tidyr::pivot_wider(names_from = no_tag, values_from = name) -> tag_df_merge
  
  ressource_df %>%
    dplyr::left_join(macro_data) %>%
    dplyr::left_join(tag_df_merge) -> global_df
  
  
  ##Filter out datasets with their tag
  if (is.null(tag) & !is.null(filter_format)) {
    global_df %>%
      dplyr::filter(format %in% filter_format) -> filtered_format_df
    
    filtered_format_df %>%
      distinct(datasource) %>%
      nrow() -> nb_filtered_datasets
    
    message("You have used the format filter(s) ", paste(filter_format, collapse = " and "), ".")
    
    for (i in 1:length(filter_format)){
      filtered_format_df %>%
        filter(format %in% filter_format [i]) -> byformat_df
      message("There are in total ", nrow(distinct(byformat_df, datasource)), 
              " datasets in the format ", filter_format [i], ".")
    }
    return(filtered_format_df) #Only datasets corresponding to format filter(s)
    
    
    } else if (!is.null(tag) & is.null(filter_format)) {
    global_df %>%
      dplyr::filter(if_any(starts_with("tag_no"), ~. %in% tag)) -> filtered_tag_df #Checks the specified filter in all columns containing tags
    
    message("You have used the category filter(s) ", paste(tag, collapse = " and "), ".") #Indicate whether one or more filters have been specified. 
    
    for (i in 1:length(tag)){
      filtered_tag_df %>%
        filter(if_any(starts_with("tag_no"), ~. %in% tag [i])) -> bytag_df
      message("There are in total ", nrow(distinct(bytag_df,datasource)), 
              " datasets under the category ", tag[i], ".")
    }
    return(filtered_tag_df) # Returns only the datasets corresponding the tag filter(s)
    
    
    
    } else if (!is.null(tag) & !is.null(filter_format)) {
    global_df %>%
      dplyr::filter(if_any(starts_with("tag_no"), ~. %in% tag) & format %in% filter_format) -> filtered_tag_format_df #Checks the specified filter in all columns containing tags
      
    message("You have used the category filter(s) ", paste(tag, collapse = " and "), " and the format filter ", filter_format, ".") #Indicate whether one or more filters have been specified. 
    for (i in 1:length(tag)){
      filtered_tag_format_df %>%
        filter(if_any(starts_with("tag_no"), ~. %in% tag [i])) -> bytag_df
      message("There are in total ", nrow(distinct(bytag_df, datasource)), 
              " datasets under the category ", tag[i], ".")
      for (i in 1:length(filter_format)) {
        bytag_df %>%
          filter(format %in% filter_format[i]) -> bytag_byformat_df
        message("In this category, there are ", nrow(distinct(bytag_byformat_df, datasource)),
                " datasets with the format ", filter_format[i], ".")
      }
    }
    return(filtered_df) # Returns only the datasets from the filter
    

    
    
    
    } else {
    message("There are in total ", nrow(macro_data), " different datasets available.\n",
            "These datasets belong to ", nrow(distinct(tag_df, name)), " groups. These groups are:\n",
            distinct(tag_df, name))
    return(global_df) # If there is no filter, the function returns all the datasets
  }
}
  
  
  
  
  






# tryout_stuff - not relevant for function call ---------------------------
function_return <- show_data() # now the function returns two lists in which two dfs are stored


#Test whether the function returns all datasets when no filter is specified
test_nofilter <- show_data()
#Test with one filter
test_onefilter <- show_data ("Soziales")
#If I want several filters, I can store my filters in a list
tag_filters <- c("Soziales", "Umwelt und Klima")
test_multifilter <- show_data(tag_filters, filter_format = c("json", "csv"))

#If I want only datasets with csv format 
test_format <- show_data(filter_format ="csv")











tag_df %>%
  dplyr::mutate(no_tag = stringr::str_c("tag_no", "_",  no_tag))


function_return %>%
  group_by(datasource) %>%
  tally()
