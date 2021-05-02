## Here is a script to test our functions


##Checking the parameters work
#Show_data function without any filter
test_nofilter <- show_data()
#Show_Data function without any filter and without any messages
test_nofilter_nomessage <- show_data(message = FALSE)


#Show_data function with one tag filter
test_onefilter <- show_data (tag ="Soziales", message = TRUE)

#Show_data function with several filters
tag_filters <- c("Transport und Verkehr")
test_multifilter <- show_data(tag = tag_filters, format = c("csv"), message = TRUE)

#Show_data function with two format filters 
test_format <- show_data(format =c("csv", "json"))


##Checking the error messages
#Error in one format filter
test_error <- show_data(format = c("cs", "json"))


#Error in tag filters
tag_filters = c("Haushalt und Steuern", "Transport", "Soziales")
test_error_tags <- show_data(tag = tag_filters, format = "csv")
