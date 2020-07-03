# files in directories to loop over the files in them
# C:/Users/blake/OneDrive - Western Michigan University/COVID-19/CORD-19-research-challenge (1)/biorxiv_medrxiv/biorxiv_medrxiv"
# C:\Users\blake\OneDrive - Western Michigan University\COVID-19\CORD-19-research-challenge (1)\comm_use_subset\comm_use_subset
# C:\Users\blake\OneDrive - Western Michigan University\COVID-19\CORD-19-research-challenge (1)\custom_license\custom_license
#C:\Users\blake\OneDrive - Western Michigan University\COVID-19\CORD-19-research-challenge (1)\noncomm_use_subset\noncomm_use_subset
require(jsonlite) # pakcage to extract jsonfiles to list format 

name_list = list.files()                      # vector of strings with names from file folder
final_infof <- NULL                            # declaring null variable holder


for (j in 1:length(name_list))               # for 1 through length of file names
{
  df <- name_list[j]                          # assign each value to df
  x  <- fromJSON(df)                          # use the name to convert the file to list format
  info <- NULL                                # declare a value to hold each element in converted list
  info$paper_id = x$paper_id                  # make a element in list paperid from jsonflile paper id section
  info$title = x$metadata$title               # grab the title 
  info$text = x$body_text$text                # grab the text
  temp = NULL                                 # create null value to hold new loop value to extract bibliographies
  for (i in 1:length(x$bib_entries))          # for 1 through length of list of bibliographies
  {
    temp$title[i] = x$bib_entries[[i]]$title  # use temp to make list element title  that holds bib entries i
    if(!is.null(x$bib_entries[[i]]$year))     # some values of year are not there  so if it is not null 
    {
      temp$year[i] = x$bib_entries[[i]]$year  # store it like bib entries
    }
    else                                      # else
    {
      temp$year[i] = NA                       # call na
    }
    temp$venue[i] = x$bib_entries[[i]]$venue  # grab venue
  }
  
  
  info$ref = temp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       # add the reference list to the info list above
  
  
  final_infof[[j]] = info                      # this holds all of the info from outside the loop!
}


final_info_fin <- final_infof   # biorxiv medrxiv files


biomed <- final_info_fin


######################################################################################################################
covid <- lapply(biomed, grep, pattern="covid", value=TRUE)
covid2 <- lapply(biomed, grep, pattern="covid")
covid3 <- sapply("covid",grep, biomed)

# This kind of works
df <- NULL;
pattern <-"covid";
for(i in 1:length(biomed)){
   df$index <- lapply(biomed, grep, pattern = "covid")
   df$text <- lapply(biomed, grep, pattern="covid", value=TRUE)
   }
df

###########################################################
# This kind of works 
df_fin <- NULL;
pattern <-"covid";
for(i in 1:length(biomed)){
    df_fin$index <- sapply("covid",grep, biomed)
    df_fin$text <- sapply("covid",grep, biomed, value = TRUE)
}

df_fin

##################################################
# This finds the article number index then extracts covid
df <- NULL;
df$index <- unlist(lapply(biomed, function(ch) grep("covid", ch)))
df$text <-unlist(lapply(biomed, function(ch) grep("covid", ch, value = TRUE)))
df <- cbind(df$index,df$text)

