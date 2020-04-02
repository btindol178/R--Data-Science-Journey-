# files in directories to loop over the files in them
# C:/Users/blake/OneDrive - Western Michigan University/COVID-19/CORD-19-research-challenge (1)/biorxiv_medrxiv/biorxiv_medrxiv"
# C:\Users\blake\OneDrive - Western Michigan University\COVID-19\CORD-19-research-challenge (1)\comm_use_subset\comm_use_subset
# C:\Users\blake\OneDrive - Western Michigan University\COVID-19\CORD-19-research-challenge (1)\custom_license\custom_license
#C:\Users\blake\OneDrive - Western Michigan University\COVID-19\CORD-19-research-challenge (1)\noncomm_use_subset\noncomm_use_subset
require(jsonlite) # pakcage to extract jsonfiles to list format 

name_list = list.files()                      # vector of strings with names from file folder
final_info <- NULL                            # declaring null variable holder


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
  
  
  info$ref = temp                             # add the reference list to the info list above
  
  
  final_info[[j]] = info                      # this holds all of the info from outside the loop!
}


final_info1 <- final_info   # biorxiv medrxiv files
final_info2 <- final_info   # com_use_subset
final_info3 <- final_info   # custome liscence
final_info4 <- final_info   # noncomm_use_subset


######################################################################################################################
# THIS IS THE LOOP TO TRY TO UNPACK THE LISTS OF LISTS of list to just list of lists !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
data_dft <- NULL; # SHOULD THIS BE DATA.FRAME???
for(i in 1:length(biomed)){
  data_df <- NULL; # SHOULD THIS BE DATA.FRAME()?????
  data_df$paper_id = biomed[[i]]$paper_id
  data_df$title = biomed[[i]]$title
  data_df$text = biomed[[i]]$text
  data_df2 <- NULL; # SHOULD THIS BE DATA.FRAME()???
  for(j in 1:length(biomed[[j]]$ref)){
    data_df2$title = biomed[[j]]$ref$title
    data_df2$year = biomed[[j]]$ref$year
    data_df2$venue = biomed[[j]]$ref$venue
  }
  data_df$title = data_df2$title
  data_df$year = data_df2$year
  data_df$venue = data_df2$venue
  
  data_dft[[i]] <- data_df # here is the final dataframe
}

data_dft # final list frame


##########################################################################
# taking a data_dft (a large list of lists with 5 items in each list) and extracting them into a dataframe
final_df1 <- NULL;
for(i in 1:length(data_dft)){
  final_df1$paper_id[i] <- data_dft[[i]]$paper_id
  final_df1$title[i] <- data_dft[[i]]$title[i] # TITLE IS WRONG!!!!!!!!!!!!! IT IS THE SAME ONE OVER AND OVER
  final_df1$text[i] <- data_dft[[i]]$text
  final_df1$year[i] <- data_dft[[i]]$year
  final_df1$venue[i] <- data_dft[[i]]$venue
  }

final_df1 <- as.data.frame(final_df1) # UNLISTING A LIST OF LISTS!!!!!!
final_df2 <- final_df1
write.csv(final_df2,file="final_df2.csv") # final dataframe!!!!!
#####################################################################################################################
