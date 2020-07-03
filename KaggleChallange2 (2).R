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
  
  
  info$ref = temp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       # add the reference list to the info list above
  
  
  final_info[[j]] = info                      # this holds all of the info from outside the loop!
}


final_info1 <- final_info   # biorxiv medrxiv files
final_info2 <- final_info   # com_use_subset
final_info3 <- final_info   # custome liscence
final_info4 <- final_info   # noncomm_use_subset

biomed <- final_info1


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
data_fff <- data_dft # save for good keeping

##########################################################################
# taking a data_dft (a large list of lists with 5 items in each list) and extracting them into a dataframe
data_dft1 <- data_dft
final_dff <- NULL;
for(i in 1:length(data_dft1)){
  final_dff$paper_id[i] <- data_dft1[[i]]$paper_id
  final_dff$title[i] <- data_dft1[[i]]$title[i] # TITLE IS WRONG!!!!!!!!!!!!! IT IS THE SAME ONE OVER AND OVER
  final_dff$text[i] <- data_dft1[[i]]$text[i]
  final_dff$year[i] <- data_dft1[[i]]$year[i]
  final_dff$venue[i] <- data_dft1[[i]]$venue[i]
}

final_df1.0 <- as.data.frame(final_dff) # UNLISTING A LIST OF LISTS!!!!!!
final_df2.0 <- final_df1.0
write.csv(final_df2,file="final_df2.csv") # final dataframe!!!!!
##################################################################################
# Get the second article 
article2 <- final_info1[[2]]$text 

# Parse by period or questions (need to figure out conditions)
unlistart3 <- unlist(strsplit(article2, "[.|?]\\s")) # FINAL SPERATERATOR 

# find a word in the document and pull out the sentence
n1 <- which(grepl('vulnerability', unlistart3) == T) # index from boolean (why useful?) this is same as GREP
n2 <- gregexpr('vulnerability',unlistart3) # list index -1 means no match
n3 <- regexec('vulnerability',unlistart3) # list index -1 means no match
n4 <- grep('vulnerability',unlistart3)  # index THIS IS WHAT WE WANT TO USE
n5 <- grepl('vulnerability',unlistart3) # boolean
# This is the one we want to use below!!!!!!!!!!!!!!!!!!!!!
nf <- grep('vulnerability',unlistart3)  # index THIS IS WHAT WE WANT TO USE


# Grabbing all of the sentences that have n1 in it
unlistart3[(nf)]  # This grabs all of the sentences based on target word


# Grabbing first sentence to left and first sentence to right
unlistart3[(n1-1):(n1+1)]# problem here lies in that it only doesthe first match not all
#########################################################
# to fix that we grab the first and 
index0 <- unlistart3[(nf)]
index1<- unlistart3[(nf-1)]  # successfully gives you all matches and then the sentence before
index2<- unlistart3[(nf+1)] # successfully gives you all matches and the sentences after

index3 <- c(index1,index2) # this concatenates them together 

# This getes all of the indexes for vulnerability
d <- c("vulnerability")
for(i in 1:length(d)){
  print(grep(paste(d),unlistart3)) # This actually works
  
  }

# this is all index for hubei
d <- c("hubei")
for(i in 1:length(d)){
  print(grep(get("i"),unlistart3))
 }

# Trying to get all indexes for both
# this is all index for hubei
# gets all of indexes but they are wrong..........
# FIGURE OUT HOW TO FIX THIS
d <- c("vulnerability","hubei")
for(i in 1:length(d)){
  print(grep(get("i"),unlistart3))
}

# This works but warning message at the end! and 
# This is correct !!!
d <- c("vulnerability","hubei","Nanchang")
for(i in 1:length(d)){
 matches <- unique(grep(paste(d, collapse = "|"),unlistart3,value = TRUE))
}
matches

# Now lets Turn this into a function!!!!!!!!
include <- function (theList, toMatch){
  matches <- unique(grep(paste(toMatch,collapse="|"), 
                          theList, value=TRUE))
    return(matches)
}
df <- include(unlistart3,d) # using list unlistart3 i use values in d 


# NOW MAKE A LOOP THAT GETS THE SENTENCES BEFORE AND AFTER FINDING MULTIPLE TARGET WORDS
# PSEUDO CODE FOR TAKING THE SENTENCE MATCHES AND THEN GETTING ALL
# SENTENCES TO RIGHT AND LEFT OFF ALL OF ALL OF THE MATCHES
for(i in 1:length(df)){
  df <- c(df[df(i-1),df[i],df[i+1]
}

