########################################################################
#found an elegant way to check if packages are already installed.  If not, install them.
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
#Thanks Stack Overflow (Shane).
list.of.packages <- c("readxl", "tidyverse","hashmap","stringr","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(readxl)
library(tidyverse)
library(hashmap)
library(stringr)
library(openxlsx)

########################################################################

# Project inspired by https://github.com/sharan-naribole/H1B_visa_eda

# Since the above project only pulls data for the entire U.S. and for the years 2011-2016,
# I wanted to update this logic for a slightly different project:  Great Lakes States H1-B Visas
# 2015-2019.  


# I am going to write my own code in parts because I prefer using tidyverse + dplyr more than the
# original source data wrangling code.  But, I will note where I borrow from the original script.  


# Reading the excel files from URL.
# Step 1.  Go to https://www.dol.gov/agencies/eta/foreign-labor/performance
# Go down to LCA Programs (H-1B, H-1B1, E-3).
# Download the files you want and put into local data folder.
# For this project, I am only interested in 2015 - 2019 data.
# Put the file names in the filenames vector so we can loop over them.
# Shout Out to https://www.kaggle.com/nsharan/h-1b-visa for
# the overall instructions on how to find this data.  

#################################################################
filenames <- c('H-1B_Disclosure_Data_FY15_Q4.xlsx',
               'H-1B_Disclosure_Data_FY16.xlsx',
               'H-1B_Disclosure_Data_FY17.xlsx',
               'H-1B_Disclosure_Data_FY2018_EOY.xlsx',
               'H-1B_Disclosure_Data_FY2019.xlsx')


#define great lakes states.
#I am using this vector filter the datasets immediately after reading in the excel file.
#https://en.wikipedia.org/wiki/Great_Lakes_region

#includes:  Illinois, Indiana,Michigan,Minnesota,New York,Ohio,Pennsylvania,Wisconsin

great_lakes_states <- c('IL','ILLINOIS',
                        'IN','INDIANA',
                        'MI','MICHIGAN',
                        'MN','MINNESOTA',
                        'NY','NEWYORK',
                        'OH','OHIO',
                        'PA','PENNSYLVANIA',
                        'WI','WISCONSIN')

####################################################################

#below is a modified version of the original FOR LOOP used to read in the excel files.
#Since the file names changed, I cannot use Sharan's original code exactly.  I'll need to modify it
#slightly and also filter down to the Great Lakes States that I care about.
#####################################################################
#initialize an empty dataframe to hold all the unioned dataframes
h1b_df = data.frame()

#####################################################################
#loop over the filename vector, open each file using read_excel
#then union the dataframe to the empty h1b_df dataframe.  As each one loops,
#the dataframe will be unioned to the bottom of the existing dataframe.

#This is essentially the same as Sharan's logic except, I am opening different year files
#and filtering to only Great Lakes States.  For some reason, the FY19 file 
#uses different headers and the worksite_state column has a mix of state abbreviations 
#and full names so we will need to clean this up later.

#One additional note:  These files are very large and my poor old laptop could not 
#handle the size of these files using the basic looping logic down below.  In reality,
#I opened each Excel file and delete 4-6 columns each to cut down on size.  The FY19
#file is huge and includes many worksite columns.  I decided to manually delete all
#worksite columns except worksite_...1 and worksite.._2 columns to cut down on memory usage.
#Your computer may not have this issue so I left the code as is.


####################################################################

#Reading in the data

for (i in filenames){
  
  datapath <- paste("./data",i,sep="/")
  
  new_df <- read_excel(datapath)
  print(datapath)
  print(dim(new_df))
  print(colnames(new_df))
  
  #the 2019 file has some new header names so we need special logic for that file only.
  if (i != 'H-1B_Disclosure_Data_FY2019.xlsx'){
  
    new_df <- new_df %>%
      #just great lakes states defined above
      #make sure all rows are uppercase
      mutate(WORKSITE_STATE = str_to_upper(WORKSITE_STATE)) %>%
      #make sure all rows do not have a space
      mutate(WORKSITE_STATE = str_replace(WORKSITE_STATE,' ','')) %>%
      #filter out non-great lakes state states.  This will also remove NA rows.
      filter(WORKSITE_STATE %in% great_lakes_states) %>%
      #create a new YEAR column based on file name.
      mutate(YEAR = case_when((i == 'H-1B_Disclosure_Data_FY15_Q4.xlsx') ~ '2015',
                              (i == 'H-1B_Disclosure_Data_FY16.xlsx') ~ '2016',
                              (i == 'H-1B_Disclosure_Data_FY17.xlsx') ~ '2017',
                              (i == 'H-1B_Disclosure_Data_FY2018_EOY.xlsx') ~ '2018',
                              (i == 'H-1B_Disclosure_Data_FY2019.xlsx') ~ '2019')) %>%
      select(CASE_NUMBER,#
             CASE_STATUS,#
             EMPLOYER_NAME,#
             SOC_NAME, #SOC_TITLE
             SOC_CODE,#
             JOB_TITLE,#
             FULL_TIME_POSITION,#
             PREVAILING_WAGE,#PREVAILING_WAGE_1
             PW_UNIT_OF_PAY, #PW_UNIT_OF_PAY_1
             WORKSITE_CITY,#WORKSITE_CITY_1
             WORKSITE_STATE,#WORKSITE_STATE_1
             YEAR)
    #after cleaning
    print('After Cleaning...')
    print(dim(new_df))
    #columns:
    print(colnames(new_df))
  }
  else {
    new_df <- new_df %>%
      #just great lakes states defined above
      #make sure all rows are uppercase
      mutate(WORKSITE_STATE_1 = str_to_upper(WORKSITE_STATE_1)) %>%
      #make sure all rows do not have a space
      mutate(WORKSITE_STATE_1 = str_replace(WORKSITE_STATE_1,' ','')) %>%
      #filter out non-great lakes state states.  This will also remove NA rows.
      filter(WORKSITE_STATE_1 %in% great_lakes_states) %>%
      #create a new YEAR column based on file name.
      mutate(YEAR = case_when((i == 'H-1B_Disclosure_Data_FY15_Q4.xlsx') ~ '2015',
                              (i == 'H-1B_Disclosure_Data_FY16.xlsx') ~ '2016',
                              (i == 'H-1B_Disclosure_Data_FY17.xlsx') ~ '2017',
                              (i == 'H-1B_Disclosure_Data_FY2018_EOY.xlsx') ~ '2018',
                              (i == 'H-1B_Disclosure_Data_FY2019.xlsx') ~ '2019')) %>%
      rename(SOC_NAME = SOC_TITLE,
             PREVAILING_WAGE = PREVAILING_WAGE_1,
             PW_UNIT_OF_PAY = PW_UNIT_OF_PAY_1,
             WORKSITE_CITY = WORKSITE_CITY_1,
             WORKSITE_STATE = WORKSITE_STATE_1) %>%
      
      select(CASE_NUMBER,#
             CASE_STATUS,#
             EMPLOYER_NAME,#
             SOC_NAME,
             SOC_CODE,#
             JOB_TITLE,#
             FULL_TIME_POSITION,#
             PREVAILING_WAGE,
             PW_UNIT_OF_PAY,
             WORKSITE_CITY,
             WORKSITE_STATE,
             YEAR)
    #after cleaning
    print('After Cleaning...')
    print(dim(new_df))
    #columns:
    print(colnames(new_df))
    
    
  }
  
  #using same merge logic as the blog mentioned above.
  h1b_df = rbind(h1b_df, new_df)
  
  print(paste0("Merged data size: ",as.character(dim(h1b_df))))
}

#######################################################################
print('Done merging.  Printing final size:')
print(dim(h1b_df))
print(colnames(h1b_df))

#######################################################################
#Below, I will recycle as much of Sharan's original code as possible.
#When deviating from that, I will make a note.

#making a copy of the original dataframe that all transformations will be done on.
h1b_df_tx <- h1b_df

#######################################################################
#Since these dataframes are so large, I am also removing the last new_df from the global
#environment to help my struggling laptop.

rm(new_df)

#######################################################################

#from the original notebook:
#"As only 0.02% of the records have missing information, 
#I remove such records from further analysis. 
#For the remaining records, I convert them to the Year scale."


#In the case of 2015-2019 data, 1% of the rows have <NA> as PW_UNIT_OF_PAY
#This step still makes sense to remove the <NA> rows.  I will recycle sharan's
#function down below.



#function used to convert pw_unit_of_pay to Yearly.

pw_unit_to_yearly <- function(prevailing_wage, pw_unit_of_pay) {
  return(ifelse(pw_unit_of_pay == "Year", 
                prevailing_wage, 
                ifelse(pw_unit_of_pay == "Hour", 
                       2080*prevailing_wage, 
                       ifelse(pw_unit_of_pay== "Week", 
                              52*prevailing_wage, 
                              ifelse(pw_unit_of_pay == "Month", 
                                     12*prevailing_wage, 
                                     26*prevailing_wage)))))
}



#Next Sharan uses the above function to remove NA PW_UNIT_OF_PAY rows
#and convert to yearly rates.

h1b_df_tx <- h1b_df_tx %>%
  filter(!is.na(PW_UNIT_OF_PAY)) %>%
  mutate(PREVAILING_WAGE = as.numeric(PREVAILING_WAGE)) %>%
  mutate(PREVAILING_WAGE =  pw_unit_to_yearly(PREVAILING_WAGE, PW_UNIT_OF_PAY)) %>%
  select(- PW_UNIT_OF_PAY) 


################################################################################

h1b_df_tx %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise(count = n(),percentage = 100*count/(dim(h1b_df_tx)[1]))

#Just as Sharan found in his previous analysis, the year 2016 does not
#have binary separation of FULL_TIME_POSITION.  So, we need to 
#impute a median salary for full-time workers since removing the whole year
#of 2016 would not be ideal.


#From his analysis:  
#"Based on the 75% percentile value for Part-Time positions, 
#I select 70000 as the Prevailing Wage cut-off for Full-Time positions 
#with missing values. Accordingly, the missing values are filled."


#But, wages seem to have gone up since 2011-2016.  
#Based on Sharan's logic, in the previous analysis, the 
#75 percentile prevailing wage cut-off for full-time positions
# is now 79,934.4!  I'll go with 80,000 to keep it a nice even number. 


# h1b_df_tx %>%
#   group_by(FULL_TIME_POSITION) %>%
#   summarise('75%' = quantile(PREVAILING_WAGE,probs = 0.75,na.rm=TRUE))


h1b_df_tx <- h1b_df_tx %>% 
  mutate(FULL_TIME_POSITION = ifelse(is.na(FULL_TIME_POSITION), 
                                     ifelse(PREVAILING_WAGE > 80000,'Y','N'), 
                                     FULL_TIME_POSITION)) 


############################################################################

#(original code from Sharan's analysis)
#"Separating the state from Worksite City"

split_first <- function(word, split = " ") {
  return(strsplit(word,split= split)[[1]][1])
}



h1b_df_tx$WORKSITE_CITY <- sapply(h1b_df_tx$WORKSITE_CITY,split_first, split=",")



############################################################################

#"Mutating a Full State Name for the Abbreviated State Names"
#(original code from Sharan's analysis)
#updated to use only the worksite_states I need for the great lakes region.

#only need a hash of the great lakes states we filtered above.
state_abbs = c('IL','ILLINOIS',
  'IN','INDIANA',
  'MI','MICHIGAN',
  'MN','MINNESOTA',
  'NY','NEWYORK',
  'OH','OHIO',
  'PA','PENNSYLVANIA',
  'WI','WISCONSIN')


state_full = c("illinois","illinois",
               "indiana","indiana",
               "michigan","michigan",
               "minnesota","minnesota",
               "new york","new york",
               "ohio","ohio",
               "pennsylvania","pennsylvania",
               "wisconsin","wisconsin")

state_hash = hashmap(state_abbs,state_full)

h1b_df_tx$WORKSITE_STATE_FULL = sapply(h1b_df_tx$WORKSITE_STATE, function(x,y) {return(toupper(y[[x]]))}, y = state_hash)


####################################################

#Merging the cities and states together.


site_merge <- function(x,y) {
  return(paste0(x,", ",y))
}
h1b_df_tx %>%
  rename(WORKSITE_STATE_ABB = WORKSITE_STATE) -> h1b_df_tx


h1b_df_tx$WORKSITE = mapply(site_merge,h1b_df_tx$WORKSITE_CITY,h1b_df_tx$WORKSITE_STATE_FULL)


####################################################


h1b_df_tx$WORKSITE %>% head()


###################################################

#Implementing Sharan's probabilistic site spell checker.



sites_count <- h1b_df_tx %>% 
  group_by(WORKSITE) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 



site_hash = hashmap(sites_count$WORKSITE, sites_count$count)


###################################################


#functions written by Sharan to implement the spell checker.
#His inspiration for this:  http://norvig.com/spell-correct.html


get_inserts <- function(split_left,split_right, i, letters) {
  # Generate insertions of a single letter
  return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = split_right[i])))
}
get_deletes <- function(split_left,split_right, i) {
  # Generate deletion of one letter from word
  return(paste0(split_left[i], substr(split_right[i],2,nchar(split_right[i]))))
}
get_replaces <- function(split_left,split_right, i,letters) {
  # Generate replacement of a letter by a-z or space
  if(!is.null(split_right[i]) &  nchar(split_right[i]) > 0) {
    return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = substr(split_right[i],2,nchar(split_right[i])))))
  }
  return(NULL)
}
get_transposes <- function(split_left, split_right,i) {
  # Generate interchanging of the positions of adjacent letters
  if(!is.null(split_right[i]) & nchar(split_right[i]) > 1) {
    return(paste0(split_left[i],substr(split_right[i],2,2),substr(split_right[i],1,1),substr(split_right[i],3,nchar(split_right[i]))))
  }
  return(NULL)
}
edits1site <- function(site) {
  # All edits that are one edit away from site
  letters = toupper(strsplit("abcdefghijklmnopqrstuvwxyz ",split='')[[1]])
  site_len <- nchar(site)
  #print(site_len)
  if(site_len < 4) {
    return(site)
  }
  split_left <- sapply(seq(0,site_len), substr,x = site,start = 1)
  split_right <- sapply(seq(1,site_len+1), substr,x = site,stop = site_len) 
  deletes <- sapply(seq(1,site_len+1),get_deletes, split_left = split_left, split_right = split_right)
  transposes <- unlist(sapply(seq(1,site_len+1),get_transposes, split_left = split_left, split_right = split_right))
  replaces <- unlist(sapply(seq(1,site_len+1),get_replaces, split_left = split_left, split_right = split_right, letters=letters))
  inserts <- unlist(sapply(seq(1,site_len+1),get_inserts, split_left = split_left, split_right = split_right,letters = letters))
  
  return(unique(c(deletes,transposes,replaces,inserts)))
}
edits2site <- function(site) { 
  # All edits that are two edits away from `word`
  edits1_sites = edits1site(site)
  return (unlist(sapply(edits1_sites, edits1site)))
}
get_prob <- function(site, site_hash) {
  # probability of site in our dataset
  return(site_hash[[site]])
}
known <- function(sites,site_hash = site_hash) {
  # The subset of candidate sites that appear in the dictionary of sites
  return(sites[site_hash$has_keys(sites)])
}
find_candidates <- function(site,...) {
  # Generate possible spelling corrections for word
  return(c(known(site,...), known(edits1site(site),...), c(site)))
}
site_spell_correcter <- function(site,...) {
  # best possible correction to the site
  candidates = find_candidates(site,...)
  best_candi = candidates[which.max(sapply(candidates,get_prob, ...))]
  
  #if(get_prob(best_candi,...) > get_prob(site,...) ) {
  #  return(best_candi)
  #}
  return(best_candi)
}
site_count <- function(site, site_hash) {
  
  if(site_hash$has_key(site)) {
    return(site_hash[[site]])
  }
  return(site)
}


######################################################


sites <- sites_count$WORKSITE
sites_before <- c()
sites_after <- c()
count <- 0
for(site in sites) {
  # Count of current Worksite
  curr_count <- site_count(site,site_hash)
  #print(paste0(site, ", ",curr_count))
  
  if(curr_count < 100) { # Threshold
    #print(paste0(site, ", ",curr_count))
    corrected <- site_spell_correcter(site,site_hash)
    
    if(corrected != site) { # Correction occurred
      count <- count + 1
      sites_before[count] <- site
      sites_after[count] <- corrected
      corrected_count <- site_count(corrected,site_hash)
      #print(paste0(site, " : ", curr_count,", ",corrected, " : ", corrected_count))
    }
  }  
}


sites_corrected_hash <- hashmap(sites_before,sites_after)


print(paste0("Number of worksite spelling corrections: ", length(sites_after)))

###################################################

#Merging the corrected spellings into the dataframe.
#Still, this is using Sharan's original code.  It still works!

worksite_correct <- function(x, hash) {
  if(hash$has_key(x)) {
    return(hash[[x]])
  }
  return(x)
}


h1b_df_tx$WORKSITE_CORRECTED <- sapply(h1b_df_tx$WORKSITE,worksite_correct,hash=sites_corrected_hash)


###################################################

#merging the corrected worksite spellings.

h1b_df_tx <- h1b_df_tx %>%
  select(-WORKSITE) %>%
  rename(WORKSITE = WORKSITE_CORRECTED)

###################################################

#Next, Sharan uses ggmap to geocode all the worksites.  But, I plan to load
#does a pretty good job locating city,state location names.

#He also adds in cost of living index.  I'm not really interested in that data point for 
#my dashboard.

##################################################

#write final dataframe to excel file for the Tableau dashboard.
openxlsx::write.xlsx(h1b_df_tx, file = "./data/h1b_kaggle_2019.xlsx")



