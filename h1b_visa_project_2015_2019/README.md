
# H1B Visa Project

## Project Overview:

- **Tableau Dashboard 1** 
  - Built first Tableau dashboard using pre-wrangled data from [Kaggle](www.kaggle.com).  [Data Source](https://www.kaggle.com/nsharan/h-1b-visa).  Inspired by Sharan Naribole's initial data submission.
- **Tableau Dashboard 2**
  - *Data Collection*:  Updated the Kaggle dataset to new time period.  The original dataset was old (2011-2015).  I wrote my own R code to perform the same data collection for the time period 2015-2019.  This involved heavily updating the original R code from kaggle in order to wrangle the new time period.  [Data Source](https://www.dol.gov/agencies/eta/foreign-labor/performance)
  - *Data cleaning*:  Collected the data, joined multiple data files, filtered and engineered features using dplyr syntax.  Tidying the dataset included probabilistic correction of location data.
  - New dashboard design on Tableau Public for the new dataset.  
  
 
## Tools Used In This Project
##### **R Version:** 3.5.3
##### **R Packages:** readxl, tidyverse, hashmap, stringr, openxlsx
##### **Excel**
##### **Inspiration and first dashboard dataset:** [Data Source](https://www.kaggle.com/nsharan/h-1b-visa)
##### **Tableau** 2020.1.4
 

## Data Cleaning [Link to R File:](https://github.com/abrambeyer/Tableau_Projects/tree/main/h1b_visa_project_2015_2019/R)
- Read in the Disclosure data files from www.dol.gov site.
- Filter down to only Great Lakes States data
- Converting the prevailing wage column to yearly
- Cleaning and spell-checking worksite cities and worksite states.
- Export to Excel to use with Tableau

 
 
 
