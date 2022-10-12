# analyzeIntrospectr

## Installation

You can install the development version of tractr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mehereta/analyzeIntrospectr")
```

This package contains various functions that help to first import, clean, and add variables to a dataset, and then plot
and save specific, updated data from the full dataset to csv files. It contains functions that can generate individual-level plots/tables and word 
level plots/tables from a dataset that contains information about the subjects in a specific group, the words they learned, the type 
of word (magic,pseudo, or common) and the accuracy of their responses based on the words they learned. It also consists of functions
that carry out statistical tests (t- tests or anova) to show the significance of the results in the experiment.

The package consists of functions that are divided into 4 categories based on functionality:  

1) Preprocessing Functions: these functions can initially be used to import data files and organize them into a cleaned dataframe with the option of adding information and variables for subjects. The two functions in this category are the clean.files function and the add.info function. A detailed description of what each function does and the parameters passed in is given below:

  clean.files (filepath): 
  This is the first function that should be used to create and save a clean dataset from a given filepath containing all data files. It imports,processes,   and selects csv files in the data folder to create an initial dataframe with experimental information about each subject. 
  
  
  add.info (data_frame,group_dictionary, timepoint_dictionary):
  Given a 1) dataframe, 2) a dictionary including the relationship between the experiment dates and the corresponding group number, and 3) another
  dictionary that has the dates as keys and their assigned timepoints as values, this function adds variables and information for each participant to the
  dataframe. It specifically adds information about the timepoint, group number, magic word group, and whether each participant answered correctly or not. 
  
2) Individual level plots and tables for data visualization: Once we have a clean dataframe that includes  all the relevant information, the functions in this category are used to create plots and tables based on subjects. A detailed description of what each function does and the parameters passed in is given below:

  wide.or.long (format, dataframe):
  This function takes in a dataframe and the specific format that the user wants the dataframe saved as and generates accuracy data for each individual
  then saves them in either or both wide and long format. 
  
  list.by.timepoint(timepointcount,dataframe):
  This function takes in a timepoint count which indicates up-to which timepoint you want to group participants into and save the groupings as a list 
  in a csv file.
  
  compare.reaction.time(dataframe,save_file_as='rt_changes.csv'):
  This function generates a data frame for comparing reaction times across timepoints by individuals and words and saves the dataframe in a specified csv
  file.
   
  plot.class.time_point.percentages(dataframe,group_number,magicword_group):
  This function first plots the percentages of Responses by Word Type Across timepoints for a specified group number, and then plots one for all classes.
  
  plot.accuracy(dataframe,group_number,magicword_group,timepoint_count:
  This function finds and plots the accuracy by word type once the group number, magic word group, and timepoint count have been specified as arguments. 


3) Word level plots and tables for data visualization: These group of functions create word-level plots and tables from a given dataframe. 

  plot.by.frequency.timepoint(dataframe,magicword_group, word_type_list= list(word1):
  this function takes in a dataframe, magic word group and a word_type_list - which is a list of all the word types you want to include in the plots - and
  then plots the percentage of response for low/high/magic words by timepoint.
  
  create.tables(dataframe,magicgroup_number):
  This function creates tables for a specific group and saves them in a csv file
  
  checking.function(dataframe,magicgroup_number,group_number):
  This is a checking function that saves the changes in a csv file
  
  
4) Statistical tests: These group of functions carry out statistical tests based on the number of timepoints, and the type of data (paired or not). In addition, they provide the option of plotting the results. 

  statistical.test(dataframe,data_quality_type, number_of_timepoints, group_number, magicgroup_number,plot=TRUE/FALSE):
  carries out a t-test for data including 2 or less than 2 timepoints and an anova test if the number of timepoints is greater than 2. The user must also
  provide information on whether the data is 'paired' or 'not' and if a plot is needed 'TRUE/FALSE' to assess magic word learning and . There is also an
  option to plot the results.



