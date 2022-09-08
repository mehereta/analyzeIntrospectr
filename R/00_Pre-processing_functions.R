
#Pre-processing Functions

#Import packages

library(tidyverse)
library(dplyr)
library(lsr)
library(rstatix)
library(emmeans)


#Function 1: Import,process, and clean files
#' Create a clean dataframe from files
#' @description imports,processes, and cleans the files in the data folder to create an initial dataframe with information about each subject
#'
#' @param filepath that contains all the data files
#'
#' @return a cleaned data frame consisting information about subjects
#'
#' @examples  clean.files('C:\\Users\\meher\\Desktop\\ED neurolab work\\data')
#' @export
clean.files <- function(filepath) {
  setwd(filepath)
  files = Sys.glob('BLC_*.csv') # grab all the BLC files
  count = 0
  # for loop that goes through all the data files in files
  full_data = data.frame()
  for (file in files) {
    #df = read.csv(file) # read each individual data file
    size = file.info(file)$size # get file size.
    if(size > 10000){ # Files less than 10kb are kind of useless so skip those
      date = file %>% substr(24,28) # get date of sesssion from file name
      if(date == '10-20'){ # first date (10-20) had slightly different backend so we process them differently
        raw_df = read_csv(file)
        raw_df = raw_df %>%
          select(mouse.time,mouse.clicked_name,pracBlock.ran,words,itemType,correctAns,participant,blockNum,blockSelecter.ran,blockNum)
        raw_df = raw_df %>%
          fill(c(words,itemType,correctAns,pracBlock.ran),.direction = "up") %>% # slide these columns up to account for weird split rows
          mutate(blockNum = case_when(pracBlock.ran==1 ~ 'practice',
                                      is.na(pracBlock.ran) ~ blockNum)) %>%  # label practice
          fill(blockNum, .direction = 'up')%>%
          mutate(repeated_word = words == lag(words)) %>% # figure out if there's a split row or not. Trials w/o a response aren't split
          mutate(repeated_word = case_when(is.na(repeated_word)~F,
                                           !is.na(repeated_word)~repeated_word)) %>%
          filter(repeated_word==F) %>% # remove split rows based on if word repeated or not
          mutate(date=date,
                 block = blockNum) %>%
          select(-blockNum)
      }
      else{ # if the date isn't from 10-20 process as such
        raw_df = read_csv(file)

        # grab only the columns we are interested in
        raw_df = raw_df %>%
          select(mouse.time,mouse.clicked_name,pracBlock.ran,words,itemType,correctAns,participant,block)
        raw_df = raw_df %>%
          fill(c(words,itemType,correctAns,pracBlock.ran,block),.direction = "up") %>% # slide these columns up to account for weird split rows
          filter(!is.na(mouse.time)) %>%
          mutate(repeated_word = words == lag(words)) %>%  # figure out if there's a split row or not. Trials w/o a response aren't split
          mutate(repeated_word = case_when(is.na(repeated_word)~F,
                                           !is.na(repeated_word)~repeated_word))%>%
          filter(repeated_word==F)%>% # remove split rows based on if word repeated or not
          mutate(date=date)
      }
      full_data = full_data %>% bind_rows(raw_df) # bind it all together
    }}
  return(full_data)
}
#-----------------------------------

#processed_dataframe = clean.files('C:\\Users\\meher\\Desktop\\ED neurolab work\\data')

#create dictionaries for the next function
group_dict <- c('10-20'= 1,'11-03'= 1,'02-08'= 1,'02-09'=1,'02-16'=1,'10-25'= 2,'11-08'=2, '02-10'= 2, '11-01' = 3,'11-16'=3, '02-18'=3)
timepoint_dict <- c('10-20'=1,'10-25'=1, '11-01'=1, '11-03'=2,'11-08'=2,'11-16'=2,'02-08'= 3,'02-10'= 3,'02-16'= 3,'02-18'= 3)

#-----------------------------------
#Function 2:  This function adds information to the full_data df about each magic word and which class learned each set of magic words
#' Add variables to dataframe
#' @description adds information to the dataframe about each magic word and which class learned each set of magic words
#'
#' @param initial data_frame
#' @param group_dictionary that has dates as keys and their corresponding group number as values
#' @param timepoint_dictionary that has dates as keys and their corresponding timepoint as values
#'
#' @return updated dataframe including the added variables
#'
#' @examples add.info(processed_dataframe,group_dict,timepoint_dict)
#' @export
add.info <- function (data_frame,group_dictionary, timepoint_dictionary){

    group_1_words = c('molar','riven','graft','acrid','spume','pecan','olden','prune','snare','inset','shunt','elfin','affix','chive','crank','waxen','prank','circa','toxin','snick')
    group_2_words = c('borax','datum','shank','anvil','sidle','totem','inapt','belle','tulle','eject','brash','octet','usurp','chafe','chump','yodel','swath','clove','covet','froth')
    group_3_words = c('carob','heron','swank','awash','tripe','serif','exalt','crepe','ladle','offal','brunt','annex','allot','binge','wrest','lapel','wrack','lathe','cedar','skulk')

    full_data = data_frame %>% mutate(block = case_when(block == 'block1.xlsx'~'block1',
                                                        block == 'block2.xlsx'~'block2',
                                                        block == 'block3.xlsx'~'block3',
                                                        TRUE ~ block)) %>%
      mutate(date = case_when(date == '0-25_' ~ '10-25',
                              date == '1-10-' ~ '10-25',
                              date == '11-10' ~ '11-08',
                              TRUE ~ date)) %>%
      filter(date %in% names(timepoint_dictionary))%>%
      mutate(timepoint = timepoint_dictionary[date],

             participant = case_when(participant=='BLC_AP'~'BLC_267',
                                     TRUE~participant),
             group = group_dictionary[date],

             magic_word_group = case_when(words %in% group_1_words==TRUE~1,
                                          words %in% group_2_words==TRUE~2,
                                          words %in% group_3_words==TRUE~3,
                                          TRUE ~ 0),
             answered_correctly = case_when(itemType == 'magic' & mouse.clicked_name == 'familliar_resp'~1,
                                            itemType == 'magic' & mouse.clicked_name == 'unfamilliar_resp'~0,
                                            itemType == 'magic' & is.na(mouse.clicked_name)~0,
                                            itemType == 'pseudo' & mouse.clicked_name == 'unfamilliar_resp'~1,
                                            itemType == 'common' & mouse.clicked_name == 'familliar_resp'~1,
                                            itemType == 'common' & mouse.clicked_name == 'unfamilliar_resp'~0,
                                            itemType == 'pseudo' & mouse.clicked_name == 'familliar_resp'~0,
                                            itemType == 'common' & is.na(mouse.clicked_name)~0,
                                            itemType == 'pseudo' & is.na(mouse.clicked_name)~0,
                                            T~12))
    full_data %>% select(mouse.time,mouse.clicked_name,words,itemType,participant,block,timepoint,group,magic_word_group,answered_correctly,date) %>% write.csv('clean_data.csv')

    return(full_data)
  }

#-----------------------------------
#final_processed_file <- add.info(processed_dataframe,group_dict,timepoint_dict)

#roxygen2::roxygenise()

