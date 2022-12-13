
# Data visualization Functions: Word level plots/tables:



# Function 8:

#' Plot percentage of response for low/high/magic words
#'
#' @description plots the percentage of response for low/high/magic words by timepoint
#' @param dataframe includes information about the participants
#' @param magicword_group group number of the magic words
#' @param word_type_list a list of word types that you want your plots based on such as 'magic','common','pseudo'
#'
#' @return plots
#'
#' @examples plot.by.frequency.timepoint(final_processed_file,1,word_type_list= list('magic','common','pseudo'))
#' @export
plot.by.frequency.timepoint <- function(dataframe,magicword_group, word_type_list= list(word1)){

  # plot Percentage of Responses by All Low Frequency Word Across Timepoints for the specified class
  print(dataframe %>%
          filter(block!='practice') %>%
          filter(itemType=='magic') %>%
          filter(!is.na(itemType)) %>%
          group_by(timepoint,words,mouse.clicked_name) %>%
          summarise(resp_count = n()) %>%
          mutate(percent_resp = resp_count/sum(resp_count))%>%
          ggplot(aes(x=words, y = percent_resp*100, fill=mouse.clicked_name))+
          geom_bar(stat="identity", width = 0.7)+
          ggtitle(paste0('Percentage of Responses by All Low Frequency Word Across Timepoints - Group ', magicword_group))+
          ylab('Percent')+
          theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))+
          coord_flip()+
          facet_wrap(.~timepoint))

  # plot Percentage of Responses for a specific magic word group by Low Frequency Word Across Timepoints
  print(dataframe %>%
          filter(block!='practice') %>%
          filter(itemType=='magic') %>%
          filter(!is.na(itemType)) %>%
          filter(magic_word_group== magicword_group) %>%
          group_by(timepoint,words,mouse.clicked_name) %>%
          summarise(resp_count = n()) %>%
          mutate(percent_resp = resp_count/sum(resp_count))%>%
          ggplot(aes(x=words, y = percent_resp*100, fill=mouse.clicked_name))+
          geom_bar(stat="identity", width = 0.7)+
          ggtitle(paste0('Percentage of Responses by Group ', magicword_group,' Low Frequency Word Across Timepoints - Group ',magicword_group))+
          ylab('Percent')+
          theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))+
          coord_flip()+
          facet_wrap(.~timepoint))

  for (word in word_type_list){
    # plot Percentage of Responses for all the given word type Across Timepoints for the specific class number
    print ( dataframe %>%
              filter(block!='practice') %>%
              filter(itemType==word) %>%
              filter(!is.na(itemType)) %>%
              group_by(timepoint,words,mouse.clicked_name) %>%
              summarise(resp_count = n()) %>%
              mutate(percent_resp = resp_count/sum(resp_count))%>%
              ggplot(aes(x=words, y = percent_resp*100, fill=mouse.clicked_name))+
              geom_bar(stat="identity", width = 0.7)+
              ggtitle(paste0('Percentage of Responses by ',word, ' Word Across Timepoints - Group ',magicword_group))+
              ylab('Percent')+
              theme(axis.text.x = element_text(angle=90, vjust=1, hjust=1))+
              coord_flip()+
              facet_wrap(.~timepoint))
  }
}
#-----------------------------------


#Function 9:

#' Create tables for specific groups
#'
#' @description creates tables for specific groups and saves them in a csv file
#'
#' @param dataframe includes information about the participants
#' @param magicgroup_number the group number of the magic word
#'
#' @return a csv file
#'
#' @examples create.tables(final_processed_file,1)
#' @export
create.tables <- function(dataframe,magicgroup_number) {

  dataframe %>%
    filter(block!='practice') %>%
    filter(itemType=='magic') %>%
    filter(!is.na(itemType)) %>%
    filter(magic_word_group==magicgroup_number) %>%
    group_by(timepoint,words,mouse.clicked_name) %>%
    summarise(resp_count = n()) %>%
    mutate(percent_resp = scales::percent(resp_count/sum(resp_count)),
           Word = words) %>%
    ungroup() %>%
    filter(mouse.clicked_name == 'familliar_resp')%>%
    select(timepoint,Word,percent_resp) %>%
    mutate(rn = row_number()) %>%
    pivot_wider(id_cols = Word, names_from = timepoint, names_prefix = 'Timepoint ', values_from = percent_resp) %>%
    write_csv(paste0('group_',magicgroup_number,'_magic_learning_three_timepoints.csv'))

  dataframe %>%
    filter(block!='practice') %>%
    filter(itemType=='common') %>%
    filter(!is.na(itemType)) %>%
    group_by(timepoint,words,mouse.clicked_name) %>%
    summarise(resp_count = n()) %>%
    mutate(percent_resp = scales::percent(resp_count/sum(resp_count)),
           Word = words) %>%
    ungroup() %>%
    filter(mouse.clicked_name == 'familliar_resp')%>%
    select(timepoint,Word,percent_resp) %>%
    mutate(rn = row_number()) %>%
    pivot_wider(id_cols = Word, names_from = timepoint, names_prefix = 'Timepoint ', values_from = percent_resp)%>%
    write_csv(paste0('group_',magicgroup_number,'_common_words_acc_three_timepoints.csv'))
}
#-----------------------------------


#Function 10:

#' Checking function
#'
#' @description this is a checking function that saves the changes in a csv file
#'
#' @param dataframe includes information about the participants
#' @param magicgroup_number the group number of the magic word
#' @param group_number the group number
#'
#' @return a csv file
#'
#' @examples checking.function(final_processed_file,2,2)
#' @export
checking.function <- function(dataframe,magicgroup_number,group_number){
  dataframe %>%
    filter(block!='practice') %>%
    filter(itemType=='magic') %>%
    filter(!is.na(itemType)) %>%
    filter(!is.na(mouse.clicked_name)) %>%
    filter(magic_word_group!= magicgroup_number) %>%
    group_by(timepoint,words,mouse.clicked_name) %>%
    summarise(resp_count = n()) %>%
    mutate(percent_resp = scales::percent(resp_count/sum(resp_count)),
           Word = words) %>%
    ungroup() %>%
    filter(mouse.clicked_name == 'familliar_resp')%>%
    select(timepoint,Word,percent_resp) %>%
    mutate(rn = row_number())%>%
    pivot_wider(id_cols = Word, names_from = timepoint, names_prefix = 'Timepoint ', values_from = percent_resp) %>%
    mutate(`Timepoint 1` = case_when(is.na(`Timepoint 1`)~'0%',
                                     T~`Timepoint 1`),
           `Timepoint 2` = case_when(is.na(`Timepoint 2`)~'0%',
                                     T~`Timepoint 2`),
           `Timepoint 3` = case_when(is.na(`Timepoint 3`)~'0%',
                                     T~`Timepoint 3`))%>%
    write_csv(paste0('group_',group_number,'_other_magic_words_three_timepoints.csv'))
}
#-----------------------------------
