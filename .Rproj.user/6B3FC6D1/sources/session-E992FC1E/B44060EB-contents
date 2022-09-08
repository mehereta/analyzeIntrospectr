
#Data visualization Functions: Individual level plots/tables



#Function 3: This function generates accuracy data for each individual and saves them in either or both wide and long format

#' Save in wide, Long, or both csv format
#'
#' @description generates accuracy data for each individual and saves them in either or both wide and long format
#'
#' @param format of the csv file you want to save as (either 'wide','long', or 'both')
#' @param dataframe to be saved
#'
#' @return saved csv file
#' @examples wide.or.long('both', final_processed_file)
#' @export
wide.or.long <- function(format, dataframe){

  if(format == 'long'| format == 'both'){
    # save long format accuracy
    blc_id_acc_long = dataframe %>%
      filter(block!='practice') %>%
      mutate(mouse.clicked_name=as.character(mouse.clicked_name)) %>%
      filter(!is.na(itemType)) %>%
      filter(itemType=='magic') %>%
      filter(group==magic_word_group) %>%
      mutate(timepoint=as.factor(timepoint),
             mouse.clicked_name = case_when(is.na(mouse.clicked_name)~'unfamilliar_resp',
                                            T~mouse.clicked_name)) %>%
      group_by(timepoint,participant) %>%
      summarise(count = n(),
                total_correct = sum(answered_correctly),
                accuracy = total_correct/count) %>%
      select(participant,timepoint,accuracy)
    blc_id_acc_long %>% write.csv('blc_id_acc_long.csv')}

  if (format== 'wide' | format == 'both'){
    # save wide format accuracy
    blc_id_acc_wide = dataframe %>%
      filter(block!='practice') %>%
      mutate(mouse.clicked_name=as.character(mouse.clicked_name)) %>%
      filter(!is.na(itemType)) %>%
      filter(itemType=='magic') %>%
      filter(group==magic_word_group) %>%
      mutate(timepoint=as.factor(timepoint),
             mouse.clicked_name = case_when(is.na(mouse.clicked_name)~'unfamilliar_resp',
                                            T~mouse.clicked_name)) %>%
      group_by(timepoint,participant) %>%
      summarise(count = n(),
                total_correct = sum(answered_correctly),
                accuracy = total_correct/count) %>%
      select(participant,timepoint,accuracy) %>%
      pivot_wider(id_cols = participant,names_from = timepoint, names_prefix = "timepoint_", values_from = accuracy)
    blc_id_acc_wide %>%  write.csv('blc_id_acc_wide.csv')}

}
#-----------------------------------

#wide.or.long('both', final_processed_file)



#Function 4: This function generates a list of each participant by time point

#' List participants by timepoint
#'
#' @description generates a list of each participant by time point
#'
#' @param timepointcount is till what time point you want to list the participants by
#' @param dataframe containing information about participants
#'
#' @return saved csv file
#'
#' @examples list.by.timepoint(3,final_processed_file)
#' @export
list.by.timepoint <- function(timepointcount,dataframe){

  for (i in 1:timepointcount){
    length(unique((dataframe %>% filter(timepoint== i))$participant))
    unique((dataframe %>% filter(timepoint==i)) %>% select(participant)) %>% write_csv(paste0('timepoint_',toString(i),'_participants.csv'))
  }

  if (timepointcount > 1){
    for (i in 2:timepointcount){
      unique((dataframe %>%
                select(participant, timepoint) %>%
                unique() %>%
                group_by(participant) %>%
                mutate(nobs = length(unique(timepoint))) %>%
                filter(nobs==i)) %>% select(participant)) %>% write_csv(paste0(toString(i),'_timepoints_participants.csv'))
    }}
}

#-----------------------------------
#list.by.timepoint(3,final_processed_file)



#Function 5:This function generates a data frame for comparing reaction times across time point by individual and word

#' Compare participant reaction times
#'
#' @description  generates a data frame for comparing reaction times across time point by individual and word
#' @param dataframe that includes information about the participants
#' @param save_file_as the name of the file we want to save as
#'
#' @return a saved csv file
#'
#' @examples compare.reaction.time(final_processed_file,save_file_as='rt_changes.csv')
#' @export
compare.reaction.time <- function(dataframe,save_file_as='rt_changes.csv'){

  dataframe %>% select(participant,group,words,timepoint,
                       mouse.time,mouse.clicked_name,itemType,magic_word_group) %>%
    filter(timepoint!=3) %>%
    mutate(timepoint=as.character(timepoint),
           row_n = row_number()) %>%
    pivot_wider(id_cols=(c('participant','group','words','itemType','magic_word_group')),
                names_from = timepoint,
                values_from = c('mouse.time','mouse.clicked_name')) %>%
    unnest(cols='mouse.time_1') %>%
    unnest(cols='mouse.time_2') %>%
    unnest(cols='mouse.clicked_name_1')%>%
    unnest(cols='mouse.clicked_name_2') %>%
    mutate(rt_change = mouse.time_2-mouse.time_1,
           learned = group==magic_word_group)%>%
    write.csv(save_file_as)
  unique(dataframe$participant)
}
#-----------------------------------

#compare.reaction.time(final_processed_file,save_file_as='rt_changes.csv')


#Function 6: this function first plots the percentages of Responses by Word Type Across Timepoints for a specified class and then plots one for all classes.

#' Plot percentages by class and timepoint
#'
#' @description first plots the percentages of Responses by Word Type Across Timepoints for a specified class, and then plots one for all classes
#'
#' @param dataframe that includes information about the participants
#' @param group_number that is used to create the plots
#' @param magicword_group the group number of the magic words
#'
#' @return two plots
#'
#' @examples plot.class.time_point.percentages(final_processed_file,1,1)
#' @export
plot.class.time_point.percentages <- function(dataframe,group_number,magicword_group){
  #Plots percentage of Responses by Word Type Across Timepoints
  print(dataframe %>%
          filter(block!='practice') %>%
          filter(!is.na(itemType)) %>%
          filter(itemType=='magic') %>%
          filter(group==group_number) %>%
          filter(magic_word_group==magicword_group) %>%
          mutate(timepoint=as.factor(timepoint)) %>%
          group_by(timepoint,group,mouse.clicked_name) %>%
          summarise(resp_count = n()) %>%
          mutate(percent_resp = resp_count/sum(resp_count))%>%
          ggplot(aes(x=timepoint, y = percent_resp*100, fill=mouse.clicked_name))+
          geom_bar(stat="identity", width = 0.7)+
          ggtitle('Percentage of Responses by Word Type Across Timepoints')+
          ylab('Percent')+
          facet_wrap(.~group))

  #Plots percentage of Responses by Word Type Across Timepoints by Class
  print(dataframe %>%
          filter(block!='practice') %>%
          filter(!is.na(itemType)) %>%
          filter(itemType=='magic') %>%
          filter(group==magic_word_group) %>%
          mutate(timepoint=as.factor(timepoint)) %>%
          group_by(timepoint,group,mouse.clicked_name) %>%
          summarise(resp_count = n()) %>%
          mutate(percent_resp = resp_count/sum(resp_count))%>%
          ggplot(aes(x=timepoint, y = percent_resp*100, fill=mouse.clicked_name))+
          geom_bar(stat="identity", width = 0.7)+
          ggtitle('Percentage of Responses by Word Type Across Timepoints by Class')+
          ylab('Percent')+
          facet_wrap(.~group))
}
#-----------------------------------

#plot.class.time_point.percentages(final_processed_file,1,1)



#Function 7: this function finds the accuracy by word type (flags for class and time point) and also plots give the accuracy for each individual.

#' Plot accuracy charts
#'
#' @description this function finds the accuracy by word type (flags for class and time point) and also plots give the accuracy for each individual.
#'
#' @param dataframe includes information about the participants
#' @param group_number used to create the plots
#' @param magicword_group group number of the magic words
#' @param timepoint_count till what timepoint you want to list the participants by
#'
#' @return plots
#'
#' @examples plot.accuracy(final_processed_file,1,1,2)
#' @export
plot.accuracy <- function(dataframe,group_number,magicword_group,timepoint_count){

  ## Builds dataframe for a specific group
  baseline_df = dataframe  %>%
    filter(group==group_number) %>%
    filter(!is.na(words)) %>%
    select(mouse.clicked_name,mouse.time,words,itemType,participant,date,block,answered_correctly,timepoint,magic_word_group)

  # plot Percentage of Responses by Word Type Across Timepoints
  print(baseline_df %>%
          filter(block!='practice') %>%
          filter(!is.na(itemType)) %>%
          group_by(timepoint,itemType,mouse.clicked_name) %>%
          summarise(resp_count = n()) %>%
          mutate(percent_resp = resp_count/sum(resp_count))%>%
          ggplot(aes(x=itemType, y = percent_resp*100, fill=mouse.clicked_name))+
          geom_bar(stat="identity", width = 0.7)+
          ggtitle('Percentage of Responses by Word Type Across Timepoints')+
          ylab('Percent')+
          facet_wrap(.~timepoint))

  # plot Mean Accuracy by Word Type
  print( baseline_df %>%
           filter(block!='practice') %>%
           filter(!(itemType=='magic' & magic_word_group !=magicword_group)) %>%  #parameter for magic group
           group_by(itemType,participant,timepoint) %>%
           summarise(count = n(),
                     total_correct = sum(answered_correctly),
                     accuracy = total_correct/count) %>%
           group_by(itemType,timepoint) %>%
           summarise(count=n(),
                     mean_accuracy = mean(accuracy, na.rm=T),
                     sd_accuracy = sd(accuracy, na.rm = T),
                     se_accuracy = sd_accuracy/sqrt(count)) %>%
           ggplot(aes(x = itemType, y = mean_accuracy,fill=itemType))+
           geom_col()+
           geom_errorbar(aes(ymin=mean_accuracy-se_accuracy,ymax=mean_accuracy+se_accuracy),width = 0.25)+
           ggtitle('Mean Accuracy by Word Type')+
           facet_wrap(.~timepoint))

  # plot Accuracy Counts by Individual till a specified timepoint count

  for (i in 1:timepoint_count){
    print(baseline_df %>%
            filter(block!='practice') %>%
            filter(timepoint==i) %>%
            filter(itemType!='magic') %>%
            group_by(itemType,participant) %>%
            summarise(count = n(),
                      total_correct = sum(answered_correctly),
                      accuracy = total_correct/count) %>%
            group_by(itemType,participant) %>%
            summarise(count=n(),
                      mean_accuracy = mean(accuracy, na.rm=T),
                      sd_accuracy = sd(accuracy, na.rm = T),
                      se_accuracy = sd_accuracy/sqrt(count)) %>%
            ggplot(aes(x = itemType, y = mean_accuracy,fill=itemType))+
            geom_col()+
            facet_wrap(.~participant))}
}
#-----------------------------------
#plot.accuracy(final_processed_file,1,1,2)

