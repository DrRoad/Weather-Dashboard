library(plyr)
library(dplyr)

#takes in one tibble and a number representing the number of rows that should be combind together
#returns a list of 3 tibbles, one generated with the max, min and mean of the data
average_time_frames <- function(data,time_frame){
  #replace *** with NA, because NA can be factored out for the max, characters cannot
  data[data == '***'] <- NA
  colnames(data)[1] <- 'date'
  
  date_time_format <- ''
  
  if(time_frame %in% 'hour'){
    #delete everthing after the colon in the time stamp so that data can be grouped by the hour
    data[,1] <- apply(data[,1],1,function(row){
      sub(':.*',':00',row)
    })
    date_time_format <- '%m/%d/%Y %H:%M'
  }else if(time_frame %in% 'day'){
    #remove the time from the time stamp so you can group by day
    data[,1] <- apply(data[,1],1,function(row){
      sub(' \\d+:.*','',row)
    })
    date_time_format <- '%m/%d/%Y'
  }else if(time_frame %in% 'month'){
    data[,1] <- apply(data[,1],1,function(row){
      part <- sub(' \\d+:.*','',row)
      part <- as.character(part)
      sub('/\\d+/','/1/',part)
    })
    date_time_format <- '%m/%d/%Y'
  }
  
  summarised_mean <- data %>% 
    group_by(date) %>% 
    summarise_all(funs(mean(., na.rm = TRUE)))
  #convert to date and time stamp, order by that
  summarised_mean <- summarised_mean[order(strptime(summarised_mean$date, date_time_format)),]
  
  summarised_max <- data %>% 
    group_by(date) %>% 
    summarise_all(funs(max(., na.rm = TRUE)))
  summarised_max <- summarised_max[order(strptime(summarised_max$date, date_time_format)),]
  
  summarised_min <- data %>% 
    group_by(date) %>% 
    summarise_all(funs(min(., na.rm = TRUE)))
  summarised_min <- summarised_min[order(strptime(summarised_min$date, date_time_format)),]
  
  list(summarised_mean,summarised_max,summarised_min)
}
