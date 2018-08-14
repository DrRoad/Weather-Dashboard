library(plyr)
library(dplyr)

#takes in one tibble and a number representing the number of rows that should be combind together
#returns a list of 3 tibbles, one generated with the max, min and mean of the data
average_time_frames <- function(data,time_frame){
  #replace *** with NA, because NA can be factored out for the max, characters cannot
  data[data == '***'] <- NA
  colnames(data)[1] <- 'date'
  if(time_frame %in% 'hour'){
    #delete everthing after the colon in the time stamp so that data can be grouped by the hour
    data[,1] <- apply(data[,1],1,function(row){
      sub(':.*','',row)
    })
  }else if(time_frame %in% 'day'){
    #remove the time from the time stamp so you can group by day
    data[,1] <- apply(data[,1],1,function(row){
      sub('\\d+:.*','',row)
    })
  }else if(time_frame %in% 'month'){
    data[,1] <- apply(data[,1],1,function(row){
      part <- sub('\\d+:.*','',row)
      part <- as.character(part)
      sub('\\d+/','',part)
    })
  }
  summarised_mean <- data %>% 
    group_by(date) %>% 
    summarise_all(funs(mean(., na.rm = TRUE)))
  summarised_max <- data %>% 
    group_by(date) %>% 
    summarise_all(funs(max(., na.rm = TRUE)))
  summarised_min <- data %>% 
    group_by(date) %>% 
    summarise_all(funs(min(., na.rm = TRUE)))
  list(summarised_mean,summarised_max,summarised_min)
}
