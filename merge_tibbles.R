#takes in a file and cleans the files names so there an no non-alphanumeric characters, removes spaces too
clean_file_names <- function(file){
  has_spaces <- gsub("[^[:alnum:] ]", "", colnames(file))
  word <- gsub(' ','',has_spaces)
  word <- gsub('[[:space:]]','',word)
  word <- gsub('\\f','',word)
  word <- gsub('\\v','',word)
  word <- gsub('\\r','',word)
  word <- gsub('\\n','',word)
  word <- gsub('\\t','',word)
  word <- gsub('\\s','',word)
  colnames(file) <- gsub('\\s','',word)
  file
}

#takes a list of tibbles
#cleans the column names by removing all spaces and non-alpha-numerics
#removes all ***
#returns list of tibbles
sanitize_files <- function(tibbles){
  for(i in seq(1,length(tibbles))){
    tibbles[[i]] <- clean_file_names(tibbles[[i]])
  }
  #replace all of the ***
  for(i in seq(1,length(tibbles))){
    tibbles[[i]][tibbles[[i]] == '***'] <- NA
  }
  return(tibbles)
}


#takes a list of tibbles and combinds all common rows and columns to a matrix, 
#returns matrix and the associated column names and dates
merge_tibbles <- function(tibbles){
  tibbles <- sanitize_files(tibbles)
  
  names <- colnames(tibbles[[1]])
  for(i in seq(2,length(tibbles))){
    names <- intersect(names,colnames(tibbles[[i]]))
  }
  time_stamps <- as.character(unlist(tibbles[[1]][,1]))
  for(i in seq(2,length(tibbles))){
    time_stamps <- intersect(time_stamps,as.character(unlist(tibbles[[i]][,1])))
  }

  #subset all of the matrixes
  for(i in seq(1,length(tibbles))){
    #filter so that only the common dates remain
    tibbles[[i]] <- data.frame(tibbles[[i]][,-1], row.names=unlist(tibbles[[i]][,1]))
    #because the command before this adds X before every column name for no reason known to me
    colnames(tibbles[[i]]) <- gsub('^X','',colnames(tibbles[[i]]))
    tibbles[[i]] <- tibbles[[i]][time_stamps,]
    #filter so only the common columns remain
    colnames(tibbles[[i]]) <- as.character(unlist(colnames(tibbles[[i]])))
    tibbles[[i]] <- tibbles[[i]][,names]
  }

  #add all the tibbles together now they are they same exact dimensions
  final_tibble <- tibbles[[1]]
  final_tibble <- as.matrix(sapply(final_tibble, as.numeric))
  num_rows <- nrow(final_tibble)
  num_cols <- ncol(final_tibble)
  for(i in seq(2,length(tibbles))){
    tibbles[[i]] <- as.matrix(sapply(tibbles[[i]], as.numeric))  
    final_tibble <- as.numeric(final_tibble) + as.numeric(tibbles[[i]])
    final_tibble <- as.matrix(sapply(final_tibble, as.numeric))  
  }
  #divide by the number of tibbles to get the average
  final_tibble <- final_tibble / length(tibbles)
  final <- matrix(final_tibble,nrow = num_rows,ncol = num_cols)
  return(list(final,names,time_stamps))
}

#takes a list of tibbles
#returns a single tibble averageing all common data between the two tibbles together
#assumes that the first column contains the date and time info
combind_tibbles <- function(tibbles){
  final_pieces <- merge_tibbles(tibbles)
  new_tibble <- as.tibble(final_pieces[[1]])
  colnames(new_tibble) <- final_pieces[[2]]
  new_tibble$date <- final_pieces[[3]]
  #move the last column to the first position
  final <- new_tibble %>%
    select(date, everything())
  final
}

#here is an example call for testing
thing <- combind_tibbles(list(read_csv('Data/clean_loggers/06-00166.csv'),read_csv('Data/clean_loggers/06-00168.csv'),read_csv('Data/clean_loggers/06-00170.csv')))

