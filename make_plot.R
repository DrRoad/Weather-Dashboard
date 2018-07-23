#tibbles is suppose to be a list of tibbles
make_plot <- function(tibbles,x_col,y_col,names,line = FALSE, point = TRUE, title = NULL, x_lab = NULL, y_lab = NULL, range = NULL){
  colors <- c('red','blue','green','orange','purple')
  
  if(is.null(range) || (range[1] == 0 && range[2] == 0) ){
    start <- 1
    end <- nrow(tibbles[[1]])
  }else{
    start <- range[1]
    end <- range[2]
  }
  print('----')
  print(start)
  print(end)
  plot <- ggplot()
  
  X <- c()
  Y <- c()
  groups <- c()
  
  if(length(tibbles) > 0){
    for(i in seq(1,length(tibbles))){
      if(colnames(tibbles[[i]])[colnames(tibbles[[i]]) == x_col] == 'date'){
        tibbles[[i]]$X <- seq(1,nrow(tibbles[[i]]))
      }else{
        colnames(tibbles[[i]])[colnames(tibbles[[i]]) == x_col] <- 'X' 
      }
      
      if(colnames(tibbles[[i]])[colnames(tibbles[[i]]) == y_col] == 'date'){
        tibbles[[i]]$X <- seq(1,nrow(tibbles[[i]]))
      }else{
        colnames(tibbles[[i]])[colnames(tibbles[[i]]) == y_col] <- 'Y' 
      }
      #merge the relevant data together
      X <- c(X,as.numeric(unlist(tibbles[[i]]$X)))
      Y <- c(Y,as.numeric(unlist(tibbles[[i]]$Y)))
      groups <- c(groups,replicate(nrow(tibbles[[i]]),names[i]))
    }
    
    df <- tibble(X,Y,groups)
    
    plot <- ggplot(data = df, aes(x=X,y=Y,colour=groups))
    
    if(line){
      plot <- plot + geom_line()
    }
    if(point){
      plot <- plot + geom_point()
    }
    
    print(names[1:length(tibbles)])
    print(colors[1:length(tibbles)])
    plot <- plot + 
      labs(title = title, x = x_lab, y = y_lab, color = "")
  }
  plot
}