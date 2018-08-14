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
  plot <- ggplot()
  
  #limit the x axis range if x_col is date
  if(x_col == 'date') {
    for(i in seq(1,length(tibbles))){
      tibbles[[i]] <- tibbles[[i]][start:end,]
    }
  }
  
  X <- c()
  Y <- c()
  groups <- c()
  
  #color blind friendly color schemes
  cbPalette <- c( "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00")
  # To use for fills, add
  scale_fill_manual(values=cbPalette)
  # To use for line and point colors, add
  scale_colour_manual(values=cbPalette)
  
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
  plot + theme_bw() + scale_colour_manual(values=cbPalette) + scale_fill_manual(values=cbPalette)
}