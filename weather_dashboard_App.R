library(shiny)
library(readr)
library(DT)
library(tibble)
library(ggplot2)

shinyApp(
  ui =    bootstrapPage(
    
    tags$head(tags$script(src="scrolling-overload.js")),
    tags$head(tags$link(rel="shortcut icon", href="/wd_cloud_90Z_icon.ico")),
    includeCSS("www/style.css"),
    navbarPage("Weather Dashboard",
                     tabPanel("Load Files",
                              sidebarPanel(uiOutput('upload_area')),
                              sidebarPanel(textInput('group_name','Enter Name of Group'),
                              actionButton('group_button','Group Selected Files')),
                              DTOutput('tbl')
                              ),
                     tabPanel("Preview Data",
                              sidebarPanel(id = "preview_sidebar",
                                           textInput("colname", "Column Name"),
                                           actionButton('rename_button','Rename Column', width = '100%'),
                                           actionButton('delete_col_button','Delete Column', width = '100%'),
                                           textOutput("text"),
                                           DTOutput('preview_file_selection_area'),
                                           actionButton('generate_group_data','Generate Group Data', width = '100%'),
                                           actionButton('generate_hour','Generate Hour Data', width = '100%'),
                                           actionButton('generate_day','Generate Day Data', width = '100%'),
                                           actionButton('generate_month','Generate Month Data', width = '100%')
                              ),
                              mainPanel( tableOutput('table'))
                              ),
                     tabPanel("Analyze",
                              sidebarPanel(id = "preview_sidebar",
                                           DTOutput('plot_file_selection_area'),
                                           actionButton('plot_button','Plot Data'),
                                           uiOutput('x_col_selector'),
                                           uiOutput('y_col_selector'),
                                           uiOutput('plot_type_selector'),
                                           uiOutput('time_range_area'),
                                           textInput('graph_title','Title'),
                                           textInput('x_lab','X Label'),
                                           textInput('y_lab','Y Label'),
                                           downloadLink('downloadbutton','Download Plot')
                              ),
                              mainPanel(plotOutput('main_plot'))
                              )
                  
    )
  ),
  server = function(input,output,session){
    #Here ALL members of the globals object is defined, even if they are only used in helper files that are defined in other files
    globals <- reactiveValues()
    #is the tibble containing the id, name and group of each file. Displayed in the fileupload section
    globals$filedisplayTibble <- t <- tibble(id = character(), filename = character(), group = character())
    #list tibbles of all files that have been uploaded
    globals$files <- list()
    #vector all the file names
    globals$filenames <- c()
    #the most recent plot generated
    globals$most_recent_plot <- NULL
    
    #this mostly only used for the previewing data section, but it has a function, clean_file_names, which is needed before then
    source('combind_tibbles.R')
    #---------------------------------------------------------------------------------------------------------------------------
    #-----------------------------------------------FILE UPLOAD TAB CODE--------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    #display the filedisplayTibble
    output$tbl = DT::renderDataTable(globals$filedisplayTibble, options = list(sDom  = '<"top">lrt<"bottom">ip'), editable=FALSE)
    
    #generate the fileupload area, reset's it self after each use
    output$upload_area <- renderUI({
      #This triggers the file upload are to reset eachtime after it is used
      dont_delete_me <- input$file_input
      fileInput("file_input", "Upload Files",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"), multiple = TRUE
      )
    })
    
    #observes the use of the fileup load widget
    #adds newly uploaded files into the globals object
    #creates the id for each file and automatically assigns it to Group1
    observeEvent(input$file_input,{
      input_files <- input$file_input
      print(input_files$name)
      files <- list()
      index <- 1
      for( the_path in input_files$datapath ) {
        file <- read_csv(the_path)
        temp_names <- colnames(file)
        temp_names[1] <- 'date'
        colnames(file) <- temp_names
        file <- clean_file_names(file)
        files[[index]] <- file
        index = index + 1
      }
      #add the new files to the list of files
      globals$files <- isolate(c(globals$files,files))
      isolate(globals$filenames <- c(globals$filenames,input_files$name))
      #create serial numbers and add the new files to the filedisplaytibble
      number = nrow(globals$filedisplayTibble)
      for(i in seq(1,length(files))){
        id <- paste('a',(number+i),sep='')
        name <- input_files$name[i]
        group <- 'Group1'
        globals$filedisplayTibble[(number+i),] <- c(id,name,group)
      }
    })
    
    #observes the Group Button
    #changes the group of the selected files to the name in the group_name inputText area
    observeEvent(input$group_button,{
      files <- as.numeric(input$tbl_rows_selected)
      name <- input$group_name
      globals$filedisplayTibble$group[files] <- name
    })
    #---------------------------------------------------------------------------------------------------------------------------
    #-------------------------------------------PREVEIW DATA TAB---------------------------------------------------------------
    #---------------------------------------------------------------------------------------------------------------------------
    source('average_time_frames.R')
    #reatuve values used just for the preview tab
    preview_values <- reactiveValues()
    preview_values$col <- 0

    #generate the preview file selection area using the globals$filedisplayTibble generated in the file upload tab
    output$preview_file_selection_area = DT::renderDataTable(globals$filedisplayTibble[,c(2,3)], options = list(sDom  = '<"top">lrt<"bottom">ip'), editable=FALSE,selection = 'single')
    
    #dumby function for keeping track of the selected column
    output$text <- renderText({
      preview_values$col <- input$col
      return('')
    })
    
    #observe the click event of the rename_button
    #if clicked and not a repeat event the selected column is renamed to what is currently in the colname textInput area
    observeEvent(input$rename_button,{
      if(input$rename_button > 0) {
        col <- preview_values$col + 1
        colnames(globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]])[col] <- input$colname
        globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]] 
      }else{
        globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]]
      }
    })
    
    #observe the clicking of the delete column button
    #if there would be less than 2 columns after deleting a column, an error message is displayed
    observeEvent(input$delete_col_button,{
      if(ncol(globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]]) > 2){
        col <- preview_values$col + 1
        globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]][col] <- NULL
        globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]]  
      }else{
        print('This is an error, you must have atleast two columns') 
        message <- 'There must be atleast two columns left in the dataframe'
        session$sendCustomMessage("messageHandler", message)
      }
    })
    
    #render the table for displaying the various files you can preview
    output$table <- renderTable({
      if(length(input$preview_file_selection_area_rows_selected) == 0){
        return(tibble())
      }
      globals$files[[as.numeric(input$preview_file_selection_area_rows_selected)]][1:10,]
      
    })
    
    #action for the generate_group_data button
    observeEvent(input$generate_group_data,{
      #get list of all groups
      delete = 0
      group_names <- unique(unlist(globals$filedisplayTibble[,3]))
      delete = 0
      new_files <- list()
      new_names <- c()
      for( i in seq(1:length(group_names))){
        #get indexes associated with each group name
        #print(globals$filedisplayTibble[(globals$filedisplayTibble[,3] == group_names[i]),])
        print(length(globals$files[(globals$filedisplayTibble[,3] == group_names[i])]))
        #send each group through
        grouped_tibble <- combind_tibbles(globals$files[(globals$filedisplayTibble[,3] == group_names[i])])
        #save the files and names to add to globals outside of the for loop
        if(length(new_files) == 0){
          new_files <- list(grouped_tibble)
        }else{
          new_files <- append(new_files,list(grouped_tibble)) 
        }
        print(dim(grouped_tibble))
        print('--')
      }
      
      #add the new_files to the globals
      globals$files <- c(globals$files,new_files)
      globals$filenames <- c(globals$filenames,group_names)
      for(i in seq(1,length(group_names))){
        length_plus <- nrow(globals$filedisplayTibble)+1
        globals$filedisplayTibble[length_plus,] <- c(paste('a',length_plus,sep=''),group_names[i],'')
      }
      delete_me = 0
      delete_me = 0
    })
    
    #action for the generate hour data button
    observeEvent(input$generate_hour,{
      files <- list()
      names <- c()
      for(i in seq(1,length(globals$files))){
        if( grepl('hourly-data',globals$filenames[[i]]) || grepl('monthly-data',globals$filenames[[i]]) || grepl('daily-data',globals$filenames[[i]])){
          #skip these data sets
        }else{
          #save what the current warning status is
          oldw <- getOption("warn")
          #turn off warnings temporarily, otherise the next function call will take an ungodly amount of time
          options(warn = -1)
          
          #calculate the mean, max and min of each hour
          new_files <- average_time_frames(globals$files[[i]],'hour')
          
          #turn warnsing back on
          options(warn = oldw)
          
          new_names <- paste(globals$filenames[[i]],'hourly-data',c('mean','max','min'),sep='-')
          files <- c(files,new_files)
          names <- c(names,new_names) 
        }
      }
      globals$files <- c(globals$files,files)
      globals$filenames <- c(globals$filenames,names)
      for(i in seq(1,length(names))){
        length_plus <- nrow(globals$filedisplayTibble)+1
        globals$filedisplayTibble[length_plus,] <- c(paste('a',length_plus,sep=''),names[i],'')
      }
    })
    
    #action for the generate day data button
    observeEvent(input$generate_day,{
      files <- list()
      names <- c()
      for(i in seq(1,length(globals$files))){
        delete = 0
        if( grepl('hourly-data',globals$filenames[[i]]) || grepl('monthly-data',globals$filenames[[i]]) || grepl('daily-data',globals$filenames[[i]])){
          #skip these data sets
          print('skipping')
          this_does_nothing = 0
          print(names)
        }else{
          print('dammit not skipping')
          #save what the current warning status is
          oldw <- getOption("warn")
          #turn off warnings temporarily, otherise the next function call will take an ungodly amount of time
          options(warn = -1)
          
          #calculate the mean, max and min of each hour
          new_files <- average_time_frames(globals$files[[i]],'day')
          
          #turn warnsing back on
          options(warn = oldw)
          
          new_names <- paste(globals$filenames[[i]],'daily-data',c('mean','max','min'),sep='-')
          files <- c(files,new_files)
          names <- c(names,new_names) 
          print(names)
        }
      }
      globals$files <- c(globals$files,files)
      globals$filenames <- c(globals$filenames,names)
      for(i in seq(1,length(names))){
        length_plus <- nrow(globals$filedisplayTibble)+1
        globals$filedisplayTibble[length_plus,] <- c(paste('a',length_plus,sep=''),names[i],'')
      }
    })
    
    #action for the generate month data button
    observeEvent(input$generate_month,{
      files <- list()
      names <- c()
      for(i in seq(1,length(globals$files))){
        if( grepl('hourly-data',globals$filenames[[i]]) || grepl('monthly-data',globals$filenames[[i]]) || grepl('daily-data',globals$filenames[[i]])){
          #skip these data sets
        }else{
          #save what the current warning status is
          oldw <- getOption("warn")
          #turn off warnings temporarily, otherise the next function call will take an ungodly amount of time
          options(warn = -1)
          
          #calculate the mean, max and min of each hour
          new_files <- average_time_frames(globals$files[[i]],'month')
          
          #turn warnsing back on
          options(warn = oldw)
          
          new_names <- paste(globals$filenames[[i]],'monthly-data',c('mean','max','min'),sep='-')
          files <- c(files,new_files)
          names <- c(names,new_names) 
        }
      }
      globals$files <- c(globals$files,files)
      globals$filenames <- c(globals$filenames,names)
      for(i in seq(1,length(names))){
        length_plus <- nrow(globals$filedisplayTibble)+1
        globals$filedisplayTibble[length_plus,] <- c(paste('a',length_plus,sep=''),names[i],'')
      }
    })
    #---------------------------------------------------------------------------------------------------------------------------
    #---------------------------------------------------PLOTING TAB-------------------------------------------------------------
    #--------------------------------------------------------------------------------------------------------------------------- 
    source('make_plot.R')
    plot_values <- reactiveValues()
    plot_values$selected <- c()
    plot_values$common_names <- c()
    
    #display the list of datasets available to choose from
    output$plot_file_selection_area = DT::renderDataTable(globals$filedisplayTibble[,c(2,3)], options = list(sDom  = '<"top">lrt<"bottom">ip'), editable=FALSE,selection = 'multiple')
    
    #monitor any chagne to which files have been selected
    observeEvent(input$plot_file_selection_area_rows_selected,{
      isolate(plot_values$selected <- input$plot_file_selection_area_rows_selected)
      get_common_colnames(input$plot_file_selection_area_rows_selected)
      print(isolate(plot_values$selected))
    })
    
    #populate the options for the x_col_selector drop down
    output$x_col_selector <- renderUI({
      options <- plot_values$common_names
      selectInput('x_selection','X Axis Variable',options, selected = options[1])
    })
    
    #populate the options for the y_col_selector drop down
    output$y_col_selector <- renderUI({
      options <- plot_values$common_names
      selectInput('y_selection','Y axis Variable',options,selected = options[2])
    })
    
    #populate the options for the plot type drop down
    output$plot_type_selector <- renderUI({
      options <- c('point','line','line & point')
      selectInput('plot_type_choice','Plot Type',options,selected = options[1])
    })
    
    #generate the main display plot
    output$main_plot <- renderPlot({
      input$plot_button
      #makes a list of the selected data sets
      isolate(print(plot_values$selected))
      isolate(selected_data_sets <- plot_values$selected)
      
      line = FALSE
      point = FALSE
      if(input$plot_type_choice %in% 'point'){
        point = TRUE
        line = FALSE
      }else if(input$plot_type_choice %in% 'line'){
        point = FALSE
        line = TRUE
      }else if(input$plot_type_choice %in% 'line & point'){
        point = TRUE
        line = TRUE
      }
      
      if(length(selected_data_sets) == 0){
        return(NULL)
      }
      globals$most_recent_plot <- make_plot(globals$files[selected_data_sets],
                input$x_selection,
                input$y_selection,
                c('Group1','Group2','Group3','Group4','Group5','Group6'),
                line = line,
                point = point,
                title = input$graph_title,
                x_lab = input$x_lab,
                y_lab = input$y_lab,
                range = input$x_axis_range)
      globals$most_recent_plot
    })
    
    #returns a vector of column names that in common between all of the selected files
    get_common_colnames <- function(indexes){
      names <- colnames(globals$files[[indexes[1]]])
      indexes <- indexes[-c(1)]
      for(i in indexes){
        names <- intersect(names,colnames(globals$files[[i]]))
      }
      plot_values$common_names <- names 
    }
    
    #display the range selector for the time period to display
    output$time_range_area <- renderUI({
      print('ranging')
      thing <- input$x_selection
      #get the max and min range of the X axis
      xs <- c()
      selected_data_sets <- plot_values$selected
      biggest <- 0
      for(i in selected_data_sets){
        num <- nrow(globals$files[[i]])
        if(num > biggest){
          biggest = num
        }
      }
      sliderInput("x_axis_range", label = "X Axis Range", min = 0, max = biggest, value = c(0, biggest), step = 1)
    })
    
    output$downloadbutton <- downloadHandler(
      filename = function(){'plot.png'},
      content = function(con) {
        ggsave(con, plot = globals$most_recent_plot, device = "png")
      }
    )
  }
)