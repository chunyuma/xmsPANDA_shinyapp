options(shiny.maxRequestSize=100*1024^2)
options(shiny.sanitize.errors=FALSE)
# Server logic
source("R/source_codes/xmsPANDA_v1.0.8.38.R")

server <- function(input, output, session) {
 
  ##################################  Additional Analysis Page ##########################
  
  check1_interactive <- reactiveValues(count = 1)
  search_interactive <- reactiveValues(count = 0)
  id2_interactive <- NULL
  #update1 <- reactiveValues(count = 0)
  off_interactive <- reactiveVal(0)
  observeEvent(input$input_file_interactive,{check1_interactive$count=1})
  observeEvent(input$feat_inte_interactive,{check1_interactive$count=1})
  observeEvent(input$classlabel_inte_interactive,{check1_interactive$count=1})
  
  observeEvent(input$start1_interactive, 
               {
                 if(isolate(input$choose_graph)=='Manhattan Plot only' || isolate(input$choose_graph)=='Volcano Plot only'){
                   output$nText_interactive <- renderText({shiny::validate(
                     need(input$input_file_interactive, "No input file provided. Please upload your input file."),
                     need(input$input_file_interactive$type=="text/csv" || input$input_file_interactive$type=="text/plain", "The format of input file is not correct. Please upload the file with correct format.")
                   )})
                   shiny::validate(
                     need(input$input_file_interactive, "No input file provided. Please upload your input file."),
                     need(input$input_file_interactive$type=="text/csv" || input$input_file_interactive$type=="text/plain", "The format of input file is not correct. Please upload the file with correct format.")
                   )
                 }
                 if(isolate(input$choose_graph)=='Manhattan Plot with Box Plot' || isolate(input$choose_graph)=='Volcano Plot with Box Plot'){
                   output$nText_interactive <- renderText({shiny::validate(
                     need(input$feat_inte_interactive, "No feature table provided. Please upload your feature table."),
                     need(input$feat_inte_interactive$type=="text/csv" || input$feat_inte_interactive$type=="text/plain", "The format of feature table is not correct. Please upload the file with correct format."),
                     need(input$classlabel_inte_interactive, "No class label file provided. Please upload class label file."),
                     need(input$classlabel_inte_interactive$type=="text/csv" || input$classlabel_inte_interactive$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format.")
                   )})
                   shiny::validate(
                     need(input$feat_inte_interactive, "No feature table provided. Please upload your feature table."),
                     need(input$feat_inte_interactive$type=="text/csv" || input$feat_inte_interactive$type=="text/plain", "The format of feature table is not correct. Please upload the file with correct format."),
                     need(input$classlabel_inte_interactive, "No class label file provided. Please upload class label file."),
                     need(input$classlabel_inte_interactive$type=="text/csv" || input$classlabel_inte_interactive$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format.")
                   )
                 }
                 check1_interactive$count=0
                 id2 <<- showNotification("Scatter plot is generating now.", duration=NULL)
               })
  
  input_file_interactive <- reactive({
    if(input$start1_interactive!=0  & check1_interactive$count==0 & !is.null(input$input_file_interactive$name) ){
      
      if((input$input_file_interactive$type=="text/csv" || input$input_file_interactive$type=="text/plain")){
        req(input$input_file_interactive)
        if(input$input_file_interactive$type=="text/plain"){
          input_file <- read.delim(input$input_file_interactive$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          input_file <- read.csv(input$input_file_interactive$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
        }
        input_file 
        
      }
    }
  })
  
  feat_inte_interactive <- reactive({
    if(input$start1_interactive!=0  & check1_interactive$count==0 & !is.null(input$feat_inte_interactive$name) ){
      
      if((input$feat_inte_interactive$type=="text/csv" || input$feat_inte_interactive$type=="text/plain")){
        req(input$feat_inte_interactive)
        if(input$feat_inte_interactive$type=="text/plain"){
          feature_table_file <- read.delim(input$feat_inte_interactive$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          feature_table_file <- read.csv(input$feat_inte_interactive$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
        }
        feature_table_file 
        
      }
    }
  })
  
  classlabel_inte_interactive <- reactive({
    if(input$start1_interactive!=0  & check1_interactive$count==0 & !is.null(input$classlabel_inte_interactive$name)){
      
      if((input$classlabel_inte_interactive$type=="text/csv" || input$classlabel_inte_interactive$type=="text/plain")){
        req(input$classlabel_inte_interactive)
        if(input$classlabel_inte_interactive$type=="text/plain"){
          class_labels_file <- read.delim(input$classlabel_inte_interactive$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          class_labels_file <- read.csv(input$classlabel_inte_interactive$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
        }
        class_labels_file 
      }
    }
  })
  
  
all_alert <- reactive({
    
    if(!is.null(input_file_interactive())){
        
        if(isolate(input$choose_graph)=='Manhattan Plot only'){
          
          if(isolate(input$yaxislabel_manhattan_only)=='pvalue' && isolate(input$adjdashline_manhattan_only)=='no'){
            
            if(ncol(input_file_interactive())==3 || ncol(input_file_interactive())==4){
              closeAlert(session, "check_input_interactive_single_alert")
              check_input_interactive_single <- TRUE
            }else{
              closeAlert(session, "check_input_interactive_single_alert")
              createAlert(session, "alert_interactive", "check_input_interactive_single_alert", title = "Input File Error", content = "The input file is only allowed to have 3 or 4 columns separately for 'Name', 'log2foldchange', 'p-value', 'adjusted p-value'(optional).", append = TRUE)
              check_input_interactive_single <- FALSE
            }   
            
          } else if(isolate(input$yaxislabel_manhattan_only)=='pvalue' && isolate(input$adjdashline_manhattan_only)=='yes') {
            
            if(ncol(input_file_interactive())==4){
              closeAlert(session, "check_input_interactive_single_alert")
              check_input_interactive_single <- TRUE
            }else{
              closeAlert(session, "check_input_interactive_single_alert")
              createAlert(session, "alert_interactive", "check_input_interactive_single_alert", title = "Input File Error", content = "The input file is only allowed to have 4 columns separately for 'Name', 'log2foldchange', 'p-value', 'adjusted p-value'.", append = TRUE)
              check_input_interactive_single <- FALSE
            } 
            
          } else{
            
            if(ncol(input_file_interactive())==3){
              closeAlert(session, "check_input_interactive_single_alert")
              check_input_interactive_single <- TRUE
            }else{
              closeAlert(session, "check_input_interactive_single_alert")
              createAlert(session, "alert_interactive", "check_input_interactive_single_alert", title = "Input File Error", content = "The input file is only allowed to have 3 columns separately for 'Name', 'log2foldchange', 'VIP'.", append = TRUE)
              check_input_interactive_single <- FALSE
            } 
          }
          
        }else{
          
          if(isolate(input$adjdashline_volcano_only)=='no'){
            
            if(ncol(input_file_interactive())==3 || ncol(input_file_interactive())==4){
              closeAlert(session, "check_input_interactive_single_alert")
              check_input_interactive_single <- TRUE
            }else{
              closeAlert(session, "check_input_interactive_single_alert")
              createAlert(session, "alert_interactive", "check_input_interactive_single_alert", title = "Input File Error", content = "The input file is only allowed to have 3 or 4 columns separately for 'Name', 'log2foldchange', 'p-value', 'adjusted p-value'(optional).", append = TRUE)
              check_input_interactive_single <- FALSE
            }   
            
          }else{
            
            if(ncol(input_file_interactive())==4){
              closeAlert(session, "check_input_interactive_single_alert")
              check_input_interactive_single <- TRUE
            }else{
              closeAlert(session, "check_input_interactive_single_alert")
              createAlert(session, "alert_interactive", "check_input_interactive_single_alert", title = "Input File Error", content = "The input file is only allowed to have 4 columns separately for 'Name', 'log2foldchange', 'p-value', 'adjusted p-value'.", append = TRUE)
              check_input_interactive_single <- FALSE
            } 
            
          }
          
        }
    
    }else{
      
      check_input_interactive_single <- TRUE
    }
  
  if(is.na(input$x_axis_spacing_type1_manhattan_only)){
    closeAlert(session, "x_axis_spacing_type1_manhattan_only_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type1_manhattan_only_alert", title = "Argument Input Error", content = "'Type1 X axis spacing' argument can't be empty.", append = TRUE)
    check_x_axis_spacing_type1_manhattan_only <- FALSE
  } else if (input$x_axis_spacing_type1_manhattan_only<0 | input$x_axis_spacing_type1_manhattan_only>1000) {
    closeAlert(session, "x_axis_spacing_type1_manhattan_only_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type1_manhattan_only_alert", title = "Argument Input Error", content = "'Type1 X axis spacing' argument should be not smaller than 0 or larger than 1000.", append = TRUE)
    check_x_axis_spacing_type1_manhattan_only <- FALSE
  } else {
    closeAlert(session, "x_axis_spacing_type1_manhattan_only_alert")
    check_x_axis_spacing_type1_manhattan_only <- TRUE
  }  
  
  if(is.na(input$x_axis_spacing_type2_manhattan_only)){
    closeAlert(session, "x_axis_spacing_type2_manhattan_only_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type2_manhattan_only_alert", title = "Argument Input Error", content = "'Type2 X axis spacing' argument can't be empty.", append = TRUE)
    check_x_axis_spacing_type2_manhattan_only <- FALSE
  } else if (input$x_axis_spacing_type2_manhattan_only<0 | input$x_axis_spacing_type2_manhattan_only>1000) {
    closeAlert(session, "x_axis_spacing_type2_manhattan_only_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type2_manhattan_only_alert", title = "Argument Input Error", content = "'Type2 X axis spacing' argument should be not smaller than 0 or larger than 1000.", append = TRUE)
    check_x_axis_spacing_type2_manhattan_only <- FALSE
  } else {
    closeAlert(session, "x_axis_spacing_type2_manhattan_only_alert")
    check_x_axis_spacing_type2_manhattan_only <- TRUE
  }
  
  if(is.na(input$y_axis_spacing_manhattan_only)){
    closeAlert(session, "y_axis_spacing_manhattan_only_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_manhattan_only_alert", title = "Argument Input Error", content = "'Y axis spacing' argument can't be empty.", append = TRUE)
    check_y_axis_spacing_manhattan_only <- FALSE
  } else if (input$y_axis_spacing_manhattan_only<0 | input$y_axis_spacing_manhattan_only>5) {
    closeAlert(session, "y_axis_spacing_manhattan_only_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_manhattan_only_alert", title = "Argument Input Error", content = "'Y axis spacing' argument should be not smaller than 0 or larger than 5.", append = TRUE)
    check_y_axis_spacing_manhattan_only <- FALSE
  } else {
    closeAlert(session, "y_axis_spacing_manhattan_only_alert")
    check_y_axis_spacing_manhattan_only <- TRUE
  }
  
  if(is.na(input$pvaluecutoff_manhattan_only)){
    closeAlert(session, "pvaluecutoff_manhattan_only_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_manhattan_only_alert", title = "Argument Input Error", content = "'Threshold for p-value' argument can't be empty.", append = TRUE)
    check_pvaluecutoff_manhattan_only <- FALSE
  } else if (input$pvaluecutoff_manhattan_only<0 | input$pvaluecutoff_manhattan_only>1) {
    closeAlert(session, "pvaluecutoff_manhattan_only_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_manhattan_only_alert", title = "Argument Input Error", content = "'Threshold for p-value' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    check_pvaluecutoff_manhattan_only <- FALSE
  } else {
    closeAlert(session, "pvaluecutoff_manhattan_only_alert")
    check_pvaluecutoff_manhattan_only <- TRUE
  }
  
  if(is.na(input$adjpvaluecutoff_manhattan_only)){
    closeAlert(session, "adjpvaluecutoff_manhattan_only_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_manhattan_only_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument can't be empty.", append = TRUE)
    check_adjpvaluecutoff_manhattan_only <- FALSE
  } else if (input$adjpvaluecutoff_manhattan_only<0 | input$adjpvaluecutoff_manhattan_only>1) {
    closeAlert(session, "adjpvaluecutoff_manhattan_only_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_manhattan_only_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    check_adjpvaluecutoff_manhattan_only <- FALSE
  } else {
    closeAlert(session, "adjpvaluecutoff_manhattan_only_alert")
    check_adjpvaluecutoff_manhattan_only <- TRUE
  }
  
  if(is.na(input$vipcutoff_manhattan_only)){
    closeAlert(session, "vipcutoff_manhattan_only_alert")
    createAlert(session, "alert_interactive", "vipcutoff_manhattan_only_alert", title = "Argument Input Error", content = "'Threshold for VIP' argument can't be empty.", append = TRUE)
    check_vipcutoff_manhattan_only <- FALSE
  } else if (input$vipcutoff_manhattan_only<0 | input$vipcutoff_manhattan_only>10) {
    closeAlert(session, "vipcutoff_manhattan_only_alert")
    createAlert(session, "alert_interactive", "vipcutoff_manhattan_only_alert", title = "Argument Input Error", content = "'Threshold for VIP' argument should be not smaller than 0 or larger than 10.", append = TRUE)
    check_vipcutoff_manhattan_only <- FALSE
  } else {
    closeAlert(session, "vipcutoff_manhattan_only_alert")
    check_vipcutoff_manhattan_only <- TRUE
  }
  
  if(is.na(input$pvaluecutoff_volcano_only)){
    closeAlert(session, "pvaluecutoff_volcano_only_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_volcano_only_alert", title = "Argument Input Error", content = "'Threshold for p-value' argument can't be empty.", append = TRUE)
    check_pvaluecutoff_volcano_only <- FALSE
  } else if (input$pvaluecutoff_volcano_only<0 | input$pvaluecutoff_volcano_only>1) {
    closeAlert(session, "pvaluecutoff_volcano_only_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_volcano_only_alert", title = "Argument Input Error", content = "'Threshold for p-value' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    check_pvaluecutoff_volcano_only <- FALSE
  } else {
    closeAlert(session, "pvaluecutoff_volcano_only_alert")
    check_pvaluecutoff_volcano_only <- TRUE
  }
  
  if(is.na(input$lfc_volcano_only)){
    closeAlert(session, "lfc_volcano_only_alert")
    createAlert(session, "alert_interactive", "lfc_volcano_only_alert", title = "Argument Input Error", content = "'Left side threshold for fold change' argument can't be empty.", append = TRUE)
    check_lfc_volcano_only <- FALSE
  } else if (input$lfc_volcano_only<0 | input$lfc_volcano_only>1) {
    closeAlert(session, "lfc_volcano_only_alert")
    createAlert(session, "alert_interactive", "lfc_volcano_only_alert", title = "Argument Input Error", content = "'Left side threshold for fold change' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    check_lfc_volcano_only <- FALSE
  } else {
    closeAlert(session, "lfc_volcano_only_alert")
    check_lfc_volcano_only <- TRUE
  }
  
  if(is.na(input$rfc_volcano_only)){
    closeAlert(session, "rfc_volcano_only_alert")
    createAlert(session, "alert_interactive", "rfc_volcano_only_alert", title = "Argument Input Error", content = "'Right side threshold for fold change' argument can't be empty.", append = TRUE)
    check_rfc_volcano_only <- FALSE
  } else if (input$rfc_volcano_only<1 | input$rfc_volcano_only>10) {
    closeAlert(session, "rfc_volcano_only_alert")
    createAlert(session, "alert_interactive", "rfc_volcano_only_alert", title = "Argument Input Error", content = "'Right side threshold for fold change' argument should be not smaller than 1 or larger than 10.", append = TRUE)
    check_rfc_volcano_only <- FALSE
  } else {
    closeAlert(session, "rfc_volcano_only_alert")
    check_rfc_volcano_only <- TRUE
  }
  
  if(is.na(input$x_axis_boundary_volcano_only)){
    closeAlert(session, "x_axis_boundary_volcano_only_alert")
    createAlert(session, "alert_interactive", "x_axis_boundary_volcano_only_alert", title = "Argument Input Error", content = "'X axis boundary' argument can't be empty.", append = TRUE)
    check_x_axis_boundary_volcano_only <- FALSE
  } else if (input$x_axis_boundary_volcano_only<0 | input$x_axis_boundary_volcano_only>20) {
    closeAlert(session, "x_axis_boundary_volcano_only_alert")
    createAlert(session, "alert_interactive", "x_axis_boundary_volcano_only_alert", title = "Argument Input Error", content = "'X axis boundary' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    check_x_axis_boundary_volcano_only <- FALSE
  } else {
    closeAlert(session, "x_axis_boundary_volcano_only_alert")
    check_x_axis_boundary_volcano_only <- TRUE
  }
  
  if(is.na(input$y_axis_spacing_volcano_only)){
    closeAlert(session, "y_axis_spacing_volcano_only_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_volcano_only_alert", title = "Argument Input Error", content = "'Y axis spacing' argument can't be empty.", append = TRUE)
    check_y_axis_spacing_volcano_only <- FALSE
  } else if (input$y_axis_spacing_volcano_only<0 | input$y_axis_spacing_volcano_only>5) {
    closeAlert(session, "y_axis_spacing_volcano_only_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_volcano_only_alert", title = "Argument Input Error", content = "'Y axis spacing' argument should be not smaller than 0 or larger than 5.", append = TRUE)
    check_y_axis_spacing_volcano_only <- FALSE
  } else {
    closeAlert(session, "y_axis_spacing_volcano_only_alert")
    check_y_axis_spacing_volcano_only <- TRUE
  }
  
  if(is.na(input$adjpvaluecutoff_volcano_only)){
    closeAlert(session, "adjpvaluecutoff_volcano_only_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_volcano_only_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument can't be empty.", append = TRUE)
    check_adjpvaluecutoff_volcano_only <- FALSE
  } else if (input$adjpvaluecutoff_volcano_only<0 | input$adjpvaluecutoff_volcano_only>1) {
    closeAlert(session, "adjpvaluecutoff_volcano_only_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_volcano_only_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    check_adjpvaluecutoff_volcano_only <- FALSE
  } else {
    closeAlert(session, "adjpvaluecutoff_volcano_only_alert")
    check_adjpvaluecutoff_volcano_only <- TRUE
  }
  
  if(is.na(input$x_axis_spacing_type1_manhattan_box)){
    closeAlert(session, "x_axis_spacing_type1_manhattan_box_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type1_manhattan_box_alert", title = "Argument Input Error", content = "'Type1 X axis spacing' argument can't be empty.", append = TRUE)
    check_x_axis_spacing_type1_manhattan_box <- FALSE
  } else if (input$x_axis_spacing_type1_manhattan_box<0 | input$x_axis_spacing_type1_manhattan_box>1000) {
    closeAlert(session, "x_axis_spacing_type1_manhattan_box_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type1_manhattan_box_alert", title = "Argument Input Error", content = "'Type1 X axis spacing' argument should be not smaller than 0 or larger than 1000.", append = TRUE)
    check_x_axis_spacing_type1_manhattan_box <- FALSE
  } else {
    closeAlert(session, "x_axis_spacing_type1_manhattan_box_alert")
    check_x_axis_spacing_type1_manhattan_box <- TRUE
  }
  
  if(is.na(input$x_axis_spacing_type2_manhattan_box)){
    closeAlert(session, "x_axis_spacing_type2_manhattan_box_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type2_manhattan_box_alert", title = "Argument Input Error", content = "'Type2 X axis spacing' argument can't be empty.", append = TRUE)
    check_x_axis_spacing_type2_manhattan_box <- FALSE
  } else if (input$x_axis_spacing_type2_manhattan_box<0 | input$x_axis_spacing_type2_manhattan_box>1000) {
    closeAlert(session, "x_axis_spacing_type2_manhattan_box_alert")
    createAlert(session, "alert_interactive", "x_axis_spacing_type2_manhattan_box_alert", title = "Argument Input Error", content = "'Type2 X axis spacing' argument should be not smaller than 0 or larger than 1000.", append = TRUE)
    check_x_axis_spacing_type2_manhattan_box <- FALSE
  } else {
    closeAlert(session, "x_axis_spacing_type2_manhattan_box_alert")
    check_x_axis_spacing_type2_manhattan_box <- TRUE
  }
  
  if(is.na(input$y_axis_spacing_manhattan_box)){
    closeAlert(session, "y_axis_spacing_manhattan_box_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_manhattan_box_alert", title = "Argument Input Error", content = "'Y axis spacing' argument can't be empty.", append = TRUE)
    check_y_axis_spacing_manhattan_box <- FALSE
  } else if (input$y_axis_spacing_manhattan_box<0 | input$y_axis_spacing_manhattan_box>5) {
    closeAlert(session, "y_axis_spacing_manhattan_box_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_manhattan_box_alert", title = "Argument Input Error", content = "'Y axis spacing' argument should be not smaller than 0 or larger than 5", append = TRUE)
    check_y_axis_spacing_manhattan_box <- FALSE
  } else {
    closeAlert(session, "y_axis_spacing_manhattan_box_alert")
    check_y_axis_spacing_manhattan_box <- TRUE
  }
  
  if(is.na(input$pvaluecutoff_manhattan_box)){
    closeAlert(session, "pvaluecutoff_manhattan_box_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_manhattan_box_alert", title = "Argument Input Error", content = "'Y axis spacing' argument can't be empty.", append = TRUE)
    check_pvaluecutoff_manhattan_box <- FALSE
  } else if (input$pvaluecutoff_manhattan_box<0 | input$pvaluecutoff_manhattan_box>1) {
    closeAlert(session, "pvaluecutoff_manhattan_box_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_manhattan_box_alert", title = "Argument Input Error", content = "'Y axis spacing' argument should be not smaller than 0 or larger than 1", append = TRUE)
    check_pvaluecutoff_manhattan_box <- FALSE
  } else {
    closeAlert(session, "pvaluecutoff_manhattan_box_alert")
    check_pvaluecutoff_manhattan_box <- TRUE
  }
  
  if(is.na(input$adjpvaluecutoff_manhattan_box)){
    closeAlert(session, "adjpvaluecutoff_manhattan_box_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_manhattan_box_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument can't be empty.", append = TRUE)
    check_adjpvaluecutoff_manhattan_box <- FALSE
  } else if (input$adjpvaluecutoff_manhattan_box<0 | input$adjpvaluecutoff_manhattan_box>1) {
    closeAlert(session, "adjpvaluecutoff_manhattan_box_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_manhattan_box_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument should be not smaller than 0 or larger than 1", append = TRUE)
    check_adjpvaluecutoff_manhattan_box <- FALSE
  } else {
    closeAlert(session, "adjpvaluecutoff_manhattan_box_alert")
    check_adjpvaluecutoff_manhattan_box <- TRUE
  }
  
  if(is.na(input$vipcutoff_manhattan_box)){
    closeAlert(session, "vipcutoff_manhattan_box_alert")
    createAlert(session, "alert_interactive", "vipcutoff_manhattan_box_alert", title = "Argument Input Error", content = "'Threshold for VIP' argument can't be empty.", append = TRUE)
    check_vipcutoff_manhattan_box <- FALSE
  } else if (input$vipcutoff_manhattan_box<0 | input$vipcutoff_manhattan_box>10) {
    closeAlert(session, "vipcutoff_manhattan_box_alert")
    createAlert(session, "alert_interactive", "vipcutoff_manhattan_box_alert", title = "Argument Input Error", content = "'Threshold for VIP' argument should be not smaller than 0 or larger than 10", append = TRUE)
    check_vipcutoff_manhattan_box <- FALSE
  } else {
    closeAlert(session, "vipcutoff_manhattan_box_alert")
    check_vipcutoff_manhattan_box <- TRUE
  }
  
  if(is.na(input$pvaluecutoff_volcano_box)){
    closeAlert(session, "pvaluecutoff_volcano_box_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_volcano_box_alert", title = "Argument Input Error", content = "'Threshold for p-value' argument can't be empty.", append = TRUE)
    check_pvaluecutoff_volcano_box <- FALSE
  } else if (input$pvaluecutoff_volcano_box<0 | input$pvaluecutoff_volcano_box>1) {
    closeAlert(session, "pvaluecutoff_volcano_box_alert")
    createAlert(session, "alert_interactive", "pvaluecutoff_volcano_box_alert", title = "Argument Input Error", content = "'Threshold for p-value' argument should be not smaller than 0 or larger than 1", append = TRUE)
    check_pvaluecutoff_volcano_box <- FALSE
  } else {
    closeAlert(session, "pvaluecutoff_volcano_box_alert")
    check_pvaluecutoff_volcano_box <- TRUE
  }
  
  if(is.na(input$lfc_volcano_box)){
    closeAlert(session, "lfc_volcano_box_alert")
    createAlert(session, "alert_interactive", "lfc_volcano_box_alert", title = "Argument Input Error", content = "'Left side threshold for fold change' argument can't be empty.", append = TRUE)
    check_lfc_volcano_box <- FALSE
  } else if (input$lfc_volcano_box<0 | input$lfc_volcano_box>1) {
    closeAlert(session, "lfc_volcano_box_alert")
    createAlert(session, "alert_interactive", "lfc_volcano_box_alert", title = "Argument Input Error", content = "'Left side threshold for fold change' argument should be not smaller than 0 or larger than 1", append = TRUE)
    check_lfc_volcano_box <- FALSE
  } else {
    closeAlert(session, "lfc_volcano_box_alert")
    check_lfc_volcano_box <- TRUE
  }
  
  if(is.na(input$rfc_volcano_box)){
    closeAlert(session, "rfc_volcano_box_alert")
    createAlert(session, "alert_interactive", "rfc_volcano_box_alert", title = "Argument Input Error", content = "'Right side threshold for fold change' argument can't be empty.", append = TRUE)
    check_rfc_volcano_box <- FALSE
  } else if (input$rfc_volcano_box<1 | input$rfc_volcano_box>10) {
    closeAlert(session, "rfc_volcano_box_alert")
    createAlert(session, "alert_interactive", "rfc_volcano_box_alert", title = "Argument Input Error", content = "'Right side threshold for fold change' argument should be not smaller than 1 or larger than 10", append = TRUE)
    check_rfc_volcano_box <- FALSE
  } else {
    closeAlert(session, "rfc_volcano_box_alert")
    check_rfc_volcano_box <- TRUE
  }
  
  if(is.na(input$x_axis_boundary_volcano_box)){
    closeAlert(session, "x_axis_boundary_volcano_box_alert")
    createAlert(session, "alert_interactive", "x_axis_boundary_volcano_box_alert", title = "Argument Input Error", content = "'X axis boundary' argument can't be empty.", append = TRUE)
    check_x_axis_boundary_volcano_box <- FALSE
  } else if (input$x_axis_boundary_volcano_box<0 | input$x_axis_boundary_volcano_box>20) {
    closeAlert(session, "x_axis_boundary_volcano_box_alert")
    createAlert(session, "alert_interactive", "x_axis_boundary_volcano_box_alert", title = "Argument Input Error", content = "'X axis boundary' argument should be not smaller than 0 or larger than 20", append = TRUE)
    check_x_axis_boundary_volcano_box <- FALSE
  } else {
    closeAlert(session, "x_axis_boundary_volcano_box_alert")
    check_x_axis_boundary_volcano_box <- TRUE
  }
  
  if(is.na(input$y_axis_spacing_volcano_box)){
    closeAlert(session, "y_axis_spacing_volcano_box_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_volcano_box_alert", title = "Argument Input Error", content = "'Y axis spacing' argument can't be empty.", append = TRUE)
    check_y_axis_spacing_volcano_box <- FALSE
  } else if (input$y_axis_spacing_volcano_box<0 | input$y_axis_spacing_volcano_box>5) {
    closeAlert(session, "y_axis_spacing_volcano_box_alert")
    createAlert(session, "alert_interactive", "y_axis_spacing_volcano_box_alert", title = "Argument Input Error", content = "'Y axis spacing' argument should be not smaller than 0 or larger than 5", append = TRUE)
    check_y_axis_spacing_volcano_box <- FALSE
  } else {
    closeAlert(session, "y_axis_spacing_volcano_box_alert")
    check_y_axis_spacing_volcano_box <- TRUE
  }
  
  if(is.na(input$adjpvaluecutoff_volcano_box)){
    closeAlert(session, "adjpvaluecutoff_volcano_box_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_volcano_box_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument can't be empty.", append = TRUE)
    check_adjpvaluecutoff_volcano_box <- FALSE
  } else if (input$adjpvaluecutoff_volcano_box<0 | input$adjpvaluecutoff_volcano_box>1) {
    closeAlert(session, "adjpvaluecutoff_volcano_box_alert")
    createAlert(session, "alert_interactive", "adjpvaluecutoff_volcano_box_alert", title = "Argument Input Error", content = "'Threshold for adjusted p-value' argument should be not smaller than 0 or larger than 1", append = TRUE)
    check_adjpvaluecutoff_volcano_box <- FALSE
  } else {
    closeAlert(session, "adjpvaluecutoff_volcano_box_alert")
    check_adjpvaluecutoff_volcano_box <- TRUE
  }
  
  all(check_input_interactive_single,check_x_axis_spacing_type1_manhattan_only,check_x_axis_spacing_type2_manhattan_only,check_y_axis_spacing_manhattan_only,check_pvaluecutoff_manhattan_only,check_adjpvaluecutoff_manhattan_only,check_vipcutoff_manhattan_only,
      check_pvaluecutoff_volcano_only,check_lfc_volcano_only,check_rfc_volcano_only,check_x_axis_boundary_volcano_only,check_y_axis_spacing_volcano_only,check_adjpvaluecutoff_volcano_only,
      check_x_axis_spacing_type1_manhattan_box,check_x_axis_spacing_type2_manhattan_box,check_y_axis_spacing_manhattan_box,check_pvaluecutoff_manhattan_box,check_adjpvaluecutoff_manhattan_box,check_vipcutoff_manhattan_box,
      check_pvaluecutoff_volcano_box,check_lfc_volcano_box,check_rfc_volcano_box,check_x_axis_boundary_volcano_box,check_y_axis_spacing_volcano_box,check_adjpvaluecutoff_volcano_box)
    
})
  
  
plot1_single_plot <- reactive({
  
    if(isolate(input$choose_graph)=='Manhattan Plot only' || isolate(input$choose_graph)=='Volcano Plot only'){
      
      if(input$start1_interactive!=0  & check1_interactive$count==0 & !is.null(input_file_interactive()) & all_alert()){
        
        if(isolate(input$choose_graph)=='Manhattan Plot only'){
          
          if(isolate(input$yaxislabel_manhattan_only)=='pvalue'){
            
            if(ncol(input_file_interactive())==3){
              manhattan_only = input_file_interactive()
              colnames(manhattan_only) = c('name','log2foldchange','p.value')
            }else{
              manhattan_only = input_file_interactive()
              colnames(manhattan_only) = c('name','log2foldchange','p.value','adjusted.p.value')
            }
          
            manhattan_only = manhattan_only[grep('_',manhattan_only$name),]
            manhattan_only = manhattan_only %>% separate('name',c("mz","time"),remove=FALSE,sep="_")
            manhattan_only$mz=as.numeric(manhattan_only$mz)
            manhattan_only$time=as.numeric(manhattan_only$time)
            
            if(isolate(input$adjdashline_manhattan_only)=='yes'){
              
              if(isolate(input$psignif_manhattan_only)=='pvalue'){
                
                cutoff_manhattan_only <- isolate(input$pvaluecutoff_manhattan_only)
              }else{
                
                cutoff_manhattan_only <- max(manhattan_only[manhattan_only$p.value<isolate(input$pvaluecutoff_manhattan_only) & manhattan_only$adjusted.p.value<isolate(input$adjpvaluecutoff_manhattan_only),'p.value'])
              }
              
              adjdashline_manhattan_only <- geom_hline(yintercept=-log10(max(manhattan_only[manhattan_only$p.value<isolate(input$pvaluecutoff_manhattan_only) & manhattan_only$adjusted.p.value<isolate(input$adjpvaluecutoff_manhattan_only),'p.value'])),linetype='dotted',color= input$dottedlinecol_manhattan_only)
            }else{
              
              cutoff_manhattan_only <- isolate(input$pvaluecutoff_manhattan_only)
              adjdashline_manhattan_only <- NULL
            }
            
      
            if(isolate(input$labelexpression_manhattan_only)=='yes'){
              
              manhattan_only[manhattan_only$p.value<cutoff_manhattan_only & manhattan_only$log2foldchange>0,'color']<- input$poscol_manhattan_only
              manhattan_only[manhattan_only$p.value<cutoff_manhattan_only & manhattan_only$log2foldchange<0,'color']<- input$negcol_manhattan_only
            }else{
              
              manhattan_only[manhattan_only$p.value<cutoff_manhattan_only,'color']<- input$sigcol_manhattan_only
            }
            
            manhattan_only[manhattan_only$p.value>=cutoff_manhattan_only,'color']<- input$insigcol_manhattan_only
            manhattan_only[manhattan_only$color== input$insigcol_manhattan_only,'size']=0.2    
            manhattan_only[!(manhattan_only$color== input$insigcol_manhattan_only),'size']=1.5 
            pal_manhattan_only <- levels(factor(manhattan_only$color))
            names(pal_manhattan_only)<-pal_manhattan_only
              
            xincrement_manhattan_only = isolate(input$x_axis_spacing_type1_manhattan_only)
            xmin_val_manhattan_only <- min(0,manhattan_only$mz)
            xmax_val_manhattan_only <- max(manhattan_only$mz)
            xlabel_manhattan_only <- xlab('mass-to-charge (m/z)')
            maintitle_manhattan_only <- ggtitle('Type 1 manhattan plot (-log10p vs mz)')
            gplotline_manhattan_only <- ggplot(manhattan_only, aes(x=mz, y=-log10(p.value), color=color, text=paste0('mz:',manhattan_only$mz," time:",manhattan_only$time)))

            yvec_manhattan_only <- -log10(manhattan_only$p.value)
            if(max(yvec_manhattan_only)>20){yvec_manhattan_only[yvec_manhattan_only>20]<-20}
            ymax_val_manhattan_only <- max(yvec_manhattan_only)
            ymax_manhattan_only =floor(max(yvec_manhattan_only))+1
            ylabel_manhattan_only <- ylab('-log10p')
            
            dashline_manhattan_only <- geom_hline(yintercept=-log10(isolate(input$pvaluecutoff_manhattan_only)),linetype='dashed',color= input$dashedlinecol_manhattan_only)
            
          }else{
            
            manhattan_only = input_file_interactive()
            colnames(manhattan_only) = c('name','log2foldchange','VIP')
            manhattan_only=manhattan_only[grep("_",manhattan_only$name),]
            rownames(manhattan_only)=1:nrow(manhattan_only)
            manhattan_only = manhattan_only %>% separate(name,c("mz","time"),remove=FALSE,sep="_")
            manhattan_only$mz=as.numeric(manhattan_only$mz)
            manhattan_only$time=as.numeric(manhattan_only$time)
            
            if(isolate(input$labelexpression_manhattan_only)=='yes'){
              
              manhattan_only[manhattan_only$VIP > isolate(input$vipcutoff_manhattan_only) & manhattan_only$log2foldchange>0,'color']<- input$poscol_manhattan_only
              manhattan_only[manhattan_only$VIP > isolate(input$vipcutoff_manhattan_only) & manhattan_only$log2foldchange<0,'color']<- input$negcol_manhattan_only
            }else{
              
              manhattan_only[manhattan_only$VIP > isolate(input$vipcutoff_manhattan_only),'color']<- input$sigcol_manhattan_only
            }
            
            manhattan_only[manhattan_only$VIP<= isolate(input$vipcutoff_manhattan_only),'color']<- input$insigcol_manhattan_only
            manhattan_only[manhattan_only$color== input$insigcol_manhattan_only,'size']=0.2
            manhattan_only[!(manhattan_only$color== input$insigcol_manhattan_only),'size']=1.5
            pal_manhattan_only <- levels(factor(manhattan_only$color))
            names(pal_manhattan_only)<-pal_manhattan_only
              
            xincrement_manhattan_only = isolate(input$x_axis_spacing_type1_manhattan_only)
            xmin_val_manhattan_only <- min(0,manhattan_only$mz)
            xmax_val_manhattan_only <- max(manhattan_only$mz)
            xlabel_manhattan_only <- xlab('mass-to-charge (m/z)')
            maintitle_manhattan_only <- ggtitle('Type 1 manhattan plot (VIP vs mz)')
            gplotline_manhattan_only <- ggplot(manhattan_only, aes(x=mz, y=VIP, color=color, text=paste0('mz:',manhattan_only$mz," time:",manhattan_only$time)))
              
            
            yvec_manhattan_only <- manhattan_only$VIP
            if(max(yvec_manhattan_only)>20){yvec_manhattan_only[yvec_manhattan_only>20]<-20}
            ymax_val_manhattan_only <- max(yvec_manhattan_only)
            ymax_manhattan_only =floor(max(yvec_manhattan_only))+1
            ylabel_manhattan_only <- ylab('VIP')
            
            dashline_manhattan_only <- geom_hline(yintercept= isolate(input$vipcutoff_manhattan_only),linetype='dashed',color= input$dashedlinecol_manhattan_only)
            
            adjdashline_manhattan_only <- NULL
            
          }
          
          
          p1 <- gplotline_manhattan_only +
            geom_point(show.legend=F,size=manhattan_only$size,alpha=0.7) + 
            xlabel_manhattan_only + ylabel_manhattan_only +
            scale_color_manual(values=pal_manhattan_only) +
            scale_x_continuous(breaks = seq(xmin_val_manhattan_only, xmax_val_manhattan_only, by = xincrement_manhattan_only)) +
            scale_y_continuous(breaks = seq(0, (ymax_val_manhattan_only + 2), by = isolate(input$y_axis_spacing_manhattan_only)), limits = c(0,ymax_manhattan_only)) +
            dashline_manhattan_only + adjdashline_manhattan_only +
            maintitle_manhattan_only +
            theme(axis.title=element_text(size=12), 
                  axis.text =element_text(size=10),
                  axis.text.x = element_text(angle=0),
                  axis.line = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                  plot.title = element_text(size=14, hjust = 0.5),
                  legend.position = "none")
          
          p1
          
        }else{
          
          if(ncol(input_file_interactive())==3){
            volcano_only = input_file_interactive()
            colnames(volcano_only) = c('name','log2foldchange','p.value')
          }else{
            volcano_only = input_file_interactive()
            colnames(volcano_only) = c('name','log2foldchange','p.value','adjusted.p.value')
          }
          
          
          if(isolate(input$adjdashline_volcano_only)=='yes'){
            
            if(isolate(input$psignif_volcano_only)=='pvalue'){
              
              cutoff_volcano_only <- isolate(input$pvaluecutoff_volcano_only)
            }else{
              
              cutoff_volcano_only <- max(volcano_only[volcano_only$p.value<isolate(input$pvaluecutoff_volcano_only) & volcano_only$adjusted.p.value<isolate(input$adjpvaluecutoff_volcano_only),'p.value'])
            }
            
            adjdashline_volcano_only <- geom_hline(yintercept=-log10(max(volcano_only[volcano_only$p.value<isolate(input$pvaluecutoff_volcano_only) & volcano_only$adjusted.p.value<isolate(input$adjpvaluecutoff_volcano_only),'p.value'])),linetype='dotted',color= input$dottedlinecol_volcano_only)
          }else{
            
            cutoff_volcano_only <- isolate(input$pvaluecutoff_volcano_only)
            adjdashline_volcano_only <- NULL
          }
          
          
          if(isolate(input$labelexpression_volcano_only)=='yes'){
            
            volcano_only[volcano_only$log2foldchange > log2(isolate(input$rfc_volcano_only)) & volcano_only$p.value < cutoff_volcano_only, 'color'] <- input$poscol_volcano_only
            volcano_only[volcano_only$log2foldchange < log2(isolate(input$lfc_volcano_only)) & volcano_only$p.value < cutoff_volcano_only, 'color'] <- input$negcol_volcano_only
            
          }else{
            
            volcano_only[volcano_only$log2foldchange > log2(isolate(input$rfc_volcano_only)) & volcano_only$p.value < cutoff_volcano_only, 'color'] <- input$sigcol_volcano_only
            volcano_only[volcano_only$log2foldchange < log2(isolate(input$lfc_volcano_only)) & volcano_only$p.value < cutoff_volcano_only, 'color'] <- input$sigcol_volcano_only

          }
          
          volcano_only[is.na(volcano_only$color),'color'] <- input$insigcol_volcano_only
          volcano_only[volcano_only$color== input$insigcol_volcano_only,'size']=0.2    
          volcano_only[!(volcano_only$color== input$insigcol_volcano_only),'size']=1.5 
          pal_volcano_only <- levels(factor(volcano_only$color))
          names(pal_volcano_only)<-pal_volcano_only
          volcano_only$log10pvalue <- -log10(volcano_only$p.value)
          
          if(isolate(input$set_x_boundary_volcano_only)=='yes'){
            
            xmin_val_volcano_only <- - isolate(input$x_axis_boundary_volcano_only)
            xmax_val_volcano_only <- isolate(input$x_axis_boundary_volcano_only)
            x_scale_volcano_only <- scale_x_continuous(name="log2(Fold Change)", limits = c(xmin_val_volcano_only,xmax_val_volcano_only))
          }else{
            x_scale_volcano_only <- scale_x_continuous(name="log2(Fold Change)")
          }
  
          yvec_volcano_only <- -log10(volcano_only$p.value)
          if(max(yvec_volcano_only)>20){yvec_volcano_only[yvec_volcano_only>20]<-20}
          ymax_val_volcano_only <- max(yvec_volcano_only)
          ymax_volcano_only =floor(max(yvec_volcano_only))+1

          dashline_volcano_only <- geom_hline(yintercept=-log10(isolate(input$pvaluecutoff_volcano_only)),linetype='dashed',color= input$dashedlinecol_volcano_only)
          
          
          p1 <- ggplot(data=volcano_only,aes(x=log2foldchange, y=log10pvalue, color=color, text=name)) +
            geom_point(show.legend=F,size=volcano_only$size,alpha=0.7) + 
            dashline_volcano_only + adjdashline_volcano_only +
            geom_vline(xintercept = log2(isolate(input$lfc_volcano_only)), linetype=2, colour= input$lfcdashed_volcano_only) + 
            geom_vline(xintercept = log2(isolate(input$rfc_volcano_only)), linetype=2, colour= input$rfcdashed_volcano_only) +
            x_scale_volcano_only + 
            scale_y_continuous(name="-log10(p-value)", breaks = seq(0, (ymax_val_volcano_only + 2), by = isolate(input$y_axis_spacing_volcano_only)), limits = c(0,ymax_volcano_only)) +
            scale_color_manual(values=pal_volcano_only) +
            theme(axis.title=element_text(size=12), 
                  axis.text =element_text(size=10),
                  axis.text.x = element_text(angle=0),
                  axis.line = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                  plot.title = element_text(size=14, hjust = 0.5),
                  legend.position = "none")
          
          
          p1
          
        }
        
        
      }
      
    }
    
  })
  
  
plot2_single_plot <- reactive({
    
    if(isolate(input$choose_graph)=='Manhattan Plot only' || isolate(input$choose_graph)=='Volcano Plot only'){
      
      if(input$start1_interactive!=0  & check1_interactive$count==0 & !is.null(input_file_interactive()) & all_alert()){
        
        if(isolate(input$choose_graph)=='Manhattan Plot only'){
          
          if(isolate(input$yaxislabel_manhattan_only)=='pvalue'){
            
            if(ncol(input_file_interactive())==3){
              manhattan_only = input_file_interactive()
              colnames(manhattan_only) = c('name','log2foldchange','p.value')
            }else{
              manhattan_only = input_file_interactive()
              colnames(manhattan_only) = c('name','log2foldchange','p.value','adjusted.p.value')
            }
            
            manhattan_only = manhattan_only[grep('_',manhattan_only$name),]
            manhattan_only = manhattan_only %>% separate('name',c("mz","time"),remove=FALSE,sep="_")
            manhattan_only$mz=as.numeric(manhattan_only$mz)
            manhattan_only$time=as.numeric(manhattan_only$time)
            
            if(isolate(input$adjdashline_manhattan_only)=='yes'){
              
              if(isolate(input$psignif_manhattan_only)=='pvalue'){
                
                cutoff_manhattan_only <- isolate(input$pvaluecutoff_manhattan_only)
              }else{
                
                cutoff_manhattan_only <- max(manhattan_only[manhattan_only$p.value<isolate(input$pvaluecutoff_manhattan_only) & manhattan_only$adjusted.p.value<isolate(input$adjpvaluecutoff_manhattan_only),'p.value'])
              }
              
              adjdashline_manhattan_only <- geom_hline(yintercept=-log10(max(manhattan_only[manhattan_only$p.value<isolate(input$pvaluecutoff_manhattan_only) & manhattan_only$adjusted.p.value<isolate(input$adjpvaluecutoff_manhattan_only),'p.value'])),linetype='dotted',color= input$dottedlinecol_manhattan_only)
            }else{
              
              cutoff_manhattan_only <- isolate(input$pvaluecutoff_manhattan_only)
              adjdashline_manhattan_only <- NULL
            }
            
            
            if(isolate(input$labelexpression_manhattan_only)=='yes'){
              
              manhattan_only[manhattan_only$p.value<cutoff_manhattan_only & manhattan_only$log2foldchange>0,'color']<- input$poscol_manhattan_only
              manhattan_only[manhattan_only$p.value<cutoff_manhattan_only & manhattan_only$log2foldchange<0,'color']<- input$negcol_manhattan_only
            }else{
              
              manhattan_only[manhattan_only$p.value<cutoff_manhattan_only,'color']<- input$sigcol_manhattan_only
            }
            
            manhattan_only[manhattan_only$p.value>=cutoff_manhattan_only,'color']<- input$insigcol_manhattan_only
            manhattan_only[manhattan_only$color== input$insigcol_manhattan_only,'size']=0.2
            manhattan_only[!(manhattan_only$color== input$insigcol_manhattan_only),'size']=1.5
            pal_manhattan_only <- levels(factor(manhattan_only$color))
            names(pal_manhattan_only)<-pal_manhattan_only
              
            xincrement_manhattan_only = isolate(input$x_axis_spacing_type2_manhattan_only) #round_any(max(manhattan$time)/10,10,f=floor)
            xmin_val_manhattan_only <- min(0,manhattan_only$time)
            xmax_val_manhattan_only <- max(manhattan_only$time)
            xlabel_manhattan_only <- xlab('Retention time (s)')
            maintitle_manhattan_only <- ggtitle('Type 2 manhattan plot (-log10p vs time)')
            gplotline_manhattan_only <- ggplot(manhattan_only, aes(x=time, y=-log10(p.value), color=color, text=paste0('mz:',manhattan_only$mz," time:",manhattan_only$time)))
            
            yvec_manhattan_only <- -log10(manhattan_only$p.value)
            if(max(yvec_manhattan_only)>20){yvec_manhattan_only[yvec_manhattan_only>20]<-20}
            ymax_val_manhattan_only <- max(yvec_manhattan_only)
            ymax_manhattan_only =floor(max(yvec_manhattan_only))+1
            ylabel_manhattan_only <- ylab('-log10p')
            
            dashline_manhattan_only <- geom_hline(yintercept=-log10(isolate(input$pvaluecutoff_manhattan_only)),linetype='dashed',color= input$dashedlinecol_manhattan_only)
            
          }else{
            
            manhattan_only = input_file_interactive()
            colnames(manhattan_only) = c('name','log2foldchange','VIP')
            manhattan_only=manhattan_only[grep("_",manhattan_only$name),]
            rownames(manhattan_only)=1:nrow(manhattan_only)
            manhattan_only = manhattan_only %>% separate(name,c("mz","time"),remove=FALSE,sep="_")
            manhattan_only$mz=as.numeric(manhattan_only$mz)
            manhattan_only$time=as.numeric(manhattan_only$time)
            
            if(isolate(input$labelexpression_manhattan_only)=='yes'){
              
              manhattan_only[manhattan_only$VIP > isolate(input$vipcutoff_manhattan_only) & manhattan_only$log2foldchange>0,'color']<- input$poscol_manhattan_only
              manhattan_only[manhattan_only$VIP > isolate(input$vipcutoff_manhattan_only) & manhattan_only$log2foldchange<0,'color']<- input$negcol_manhattan_only
            }else{
              
              manhattan_only[manhattan_only$VIP > isolate(input$vipcutoff_manhattan_only),'color']<- input$sigcol_manhattan_only
            }
            
            manhattan_only[manhattan_only$VIP<= isolate(input$vipcutoff_manhattan_only),'color']<- input$insigcol_manhattan_only
            manhattan_only[manhattan_only$color== input$insigcol_manhattan_only,'size']=0.2
            manhattan_only[!(manhattan_only$color== input$insigcol_manhattan_only),'size']=1.5
            pal_manhattan_only <- levels(factor(manhattan_only$color))
            names(pal_manhattan_only)<-pal_manhattan_only
            
            xincrement_manhattan_only = isolate(input$x_axis_spacing_type2_manhattan_only)
            xmin_val_manhattan_only <- min(0,manhattan_only$time)
            xmax_val_manhattan_only <- max(manhattan_only$time)
            xlabel_manhattan_only <- xlab('Retention time (s)')
            maintitle_manhattan_only <- ggtitle('Type 2 manhattan plot (VIP vs time)')
            gplotline_manhattan_only <- ggplot(manhattan_only, aes(x=time, y=VIP, color=color, text=paste0('mz:',manhattan_only$mz," time:",manhattan_only$time)))
            
            yvec_manhattan_only <- manhattan_only$VIP
            if(max(yvec_manhattan_only)>20){yvec_manhattan_only[yvec_manhattan_only>20]<-20}
            ymax_val_manhattan_only <- max(yvec_manhattan_only)
            ymax_manhattan_only =floor(max(yvec_manhattan_only))+1
            ylabel_manhattan_only <- ylab('VIP')
            
            dashline_manhattan_only <- geom_hline(yintercept= isolate(input$vipcutoff_manhattan_only),linetype='dashed',color= input$dashedlinecol_manhattan_only)
            
            adjdashline_manhattan_only <- NULL
            
          }
          
          
          p2 <- gplotline_manhattan_only +
            geom_point(show.legend=F,size=manhattan_only$size,alpha=0.7) + 
            xlabel_manhattan_only + ylabel_manhattan_only +
            scale_color_manual(values=pal_manhattan_only) +
            scale_x_continuous(breaks = seq(xmin_val_manhattan_only, xmax_val_manhattan_only, by = xincrement_manhattan_only)) +
            scale_y_continuous(breaks = seq(0, (ymax_val_manhattan_only + 2), by = isolate(input$y_axis_spacing_manhattan_only)), limits = c(0,ymax_manhattan_only)) +
            dashline_manhattan_only + adjdashline_manhattan_only +
            maintitle_manhattan_only +
            theme(axis.title=element_text(size=12), 
                  axis.text =element_text(size=10),
                  axis.text.x = element_text(angle=0),
                  axis.line = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                  plot.title = element_text(size=14, hjust = 0.5),
                  legend.position = "none")
          
          p2
          
        }else{
          
          NULL
        }
        
        
      }
      
    }
    
  })
  
  
  pplot1 <- reactive({

    if(isolate(input$choose_graph)=='Manhattan Plot only' || isolate(input$choose_graph)=='Volcano Plot only'){
      
      if(!is.null(plot1_single_plot())){
        
        ggp1 <- ggplotly(plot1_single_plot(),tooltip = c("text"),source="scatterplot") %>% config(displayModeBar = F)
        ggp1
        
      }
      
    }else{
      
      
    }
  
 
  })

  pplot2 <- reactive({
    
    if(isolate(input$choose_graph)=='Manhattan Plot only' || isolate(input$choose_graph)=='Volcano Plot only'){
      
      if(!is.null(plot2_single_plot())){
        
        ggp2 <- ggplotly(plot2_single_plot(),tooltip = c("text"),source="scatterplot") %>% config(displayModeBar = F)
        ggp2
        
      }
      
    }else{
      
      
    }
    

  })

  output$check_plot_mahattan_only <- reactive({
    
    if(isolate(input$choose_graph)=='Manhattan Plot only' & !is.null(pplot1()) & !is.null(pplot2())){
      TRUE
    }else{
      FALSE
    }
    
  })
  outputOptions(output, "check_plot_mahattan_only", suspendWhenHidden = FALSE)
  
  
  output$check_plot_volcano_only <- reactive({
    
    if(isolate(input$choose_graph)=='Volcano Plot only' & !is.null(pplot1()) & is.null(pplot2())){
      TRUE
    }else{
      FALSE
    }
    
  })
  outputOptions(output, "check_plot_volcano_only", suspendWhenHidden = FALSE)
  
  
  output$interactive_plot <- renderUI({

    if(isolate(input$choose_graph)=='Manhattan Plot only' & !is.null(pplot1()) & !is.null(pplot2())){

      output$manhattan_only_plot1 <- renderPlotly({if(!is.null(pplot1())){pplot1()}})
      output$manhattan_only_plot2 <- renderPlotly({if(!is.null(pplot2())){pplot2()}})

      column(width=12,
             style='margin-bottom:10px',
             column(width=6,plotlyOutput("manhattan_only_plot1", width = "400px", height = "400px")),
             column(width=6,plotlyOutput("manhattan_only_plot2", width = "400px", height = "400px"))

      )

    } else if(isolate(input$choose_graph)=='Volcano Plot only' & !is.null(pplot1()) & is.null(pplot2())){
      
      output$volcano_only_plot1 <- renderPlotly({if(!is.null(pplot1())){pplot1()}})
      
      column(width=12,
             style='margin-bottom:10px',
             column(width=12,style='text-align:center',plotlyOutput("volcano_only_plot1",width = "550px", height = "400px"))
             
      )
      
    }


  })
  
  
  output$downloadPlot1_manhattan_only <- downloadHandler(
    
    filename <- "type1_manhattan_plot.png",
    content = function(file) {
      ggsave(file, plot = plot1_single_plot(), device = "png", width=4, height=4)
    }
  )
  
  output$downloadPlot2_manhattan_only <- downloadHandler(
    
    filename <- "type2_manhattan_plot.png",
    content = function(file) {
      ggsave(file, plot = plot2_single_plot(), device = "png", width=4, height=4)
    }
  )
  
  output$downloadPlot1_volcano_vonly <- downloadHandler(
    
    filename <- "volcano_plot.png",
    content = function(file) {
      ggsave(file, plot = plot1_single_plot(), device = "png", width=4, height=4)
    }
  )
  
  
} 



