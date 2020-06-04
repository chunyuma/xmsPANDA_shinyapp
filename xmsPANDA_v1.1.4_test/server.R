options(shiny.maxRequestSize=100*1024^2)
options(shiny.sanitize.errors=FALSE)
# Server logic
source("R/source_codes/xmsPANDA_v1.0.8.40.R")

server <- function(input, output, session) {
  
  ##################################  Introduction Page #################################################  
  
  ##################################  Analysis Page #################################################
  
  
  ##### workflow1
  done_wf1 <- reactiveValues(count_wf1 = 0)
  go_wf1 <- reactiveValues(count_wf1 = 0)
  id1_wf1 <- NULL
  check_wf1 <- reactiveValues(count_wf1 = 0)
  
  output$checkpvalue_wf1 <- reactive({
    if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'FALSE'){
      sum(input$featselmethodi_wf1%in%c('limma','limma2way','lm1wayanova','lm2wayanova','lmreg','logitreg','rfesvm','ttest','wilcox','RF','MARS','pamr'))>0
    }else{
      if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'TRUE'){
        sum(input$featselmethodii_wf1%in%c('limma1wayrepeat','limma2wayrepeat','lm1wayanovarepeat','lm2wayanovarepeat','ttestrepeat','wilcoxrepeat'))>0
      }else{
        if(input$analysismode_wf1 == 'regression' && input$pairedanalysis_wf1 == 'FALSE'){
          sum(input$featselmethodiii_wf1%in%c('lmreg','RF','MARS'))>0
        }}}})
  outputOptions(output, 'checkpvalue_wf1', suspendWhenHidden = FALSE)
  
  output$checkvip_wf1 <- reactive({
    if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'FALSE'){
      sum(input$featselmethodi_wf1%in%c('pls','o1pls','spls'))>0
    }else{
      if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'TRUE'){
        sum(input$featselmethodii_wf1%in%c('spls1wayrepeat','spls2wayrepeat'))>0
      }else{
        if(input$analysismode_wf1 == 'regression' && input$pairedanalysis_wf1 == 'FALSE'){
          sum(input$featselmethodiii_wf1%in%c('pls','o1pls','spls'))>0
        }}}})
  outputOptions(output, "checkvip_wf1", suspendWhenHidden = FALSE)
  
  output$checkmax_varsel_wf1 <- reactive({
    if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'FALSE'){
      sum(input$featselmethodi_wf1%in%c('rfesvm','RF','spls'))>0
    }else{
      if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'TRUE'){
        sum(input$featselmethodii_wf1%in%c('spls1wayrepeat','spls2wayrepeat'))>0
      }else{
        if(input$analysismode_wf1 == 'regression' && input$pairedanalysis_wf1 == 'FALSE'){
          sum(input$featselmethodiii_wf1%in%c('RF','spls'))>0
        }}}})
  outputOptions(output, "checkmax_varsel_wf1", suspendWhenHidden = FALSE)
  
  output$checkaggregationmethod_wf1 <- reactive({
    if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'FALSE'){
      length(input$featselmethodi_wf1)>1
    }else{
      if(input$analysismode_wf1 == 'classification' && input$pairedanalysis_wf1 == 'TRUE'){
        length(input$featselmethodii_wf1)>1
      }else{
        if(input$analysismode_wf1 == 'regression' && input$pairedanalysis_wf1 == 'FALSE'){
          length(input$featselmethodiii_wf1)>1
        }}}})
  outputOptions(output, "checkaggregationmethod_wf1", suspendWhenHidden = FALSE)
  
  observe({
    if (input$permu_switch_wf1==TRUE) {
      shinyjs::enable("pls_permut_count_wf1")
    } else {
      shinyjs::disable("pls_permut_count_wf1")
    }
  })
  
  all_alert_wf1 <- reactive({
    
    if(!is.integer(input$numreplicate_wf1)) {
      closeAlert(session, "numreplicateAlert_wf1")
      createAlert(session, "alert_wf1", "numreplicateAlert_wf1", title = "Argument Input Error", content = "'Number of technical replicates' argument should be a integer.", append = TRUE)
      checknumreplicateAlert_wf1 <- FALSE
    } else if (input$numreplicate_wf1<1 | input$numreplicate_wf1>10) {
      closeAlert(session, "numreplicateAlert_wf1")
      createAlert(session, "alert_wf1", "numreplicateAlert_wf1", title = "Argument Input Error", content = "'Number of technical replicates' argument should be not smaller than 1 or larger than 10.", append = TRUE)
      checknumreplicateAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "numreplicateAlert_wf1")
      checknumreplicateAlert_wf1 <- TRUE
    }
    
    if(is.na(input$summarization_ratio_wf1)){
      closeAlert(session, "summarization_ratioAlert_wf1")
      createAlert(session, "alert_wf1", "summarization_ratioAlert_wf1", title = "Argument Input Error", content = "'Maximum missing value ratio' argument can't be empty.", append = TRUE)
      checksummarization_ratioAlert_wf1 <- FALSE
    } else if (input$summarization_ratio_wf1<0 | input$summarization_ratio_wf1>1) {
      closeAlert(session, "summarization_ratioAlert_wf1")
      createAlert(session, "alert_wf1", "summarization_ratioAlert_wf1", title = "Argument Input Error", content = "'Maximum missing value ratio' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checksummarization_ratioAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "summarization_ratioAlert_wf1")
      checksummarization_ratioAlert_wf1 <- TRUE
    }
    
    if(is.na(input$all_missing_thresh_wf1)){
      closeAlert(session, "all_missing_threshAlert_wf1")
      createAlert(session, "alert_wf1", "all_missing_threshAlert_wf1", title = "Argument Input Error", content = "'Minimum non-missing sample ratio' argument can't be empty.", append = TRUE)
      checkall_missing_threshAlert_wf1 <- FALSE
    } else if (input$all_missing_thresh_wf1<0 | input$all_missing_thresh_wf1>1) {
      closeAlert(session, "all_missing_threshAlert_wf1")
      createAlert(session, "alert_wf1", "all_missing_threshAlert_wf1", title = "Argument Input Error", content = "'Minimum non-missing sample ratio' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkall_missing_threshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "all_missing_threshAlert_wf1")
      checkall_missing_threshAlert_wf1 <- TRUE
    }
    
    
    if(is.na(input$rsd_filt_list_wf1)){
      closeAlert(session, "rsd_filt_listAlert_wf1")
      createAlert(session, "alert_wf1", "rsd_filt_listAlert_wf1", title = "Argument Input Error", content = "'Minimum overall variance' argument can't be empty.", append = TRUE)
      checkrsd_filt_listAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "rsd_filt_listAlert_wf1")
      checkrsd_filt_listAlert_wf1 <- TRUE
    }
    
    if(is.na(input$group_missing_thresh_wf1)){
      closeAlert(session, "group_missing_threshAlert_wf1")
      createAlert(session, "alert_wf1", "group_missing_threshAlert_wf1", title = "Argument Input Error", content = "'Minimum non-missing sample ratio for group' argument can't be empty.", append = TRUE)
      checkgroup_missing_threshAlert_wf1 <- FALSE
    } else if (input$group_missing_thresh_wf1<0 | input$group_missing_thresh_wf1>1) {
      closeAlert(session, "group_missing_threshAlert_wf1")
      createAlert(session, "alert_wf1", "group_missing_threshAlert_wf1", title = "Argument Input Error", content = "'Minimum non-missing sample ratio for group' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkgroup_missing_threshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "group_missing_threshAlert_wf1")
      checkgroup_missing_threshAlert_wf1 <- TRUE
    }
    
    if(is.na(input$pvalue_thresh_wf1)){
      closeAlert(session, "pvalue_threshAlert_wf1")
      createAlert(session, "alert_wf1", "pvalue_threshAlert_wf1", title = "Argument Input Error", content = "'P-value threshold' argument can't be empty.", append = TRUE)
      checkpvalue_threshAlert_wf1 <- FALSE
    } else if (input$pvalue_thresh_wf1<0 | input$pvalue_thresh_wf1>1) {
      closeAlert(session, "pvalue_threshAlert_wf1")
      createAlert(session, "alert_wf1", "pvalue_threshAlert_wf1", title = "Argument Input Error", content = "'P-value threshold' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkpvalue_threshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "pvalue_threshAlert_wf1")
      checkpvalue_threshAlert_wf1 <- TRUE
    }
    
    if(is.na(input$fdrthresh_wf1)){
      closeAlert(session, "fdrthreshAlert_wf1")
      createAlert(session, "alert_wf1", "fdrthreshAlert_wf1", title = "Argument Input Error", content = "'False discovery threshold' argument can't be empty.", append = TRUE)
      checkfdrthreshAlert_wf1 <- FALSE
    } else if (input$fdrthresh_wf1<0 | input$fdrthresh_wf1>1) {
      closeAlert(session, "fdrthreshAlert_wf1")
      createAlert(session, "alert_wf1", "fdrthreshAlert_wf1", title = "Argument Input Error", content = "'False discovery threshold' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkfdrthreshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "fdrthreshAlert_wf1")
      checkfdrthreshAlert_wf1 <- TRUE
    }
    
    if(is.na(input$foldchangethresh_wf1)){
      closeAlert(session, "foldchangethreshAlert_wf1")
      createAlert(session, "alert_wf1", "foldchangethreshAlert_wf1", title = "Argument Input Error", content = "'Fold change threshold' argument can't be empty.", append = TRUE)
      checkfoldchangethreshAlert_wf1 <- FALSE
    } else if (input$foldchangethresh_wf1<0 | input$foldchangethresh_wf1>100) {
      closeAlert(session, "foldchangethreshAlert_wf1")
      createAlert(session, "alert_wf1", "foldchangethreshAlert_wf1", title = "Argument Input Error", content = "'Fold change threshold' argument should be not smaller than 0 or larger than 100.", append = TRUE)
      checkfoldchangethreshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "foldchangethreshAlert_wf1")
      checkfoldchangethreshAlert_wf1 <- TRUE
    }
    
    if(is.na(input$kfold_wf1)){
      closeAlert(session, "kfoldAlert_wf1")
      createAlert(session, "alert_wf1", "kfoldAlert_wf1", title = "Argument Input Error", content = "'k for k-fold Cross Validation' argument can't be empty.", append = TRUE)
      checkkfoldAlert_wf1 <- FALSE
    } else if (input$kfold_wf1<1 | input$kfold_wf1>10000) {
      closeAlert(session, "kfoldAlert_wf1")
      createAlert(session, "alert_wf1", "kfoldAlert_wf1", title = "Argument Input Error", content = "'k for k-fold Cross Validation' argument should be not smaller than 1 or larger than 10000.", append = TRUE)
      checkkfoldAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "kfoldAlert_wf1")
      checkkfoldAlert_wf1 <- TRUE
    }
    
    
    if(is.na(input$pls_vip_thresh_wf1)){
      closeAlert(session, "pls_vip_threshAlert_wf1")
      createAlert(session, "alert_wf1", "pls_vip_threshAlert_wf1", title = "Argument Input Error", content = "'VIP threshold' argument can't be empty.", append = TRUE)
      checkpls_vip_threshAlert_wf1 <- FALSE
    } else if (input$pls_vip_thresh_wf1<1 | input$pls_vip_thresh_wf1>100) {
      closeAlert(session, "pls_vip_threshAlert_wf1")
      createAlert(session, "alert_wf1", "pls_vip_threshAlert_wf1", title = "Argument Input Error", content = "'VIP threshold' argument should be not smaller than 1 or larger than 100.", append = TRUE)
      checkpls_vip_threshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "pls_vip_threshAlert_wf1")
      checkpls_vip_threshAlert_wf1 <- TRUE
    }
    
    
    if(is.na(input$pls_ncomp_wf1)){
      closeAlert(session, "pls_ncompAlert_wf1")
      createAlert(session, "alert_wf1", "pls_ncompAlert_wf1", title = "Argument Input Error", content = "'Max number of components to consider' argument can't be empty.", append = TRUE)
      checkpls_ncompAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "pls_ncompAlert_wf1")
      checkpls_ncompAlert_wf1 <- TRUE
    }
    
    
    if(is.na(input$max_comp_sel_wf1)){
      closeAlert(session, "max_comp_selAlert_wf1")
      createAlert(session, "alert_wf1", "max_comp_selAlert_wf1", title = "Argument Input Error", content = "'Number of components to use for VIP selection' argument can't be empty.", append = TRUE)
      checkmax_comp_selAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "max_comp_selAlert_wf1")
      checkmax_comp_selAlert_wf1 <- TRUE
    }
    
    if(is.na(input$pls_permut_count_wf1)){
      closeAlert(session, "pls_permut_countAlert_wf1")
      createAlert(session, "alert_wf1", "pls_permut_countAlert_wf1", title = "Argument Input Error", content = "'Number of permutations for calculating p-values' argument can't be empty.", append = TRUE)
      checkpls_permut_countAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "pls_permut_countAlert_wf1")
      checkpls_permut_countAlert_wf1 <- TRUE
    }
    
    if(is.na(input$max_varsel_wf1)){
      closeAlert(session, "max_varselAlert_wf1")
      createAlert(session, "alert_wf1", "max_varselAlert_wf1", title = "Argument Input Error", content = "'Max number of variables to be used' argument can't be empty.", append = TRUE)
      checkmax_varselAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "max_varselAlert_wf1")
      checkmax_varselAlert_wf1 <- TRUE
    }
    
    if(is.na(input$abs_cor_thresh_wf1)){
      closeAlert(session, "abs_cor_threshAlert_wf1")
      createAlert(session, "alert_wf1", "abs_cor_threshAlert_wf1", title = "Argument Input Error", content = "'Absolute correlation threshold' argument can't be empty.", append = TRUE)
      checkabs_cor_threshAlert_wf1 <- FALSE
    } else if (input$abs_cor_thresh_wf1<0 | input$abs_cor_thresh_wf1>1) {
      closeAlert(session, "abs_cor_threshAlert_wf1")
      createAlert(session, "alert_wf1", "abs_cor_threshAlert_wf1", title = "Argument Input Error", content = "'Absolute correlation threshold' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkabs_cor_threshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "abs_cor_threshAlert_wf1")
      checkabs_cor_threshAlert_wf1 <- TRUE
    }
    
    if(is.na(input$cor_fdrthresh_wf1)){
      closeAlert(session, "abs_cor_threshAlert_wf1")
      createAlert(session, "alert_wf1", "cor_fdrthreshAlert_wf1", title = "Argument Input Error", content = "'FDR threshold for correlation analysis' argument can't be empty.", append = TRUE)
      checkcor_fdrthreshAlert_wf1 <- FALSE
    } else if (input$cor_fdrthresh_wf1<0 | input$cor_fdrthresh_wf1>1) {
      closeAlert(session, "abs_cor_threshAlert_wf1")
      createAlert(session, "alert_wf1", "cor_fdrthreshAlert_wf1", title = "Argument Input Error", content = "'FDR threshold for correlation analysis' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkcor_fdrthreshAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "abs_cor_threshAlert_wf1")
      checkcor_fdrthreshAlert_wf1 <- TRUE
    }
    
    if(is.na(input$ellipse_conf_level_wf1)){
      closeAlert(session, "ellipse_conf_levelAlert_wf1")
      createAlert(session, "alert_wf1", "ellipse_conf_levelAlert_wf1", title = "Argument Input Error", content = "'Confidence interval for PCA ellipses' argument can't be empty.", append = TRUE)
      checkellipse_conf_levelAlert_wf1 <- FALSE
    } else if (input$ellipse_conf_level_wf1<0 | input$ellipse_conf_level_wf1>1) {
      closeAlert(session, "ellipse_conf_levelAlert_wf1")
      createAlert(session, "alert_wf1", "ellipse_conf_levelAlert_wf1", title = "Argument Input Error", content = "'Confidence interval for PCA ellipses' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkellipse_conf_levelAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "ellipse_conf_levelAlert_wf1")
      checkellipse_conf_levelAlert_wf1 <- TRUE
    }
    
    if(is.na(input$pca_cex_val_wf1)){
      closeAlert(session, "pca_cex_valAlert_wf1")
      createAlert(session, "alert_wf1", "pca_cex_valAlert_wf1", title = "Argument Input Error", content = "'Size of points on PCA plots' argument can't be empty.", append = TRUE)
      checkpca_cex_valAlert_wf1 <- FALSE
    } else if (input$pca_cex_val_wf1<1 | input$pca_cex_val_wf1>20) {
      closeAlert(session, "pca_cex_valAlert_wf1")
      createAlert(session, "alert_wf1", "pca_cex_valAlert_wf1", title = "Argument Input Error", content = "'Size of points on PCA plots' argument should be not smaller than 1 or larger than 20.", append = TRUE)
      checkpca_cex_valAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "pca_cex_valAlert_wf1")
      checkpca_cex_valAlert_wf1 <- TRUE
    }
    
    if(is.na(input$ellipse_conf_level_wf1)){
      closeAlert(session, "ellipse_conf_levelAlert_wf1")
      createAlert(session, "alert_wf1", "ellipse_conf_levelAlert_wf1", title = "Argument Input Error", content = "'Confidence interval for PCA ellipses' argument can't be empty.", append = TRUE)
      checkellipse_conf_levelAlert_wf1 <- FALSE
    } else if (input$ellipse_conf_level_wf1<0 | input$ellipse_conf_level_wf1>1) {
      closeAlert(session, "ellipse_conf_levelAlert_wf1")
      createAlert(session, "alert_wf1", "ellipse_conf_levelAlert_wf1", title = "Argument Input Error", content = "'Confidence interval for PCA ellipses' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkellipse_conf_levelAlert_wf1 <- FALSE
    } else {
      closeAlert(session, "ellipse_conf_levelAlert_wf1")
      checkellipse_conf_levelAlert_wf1 <- TRUE
    }
    
    all(checknumreplicateAlert_wf1, checksummarization_ratioAlert_wf1, checkall_missing_threshAlert_wf1, checkrsd_filt_listAlert_wf1, checkgroup_missing_threshAlert_wf1,
        checkpvalue_threshAlert_wf1, checkfdrthreshAlert_wf1, checkfoldchangethreshAlert_wf1, checkkfoldAlert_wf1, checkpls_vip_threshAlert_wf1,
        checkpls_ncompAlert_wf1, checkmax_comp_selAlert_wf1, checkpls_permut_countAlert_wf1, checkmax_varselAlert_wf1, checkabs_cor_threshAlert_wf1,checkcor_fdrthreshAlert_wf1,
        checkpca_cex_valAlert_wf1, checkellipse_conf_levelAlert_wf1)
    
  })
  
  
  ##############################################################
  
  observeEvent(input$go_wf1,{check_wf1$count_wf1=0})
  
  observeEvent(input$go_wf1, 
               {
                 output$nText2_wf1 <- renderText({shiny::validate(
                   need(input$featuretable_wf1, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                   need(input$featuretable_wf1$type=="text/csv" || input$featuretable_wf1$type=="text/plain", "The format of datasetA is not correct. Please upload the file with correct format."),
                   need(input$classlabel_wf1, "No class label file provided. Please upload class label file in 'Choose Files'."),
                   need(input$classlabel_wf1$type=="text/csv" || input$classlabel_wf1$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format."),
                   need(featselmethod_check_wf1(),"No feature selection method was selected. Please select at least one method.") 
                 )})
                 shiny::validate(
                   need(input$featuretable_wf1, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                   need(input$featuretable_wf1$type=="text/csv" || input$featuretable_wf1$type=="text/plain", "The format of datasetA is not correct. Please upload the file with correct format."),
                   need(input$classlabel_wf1, "No class label file provided. Please upload class label file in 'Choose Files'."),
                   need(input$classlabel_wf1$type=="text/csv" || input$classlabel_wf1$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format."),
                   need(featselmethod_check_wf1(),"No feature selection method was selected. Please select at least one method.")
                 )
                 check_wf1$count_wf1=1
                 id1_wf1 <<- showNotification("Starting processing now. Your results will be available for download shortly. The processing time depends on the number of methods you used.", duration=NULL)
                 
               })
  
  #########################################
  
  
  featselmethod_check_wf1 <-reactive({
    if(input$analysismode_wf1=='classification' && input$pairedanalysis_wf1=='FALSE'){
      featselmethod_wf1<-input$featselmethodi_wf1
    }else{
      if(input$analysismode_wf1=='classification' && input$pairedanalysis_wf1=='TRUE'){
        featselmethod_wf1<-input$featselmethodii_wf1
      }else{
        if(input$analysismode_wf1=='regression' && input$pairedanalysis_wf1=='FALSE'){
          featselmethod_wf1<-input$featselmethodiii_wf1
        }else{
          featselmethod_wf1<-NULL
        }
      }
    }
    featselmethod_wf1
  })
  
  
  featuretable_wf1 <- reactive({
    if(input$go_wf1!=0 & check_wf1$count_wf1==1 & !is.null(input$featuretable_wf1$name) ){
      
      if((input$featuretable_wf1$type=="text/csv" || input$featuretable_wf1$type=="text/plain")){
        req(input$featuretable_wf1)
        if(input$featuretable_wf1$type=="text/plain"){
          featuretable_wf1 <- read.delim(input$featuretable_wf1$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$featuretable_wf1$type=="text/csv"){
            featuretable_wf1 <- read.csv(input$featuretable_wf1$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        featuretable_wf1 
      }
    }else{
      
      NA
    }
  })
  
  classlabel_wf1 <- reactive({
    if(input$go_wf1!=0 & check_wf1$count_wf1==1 & !is.null(input$classlabel_wf1$name) ){
      
      if((input$classlabel_wf1$type=="text/csv" || input$classlabel_wf1$type=="text/plain")){
        req(input$classlabel_wf1)
        if(input$classlabel_wf1$type=="text/plain"){
          classlabel_wf1 <- read.delim(input$classlabel_wf1$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$classlabel_wf1$type=="text/csv"){
            classlabel_wf1 <- read.csv(input$classlabel_wf1$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        classlabel_wf1 
      }
    }else{
      
      NA
    }
  })
  
  session_outloc_wf1 <- reactive({
    if(input$go_wf1!=0 & check_wf1$count_wf1==1){
      cur_date_wf1<-Sys.time()
      cur_date_wf1<-gsub(x=cur_date_wf1,pattern="-",replacement="")
      cur_date_wf1<-gsub(x=cur_date_wf1,pattern=":",replacement="")
      cur_date_wf1<-gsub(x=cur_date_wf1,pattern=" ",replacement="")
      if(input$outloc_wf1==""){
        outloc_wf1<-paste('~/xmsPANDAresults',cur_date_wf1,sep="")
      }else{
        outloc_wf1<-paste('~/',input$outloc_wf1,cur_date_wf1,sep="")
      }
      outloc_wf1
    }else{
      NULL
    }
  })
  
  ##########################################
  
  output$nText_wf1 <- renderText({
    if(input$go_wf1!=0  & check_wf1$count_wf1==1 & !is.null(featselmethod_check_wf1()) & is.data.frame(featuretable_wf1()) & is.data.frame(classlabel_wf1()) & all_alert_wf1()==TRUE){

      if(input$globalcor_wf1 == 'TRUE'){
        globalcor_wf1=TRUE
      }else{
        globalcor_wf1=FALSE
      }
      if(input$WGCNAmodules_wf1=='TRUE'){
        WGCNAmodules_wf1=TRUE
      }else{
        WGCNAmodules_wf1=FALSE
      }
      if(input$globalclustering_wf1=='TRUE'){
        globalclustering_wf1=TRUE
      }else{
        globalclustering_wf1=FALSE
      }
      
      if(globalcor_wf1==TRUE){
        abs_cor_thresh_wf1=input$abs_cor_thresh_wf1
        cor_fdrthresh_wf1=input$cor_fdrthresh_wf1
        cor_method_wf1=input$cor_method_wf1
        networktype_wf1=input$networktype_wf1
      }else{
        abs_cor_thresh_wf1=0.4
        cor_fdrthresh_wf1=0.2
        cor_method_wf1="spearman"
        networktype_wf1="complete"
      }
      
      if(input$use_summarizion_wf1=="TRUE"){
        summarize.replicates_wf1=TRUE
      }else{
        summarize.replicates_wf1=FALSE
      }
      
      if(input$pairedanalysis_wf1=='TRUE'){
        pairedanalysis_wf1=TRUE
      }else{
        pairedanalysis_wf1=FALSE
      }
      
      if(input$missing_val_wf1=='0'){
        missing_val_wf1=0
      }else{
        missing_val_wf1=NA
      }
      
      if(input$pca_ellipse_wf1=='TRUE'){
        pca_ellipse_wf1=TRUE
      }else{
        pca_ellipse_wf1=FALSE
      }
      
      if(input$netbasedfeatranking_switch_wf1==FALSE){
        degree_rank_method_wf1=NA
      }else{
        degree_rank_method_wf1="DiffRank"
      }
      
      
      check_pvalue_thresh_wf1<-reactive({need(input$pvalue_thresh_wf1,"error")})
      if(is.null(check_pvalue_thresh_wf1())){
        pvalue_thresh_wf1=input$pvalue_thresh_wf1
      }else{
        pvalue_thresh_wf1=0.05
      }
      
      check_foldchangethresh_wf1<-reactive({need(input$foldchangethresh_wf1,"error")})
      if(is.null(check_foldchangethresh_wf1())){
        foldchangethresh_wf1=input$foldchangethresh_wf1
      }else{
        foldchangethresh_wf1=0
      }
      
      check_fdr_method_wf1<-reactive({need(input$fdr_method_wf1,"error")})
      if(is.null(check_fdr_method_wf1())){
        fdr_method_wf1=input$fdr_method_wf1  #gsub(" \\(.*\\)","",input$fdr_method_wf1)
      }else{
        fdr_method_wf1="BH"
      }
      
      check_fdrthresh_wf1<-reactive({need(input$fdrthresh_wf1,"error")})
      if(is.null(check_fdrthresh_wf1())){
        fdrthresh_wf1=input$fdrthresh_wf1
      }else{
        fdrthresh_wf1=0.2
      }
      
      check_ellipse_conf_level_wf1<-reactive({need(input$ellipse_conf_level_wf1,"error")})
      if(is.null(check_fdrthresh_wf1())){
        ellipse_conf_level_wf1=input$ellipse_conf_level_wf1
      }else{
        ellipse_conf_level_wf1=0.95
      }
      
      check_kfold_wf1<-reactive({need(input$kfold_wf1,"error")})
      if(is.null(check_kfold_wf1())){
        kfold_wf1=input$kfold_wf1
      }else{
        kfold_wf1=10
      }
      
      check_pls_vip_thresh_wf1<-reactive({need(input$pls_vip_thresh_wf1,"error")})
      if(is.null(check_pls_vip_thresh_wf1())){
        pls_vip_thresh_wf1=input$pls_vip_thresh_wf1
      }else{
        pls_vip_thresh_wf1=2
      }
      
      if(input$alphabetical_order_wf1=='TRUE'){
        alphabetical_order_wf1=TRUE
      }else{
        alphabetical_order_wf1=FALSE      
      }
      
      if(input$timeseries_lineplots_wf1=="TRUE"){
        timeseries_lineplots_wf1=TRUE
      }else{
        timeseries_lineplots_wf1=FALSE
      }
      
      if(input$boxplot_jitter_wf1=='TRUE'){
        boxplot_jitter_wf1=TRUE
      }else{
        boxplot_jitter_wf1=FALSE
      }
      
      check_pls_permut_count_wf1<-reactive({need(input$pls_permut_count_wf1,"error")})
      if(is.null(check_pls_permut_count_wf1())){
        if(input$permu_switch_wf1==TRUE){
          pls_permut_count_wf1=input$pls_permut_count_wf1
        }else{
          pls_permut_count_wf1=NA
        }
      }else{
        pls_permut_count_wf1=NA
      }
      
      check_max_varsel_wf1<-reactive({need(input$max_varsel_wf1,"error")})
      if(is.null(check_max_varsel_wf1())){
        max_varsel_wf1=input$max_varsel_wf1
      }else{
        max_varsel_wf1=100
      }
      
      featselmethod_wf1 <- featselmethod_check_wf1()
      
      check_aggregation_method_wf1<-reactive({need(input$aggregation_method_wf1,"error")})
      if(is.null(check_aggregation_method_wf1())){
        aggregation_method_wf1=input$aggregation_method_wf1 #gsub(" \\(.*\\)","",input$aggregation_method_wf1)
        if(aggregation_method_wf1=='None'){
          aggregation_method_wf1=NA
        }
      }else{
        aggregation_method_wf1=NA
      }
      
      ###################
      ## main function for workflow1
      
      #start: see manual for additional arguments and description
      demetabs_res<-diffexp(
        #1) arguments for input files
        Xmat=featuretable_wf1(),
        parentoutput_dir=session_outloc_wf1(),
        Ymat=classlabel_wf1(),
        feature_table_file=NA,
        class_labels_file=NA,
        input.intensity.scale=input$input_intensity_scale_wf1,

        ##2) data preprocessing order: 1) summarization, 2) filtering by missing values, 3) imputation; 4) transformation and normalization: halffeaturemin
        num_replicates = input$numreplicate_wf1,
        summarize.replicates =summarize.replicates_wf1, summary.method=input$summarization_method_wf1,summary.na.replacement=input$summary_na_replacement_wf1,
        rep.max.missing.thresh=input$summarization_ratio_wf1,
        all.missing.thresh=input$all_missing_thresh_wf1, group.missing.thresh=input$group_missing_thresh_wf1, missing.val=missing_val_wf1,
        log2transform = FALSE, medcenter=FALSE, znormtransform = FALSE,
        quantile_norm = FALSE, lowess_norm = FALSE, madscaling = FALSE,
        normalization.method=input$normmethod_wf1,
        TIC_norm=FALSE,
        rsd.filt.list = input$rsd_filt_list_wf1,

        ##3) arguments for feature seletion: c("limma","pls","pamr","spls","pls","MARS","RF","rfesvm","logitreg","ttest","wilcox","o1pls","lmreg")
        #"rfesvm","pamr","MARS","RF","logitreg","ttest","wilcox","o1pls","lmreg","lm1wayanova"
        #c("limma","pls","spls","pls","MARS","RF","rfesvm","logitreg","ttest","wilcox","o1pls","lmreg","lm1wayanova")
        pairedanalysis = pairedanalysis_wf1, featselmethod=featselmethod_wf1,
        pvalue.thresh=pvalue_thresh_wf1,
        fdrthresh = fdrthresh_wf1, fdrmethod=fdr_method_wf1,
        kfold=kfold_wf1,networktype=networktype_wf1,
        samplermindex=NA,numtrees=5000,analysismode=input$analysismode_wf1, pls_vip_thresh = pls_vip_thresh_wf1, num_nodes = detectCores(),
        max_varsel = max_varsel_wf1, pls_ncomp = input$pls_ncomp_wf1, pred.eval.method="BER", rocfeatlist=seq(2,10,1),
        rocfeatincrement=TRUE,
        rocclassifier="svm",foldchangethresh=foldchangethresh_wf1,
        optselect=input$optselect_wf1,max_comp_sel=input$max_comp_sel_wf1,saveRda=FALSE,pls.permut.count=pls_permut_count_wf1,
        pca.ellipse=pca_ellipse_wf1,ellipse.conf.level=ellipse_conf_level_wf1,svm.acc.tolerance=5,pamr.threshold.select.max=FALSE,
        aggregation.method=aggregation_method_wf1,mars.gcv.thresh=1,pls.vip.selection=input$pls_vip_selection_wf1,limmadecideTests=TRUE,

        #4) arguments for WGCNA and global clustering analysis (HCA and EM clustering)
        wgcnarsdthresh=30,WGCNAmodules=WGCNAmodules_wf1,globalclustering=globalclustering_wf1,

        #5) arguments for correlation and network analysis using the selected features
        cor.method=cor_method_wf1, abs.cor.thresh = abs_cor_thresh_wf1, cor.fdrthresh=cor_fdrthresh_wf1,
        globalcor=globalcor_wf1,target.metab.file=NA,
        target.mzmatch.diff=10,target.rtmatch.diff=NA,max.cor.num=NA,
        degree_rank_method=degree_rank_method_wf1, #set to NA or "DiffRank"

        #6) arguments for graphical options: see manual for additional arguments
        output.device.type="png",pca.cex.val=input$pca_cex_val_wf1,legendlocation="bottomleft",
        net_node_colors=c("green","red"),net_legend=FALSE,
        manhattanplot.col.opt=c("darkblue","red3"),
        heatmap.col.opt=input$heatmap_color_scheme_wf1,sample.col.opt=input$sample_color_theme_wf1,
        boxplot.col.opt=input$boxplot_color_theme_wf1,barplot.col.opt=input$barplot_color_theme_wf1,
        aggregation.max.iter=100,
        plots.width=8,
        plots.height=8,
        plots.type="cairo",
        boxplot.type=input$boxplot_type_wf1,
        add.jitter=boxplot_jitter_wf1,
        timeseries.lineplots=timeseries_lineplots_wf1,
        alphabetical.order=alphabetical_order_wf1,
        ylab_text=input$ylabel_text_wf1,
        kegg_species_code="hsa",database="pathway",reference_set=NA,metab_annot=NA,match_class_dist=TRUE
      )
 
      done_wf1$count_wf1=1
      #file.copy(paste(getwd(),'matrix_centrality.txt',sep='/'),session_outloc())
      setwd(session_outloc_wf1())
      zip(zipfile=paste(basename(session_outloc_wf1()),'zip',sep='.'), files='.')
      print("Processing complete. Please click on download button to save the results.")
      
    }else{
      
      NULL
    }
  })
  
  ##########################################
  
  observeEvent({if(done_wf1$count_wf1==1) TRUE else return()},{
    if (!is.null(id1_wf1)){
      removeNotification(id1_wf1)
      id1_wf1 <<- NULL
    }
    
    if(length(featselmethod_check_wf1())>1 & !input$aggregation_method_wf1=="None"){
      featselmethodout_wf1 <- c('AggregatedResults',featselmethod_check_wf1())
    }else{
      featselmethodout_wf1 <-featselmethod_check_wf1()
    }
    
    output$output_results_wf1 <- renderUI({
      
      column(12,
             column(12,style='padding-top:10px;padding-left:0;',tags$div(h4("Output"))),
             column(8,align='center',style="display:block; margin-left: auto;margin-right:auto;",
                    imageOutput("myImage_wf1",width="400px",height="400px",inline=TRUE)
             ),
             column(4,
                    div(style='margin-bottom:40px;', selectInput(width="250px","methodout_wf1","Chooso method to display figures:",featselmethodout_wf1)),
                    uiOutput("figureradio_wf1")
             )
      )
      
    })
    
  })
  
  observeEvent(input$methodout_wf1,{
    
    if(input$methodout_wf1=="AggregatedResults"){
      l1_wf1 <- list.files(paste(session_outloc_wf1(),'AggregatedResults',sep="/"),".png",recursive=TRUE,full.names=FALSE)
      figurenum_wf1 <- paste('Figure',seq(1:length(l1_wf1)))
    }else{
      folder_wf1 <- grep(input$methodout_wf1,list.dirs(paste(session_outloc_wf1(),'Stage2',sep='/'),recursive=FALSE,full.names=FALSE),value=TRUE)
      l1_wf1 <- list.files(paste(session_outloc_wf1(),'Stage2',folder_wf1,sep="/"),".png",recursive=TRUE,full.names=FALSE)
      figurenum_wf1 <- paste('Figure',seq(1:length(l1_wf1)))
    }
    
    if(length(l1_wf1)>=1){
      output$figureradio_wf1 <- renderUI({
        div(
          div(style='margin-bottom:10px;',tags$label("Figure Choices", `for` = "figure_choices_wf1" )),
          awesomeRadio(inputId = "figure_choices_wf1",label = NULL, choices = figurenum_wf1, selected = "Figure 1", status = "primary")
        )
      })
    }
    
    
    if(!is.null(input$methodout_wf1) & length(l1_wf1)>=1){
      
      # if(input$methodout=="AggregatedResults"){
      #   output$siderbar<-renderUI({sidebarPanel(style="margin-left:0;",sliderInput("obs", "Slide to go to next figure:", min = 1, max = 5,  value = 1),width=3)})
      # }else{
      #   output$siderbar<-renderUI({sidebarPanel(style="margin-left:0;",sliderInput("obs", "Slide to go to next figure:", min = 1, max = 8,  value = 1),width=3)})
      # }
      
      output$myImage_wf1 <- renderImage({
        
        if(input$methodout_wf1=="AggregatedResults"){
          req(input$figure_choices_wf1)
          filename_wf1 <- normalizePath(file.path(paste(session_outloc_wf1(),'AggregatedResults',sep='/'),l1_wf1[as.numeric(gsub('Figure','',input$figure_choices_wf1))]))
        }else{
          req(input$figure_choices_wf1)
          filename_wf1 <- normalizePath(file.path(paste(paste(session_outloc_wf1(),'Stage2',sep='/'),folder_wf1,sep="/"),l1_wf1[as.numeric(gsub('Figure','',input$figure_choices_wf1))]))
        }
        
        list(src = filename_wf1,width=600,height=600,
             alt = "This is an image")
        
      }, deleteFile = FALSE)
      
    }
    
  })
  
  
  output$downloadData_wf1 <- downloadHandler(
    
    #if(input$go!=0 && input$featselmethod!="-" && input$feature_table_file!="" && input$class_labels_file!=""){
    
    filename <- function() {
      paste(basename(session_outloc_wf1()), "zip", sep=".")
    },
    content <- function(file_wf1) {
      fname1_wf1<-paste(session_outloc_wf1(),"/",basename(session_outloc_wf1()), ".zip", sep="")
      file.copy(fname1_wf1, file_wf1)
    },
    contentType = "application/zip"
    #}
  )
  
  #####
  
  ##### workflow2
  done_wf2 <- reactiveValues(count_wf2 = 0)
  go_wf2 <- reactiveValues(count_wf2 = 0)
  id1_wf2 <- NULL
  check_wf2 <- reactiveValues(count_wf2 = 0)
  
  output$checkpvalue_wf2 <- reactive({
    sum(input$featselmethodi_wf2%in%c('limma','lmreg','lmregrobust','logitreg','logitregrobust','rferadial','rfe','t.test','wilcox.test','welch.test','f.test','kruskal.test','rf','rfboruta','lasso','elasticnet'))>0
  })
  outputOptions(output, 'checkpvalue_wf2', suspendWhenHidden = FALSE)
  
  output$checkvip_wf2 <- reactive({
    sum(input$featselmethodi_wf2%in%c('pls','spls','o1pls'))>0
  })
  outputOptions(output, "checkvip_wf2", suspendWhenHidden = FALSE)
  
  output$checkmax_varsel_wf2 <- reactive({
    sum(input$featselmethodi_wf2%in%c('rferadial','rfe','rf','rfboruta','spls'))>0
  })
  outputOptions(output, "checkmax_varsel_wf2", suspendWhenHidden = FALSE)
  
  output$checkaggregationmethod_wf2 <- reactive({
    length(input$featselmethodi_wf2)>1
  })
  outputOptions(output, "checkaggregationmethod_wf2", suspendWhenHidden = FALSE)
  
  all_alert_wf2 <- reactive({
    
    # if(!is.integer(input$numreplicate_wf2)) {
    #   closeAlert(session, "numreplicateAlert_wf2")
    #   createAlert(session, "alert_wf2", "numreplicateAlert_wf2", title = "Argument Input Error", content = "'Number of technical replicates' argument should be a integer.", append = TRUE)
    #   checknumreplicateAlert_wf2 <- FALSE
    # } else if (input$numreplicate_wf2<1 | input$numreplicate_wf2>10) {
    #   closeAlert(session, "numreplicateAlert_wf2")
    #   createAlert(session, "alert_wf2", "numreplicateAlert_wf2", title = "Argument Input Error", content = "'Number of technical replicates' argument should be not smaller than 1 or larger than 10.", append = TRUE)
    #   checknumreplicateAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "numreplicateAlert_wf2")
    #   checknumreplicateAlert_wf2 <- TRUE
    # }
    
    # if(is.na(input$summarization_ratio_wf2)){
    #   closeAlert(session, "summarization_ratioAlert_wf2")
    #   createAlert(session, "alert_wf2", "summarization_ratioAlert_wf2", title = "Argument Input Error", content = "'Maximum missing value ratio' argument can't be empty.", append = TRUE)
    #   checksummarization_ratioAlert_wf2 <- FALSE
    # } else if (input$summarization_ratio_wf2<0 | input$summarization_ratio_wf2>1) {
    #   closeAlert(session, "summarization_ratioAlert_wf2")
    #   createAlert(session, "alert_wf2", "summarization_ratioAlert_wf2", title = "Argument Input Error", content = "'Maximum missing value ratio' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    #   checksummarization_ratioAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "summarization_ratioAlert_wf2")
    #   checksummarization_ratioAlert_wf2 <- TRUE
    # }
    
    # if(is.na(input$all_missing_thresh_wf2)){
    #   closeAlert(session, "all_missing_threshAlert_wf2")
    #   createAlert(session, "alert_wf2", "all_missing_threshAlert_wf2", title = "Argument Input Error", content = "'Minimum non-missing sample ratio' argument can't be empty.", append = TRUE)
    #   checkall_missing_threshAlert_wf2 <- FALSE
    # } else if (input$all_missing_thresh_wf2<0 | input$all_missing_thresh_wf2>1) {
    #   closeAlert(session, "all_missing_threshAlert_wf2")
    #   createAlert(session, "alert_wf2", "all_missing_threshAlert_wf2", title = "Argument Input Error", content = "'Minimum non-missing sample ratio' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    #   checkall_missing_threshAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "all_missing_threshAlert_wf2")
    #   checkall_missing_threshAlert_wf2 <- TRUE
    # }
    
    # if(is.na(input$rsd_filt_list_wf2)){
    #   closeAlert(session, "rsd_filt_listAlert_wf2")
    #   createAlert(session, "alert_wf2", "rsd_filt_listAlert_wf2", title = "Argument Input Error", content = "'Minimum overall variance' argument can't be empty.", append = TRUE)
    #   checkrsd_filt_listAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "rsd_filt_listAlert_wf2")
    #   checkrsd_filt_listAlert_wf2 <- TRUE
    # }
    
    # if(is.na(input$group_missing_thresh_wf2)){
    #   closeAlert(session, "group_missing_threshAlert_wf2")
    #   createAlert(session, "alert_wf2", "group_missing_threshAlert_wf2", title = "Argument Input Error", content = "'Minimum non-missing sample ratio for group' argument can't be empty.", append = TRUE)
    #   checkgroup_missing_threshAlert_wf2 <- FALSE
    # } else if (input$group_missing_thresh_wf2<0 | input$group_missing_thresh_wf2>1) {
    #   closeAlert(session, "group_missing_threshAlert_wf2")
    #   createAlert(session, "alert_wf2", "group_missing_threshAlert_wf2", title = "Argument Input Error", content = "'Minimum non-missing sample ratio for group' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    #   checkgroup_missing_threshAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "group_missing_threshAlert_wf2")
    #   checkgroup_missing_threshAlert_wf2 <- TRUE
    # }
    
    if(is.na(input$pvalue_thresh_wf2)){
      closeAlert(session, "pvalue_threshAlert_wf2")
      createAlert(session, "alert_wf2", "pvalue_threshAlert_wf2", title = "Argument Input Error", content = "'P-value threshold' argument can't be empty.", append = TRUE)
      checkpvalue_threshAlert_wf2 <- FALSE
    } else if (input$pvalue_thresh_wf2<0 | input$pvalue_thresh_wf2>1) {
      closeAlert(session, "pvalue_threshAlert_wf2")
      createAlert(session, "alert_wf2", "pvalue_threshAlert_wf2", title = "Argument Input Error", content = "'P-value threshold' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkpvalue_threshAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "pvalue_threshAlert_wf2")
      checkpvalue_threshAlert_wf2 <- TRUE
    }
    
    if(is.na(input$fdrthresh_wf2)){
      closeAlert(session, "fdrthreshAlert_wf2")
      createAlert(session, "alert_wf2", "fdrthreshAlert_wf2", title = "Argument Input Error", content = "'False discovery threshold' argument can't be empty.", append = TRUE)
      checkfdrthreshAlert_wf2 <- FALSE
    } else if (input$fdrthresh_wf2<0 | input$fdrthresh_wf2>1) {
      closeAlert(session, "fdrthreshAlert_wf2")
      createAlert(session, "alert_wf2", "fdrthreshAlert_wf2", title = "Argument Input Error", content = "'False discovery threshold' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkfdrthreshAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "fdrthreshAlert_wf2")
      checkfdrthreshAlert_wf2 <- TRUE
    }
    
    if(is.na(input$kfold_wf2)){
      closeAlert(session, "kfoldAlert_wf2")
      createAlert(session, "alert_wf2", "kfoldAlert_wf2", title = "Argument Input Error", content = "'k for k-fold Cross Validation' argument can't be empty.", append = TRUE)
      checkkfoldAlert_wf2 <- FALSE
    } else if (input$kfold_wf2<1 | input$kfold_wf2>10000) {
      closeAlert(session, "kfoldAlert_wf2")
      createAlert(session, "alert_wf2", "kfoldAlert_wf2", title = "Argument Input Error", content = "'k for k-fold Cross Validation' argument should be not smaller than 1 or larger than 10000.", append = TRUE)
      checkkfoldAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "kfoldAlert_wf2")
      checkkfoldAlert_wf2 <- TRUE
    }
    
    if(is.na(input$pls_vip_thresh_wf2)){
      closeAlert(session, "pls_vip_threshAlert_wf2")
      createAlert(session, "alert_wf2", "pls_vip_threshAlert_wf2", title = "Argument Input Error", content = "'VIP threshold' argument can't be empty.", append = TRUE)
      checkpls_vip_threshAlert_wf2 <- FALSE
    } else if (input$pls_vip_thresh_wf2<1 | input$pls_vip_thresh_wf2>100) {
      closeAlert(session, "pls_vip_threshAlert_wf2")
      createAlert(session, "alert_wf2", "pls_vip_threshAlert_wf2", title = "Argument Input Error", content = "'VIP threshold' argument should be not smaller than 1 or larger than 100.", append = TRUE)
      checkpls_vip_threshAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "pls_vip_threshAlert_wf2")
      checkpls_vip_threshAlert_wf2 <- TRUE
    }
    
    
    # if(is.na(input$pls_ncomp_wf2)){
    #   closeAlert(session, "pls_ncompAlert_wf2")
    #   createAlert(session, "alert_wf2", "pls_ncompAlert_wf2", title = "Argument Input Error", content = "'Max number of components to consider' argument can't be empty.", append = TRUE)
    #   checkpls_ncompAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "pls_ncompAlert_wf2")
    #   checkpls_ncompAlert_wf2 <- TRUE
    # }
    
    
    # if(is.na(input$max_comp_sel_wf2)){
    #   closeAlert(session, "max_comp_selAlert_wf2")
    #   createAlert(session, "alert_wf2", "max_comp_selAlert_wf2", title = "Argument Input Error", content = "'Number of components to use for VIP selection' argument can't be empty.", append = TRUE)
    #   checkmax_comp_selAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "max_comp_selAlert_wf2")
    #   checkmax_comp_selAlert_wf2 <- TRUE
    # }
    
    if(is.na(input$max_var_sel_wf2)){
      closeAlert(session, "max_var_selAlert_wf2")
      createAlert(session, "alert_wf2", "max_var_selAlert_wf2", title = "Argument Input Error", content = "'Max number of variables to be used' argument can't be empty.", append = TRUE)
      checkmax_var_selAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "max_var_selAlert_wf2")
      checkmax_var_selAlert_wf2 <- TRUE
    }
    
    if(is.na(input$abs_cor_thresh_wf2)){
      closeAlert(session, "abs_cor_threshAlert_wf2")
      createAlert(session, "alert_wf2", "abs_cor_threshAlert_wf2", title = "Argument Input Error", content = "'Absolute correlation threshold' argument can't be empty.", append = TRUE)
      checkabs_cor_threshAlert_wf2 <- FALSE
    } else if (input$abs_cor_thresh_wf2<0 | input$abs_cor_thresh_wf2>1) {
      closeAlert(session, "abs_cor_threshAlert_wf2")
      createAlert(session, "alert_wf2", "abs_cor_threshAlert_wf2", title = "Argument Input Error", content = "'Absolute correlation threshold' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkabs_cor_threshAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "abs_cor_threshAlert_wf2")
      checkabs_cor_threshAlert_wf2 <- TRUE
    }
    
    
    if(is.na(input$cor_fdrthresh_wf2)){
      closeAlert(session, "abs_cor_threshAlert_wf2")
      createAlert(session, "alert_wf2", "cor_fdrthreshAlert_wf2", title = "Argument Input Error", content = "'FDR threshold for correlation analysis' argument can't be empty.", append = TRUE)
      checkcor_fdrthreshAlert_wf2 <- FALSE
    } else if (input$cor_fdrthresh_wf2<0 | input$cor_fdrthresh_wf2>1) {
      closeAlert(session, "abs_cor_threshAlert_wf2")
      createAlert(session, "alert_wf2", "cor_fdrthreshAlert_wf2", title = "Argument Input Error", content = "'FDR threshold for correlation analysis' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkcor_fdrthreshAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "abs_cor_threshAlert_wf2")
      checkcor_fdrthreshAlert_wf2 <- TRUE
    }
    
    
    if(is.na(input$prop_select_thresh_wf2)){
      closeAlert(session, "prop_select_threshAlert_wf2")
      createAlert(session, "alert_wf2", "prop_select_threshAlert_wf2", title = "Argument Input Error", content = "'Min. proportion of subsets in which a variable is selected' argument can't be empty.", append = TRUE)
      checkprop_select_threshAlert_wf2 <- FALSE
    } else if (input$prop_select_thresh_wf2<0 | input$prop_select_thresh_wf2>1) {
      closeAlert(session, "prop_select_threshAlert_wf2")
      createAlert(session, "alert_wf2", "prop_select_threshAlert_wf2", title = "Argument Input Error", content = "'Min. proportion of subsets in which a variable is selected' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checkprop_select_threshAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "prop_select_threshAlert_wf2")
      checkprop_select_threshAlert_wf2 <- TRUE
    }
    
    
    if(is.na(input$train_pct_wf2)){
      closeAlert(session, "train_pctAlert_wf2")
      createAlert(session, "alert_wf2", "train_pctAlert_wf2", title = "Argument Input Error", content = "'Proportion of samples to use for training' argument can't be empty.", append = TRUE)
      checktrain_pctAlert_wf2 <- FALSE
    } else if (input$train_pct_wf2<0 | input$train_pct_wf2>1) {
      closeAlert(session, "train_pctAlert_wf2")
      createAlert(session, "alert_wf2", "train_pctAlert_wf2", title = "Argument Input Error", content = "'Proportion of samples to use for training' argument should be not smaller than 0 or larger than 1.", append = TRUE)
      checktrain_pctAlert_wf2 <- FALSE
    } else {
      closeAlert(session, "train_pctAlert_wf2")
      checktrain_pctAlert_wf2 <- TRUE
    }
    
    # if(is.na(input$pca_cex_val_wf2)){
    #   closeAlert(session, "pca_cex_valAlert_wf2")
    #   createAlert(session, "alert_wf2", "pca_cex_valAlert_wf2", title = "Argument Input Error", content = "'Size of points on PCA plots' argument can't be empty.", append = TRUE)
    #   checkpca_cex_valAlert_wf2 <- FALSE
    # } else if (input$pca_cex_val_wf2<1 | input$pca_cex_val_wf2>20) {
    #   closeAlert(session, "pca_cex_valAlert_wf2")
    #   createAlert(session, "alert_wf2", "pca_cex_valAlert_wf2", title = "Argument Input Error", content = "'Size of points on PCA plots' argument should be not smaller than 1 or larger than 20.", append = TRUE)
    #   checkpca_cex_valAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "pca_cex_valAlert_wf2")
    #   checkpca_cex_valAlert_wf2 <- TRUE
    # }
    
    # if(is.na(input$ellipse_conf_level_wf2)){
    #   closeAlert(session, "ellipse_conf_levelAlert_wf2")
    #   createAlert(session, "alert_wf2", "ellipse_conf_levelAlert_wf2", title = "Argument Input Error", content = "'Confidence interval for PCA ellipses' argument can't be empty.", append = TRUE)
    #   checkellipse_conf_levelAlert_wf2 <- FALSE
    # } else if (input$ellipse_conf_level_wf2<0 | input$ellipse_conf_level_wf2>1) {
    #   closeAlert(session, "ellipse_conf_levelAlert_wf2")
    #   createAlert(session, "alert_wf2", "ellipse_conf_levelAlert_wf2", title = "Argument Input Error", content = "'Confidence interval for PCA ellipses' argument should be not smaller than 0 or larger than 1.", append = TRUE)
    #   checkellipse_conf_levelAlert_wf2 <- FALSE
    # } else {
    #   closeAlert(session, "ellipse_conf_levelAlert_wf2")
    #   checkellipse_conf_levelAlert_wf2 <- TRUE
    # }
    
    #checknumreplicateAlert_wf2,checksummarization_ratioAlert_wf2,checkall_missing_threshAlert_wf2,checkrsd_filt_listAlert_wf2,checkgroup_missing_threshAlert_wf2,checkpls_ncompAlert_wf2,checkmax_comp_selAlert_wf2,checkpca_cex_valAlert_wf2,checkellipse_conf_levelAlert_wf2
    
    all(checkpvalue_threshAlert_wf2, checkfdrthreshAlert_wf2, checkkfoldAlert_wf2, checkpls_vip_threshAlert_wf2, checkprop_select_threshAlert_wf2,
        checkabs_cor_threshAlert_wf2,checkcor_fdrthreshAlert_wf2, checkmax_var_selAlert_wf2,checktrain_pctAlert_wf2)
    
  })
  
  
  ##############################################################
  
  observeEvent(input$go_wf2,{check_wf2$count_wf2=0})
  
  observeEvent(input$go_wf2, 
               {
                 output$nText2_wf2 <- renderText({shiny::validate(
                   need(input$featuretable_wf2, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                   need(input$featuretable_wf2$type=="text/csv" || input$featuretable_wf2$type=="text/plain", "The format of datasetA is not correct. Please upload the file with correct format."),
                   need(input$classlabel_wf2, "No class label file provided. Please upload class label file in 'Choose Files'."),
                   need(input$classlabel_wf2$type=="text/csv" || input$classlabel_wf2$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format."),
                   need(input$featselmethodi_wf2,"No feature selection method was selected. Please select at least one method.") 
                 )})
                 shiny::validate(
                   need(input$featuretable_wf2, "No datasetA provided. Please upload dataset A in 'Choose Files'."),
                   need(input$featuretable_wf2$type=="text/csv" || input$featuretable_wf2$type=="text/plain", "The format of datasetA is not correct. Please upload the file with correct format."),
                   need(input$classlabel_wf2, "No class label file provided. Please upload class label file in 'Choose Files'."),
                   need(input$classlabel_wf2$type=="text/csv" || input$classlabel_wf2$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format."),
                   need(input$featselmethodi_wf2,"No feature selection method was selected. Please select at least one method.")
                 )
                 check_wf2$count_wf2=1
                 id1_wf2 <<- showNotification("Starting processing now. Your results will be available for download shortly. The processing time depends on the number of methods you used.", duration=NULL)
                 
               })
  
  
  #########################################
  
  
  featuretable_wf2 <- reactive({
    if(input$go_wf2!=0 & check_wf2$count_wf2==1 & !is.null(input$featuretable_wf2$name) ){
      
      if((input$featuretable_wf2$type=="text/csv" || input$featuretable_wf2$type=="text/plain")){
        req(input$featuretable_wf2)
        if(input$featuretable_wf2$type=="text/plain"){
          featuretable_wf2 <- read.delim(input$featuretable_wf2$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$featuretable_wf2$type=="text/csv"){
            featuretable_wf2 <- read.csv(input$featuretable_wf2$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        featuretable_wf2 
      }
    }else{
      
      NA
    }
  })
  
  classlabel_wf2 <- reactive({
    if(input$go_wf2!=0 & check_wf2$count_wf2==1 & !is.null(input$classlabel_wf2$name) ){
      
      if((input$classlabel_wf2$type=="text/csv" || input$classlabel_wf2$type=="text/plain")){
        req(input$classlabel_wf2)
        if(input$classlabel_wf2$type=="text/plain"){
          classlabel_wf2 <- read.delim(input$classlabel_wf2$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$classlabel_wf2$type=="text/csv"){
            classlabel_wf2 <- read.csv(input$classlabel_wf2$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        classlabel_wf2 
      }
    }else{
      
      NA
    }
  })
  
  session_outloc_wf2 <- reactive({
    if(input$go_wf2!=0 & check_wf2$count_wf2==1){
      cur_date_wf2<-Sys.time()
      cur_date_wf2<-gsub(x=cur_date_wf2,pattern="-",replacement="")
      cur_date_wf2<-gsub(x=cur_date_wf2,pattern=":",replacement="")
      cur_date_wf2<-gsub(x=cur_date_wf2,pattern=" ",replacement="")
      if(input$outloc_wf2==""){
        outloc_wf2<-paste('~/xmsPANDAresults',cur_date_wf2,sep="")
      }else{
        outloc_wf2<-paste('~/',input$outloc_wf2,cur_date_wf2,sep="")
      }
      outloc_wf2
    }else{
      NULL
    }
  })
  
  ##########################################
  
  output$nText_wf2 <- renderText({
    if(input$go_wf2!=0  & check_wf2$count_wf2==1 & !is.null(input$featselmethodi_wf2) & is.data.frame(featuretable_wf2()) & is.data.frame(classlabel_wf2()) & all_alert_wf2()==TRUE){
      
      
      if(input$globalcor_wf2 == 'TRUE'){
        globalcor_wf2=TRUE
      }else{
        globalcor_wf2=FALSE
      }
      
      if(globalcor_wf2==TRUE){
        abs_cor_thresh_wf2=input$abs_cor_thresh_wf2
        cor_fdrthresh_wf2=input$cor_fdrthresh_wf2
        cor_method_wf2=input$cor_method_wf2
        networktype_wf2=input$networktype_wf2
      }else{
        abs_cor_thresh_wf2=0.4
        cor_fdrthresh_wf2=0.2
        cor_method_wf2="spearman"
        networktype_wf2="complete"
      }
      
      if(input$balance_classes_wf2=='TRUE'){
        balance_classes_wf2=TRUE
      }else{
        balance_classes_wf2=FALSE
      }
      
      if(input$split_train_test_wf2==TRUE){
        split_train_test_wf2=TRUE
      }else{
        split_train_test_wf2=FALSE
      }
      
      if(input$boxplot_jitter_wf2=='TRUE'){
        boxplot_jitter_wf2=TRUE
      }else{
        boxplot_jitter_wf2=FALSE
      }
      
      if(input$alphabetical_order_wf2=='TRUE'){
        alphabetical_order_wf2=TRUE
      }else{
        alphabetical_order_wf2=FALSE      
      }
      
      check_pvalue_thresh_wf2<-reactive({need(input$pvalue_thresh_wf2,"error")})
      if(is.null(check_pvalue_thresh_wf2())){
        pvalue_thresh_wf2=input$pvalue_thresh_wf2
      }else{
        pvalue_thresh_wf2=0.05
      }
      
      check_fdr_method_wf2<-reactive({need(input$fdr_method_wf2,"error")})
      if(is.null(check_fdr_method_wf2())){
        fdr_method_wf2=input$fdr_method_wf2 #gsub(" \\(.*\\)","",input$fdr_method_wf2)
      }else{
        fdr_method_wf2="BH"
      }
      
      check_fdrthresh_wf2<-reactive({need(input$fdrthresh_wf2,"error")})
      if(is.null(check_fdrthresh_wf2())){
        fdrthresh_wf2=input$fdrthresh_wf2
      }else{
        fdrthresh_wf2=0.2
      }
      
      check_kfold_wf2<-reactive({need(input$kfold_wf2,"error")})
      if(is.null(check_kfold_wf2())){
        kfold_wf2=input$kfold_wf2
      }else{
        kfold_wf2=10
      }
      
      check_prop_select_thresh_wf2<-reactive({need(input$prop_select_thresh_wf2,"error")})
      if(is.null(check_prop_select_thresh_wf2())){
        prop_select_thresh_wf2=input$prop_select_thresh_wf2
      }else{
        prop_select_thresh_wf2=0.7
      }
      
      check_train_pct_wf2<-reactive({need(input$train_pct_wf2,"error")})
      if(is.null(check_train_pct_wf2())){
        train_pct_wf2=input$train_pct_wf2
      }else{
        train_pct_wf2=0.7
      }
      
      check_pls_vip_thresh_wf2<-reactive({need(input$pls_vip_thresh_wf2,"error")})
      if(is.null(check_pls_vip_thresh_wf2())){
        pls_vip_thresh_wf2=input$pls_vip_thresh_wf2
      }else{
        pls_vip_thresh_wf2=2
      }
      
      check_max_varsel_wf2<-reactive({need(input$max_var_sel_wf2,"error")})
      if(is.null(check_max_varsel_wf2())){
        max_varsel_wf2=input$max_var_sel_wf2
      }else{
        max_varsel_wf2=100
      }
      
      featselmethod_wf2 <- input$featselmethodi_wf2
      
      check_aggregation_method_wf2<-reactive({need(input$aggregation_method_wf2,"error")})
      if(is.null(check_aggregation_method_wf2())){
        aggregation_method_wf2=input$aggregation_method_wf2 #gsub(" \\(.*\\)","",input$aggregation_method_wf2)
        if(aggregation_method_wf2=='None'){
          aggregation_method_wf2=NA
        }
      }else{
        aggregation_method_wf2=NA
      }
      
      ###################
      ## main function for workflow2
      
      #start: see manual for additional arguments and description
      demetabs_res<-demetabs_res<-diffexp.biomarkers(X=featuretable_wf2(),Y=classlabel_wf2(),feature_table_file=NA,class_labels_file=NA,feat.sel.methods=featselmethod_wf2,
                                                     num.var.sel=max_varsel_wf2,prop.select.thresh=prop_select_thresh_wf2,split.train.test=split_train_test_wf2,
                                                     train.pct=train_pct_wf2,outloc=session_outloc_wf2(),kfold=kfold_wf2,pca.ellipse=TRUE,Xtest=NA,Ytest=NA,rsdthresh=1,
                                                     pls_vip_thresh=pls_vip_thresh_wf2,seedvalue=27611,learningsetmethod=input$learningsetmethod_wf2,confounder.matrix=NA,fdrmethod=fdr_method_wf2,
                                                     fdrthresh=fdrthresh_wf2,num.methods.sel=1,globalcor=globalcor_wf2,cor.method=cor_method_wf2,networktype=networktype_wf2,abs.cor.thresh=abs_cor_thresh_wf2,
                                                     cor.fdrthresh=cor_fdrthresh_wf2,max.cor.num=NA,net_node_colors=c("green","red"), net_legend=TRUE,niter=10,output.device.type="png",
                                                     heatmap.col.opt=input$heatmap_color_scheme_wf2,boxplot.col.opt=input$boxplot_color_theme_wf2,barplot.col.opt=c("grey57","grey90"),sample.col.opt=input$sample_color_theme_wf2,mz.thresh=1,
                                                     time.thresh=10,svm_kernel="radial",good_feats_index=NA,pvalue.thresh=pvalue_thresh_wf2,plots.width=8,plots.height=8,plots.res=600, 
                                                     plots.type="cairo",num_nodes=detectCores(),ylabel=input$ylabel_text_wf2,cex.plots=0.7,tune_classifiers=FALSE,find.common.features=FALSE,
                                                     aggregation.method=aggregation_method_wf2,aggregation.max.iter=1000,add.pvalues=TRUE,add.jitter=boxplot_jitter_wf2,
                                                     alphabetical.order=alphabetical_order_wf2,boxplot.type=input$boxplot_type_wf2,balance.classes=balance_classes_wf2)
      
      
      done_wf2$count_wf2=1
      setwd(session_outloc_wf2())
      zip(zipfile=paste(basename(session_outloc_wf2()),'zip',sep='.'), files='.')
      print("Processing complete. Please click on download button to save the results.")
      
    }else{
      
      NULL
    }
  })
  
  ##########################################
  
  observeEvent({if(done_wf2$count_wf2==1) TRUE else return()},{
    if (!is.null(id1_wf2)){
      removeNotification(id1_wf2)
      id1_wf2 <<- NULL
    }
    
    # if(length(input$featselmethodi_wf2)>1 & !input$aggregation_method_wf2=="None"){
    #   featselmethodout_wf2 <- c('AggregatedResults',input$featselmethodi_wf2)
    # }else{
    #   featselmethodout_wf2 <- input$featselmethodi_wf2
    # }
    
    # output$output_results_wf2 <- renderUI({
    #   
    #   column(12,
    #          column(12,style='padding-top:10px;padding-left:0;',tags$div(h4("Output"))),
    #          column(8,align='center',style="display:block; margin-left: auto;margin-right:auto;",
    #                 imageOutput("myImage_wf2",width="400px",height="400px",inline=TRUE)
    #          ),
    #          column(4,
    #                 div(style='margin-bottom:40px;', selectInput(width="250px","methodout_wf2","Chooso method to display figures:",featselmethodout_wf2)),
    #                 uiOutput("figureradio_wf2")
    #          )
    #   )
    #   
    # })
    
    l1_wf2 <- list.files(paste(session_outloc_wf2(),'Figures',sep="/"),".png",recursive=TRUE,full.names=FALSE)
    figurenum_wf2 <- paste('Figure',seq(1:length(l1_wf2)))
    
    if(length(l1_wf2)>=1){
      output$figureradio_wf2 <- renderUI({
        div(
          div(style='margin-bottom:10px;',tags$label("Figure Choices", `for` = "figure_choices_wf2" )),
          awesomeRadio(inputId = "figure_choices_wf2",label = NULL, choices = figurenum_wf2, selected = "Figure 1", status = "primary")
        )
      })
      
      output$myImage_wf2 <- renderImage({
        
          req(input$figure_choices_wf2)
          filename_wf2 <- normalizePath(file.path(paste(paste(session_outloc_wf2(),'Figures',sep='/'),sep="/"),l1_wf2[as.numeric(gsub('Figure','',input$figure_choices_wf2))]))
        
        list(src = filename_wf2,width=600,height=600,
             alt = "This is an image")
        
      }, deleteFile = FALSE)
    }
    
  })
  
  # observeEvent(input$methodout_wf2,{
  #   
  #   if(input$methodout_wf2=="AggregatedResults"){
  #     l1_wf2 <- list.files(paste(session_outloc_wf2(),'AggregatedResults',sep="/"),".png",recursive=TRUE,full.names=FALSE)
  #     figurenum_wf2 <- paste('Figure',seq(1:length(l1_wf2)))
  #   }else{
  #     folder_wf2 <- grep(input$methodout_wf2,list.dirs(paste(session_outloc_wf2(),'Stage2',sep='/'),recursive=FALSE,full.names=FALSE),value=TRUE)
  #     l1_wf2 <- list.files(paste(session_outloc_wf2(),'Stage2',folder_wf2,sep="/"),".png",recursive=TRUE,full.names=FALSE)
  #     figurenum_wf2 <- paste('Figure',seq(1:length(l1_wf2)))
  #   }
  #   
  #   if(length(l1_wf2)>=1){
  #     output$figureradio_wf2 <- renderUI({
  #       div(
  #         div(style='margin-bottom:10px;',tags$label("Figure Choices", `for` = "figure_choices_wf2" )),
  #         awesomeRadio(inputId = "figure_choices_wf2",label = NULL, choices = figurenum_wf2, selected = "Figure 1", status = "primary")
  #       )
  #     })
  #   }
  #   
  #   
  #   if(!is.null(input$methodout_wf2) & length(l1_wf2)>=1){
  #     
  #     
  #     output$myImage_wf2 <- renderImage({
  #       
  #       if(input$methodout_wf2=="AggregatedResults"){
  #         req(input$figure_choices_wf2)
  #         filename_wf2 <- normalizePath(file.path(paste(session_outloc_wf2(),'AggregatedResults',sep='/'),l1_wf2[as.numeric(gsub('Figure','',input$figure_choices_wf2))]))
  #       }else{
  #         req(input$figure_choices_wf2)
  #         filename_wf2 <- normalizePath(file.path(paste(paste(session_outloc_wf2(),'Stage2',sep='/'),folder_wf2,sep="/"),l1_wf2[as.numeric(gsub('Figure','',input$figure_choices_wf2))]))
  #       }
  #       
  #       list(src = filename_wf2,width=600,height=600,
  #            alt = "This is an image")
  #       
  #     }, deleteFile = FALSE)
  #     
  #   }
  #   
  # })
  
  
  output$downloadData_wf2 <- downloadHandler(
    
    #if(input$go!=0 && input$featselmethod!="-" && input$feature_table_file!="" && input$class_labels_file!=""){
    
    filename <- function() {
      paste(basename(session_outloc_wf2()), "zip", sep=".")
    },
    content <- function(file_wf2) {
      fname1_wf2<-paste(session_outloc_wf2(),"/",basename(session_outloc_wf2()), ".zip", sep="")
      file.copy(fname1_wf2, file_wf2)
    },
    contentType = "application/zip"
    #}
  )
  
  #####
  
  ##################################  Help Page #################################################  
  
  example_feat <- read.delim("example_data/feature_table_one_or_two_factor_analysis.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_feat <- example_feat[1:12,1:7]
  colnames(example_feat) <- c(colnames(example_feat)[1:6],"...")
  output$example_feat <- renderTable({ example_feat }, striped = TRUE) 
  
  example_multiclass_comparison <- read.delim("example_data/classlabels_multiclass_comparison.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_multiclass_comparison <- rbind(head(example_multiclass_comparison[example_multiclass_comparison$Factor1=="Group1",],8),head(example_multiclass_comparison[example_multiclass_comparison$Factor1=="Group2",],8))
  example_multiclass_comparison_covariates <- read.delim("example_data/classlabels_with_covariates.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_multiclass_comparison_covariates <- rbind(head(example_multiclass_comparison_covariates[example_multiclass_comparison_covariates$Class=="NonSmoker",],8),head(example_multiclass_comparison_covariates[example_multiclass_comparison_covariates$Class=="Smoker",],8))
  example_regression <- read.delim("example_data/classlabels_regression.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_regression <- example_regression[1:16,]
  example_two_way_anova <- read.delim("example_data/classlabels_two_way_anova.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_two_way_anova <- rbind(head(example_two_way_anova[example_two_way_anova$Factor1=="Group1",],8),head(example_two_way_anova[example_two_way_anova$Factor1=="Group2",],8))
  example_one_factor_repeatedmeasures <- read.delim("example_data/classlabels_one_factor_repeatedmeasures.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_one_factor_repeatedmeasures <- example_one_factor_repeatedmeasures[1:16,]
  example_two_factor_repeatedmeasures <- read.delim("example_data/classlabels_two_factor_repeatedmeasures.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
  example_two_factor_repeatedmeasures <- example_two_factor_repeatedmeasures[1:16,]
  
  
  
  output$example_classlabel_text <- renderUI({
    txt <- switch(input$classlabel_option, 
                  'multiclass comparison' = tags$p(style="text-align:justify;font-size: 15px;","To do multiclass comparison, the class labels file should include two columns. The first column is the the sample ID (or filename) which should correspond to the column name of each sample in feature table. The second column is the factor1 (or class1) which contains the group information."),
                  'multiclass comparison with covariates'= tags$p(style="text-align:justify;font-size: 15px;","To do multiclass comparison adjusted for some covariates, the class labels file should include sampleID, group information and covariates. The first column is the the sample ID (or filename) which should correspond to the column name of each sample in feature table. The second column is the class which contains the group information. The remaing columns should be covariates information."),
                  'regression' = tags$p(style="text-align:justify;font-size: 15px;","To do the regression analysis (adjusted for covariates), the class labels file should include at least two columns. The first column is the the sample ID (or filename) which should correspond to the column name of each sample in feature table. The second column is the response variable which should be numeric variable. If you want to adjust for some covariates, put the covariates information after the first 2 columns."),
                  'two-way anova' = tags$p(style="text-align:justify;font-size: 15px;","To do the two-way ANOVA analysis, the class labels file should include three columns. The first column is the the sample ID (or filename) which should correspond to the column name of each sample in feature table. The second column is the factor1 information and the third column is the factor2 information."),
                  'one factor repeatedmeasures' = tags$p(style="text-align:justify;font-size: 15px;","To do the one-way ANOVA analysis with repeated-measurement data, the class labels file should include three columns. The first column is the the sample ID (or filename) which should correspond to the column name of each sample in feature table. The second column is the subject ID and the third column is the factor1 which contains the group information."),
                  'two factor repeatedmeasures' = tags$p(style="text-align:justify;font-size: 15px;","To do the two-way ANOVA analysis with repeated-measurement data, the class labels file should include four columns. The first column is the the sample ID (or filename) which should correspond to the column name of each sample in feature table. The second column is the subject ID. The third and the fourth column are the factor1 and factor2 information.")
    )
    txt
  })
  
  output$example_classlabel_table <- renderTable({
    tb <- switch(input$classlabel_option, 
                 'multiclass comparison' = example_multiclass_comparison,
                 'multiclass comparison with covariates'= example_multiclass_comparison_covariates,
                 'regression' = example_regression,
                 'two-way anova' = example_two_way_anova,
                 'one factor repeatedmeasures' = example_one_factor_repeatedmeasures,
                 'two factor repeatedmeasures' = example_two_factor_repeatedmeasures
    )
    tb
  }, striped = TRUE) 
  
} 
