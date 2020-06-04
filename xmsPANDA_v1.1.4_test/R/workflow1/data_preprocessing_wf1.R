library(shiny)

data_preprocessing_wf1 <-fluidRow(
  tags$div(
    id="maindiv",
    column(width=12,
           mainPanel(
             width=12,
             bsCollapse(open = "Replicate summarization",
                        
                        bsCollapsePanel("Replicate summarization", 
                                        column(width=6,numericInput(width="350px","numreplicate_wf1", "Number of technical replicates (1-10 limit):", 1, min = 1, max = 10)),
                                        column(width=6,selectInput(width="350px","summarization_method_wf1","Choose a replicate summarization method:",c("median","mean"))),
                                        column(width=6,radioButtons("use_summarizion_wf1", "Should the replicates be summarized?", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "TRUE")),
                                        column(width=6,numericInput(width="350px","summarization_ratio_wf1", "Maximum missing value ratio:", 0.3, min = 0, max = 1)),
                                        bsTooltip("summarization_ratio_wf1", "What propotion of replicates are allowed to have missing values during the averaging or median summarization step of each biological sample?","bottom"),
                                        style = "primary"),
                        
                        bsCollapsePanel("Filtering",
                                        column(width=6,numericInput(width="350px","all_missing_thresh_wf1", "Minimum non-missing sample ratio:", 0.5, min = 0, max = 1)),
                                        bsTooltip("all_missing_thresh_wf1", "What propotion of total number of samples should have an intensity?","bottom"),
                                        column(width=6,numericInput(width="350px","rsd_filt_list_wf1", "Minimum overall variance:", 0, min = 0, max = 100)),
                                        bsTooltip("rsd_filt_list_wf1", "Minimum relative standard deviation across all samples","bottom"),
                                        column(width=6,numericInput(width="350px","group_missing_thresh_wf1", "Minimum non-missing sample ratio for group:", 0.8, min = 0, max = 1)),
                                        bsTooltip("group_missing_thresh_wf1", "Minimum propotion of samples in at least one group in which a non-missing signal value should be present","bottom"),
                                        style = "primary"),
                        
                        bsCollapsePanel("Imputation, transformation, and normalization",
                                        column(width=6,selectInput(width="350px","summary_na_replacement_wf1","Choose an imputation method:",c("halffeaturemin","zeros","halfsamplemin","halfdatamin","none"))),
                                        bsTooltip("summary_na_replacement_wf1", "How should the missing values be represented?","bottom"),
                                        column(width=6,selectInput(width="350px","normmethod_wf1","Normalization method:",c("No normalization"="none","log2 and quantile normalization"="log2quantilenorm",
                                                                                                                        "log2 transformation"="log2transform",
                                                                                                                        "auto-scaling"="znormtransform",
                                                                                                                        "lowess normalization"="lowess_norm",
                                                                                                                        "quantile normalization"="quantile_norm",
                                                                                                                        "range scaling"="rangescaling",
                                                                                                                        "paretoscaling"="paretoscaling",
                                                                                                                        "most useful total signal"="mstus",
                                                                                                                        "EigenMS normalizaiton"="eigenms_norm",
                                                                                                                        "Variance stabilizing normalization"="vsn_norm",
                                                                                                                        "Surrogate variable analysis"="sva_norm",
                                                                                                                        "Total ion intensity normalization"="tic_norm",
                                                                                                                        "cubicspline normalization"="cubicspline_norm"))),
                                        style = "primary")
             ))))
)
