library(shiny)

network_analysis_wf1 <-fluidRow(
  tags$div(
    id="maindiv",
    column(12,radioButtons("globalcor_wf1", "3.1 Perform correlation-based network analysis of selected features:", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE")),
    conditionalPanel(
      condition = "input.globalcor_wf1 == 'TRUE'",
      column(width=12, 
             id="inputarea",
             column(width=6,numericInput(width="350px","abs_cor_thresh_wf1", "Absolute correlation threshold:", 0.4, min = 0, max = 1)),
             column(width=6,numericInput(width="350px","cor_fdrthresh_wf1", "FDR threshold for correlation analysis:", 0.05, min = 0, max = 1)),
             column(width=6,selectInput(width="350px","cor_method_wf1","Correlation method:",c("spearman","pearson"))),
             column(width=6,selectInput(width="350px","networktype_wf1","Network type:",c("complete","GGM")))
      )
    ),
    column(width=12,radioButtons("WGCNAmodules_wf1", "3.2 Perform WGCNA module preservation analysis:", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE")),
    column(width=12,radioButtons("globalclustering_wf1", "3.3 Perform global clustering analysis (HCA and EM clustering):", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE"))
  )
)