library(shiny)

network_analysis_wf2 <-fluidRow(
  tags$div(
    id="maindiv",
    column(12,radioButtons("globalcor_wf2", "Perform correlation-based network analysis of selected features:", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE")),
    conditionalPanel(
      condition = "input.globalcor_wf2 == 'TRUE'",
      column(width=12, 
             id="inputarea",
             column(width=6,numericInput(width="350px","abs_cor_thresh_wf2", "Absolute correlation threshold:", 0.4, min = 0, max = 1)),
             column(width=6,numericInput(width="350px","cor_fdrthresh_wf2", "FDR threshold for correlation analysis:", 0.05, min = 0, max = 1)),
             column(width=6,selectInput(width="350px","cor_method_wf2","Correlation method:",c("spearman","pearson"))),
             column(width=6,selectInput(width="350px","networktype_wf2","Network type:",c("complete","GGM")))
      )
    )
  )
)