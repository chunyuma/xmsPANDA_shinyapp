library(shiny)

library(shiny)
source("R/workflow2/input_wf2.R")
source("R/workflow2/data_preprocessing_wf2.R")
source("R/workflow2/method_selection_wf2.R")
source("R/workflow2/network_analysis_wf2.R")
source("R/workflow2/graphical_options_wf2.R")

# Define UI for data upload app ----

workflow2_page <- fluidPage(
  
  
  # Make the Layout of parameter ----
  column(12,style="padding-top:10px;",navlistPanel("Input Files", tabPanel("Choose Files (see help and support)", input_wf2), "Parameter Settings for analysis",
                                                   tabPanel("1. Data preprocessing", data_preprocessing_wf2),
                                                   tabPanel("2. Method selection",method_selection_wf2),
                                                   tabPanel("3. Network analysis",network_analysis_wf2),
                                                   tabPanel("4. Graphical options",graphical_options_wf2),
                                                   widths = c(3, 9)
  )),
  column(12,style='padding-left:0px;margin-bottom:20px;',
         mainPanel(style='padding-left:0px;',
                   verbatimTextOutput("nText2_wf2"),
                   verbatimTextOutput("nText_wf2"),
                   bsAlert("alert_wf2"),
                   
                   actionButton("go_wf2","Start processing",icon=icon("play-circle")),
                   downloadButton("downloadData_wf2", label = "Download results")
         )),
  #column(12,style='padding-top:5px;padding-left:0;',tags$div(h4("Output"))),
  uiOutput("output_results_wf2")
  
)
