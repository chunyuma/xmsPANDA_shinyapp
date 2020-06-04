library(shiny)

library(shiny)
source("R/workflow1/input_wf1.R")
source("R/workflow1/data_preprocessing_wf1.R")
source("R/workflow1/method_selection_wf1.R")
source("R/workflow1/network_analysis_wf1.R")
source("R/workflow1/graphical_options_wf1.R")

# Define UI for data upload app ----

workflow1_page <- fluidPage(
  
  
  # Make the Layout of parameter ----
  column(12,style="padding-top:10px;",navlistPanel("Input Files", tabPanel("Choose Files (see help and support)", input_wf1), "Parameter Settings for analysis",
                                                   tabPanel("1. Data preprocessing", data_preprocessing_wf1),
                                                   tabPanel("2. Method selection",method_selection_wf1),
                                                   tabPanel("3. Network analysis",network_analysis_wf1),
                                                   tabPanel("4. Graphical options",graphical_options_wf1),
                                                   widths = c(3, 9)
  )),
  column(12,style='padding-left:0px;margin-bottom:20px;',
         mainPanel(style='padding-left:0px;',width = 12,
                   verbatimTextOutput("nText2_wf1"),
                   verbatimTextOutput("nText_wf1"),
                   bsAlert("alert_wf1"),
                   
                   actionButton("go_wf1","Start processing",icon=icon("play-circle")),
                   downloadButton("downloadData_wf1", label = "Download results")
         )),
  #column(12,style='padding-top:5px;padding-left:0;',tags$div(h4("Output"))),
  uiOutput("output_results_wf1")
  
)
