library(shiny)

input_wf2<-fluidRow(
         tags$div(
           id="maindiv",
           column(width=12,
           h4("Choose your Files:"),
           column(width=6, 
                  id="inputarea",
                  fileInput("featuretable_wf2", "Select feature table file ('.csv' or .txt', 100MB limit)",
                            multiple = FALSE,
                            width="350px",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".txt")),
                  tags$div(
                    column(width=6,style="margin-left:0;margin-right:0;",radioButtons("input_intensity_scale_wf2", "input intensity scale:", inline=TRUE,c("raw"= "raw","log2"="log2"),selected = "raw")),
                    bsTooltip("input_intensity_scale_wf2", "Are the intensities in the input feature table at raw scale or log2 scale?","bottom", options = list(container = "body")),
                    column(width=6,radioButtons("missing_val_wf2", "missing value notation:", inline=TRUE,c("0"= "0","NA"="NA"),selected = "0")),
                    bsTooltip("missing_val_wf2", "How are the missing values represented in the input data?","bottom", options = list(container = "body"))
                  )
           ),
           column(width=6, 
                  id="inputarea",
                  fileInput("classlabel_wf2", "Select class label file ('.csv' or '.txt', 100MB limit)",
                            multiple = FALSE,
                            width="350px",
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".txt"))
           ),
          column(style="padding:0px;",12,tags$hr(style="border-top: 1px solid #000000;")),
          column(width=6, 
                 id="inputarea",
          textInput(width="350px","outloc_wf2", "Output folder name:","",placeholder="Default: xmsPANDAresults")
          )
  ))
)
