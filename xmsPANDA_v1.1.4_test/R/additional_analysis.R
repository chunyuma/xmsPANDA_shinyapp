library(shiny)
library(plotly)
library(plyr)
library(ggpubr)
library(DT)
library(shinycssloaders)
library(colourpicker)
library(R.utils)
library(shinyWidgets)

# Define UI for data upload app ----

additional_analysis_page <- fluidPage(
  
  # Make the Layout of parameter ----
    tags$div(
      id="maindiv",
      style='margin-top:12px',
      bsCollapse(open = "Interactive Analysis: Scatter Plot & Box Plot",
       bsCollapsePanel("Interactive Analysis: Scatter Plot & Box Plot", 
         column(width=12,
             column(width=4, 
                    id="inputarea",
                    fileInput("feat_inte", "Select feature table file ('.csv' or .txt', 100MB limit)",
                              multiple = FALSE,
                              width="350px",
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".txt"))
             ),
             column(width=4, 
                    id="inputarea",
                    fileInput("classlabel_inte", "Select class label file ('.csv' or '.txt', 100MB limit)",
                              multiple = FALSE,
                              width="350px",
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".txt"))
             ),
             column(width=4,
                    style='margin-top:24px',
                    actionButton("optionbutton1", "More options"),
                    bsModal("moreoptions1", "More options for figure", "optionbutton1", size = "large",
                     tags$div(
                       width=12,
                       style="height:400px",
                       column(width=12,selectInput(width="350px","diagramtype","Manhattan Plot or Volcano Plot?",c("manhattan","volcano"))),
                       column(style="padding:0px;",12,tags$hr(style="border-top: 1px solid #000000;")),
                       conditionalPanel(
                         condition = "input.diagramtype=='manhattan'",
                         column(width=6,selectInput(width="350px","yaxislabel","Is your data P-value or VIP?",c("pvalue","vip"))),
                         column(width=6,selectInput(width="350px","plottype","What type of manhattan plot?",c("type1","type2"))),
                         column(width=6,selectInput(width="350px","labelexpression","Different colors to tell expression by group?",c("yes","no"))),
                         column(width=6,selectInput(width="350px","boxplotcolor","Is boxplot colored?",c("yes","no"))),
                         column(width=6,conditionalPanel(
                           condition = "input.yaxislabel=='pvalue'",
                           numericInput(width="350px","pvaluecutoff", "Threshold for p-value  (0-1 limit):", 0.05, min = 0, max = 1)
                         )),
                         column(width=6,conditionalPanel(
                           condition = "input.yaxislabel=='pvalue'",
                           selectInput(width="350px","adjdashline","Draw dotted line for adjusted p-value?",c("no","yes"))
                         )),
                         column(width=6,conditionalPanel(
                           condition = "input.adjdashline=='yes' & input.yaxislabel=='pvalue'",
                           numericInput(width="350px","adjpvaluecutoff", "Threshold for adjusted p-value  (0-1 limit):", 0.2, min = 0, max = 1)
                         )),
                         column(width=6,conditionalPanel(
                           condition = "input.adjdashline=='yes' & input.yaxislabel=='pvalue'",
                           selectInput(width="350px","psignif","Based on which p-value to define significance?",c("pvalue","adjustedpvalue"))
                         )),
                         column(width=6,conditionalPanel(
                           condition = "input.yaxislabel=='vip'",
                           numericInput(width="350px","vipcutoff", "Threshold for VIP  (1-10 limit):", 2, min = 1, max = 10)
                         ))
                       ),
                       conditionalPanel(
                         condition = "input.diagramtype=='volcano'",
                         column(width=6,numericInput(width="350px","volcanopcutoff","Threshold for p-value  (0-1 limit):", 0.05 , min = 0, max = 1)),
                         column(width=6,numericInput(width="350px","volcanolfc","Left side threshold for fold change (1-10 limit):", 0.5 , min = 0.1, max = 1)),
                         column(width=6,numericInput(width="350px","volcanorfc","Right side threshold for fold change  (1-10 limit):", 2 , min = 1, max = 10))
                       )
                     )
                    ),
                    actionButton("start1","Start processing",icon=icon("play-circle"))
             )
        ),
      column(width=12,verbatimTextOutput("nText3")),
      column(width=12, style="padding-left:25px;padding-right:25px;",
             conditionalPanel(condition = "!(output.checkplot1 || output.checkplot2)",
                              tags$div(tags$h5(style='font-weight:bold',"Input Files:")),
                              tags$p(style='text-align:justify;font-weight:bold',"Both feature table file and class label file can be found from xmsPANDA statistical analysis results. Once you finish statistical analysis, you will see Stage1 and Stage2 folder in the analysis results. 
                                     You can find the feature table file under Stage2 folder where there is one file called '[method]results_allfeatures.txt', e.g. limmaresults_allfeatures.txt. You can find the class label file under Stage1 folder where there is one
                                     file called 'ordered_classlabels_file.txt'.")
             )
      ),     
      column(width=12,
             style='margin-bottom:10px',
             column(width=6,
                    conditionalPanel(condition = "output.checkplot1",
                                     withSpinner(plotlyOutput("scatterplot"),type=7)
                    )
             ),
             column(width=6,
                    conditionalPanel(condition = "output.checkplot2",
                                     withSpinner(plotlyOutput("boxplot"),type=7)
                    )
             )
      ),
      column(width=12,
             style='margin-bottom:10px',
             column(width=6,
               style='text-align:center',
               conditionalPanel(condition = "output.checkplot1", 
                                "mz: ",tags$input(id='mzvalue',type='text',style='width:80px;height:30px'),
                                'time:',tags$input(id='timevalue',type='text',style='width:50px;height:30px'),
                                actionButton("search","Search",icon=icon("search")),
                                actionButton("colorbutton", "Select Color"),
                                bsModal("coloroptions", "Colors for scatter plot", "colorbutton", size = "large",
                                        tags$div(
                                          width=12,
                                          style='text-align:left;height:500px;',
                                          conditionalPanel(
                                            condition = "input.diagramtype=='manhattan'",
                                            column(width=6,colourpicker::colourInput("insigcol", "Insignificant points:", "black", showColour = "background")),
                                            column(width=6,colourpicker::colourInput("dashedlinecol", "Dashed line for p-value/vip:", "blue", showColour = "background")),
                                            column(width=6,conditionalPanel(condition = "input.adjdashline=='yes' & input.yaxislabel=='pvalue'",
                                                                            colourpicker::colourInput("dottedlinecol", "Dotted line for adjusted p-value:", "green", showColour = "background")
                                            )),                             
                                            column(width=6,conditionalPanel(condition = "input.labelexpression=='no'",
                                                                            colourpicker::colourInput("sigcol", "Significant points:", "red", showColour = "background")
                                            )),
                                            column(width=6,conditionalPanel(condition = "input.labelexpression=='yes'",
                                                                            colourpicker::colourInput("poscol", "Positive significant points:", "red", showColour = "background")
                                            )),
                                            column(width=6,conditionalPanel(condition = "input.labelexpression=='yes'",
                                                                            colourpicker::colourInput("negcol", "Negative significant points:", "blue", showColour = "background")
                                            ))
                                          ),
                                          conditionalPanel(
                                            condition = "input.diagramtype=='volcano'",
                                            column(width=6,colourpicker::colourInput("volcanoinsigcol", "Insignificant points:", "black", showColour = "background")),
                                            column(width=6,colourpicker::colourInput("volcanoposcol", "Positive significant points:", "red", showColour = "background")),
                                            column(width=6,colourpicker::colourInput("volcanonegcol", "Negative significant points:", "blue", showColour = "background")),
                                            column(width=6,colourpicker::colourInput("volcanopcutoffdashed", "Dashed line for p-value:", "red", showColour = "background")),
                                            column(width=6,colourpicker::colourInput("volcanolfcdashed", "Left side dashed line:", "red", showColour = "background")),
                                            column(width=6,colourpicker::colourInput("volcanorfcdashed", "Right side dashed line:", "red", showColour = "background"))
                                          )
                                        )
                                ))
               ),
             column(width=6,
               style='text-align:center',
               conditionalPanel(condition = "output.checkplot2",
                                actionButton("statbutton", "Statistics Tests"),
                                bsModal("pairwisecomp", "Pairwise Comparison", "statbutton", size = "large",tags$div(width=12, style="output.tblen",DTOutput('boxplottbl'))))
               )
      ),
      column(width=12,
             style='margin-bottom:10px',
             column(width=6,
                    style='text-align:center',
                    conditionalPanel(condition = "output.checkplot1",
                                     downloadButton("downloadPlot1", label = "Download scatterplot", style='color: #fff; background-color: #337ab7'))
                                     
             ),
             column(width=6,
                    style='text-align:center',
                    conditionalPanel(condition = "output.checkplot1",
                                     downloadButton("downloadPlot2", label = "Download boxplot", style='color: #fff; background-color: #337ab7'))
             )
      ),
      style='primary'
    ),
    bsCollapsePanel("Functional Class Scoring",
      column(width=12,
             column(width=6, 
                    id="inputarea",
                    fileInput("clusterinput", "Select your target metabolite data ('.csv' or .txt')",
                              multiple = FALSE,
                              width="350px",
                              accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
             ),
             column(width=6,
                    style='margin-top:24px',
                    actionButton("optionbutton2", "More options"),
                    bsModal("moreoptions2", "More options for analysis", "optionbutton2", size = "large",
                            tags$div(
                              width=12,
                              style="height:300px",
                              column(width=6,selectInput(width="350px","kegg_species_code","Select a species:",c("Homo sapiens(default)",
                                                                                                                 "Mus musculus",
                                                                                                                 "Pan troglodytes",
                                                                                                                 "Macaca mulatta",
                                                                                                                 "Bos taurus",
                                                                                                                 "Rattus norvegicus",
                                                                                                                 "Danio rerio",
                                                                                                                 "C. elegans",
                                                                                                                 "Drosophila melanogaster"))),
                              column(width=6,selectInput(width="350px","database","Select a KEGG database:",c("pathway(default)","module"))),
                              column(width=6,selectInput(width="350px","type.statistic","Statistic type is p-value?",c("TRUE","FALSE")))
                              
                            )
                    ),
                    actionButton("start2","Start processing",icon=icon("play-circle"))
             )
      ),
      column(width=12,verbatimTextOutput("nText4")),
      column(width=12,verbatimTextOutput("nText5")),
      column(width=12,style="padding-left:25px;padding-right:25px;",
             conditionalPanel(condition = "!output.checktable1",
                              tags$div(tags$h5(style='font-weight:bold',"Input File Format:")),
                              tags$p(style='font-weight:bold',"The input file should have two columns: 1. KEGG ID; 2. Statistic (p-value,vip,beta coefficient,fold change)"),
                              tags$p(style='font-weight:bold',"Example:"),
                              tags$table(tags$tr(tags$th("KEGGID"),tags$th("Statistic")),
                                         tags$tr(tags$td("C17601"),tags$td("0.2368648")),
                                         tags$tr(tags$td("C02648"),tags$td("0.2319177")),
                                         tags$tr(tags$td("C19201"),tags$td("0.8520213"))
                              )
             )
      ),                        
      column(width=12,
             conditionalPanel(condition = "output.checktable1",
                              uiOutput("downloadbutton"),
                              DTOutput('pathwaytb')          
             )
             #tableOutput('pathwaytb')
      ),
      #column(width=12,verbatimTextOutput('hover')),
      style='primary'
    ),
    bsCollapsePanel("Metabolite Quantification Analysis",
      column(width=12,
        column(width=4,
             id="inputarea",
             fileInput("featuretable_file", "Select feature table file ('.csv' or .txt', 100MB limit)",
             multiple = FALSE,
             width="350px",
             accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
        ),
        column(width=4,
             id="inputarea",
             fileInput("classlabel_file", "Select class label file ('.csv' or '.txt')",
                       multiple = FALSE,
                       width="350px",
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".txt"))
        ),
        column(width=4,
             style='margin-top:24px',
             actionButton("optionbutton3", "More options"),
             bsModal("moreoptions3", "More options for analysis", "optionbutton3", size = "large",
                     tags$div(
                       width=12,
                       style= "height:660px",
                       column(width=12, tags$p(style="font-weight:bold;font-size:16px;","Select the step you need to run:")),
                       column(width=12, style='margin-bottom:10px',
                              prettyCheckbox(inputId = "step1",
                                             label = "Step1: Draw distribution plots",
                                             value = TRUE,
                                             thick = TRUE,
                                             shape = "round",
                                             animation = "smooth",
                                             status = "primary",
                                             inline = TRUE),
                              prettyCheckbox(inputId = "step2",
                                             label = "Step2: Quantify concentration",
                                             value = TRUE,
                                             thick = TRUE,
                                             shape = "round",
                                             animation = "smooth",
                                             status = "primary",
                                             inline = TRUE),
                              prettyCheckbox(inputId = "step3",
                                             label = "Step3: Pull the KEGG map from KEGG database",
                                             value = FALSE,
                                             thick = TRUE,
                                             shape = "round",
                                             animation = "smooth",
                                             status = "primary",
                                             inline = TRUE)
                       ),
                       column(width=12,
                              column(width=6,selectInput(width="350px","summarize_replicates","Summarize technical replicate?",c("TRUE","FALSE"))),
                              column(width=6,
                                     conditionalPanel(condition = "input.summarize_replicates == 'TRUE'",
                                                      numericInput(width="350px","num_replicate2", "Number of technical replicates  (1-10 limit):", 3, min = 1, max = 10)
                                     )
                              )
                       ),
                       column(width=12,
                              column(width=6,
                                     conditionalPanel(condition = "input.summarize_replicates == 'TRUE'",
                                                      numericInput(width="350px","rep_max_missing_thresh", "Maximum missing value ratio:", 0.3, min = 0, max = 1)
                                     )
                              ),
                              column(width=6,
                                     conditionalPanel(condition = "input.summarize_replicates == 'TRUE'",
                                                      selectInput(width="350px","summary_method","Choose a replicate summarization method:",c("median","mean"))
                                     )
                              )
                       ),
                       column(width=12,
                              column(width=6,numericInput(width="350px","mass_error", "Mass-to-charge tolerance(ppm)  (0-100 limit):", 10, min = 0, max = 100)),
                              column(width=6,numericInput(width="350px","time_error", "Retention time tolerance(second)  (0-1000 limit):", 30, min = 0, max = 1000))
                       ),
                       conditionalPanel(condition = "input.step1 || input.step3",
                                        column(style="padding:0px;",12,tags$hr(style="border-top: 1px solid #000000;"))
                                        
                       ),
                       conditionalPanel(condition = "input.step1",
                                        column(width=12, tags$p(style="font-weight:bold;font-size:16px;","Parameters for step1:")),
                                        column(width=12,
                                               column(width=6,selectInput(width="300px","groupcheck","More than one group in your sample?",c("FALSE","TRUE"))),
                                               column(width=6,textInput(width="300px","targetID", "Target IDs:","",placeholder="Default: None"))
                                        ),
                                        column(width=12,tags$p(style="color:red","For the target ids, you can enter the sample id that you want to highlight in the sample distribution plot. Each
                                                      sample id should be separated by comma, e.g. sample1,sample2"))
                       ),
                       conditionalPanel(condition = "input.step3",
                                        column(width=12, tags$p(style="font-weight:bold;font-size:16px;","Parameters for step3:")),
                                        column(width=12,
                                               column(width=6,numericInput(width="300px","foldchange_thresh", "Fold Change Threshold (1-100 limit):", 2, min = 1, max = 100)),
                                               column(width=6,numericInput(width="300px","minhit", "Minimum #metablites hitted in KEGG map:", 3, min = 1, max = 100))
                                        ),
                                        column(width=12,
                                               column(width=6,colourpicker::colourInput("highcolor", "Color for up-regulation in KEGG map:", "red", showColour = "background")),
                                               column(width=6,colourpicker::colourInput("lowcolor", "Color for down-regulation in KEGG map:", "blue", showColour = "background"))
                                        )
                       )
                  )
             ),
             actionButton("start3","Start processing",icon=icon("play-circle"))
        )
      ),
      column(width=12,
        column(width=4, 
               id="inputarea",
               fileInput("ref_meta_file", "Select standard metabolite library ('.csv' or .txt')",
                         multiple = FALSE,
                         width="350px",
                         accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
        ),
        conditionalPanel(condition = "input.step3",
                         column(width=4, 
                                id="inputarea",
                                fileInput("foldchange_file", "Select fold change file ('.csv' or .txt')",
                                          multiple = FALSE,
                                          width="350px",
                                          accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
                         )
        )
      ),
      column(width=12,verbatimTextOutput("nText6")),
      column(width=12,verbatimTextOutput("nText7")),
      column(width=12,style="padding-left:10px;padding-right:10px;",
        conditionalPanel(condition = "!output.done",
                       column(width=12, style="text-align:left;",tags$a(target="_blank",href="metabolite_quantification_description.html","Introduction & Input and output file descriptions"))
        )
      ),
      conditionalPanel(condition = "output.done",
                       column(width=12, style="text-align:center;",
                              id="inputarea",
                              downloadButton(style="background-color:#417ee0;color:#ffffff;","downloadQdata", label = "Download results")
                       )
      ),
      style='primary'
    ),
    multiple=TRUE))
)
  

  
  