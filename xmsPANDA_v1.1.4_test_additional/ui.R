library(shiny)
library(shinyBS)
library(plotly)
library(plyr)
library(ggpubr)
library(DT)
library(shinycssloaders)
library(colourpicker)
library(R.utils)
library(shinyWidgets)
library(tidyr)
library(stringr)

# Define UI for data upload app ----

additional_analysis_page <- fluidPage(
  
  includeCSS("www/mystyle.css"),
  # Make the Layout of parameter ----
  tags$div(
    id="maindiv",
    style='margin-top:12px',
    bsCollapse(open = "Interactive Plot",
               bsCollapsePanel("Interactive Plot", 
                               column(width=12,
                                      column(width=3,
                                             div(
                                               div(style='margin-bottom:10px;',tags$label("Choose the interactive graph:", `for` = "choose_graph" )),
                                               awesomeRadio(inputId = "choose_graph", inline=FALSE, label = NULL, choices = c('Manhattan Plot only', 'Volcano Plot only', 'Manhattan Plot with Box Plot', 'Volcano Plot with Box Plot'), selected = "Manhattan Plot only", status = "primary")
                                             )  
                                      ),
                                      column(width=5,
                                             div(
                                               conditionalPanel(
                                                 condition = "input.choose_graph=='Manhattan Plot only' || input.choose_graph=='Volcano Plot only'",
                                                 column(width=12, 
                                                        id="inputarea",
                                                        fileInput("input_file_interactive", "Select input file ('.csv' or .txt', 100MB limit)",
                                                                  multiple = FALSE,
                                                                  width="350px",
                                                                  accept = c("text/csv",
                                                                             "text/comma-separated-values,text/plain",
                                                                             ".txt"))
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.choose_graph=='Manhattan Plot with Box Plot' || input.choose_graph=='Volcano Plot with Box Plot'",
                                                 column(width=12, 
                                                        id="inputarea",
                                                        fileInput("feat_inte_interactive", "Select feature table file ('.csv' or .txt', 100MB limit)",
                                                                  multiple = FALSE,
                                                                  width="350px",
                                                                  accept = c("text/csv",
                                                                             "text/comma-separated-values,text/plain",
                                                                             ".txt"))
                                                 ),
                                                 column(width=12, 
                                                        id="inputarea",
                                                        fileInput("classlabel_inte_interactive", "Select class label file ('.csv' or '.txt', 100MB limit)",
                                                                  multiple = FALSE,
                                                                  width="350px",
                                                                  accept = c("text/csv",
                                                                             "text/comma-separated-values,text/plain",
                                                                             ".txt"))
                                                 )
                                               ),
                                               column(12,tags$p(style="font-size:15px;font-weight:bold","See the Format of Input File ",tags$a(href="Interactive_Plot.html",target="_blank","here")))
                                             )  
                                      ),
                                      column(width=4,
                                             style='margin-top:24px',
                                             actionButton("optionbutton1", "More options"),
                                             bsModal("moreoptions1", "More options for figure", "optionbutton1", size = "large",
                                                     conditionalPanel(
                                                       condition = "input.choose_graph=='Manhattan Plot only'",
                                                       style="height:400px",
                                                       column(width=6,selectInput(width="370px","yaxislabel_manhattan_only","Does your data have P-value or VIP?",c("pvalue","vip"))),
                                                       column(width=6,selectInput(width="370px","labelexpression_manhattan_only","Different colors to distinguish differential expression?",c("yes","no"))),
                                                       column(width=6,numericInput(width="370px","x_axis_spacing_type1_manhattan_only", "Type1 X axis spacing (0-1000 limit):", 50, min = 0, max = 1000)),
                                                       column(width=6,numericInput(width="370px","x_axis_spacing_type2_manhattan_only", "Type2 X axis spacing (0-1000 limit):", 50, min = 0, max = 1000)),
                                                       column(width=6,numericInput(width="370px","y_axis_spacing_manhattan_only", "Y axis spacing (0-5 limit):", 1, min = 0, max = 5)),
                                                       conditionalPanel(
                                                         condition = "input.yaxislabel_manhattan_only=='pvalue'",
                                                         column(width=6,numericInput(width="370px","pvaluecutoff_manhattan_only", "Threshold for p-value  (0-1 limit):", 0.05, min = 0, max = 1))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.yaxislabel_manhattan_only=='pvalue'",
                                                         column(width=6,selectInput(width="370px","adjdashline_manhattan_only","Draw dotted line for adjusted p-value?",c("no","yes")))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_manhattan_only=='yes' & input.yaxislabel_manhattan_only=='pvalue'",
                                                         column(width=6,selectInput(width="370px","psignif_manhattan_only","Based on which p-value to define significance?",c("pvalue","adjustedpvalue")))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_manhattan_only=='yes' & input.yaxislabel_manhattan_only=='pvalue'",
                                                         column(width=6,numericInput(width="370px","adjpvaluecutoff_manhattan_only", "Threshold for adjusted p-value  (0-1 limit):", 0.2, min = 0, max = 1))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.yaxislabel_manhattan_only=='vip'",
                                                         column(width=6,numericInput(width="370px","vipcutoff_manhattan_only", "Threshold for VIP  (0-10 limit):", 2, min = 0, max = 10))
                                                       )
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.choose_graph=='Volcano Plot only'",
                                                       style="height:400px",
                                                       column(width=6,numericInput(width="370px","pvaluecutoff_volcano_only","Threshold for p-value  (0-1 limit):", 0.05 , min = 0, max = 1)),
                                                       column(width=6,selectInput(width="370px","labelexpression_volcano_only","Different colors to distinguish differential expression?",c("yes","no"))),
                                                       column(width=6,numericInput(width="370px","lfc_volcano_only","Left side threshold for fold change (0-1 limit):", 0.5 , min = 0, max = 0)),
                                                       column(width=6,numericInput(width="370px","rfc_volcano_only","Right side threshold for fold change  (1-10 limit):", 2 , min = 1, max = 10)),
                                                       column(width=6,selectInput(width="370px","set_x_boundary_volcano_only","Set the boundary of X axis?",c("no","yes"))),
                                                       column(width=6,selectInput(width="370px","adjdashline_volcano_only","Draw dotted line for adjusted p-value?",c("no","yes"))),
                                                       conditionalPanel(
                                                         condition = "input.set_x_boundary_volcano_only=='yes'",
                                                         column(width=6,numericInput(width="370px","x_axis_boundary_volcano_only", "X axis boundary (0-20 limit):", 5, min = 0, max = 20))
                                                       ),
                                                       column(width=6,numericInput(width="370px","y_axis_spacing_volcano_only", "Y axis spacing (0-5 limit):", 1, min = 0, max = 5)),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_volcano_only=='yes'",
                                                         column(width=6,selectInput(width="370px","psignif_volcano_only","Based on which p-value to define significance?",c("pvalue","adjustedpvalue")))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_volcano_only=='yes'",
                                                         column(width=6,numericInput(width="370px","adjpvaluecutoff_volcano_only", "Threshold for adjusted p-value  (0-1 limit):", 0.2, min = 0, max = 1))
                                                       )
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.choose_graph=='Manhattan Plot with Box Plot'",
                                                       style="height:400px",
                                                       column(width=6,selectInput(width="370px","yaxislabel_manhattan_box","Does your data have P-value or VIP?",c("pvalue","vip"))),
                                                       column(width=6,selectInput(width="370px","plottype_manhattan_box","What type of manhattan plot?",c("type1","type2"))),
                                                       conditionalPanel(
                                                         condition = "input.plottype_manhattan_box=='type1'",
                                                         column(width=6,numericInput(width="370px","x_axis_spacing_type1_manhattan_box", "Type1 X axis spacing (0-1000 limit):", 50, min = 0, max = 1000))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.plottype_manhattan_box=='type2'",
                                                         column(width=6,numericInput(width="370px","x_axis_spacing_type2_manhattan_box", "Type2 X axis spacing (0-1000 limit):", 50, min = 0, max = 1000))
                                                       ),
                                                       column(width=6,numericInput(width="370px","y_axis_spacing_manhattan_box", "Y axis spacing (0-5 limit):", 1, min = 0, max = 5)),
                                                       column(width=6,selectInput(width="370px","labelexpression_manhattan_box","Different colors to distinguish differential expression?",c("yes","no"))),
                                                       conditionalPanel(
                                                         condition = "input.yaxislabel_manhattan_box=='pvalue'",
                                                         column(width=6,selectInput(width="370px","adjdashline_manhattan_box","Draw dotted line for adjusted p-value?",c("no","yes")))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.yaxislabel_manhattan_box=='pvalue'",
                                                         column(width=6,numericInput(width="370px","pvaluecutoff_manhattan_box", "Threshold for p-value  (0-1 limit):", 0.05, min = 0, max = 1))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_manhattan_box=='yes' & input.yaxislabel_manhattan_box=='pvalue'",
                                                         column(width=6,numericInput(width="370px","adjpvaluecutoff_manhattan_box", "Threshold for adjusted p-value  (0-1 limit):", 0.2, min = 0, max = 1))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.yaxislabel_manhattan_box=='vip'",
                                                         column(width=6,numericInput(width="370px","vipcutoff_manhattan_box", "Threshold for VIP  (0-10 limit):", 2, min = 0, max = 10))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_manhattan_box=='yes' & input.yaxislabel_manhattan_box=='pvalue'",
                                                         column(width=6,selectInput(width="370px","psignif_manhattan_box","Based on which p-value to define significance?",c("pvalue","adjustedpvalue")))
                                                       ),
                                                       column(width=6,selectInput(width="370px","boxplotcolor_manhattan_box","Is boxplot colored?",c("yes","no")))
                                                     ),
                                                     conditionalPanel(
                                                       condition = "input.choose_graph=='Volcano Plot with Box Plot'",
                                                       style="height:400px",
                                                       column(width=6,numericInput(width="370px","pvaluecutoff_volcano_box","Threshold for p-value  (0-1 limit):", 0.05 , min = 0, max = 1)),
                                                       column(width=6,selectInput(width="370px","labelexpression_volcano_box","Different colors to distinguish differential expression?",c("yes","no"))),
                                                       column(width=6,numericInput(width="370px","lfc_volcano_box","Left side threshold for fold change (0-1 limit):", 0.5 , min = 0, max = 1)),
                                                       column(width=6,numericInput(width="370px","rfc_volcano_box","Right side threshold for fold change  (1-10 limit):", 2 , min = 1, max = 10)),
                                                       column(width=6,selectInput(width="370px","set_x_boundary_volcano_box","Set the boundary of X axis?",c("no","yes"))),
                                                       column(width=6,selectInput(width="370px","adjdashline_volcano_box","Draw dotted line for adjusted p-value?",c("no","yes"))),
                                                       conditionalPanel(
                                                         condition = "input.set_x_boundary_volcano_box=='yes'",
                                                         column(width=6,numericInput(width="370px","x_axis_boundary_volcano_box", "X axis boundary (0-20 limit):", 5, min = 0, max = 20))
                                                       ),
                                                       column(width=6,numericInput(width="370px","y_axis_spacing_volcano_box", "Y axis spacing (0-5 limit):", 1, min = 0, max = 5)),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_volcano_box=='yes'",
                                                         column(width=6,selectInput(width="370px","psignif_volcano_box","Based on which p-value to define significance?",c("pvalue","adjustedpvalue")))
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.adjdashline_volcano_box=='yes'",
                                                         column(width=6,numericInput(width="370px","adjpvaluecutoff_volcano_box", "Threshold for adjusted p-value  (0-1 limit):", 0.2, min = 0, max = 1))
                                                       ),
                                                       column(width=6,selectInput(width="370px","boxplotcolor_volcano_box","Is boxplot colored?",c("yes","no")))
                                                     )
                                             ),
                                             actionButton("start1_interactive","Start processing",icon=icon("play-circle"))
                                      )
                               ),
                               column(width=12,verbatimTextOutput("nText_interactive")),
                               column(width=12,bsAlert("alert_interactive")),
                               uiOutput("interactive_plot"),
                               column(width=12,
                                      style='margin-bottom:10px',
                                      column(width=12,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_manhattan_only",
                                                              actionButton("colorbutton_manhattan_only", "Select Color"),
                                                              bsModal("coloroptions_manhattan_only", "Colors for scatter plot", "colorbutton_manhattan_only", size = "large",
                                                                      tags$div(
                                                                        width=12,
                                                                        style='text-align:left;height:250px;',
                                                                        column(width=6,colourpicker::colourInput("insigcol_manhattan_only", "Insignificant points:", "black", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("dashedlinecol_manhattan_only", "Dashed line for p-value/vip:", "blue", showColour = "background")),
                                                                        column(width=6,conditionalPanel(condition = "input.adjdashline_manhattan_only=='yes' & input.yaxislabel_manhattan_only=='pvalue'",
                                                                                                        colourpicker::colourInput("dottedlinecol_manhattan_only", "Dotted line for adjusted p-value:", "green", showColour = "background")
                                                                        )),                             
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_manhattan_only=='no'",
                                                                                                        colourpicker::colourInput("sigcol_manhattan_only", "Significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_manhattan_only=='yes'",
                                                                                                        colourpicker::colourInput("poscol_manhattan_only", "Positive significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_manhattan_only=='yes'",
                                                                                                        colourpicker::colourInput("negcol_manhattan_only", "Negative significant points:", "blue", showColour = "background")
                                                                        ))
                                                                      )
                                                              )),
                                             conditionalPanel(condition = "output.check_plot_volcano_only",
                                                              actionButton("colorbutton_volcano_only", "Select Color"),
                                                              bsModal("coloroptions_volcano_only", "Colors for scatter plot", "colorbutton_volcano_only", size = "large",
                                                                      tags$div(
                                                                        width=12,
                                                                        style='text-align:left;height:250px;',
                                                                        column(width=6,colourpicker::colourInput("insigcol_volcano_only", "Insignificant points:", "black", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("dashedlinecol_volcano_only", "Dashed line for p-value/vip:", "blue", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("lfcdashed_volcano_only", "Left side dashed line:", "red", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("rfcdashed_volcano_only", "Right side dashed line:", "red", showColour = "background")),
                                                                        column(width=6,conditionalPanel(condition = "input.adjdashline_volcano_only=='yes'",
                                                                                                        colourpicker::colourInput("dottedlinecol_volcano_only", "Dotted line for adjusted p-value:", "green", showColour = "background")
                                                                        )),                             
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_volcano_only=='no'",
                                                                                                        colourpicker::colourInput("sigcol_volcano_only", "Significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_volcano_only=='yes'",
                                                                                                        colourpicker::colourInput("poscol_volcano_only", "Positive significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_volcano_only=='yes'",
                                                                                                        colourpicker::colourInput("negcol_volcano_only", "Negative significant points:", "blue", showColour = "background")
                                                                        ))
                                                                      )
                                                              ))
                                      ),
                                      column(width=6,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_manhattan_box1",
                                                              "Name: ",tags$input(id='name_manhattan_box',type='text',style='width:120px;height:30px'),
                                                              bsTooltip("name_manhattan_box", "eg. '132.07675_102.12' for metabolite with mz:132.07675 and time:102.12 or 'Lysine'","bottom"),
                                                              actionButton("search_manhattan_box","Search",icon=icon("search")),
                                                              actionButton("colorbutton_manhattan_box", "Select Color"),
                                                              bsModal("coloroptions_manhattan_box", "Colors for scatter plot", "colorbutton_manhattan_box", size = "large",
                                                                      tags$div(
                                                                        width=12,
                                                                        style='text-align:left;height:250px;',
                                                                        column(width=6,colourpicker::colourInput("insigcol_manhattan_box", "Insignificant points:", "black", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("dashedlinecol_manhattan_box", "Dashed line for p-value/vip:", "blue", showColour = "background")),
                                                                        column(width=6,conditionalPanel(condition = "input.adjdashline_manhattan_box=='yes' & input.yaxislabel_manhattan_box=='pvalue'",
                                                                                                        colourpicker::colourInput("dottedlinecol_manhattan_box", "Dotted line for adjusted p-value:", "green", showColour = "background")
                                                                        )),                             
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_manhattan_box=='no'",
                                                                                                        colourpicker::colourInput("sigcol_manhattan_box", "Significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_manhattan_box=='yes'",
                                                                                                        colourpicker::colourInput("poscol_manhattan_box", "Positive significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_manhattan_box=='yes'",
                                                                                                        colourpicker::colourInput("negcol_manhattan_box", "Negative significant points:", "blue", showColour = "background")
                                                                        ))
                                                                      )
                                                              )),
                                             conditionalPanel(condition = "output.check_plot_volcano_box1",
                                                              "Name: ",tags$input(id='name_volcano_box',type='text',style='width:120px;height:30px'),
                                                              bsTooltip("name_volcano_box", "eg. '132.07675_102.12' for metabolite with mz:132.07675 and time:102.12 or 'Lysine'","bottom"),
                                                              actionButton("search_volcano_box","Search",icon=icon("search")),
                                                              actionButton("colorbutton_volcano_box", "Select Color"),
                                                              bsModal("coloroptions_volcano_box", "Colors for scatter plot", "colorbutton_volcano_box", size = "large",
                                                                      tags$div(
                                                                        width=12,
                                                                        style='text-align:left;height:250px;',
                                                                        column(width=6,colourpicker::colourInput("insigcol_volcano_box", "Insignificant points:", "black", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("dashedlinecol_volcano_box", "Dashed line for p-value/vip:", "blue", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("lfcdashed_volcano_box", "Left side dashed line:", "red", showColour = "background")),
                                                                        column(width=6,colourpicker::colourInput("rfcdashed_volcano_box", "Right side dashed line:", "red", showColour = "background")),
                                                                        column(width=6,conditionalPanel(condition = "input.adjdashline_volcano_box=='yes'",
                                                                                                        colourpicker::colourInput("dottedlinecol_volcano_box", "Dotted line for adjusted p-value:", "green", showColour = "background")
                                                                        )),                             
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_volcano_box=='no'",
                                                                                                        colourpicker::colourInput("sigcol_volcano_box", "Significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_volcano_box=='yes'",
                                                                                                        colourpicker::colourInput("poscol_volcano_box", "Positive significant points:", "red", showColour = "background")
                                                                        )),
                                                                        column(width=6,conditionalPanel(condition = "input.labelexpression_volcano_box=='yes'",
                                                                                                        colourpicker::colourInput("negcol_volcano_box", "Negative significant points:", "blue", showColour = "background")
                                                                        ))
                                                                      )
                                                              ))
                                      ),
                                      column(width=6,
                                             conditionalPanel(condition = "output.check_plot_manhattan_box2_1",
                                                              style='text-align:center;padding-right:55px',
                                                              actionButton("change_manhattan_box_1","Change Label"),
                                                              bsModal("axisoptions_manhattan_box_1", "Change label for the axes of Box plot ", "change_manhattan_box_1", size = "large",
                                                                      tags$div(
                                                                        style='text-align:left;height:80px;',
                                                                        column(width=6,textInput(width="370px","xaxis_name_manhattan_box_1", "X axis Label:","",placeholder="Default: group")),
                                                                        column(width=6,textInput(width="370px","yaxis_name_manhattan_box_1", "Y axis Label:","",placeholder="Default: intensity"))
                                                                      )
                                                              )
                                             ),
                                             conditionalPanel(condition = "output.check_plot_manhattan_box2_2",
                                                              style='text-align:center;padding-left:55px',
                                                              actionButton("change_manhattan_box_2","Change Label"),
                                                              bsModal("axisoptions_manhattan_box_2", "Change label for the axes of Box plot ", "change_manhattan_box_2", size = "large",
                                                                      tags$div(
                                                                        style='text-align:left;height:80px;',
                                                                        column(width=6,textInput(width="370px","xaxis_name_manhattan_box_2", "X axis Label:","",placeholder="Default: group")),
                                                                        column(width=6,textInput(width="370px","yaxis_name_manhattan_box_2", "Y axis Label:","",placeholder="Default: intensity"))
                                                                      )
                                                              )
                                             ),
                                               conditionalPanel(condition = "output.check_plot_volcano_box2_1",
                                                                style='text-align:center;padding-right:55px',
                                                                actionButton("change_volcano_box_1","Change Label"),
                                                                bsModal("axisoptions_volcano_box_1", "Change label for the axes of Box plot ", "change_volcano_box_1", size = "large",
                                                                        tags$div(
                                                                          style='text-align:left;height:80px;',
                                                                          column(width=6,textInput(width="370px","xaxis_name_volcano_box_1", "X axis Label:","",placeholder="Default: group")),
                                                                          column(width=6,textInput(width="370px","yaxis_name_volcano_box_1", "Y axis Label:","",placeholder="Default: intensity"))
                                                                        )
                                                                )
                                               ),
                                               conditionalPanel(condition = "output.check_plot_volcano_box2_2",
                                                                style='text-align:center;padding-left:55px',
                                                                actionButton("change_volcano_box_2","Change Label"),
                                                                bsModal("axisoptions_volcano_box_2", "Change label for the axes of Box plot ", "change_volcano_box_2", size = "large",
                                                                        tags$div(
                                                                          style='text-align:left;height:80px;',
                                                                          column(width=6,textInput(width="370px","xaxis_name_volcano_box_2", "X axis Label:","",placeholder="Default: group")),
                                                                          column(width=6,textInput(width="370px","yaxis_name_volcano_box_2", "Y axis Label:","",placeholder="Default: intensity"))
                                                                        )
                                                                )
                                               )
                                      )
                               ),
                               column(width=12,
                                      style='margin-bottom:10px',
                                      column(width=6,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_manhattan_only",
                                                              downloadButton("downloadPlot1_manhattan_only", label = "Download type1 manhattan plot", style='color: #fff; background-color: #337ab7'))
                                             
                                      ),
                                      column(width=6,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_manhattan_only",
                                                              downloadButton("downloadPlot2_manhattan_only", label = "Download type2 manhattan plot", style='color: #fff; background-color: #337ab7'))
                                      ),
                                      column(width=12,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_volcano_only",
                                                              downloadButton("downloadPlot1_volcano_vonly", label = "Download volcano plot", style='color: #fff; background-color: #337ab7'))
                                             
                                      ),
                                      column(width=6,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_manhattan_box1",
                                                              downloadButton("downloadPlot1_manhattan_box", label = "Download manhattan plot", style='color: #fff; background-color: #337ab7'))
                                             
                                      ),
                                      column(width=6,
                                             conditionalPanel(condition = "output.check_plot_manhattan_box2_1",
                                                              style='text-align:center;padding-right:55px',
                                                              downloadButton("downloadPlot2_1_manhattan_box", label = "Download box plot", style='color: #fff; background-color: #337ab7')
                                            ),
                                            conditionalPanel(condition = "output.check_plot_manhattan_box2_2",
                                                             style='text-align:center;padding-left:55px',
                                                             downloadButton("downloadPlot2_2_manhattan_box", label = "Download box plot", style='color: #fff; background-color: #337ab7')
                                            )
                                      ),
                                      column(width=6,
                                             style='text-align:center',
                                             conditionalPanel(condition = "output.check_plot_volcano_box1",
                                                              downloadButton("downloadPlot1_volcano_box", label = "Download volcano plot", style='color: #fff; background-color: #337ab7'))
                                             
                                      ),
                                      column(width=6,
                                             conditionalPanel(condition = "output.check_plot_volcano_box2_1",
                                                              style='text-align:center;padding-right:55px',
                                                              downloadButton("downloadPlot2_1_volcano_box", label = "Download box plot", style='color: #fff; background-color: #337ab7')
                                             ),
                                             conditionalPanel(condition = "output.check_plot_volcano_box2_2",
                                                              style='text-align:center;padding-left:55px',
                                                              downloadButton("downloadPlot2_2_volcano_box", label = "Download box plot", style='color: #fff; background-color: #337ab7')
                                             )
                                      )
                               ),
                               style='primary'
               ),
               bsCollapsePanel("Functional Class Scoring",
                               column(width=12,
                                      column(width=6, 
                                             id="inputarea",
                                             fileInput("clusterinput_fcs", "Select your target metabolite data ('.csv' or .txt')",
                                                       multiple = FALSE,
                                                       width="350px",
                                                       accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
                                      ),
                                      column(width=6,
                                             style='margin-top:24px',
                                             actionButton("optionbutton_fcs", "More options"),
                                             bsModal("moreoptions_fcs", "More options for analysis", "optionbutton_fcs", size = "large",
                                                     tags$div(
                                                       width=12,
                                                       style="height:300px",
                                                       column(width=6,selectInput(width="350px","kegg_species_code_fcs","Select a species:",c("Homo sapiens(default)",
                                                                                                                                          "Mus musculus",
                                                                                                                                          "Pan troglodytes",
                                                                                                                                          "Macaca mulatta",
                                                                                                                                          "Bos taurus",
                                                                                                                                          "Rattus norvegicus",
                                                                                                                                          "Danio rerio",
                                                                                                                                          "C. elegans",
                                                                                                                                          "Drosophila melanogaster"))),
                                                       column(width=6,selectInput(width="350px","database_fcs","Select a KEGG database:",c("pathway(default)","module"))),
                                                       column(width=6,selectInput(width="350px","type.statistic_fcs","Statistic type is p-value?",c("TRUE","FALSE")))
                                                       
                                                     )
                                             ),
                                             actionButton("start_fcs","Start processing",icon=icon("play-circle"))
                                      )
                               ),
                               column(width=12,verbatimTextOutput("nText_fcs1")),
                               column(width=12,verbatimTextOutput("nText_fcs2")),
                               column(width=12,style="padding-left:25px;padding-right:25px;",
                                      conditionalPanel(condition = "!output.checktable_fcs",
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
                                      conditionalPanel(condition = "output.checktable_fcs",
                                                       uiOutput("downloadbutton_fcs"),
                                                       DTOutput('pathwaytb_fcs')          
                                      )
                                      
                               ),
                               style='primary'
               ),
               bsCollapsePanel("Metabolite Quantification Analysis",
                               column(width=12,
                                      column(width=4,
                                             id="inputarea",
                                             fileInput("feat_inte_mqa", "Select feature table file ('.csv' or .txt', 100MB limit)",
                                                       multiple = FALSE,
                                                       width="350px",
                                                       accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
                                      ),
                                      column(width=4,
                                             id="inputarea",
                                             fileInput("classlabel_inte_mqa", "Select class label file ('.csv' or '.txt')",
                                                       multiple = FALSE,
                                                       width="350px",
                                                       accept = c("text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".txt"))
                                      ),
                                      column(width=4,
                                             style='margin-top:24px',
                                             actionButton("optionbutton_mqa", "More options"),
                                             bsModal("moreoptions_mqa", "More options for analysis", "optionbutton_mqa", size = "large",
                                                     tags$div(
                                                       width=12,
                                                       style= "height:660px",
                                                       column(width=12, tags$p(style="font-weight:bold;font-size:16px;","Select the step you need to run:")),
                                                       column(width=12, style='margin-bottom:10px',
                                                              prettyCheckbox(inputId = "step1_mqa",
                                                                             label = "Step1: Draw distribution plots",
                                                                             value = TRUE,
                                                                             thick = TRUE,
                                                                             shape = "round",
                                                                             animation = "smooth",
                                                                             status = "primary",
                                                                             inline = TRUE),
                                                              prettyCheckbox(inputId = "step2_mqa",
                                                                             label = "Step2: Quantify concentration",
                                                                             value = TRUE,
                                                                             thick = TRUE,
                                                                             shape = "round",
                                                                             animation = "smooth",
                                                                             status = "primary",
                                                                             inline = TRUE),
                                                              prettyCheckbox(inputId = "step3_mqa",
                                                                             label = "Step3: Pull the KEGG map from KEGG database",
                                                                             value = FALSE,
                                                                             thick = TRUE,
                                                                             shape = "round",
                                                                             animation = "smooth",
                                                                             status = "primary",
                                                                             inline = TRUE)
                                                       ),
                                                       column(width=12,
                                                              column(width=6,selectInput(width="350px","summarize_replicates_mqa","Summarize technical replicate?",c("TRUE","FALSE"))),
                                                              column(width=6,
                                                                     conditionalPanel(condition = "input.summarize_replicates_mqa == 'TRUE'",
                                                                                      numericInput(width="350px","num_replicate_mqa", "Number of technical replicates  (1-10 limit):", 3, min = 1, max = 10)
                                                                     )
                                                              )
                                                       ),
                                                       column(width=12,
                                                              column(width=6,
                                                                     conditionalPanel(condition = "input.summarize_replicates_mqa == 'TRUE'",
                                                                                      numericInput(width="350px","rep_max_missing_thresh_mqa", "Maximum missing value ratio:", 0.3, min = 0, max = 1)
                                                                     )
                                                              ),
                                                              column(width=6,
                                                                     conditionalPanel(condition = "input.summarize_replicates_mqa == 'TRUE'",
                                                                                      selectInput(width="350px","summary_method_mqa","Choose a replicate summarization method:",c("median","mean"))
                                                                     )
                                                              )
                                                       ),
                                                       column(width=12,
                                                              column(width=6,numericInput(width="350px","mass_error_mqa", "Mass-to-charge tolerance(ppm)  (0-100 limit):", 10, min = 0, max = 100)),
                                                              column(width=6,numericInput(width="350px","time_error_mqa", "Retention time tolerance(second)  (0-1000 limit):", 30, min = 0, max = 1000))
                                                       ),
                                                       conditionalPanel(condition = "input.step1_mqa || input.step3_mqa",
                                                                        column(style="padding:0px;",12,tags$hr(style="border-top: 1px solid #000000;"))
                                                                        
                                                       ),
                                                       conditionalPanel(condition = "input.step1_mqa",
                                                                        column(width=12, tags$p(style="font-weight:bold;font-size:16px;","Parameters for step1:")),
                                                                        column(width=12,
                                                                               column(width=6,selectInput(width="300px","groupcheck_mqa","More than one group in your sample?",c("FALSE","TRUE"))),
                                                                               column(width=6,textInput(width="300px","targetID_mqa", "Target IDs:","",placeholder="Default: None"))
                                                                        ),
                                                                        column(width=12,tags$p(style="color:red","For the target ids, you can enter the sample id that you want to highlight in the sample distribution plot. Each
                                                                                               sample id should be separated by comma, e.g. sample1,sample2"))
                                                                        ),
                                                       conditionalPanel(condition = "input.step3_mqa",
                                                                        column(width=12, tags$p(style="font-weight:bold;font-size:16px;","Parameters for step3:")),
                                                                        column(width=12,
                                                                               column(width=6,numericInput(width="300px","foldchange_thresh_mqa", "Fold Change Threshold (1-100 limit):", 2, min = 1, max = 100)),
                                                                               column(width=6,numericInput(width="300px","minhit_mqa", "Minimum #metablites hitted in KEGG map:", 3, min = 1, max = 100))
                                                                        ),
                                                                        column(width=12,
                                                                               column(width=6,colourpicker::colourInput("highcolor_mqa", "Color for up-regulation in KEGG map:", "red", showColour = "background")),
                                                                               column(width=6,colourpicker::colourInput("lowcolor_mqa", "Color for down-regulation in KEGG map:", "blue", showColour = "background"))
                                                                        )
                                                       )
                                                       )
                                      ),
                                      actionButton("start_mqa","Start processing",icon=icon("play-circle"))
                                      )
               ),
               column(width=12,
                      column(width=4, 
                             id="inputarea",
                             fileInput("ref_meta_file_mqa", "Select standard metabolite library ('.csv' or .txt')",
                                       multiple = FALSE,
                                       width="350px",
                                       accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
                      ),
                      conditionalPanel(condition = "input.step3_mqa",
                                       column(width=4, 
                                              id="inputarea",
                                              fileInput("foldchange_file_mqa", "Select fold change file ('.csv' or .txt')",
                                                        multiple = FALSE,
                                                        width="350px",
                                                        accept = c("text/csv","text/comma-separated-values,text/plain",".txt"))
                                       )
                      )
               ),
               column(width=12,verbatimTextOutput("nText_mqa1")),
               column(width=12,verbatimTextOutput("nText_mqa2")),
               column(width=12,style="padding-left:10px;padding-right:10px;",
                      conditionalPanel(condition = "!output.done_mqa",
                                       column(width=12, style="text-align:left;",tags$a(target="_blank",href="metabolite_quantification_description.html","Introduction & Input and output file descriptions"))
                      )
               ),
               conditionalPanel(condition = "output.done_mqa",
                                column(width=12, style="text-align:center;",
                                       id="inputarea",
                                       downloadButton(style="background-color:#417ee0;color:#ffffff;","downloaddata_mqa", label = "Download results")
                                )
               ),
               style='primary'
               ),
               multiple=TRUE))
)

