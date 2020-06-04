library(shiny)
library(shinyWidgets)

help_page <- fluidPage(
  
  column(12,tags$h4("User Manual:")),
  column(12,tags$p(style="font-size: 15px;","Click ",tags$a(href='xmsPANDA-manual.pdf',target="_blank","here")," to see the user manual.")),
  column(12,tags$h4("Input File Format:")),
  column(12,tags$p(style="margin-top:10px;font-weight:bold; font-size: 17px;","feature table file")),
  column(12,tags$p(style="text-align:justify;font-size: 15px;","In feature table file, each analyte should include m/z, retention time, and measured intensity for each sample. The first 2 columns should be the m/z and time. The remaining columns should correspond to the samples in the class labels file and each column is the intensity profile of a sample. The example format is as follows:",icon("hand-point-down", lib = "font-awesome", "fa-2x"))),
  column(12, div(style="display:block;margin-top:10px; margin-left: auto;margin-right: auto;width: 50%;",tableOutput('example_feat'))
  ),
  column(12,div(style="margin-top:23px;margin-right:10px;display: inline-block;vertical-align:top",tags$p(style="font-weight:bold; font-size: 17px;","class label file"))
  ),
  column(12,
         column(4,align='center',style="display:block; margin-left: auto;margin-right:auto;",
                tableOutput('example_classlabel_table')
         ),
         column(8,
                div(style='margin-bottom:30px;',uiOutput("example_classlabel_text"),
                    tags$p(icon("hand-point-left", lib = "font-awesome", "fa-2x"),'the example format is on the left side.')
                ),
                div(
                  div(style='margin-bottom:10px;',tags$label("Class Label Options", `for` = "classlabel_option" )),
                  awesomeRadio(inputId = "classlabel_option",label = NULL, choices = c("multiclass comparison", "multiclass comparison with covariates", "regression", "two-way anova", "one factor repeatedmeasures", "two factor repeatedmeasures"), selected = "multiclass comparison", status = "primary")
                ) 
         )
  ),
  column(12,tags$h4("Download Resources:")),
  column(12,tags$p(style="font-size: 15px;","Download ",tags$a(href='https://github.com/kuppal2/xmsPANDA',target="_blank","xmsPANDA")," from Github")),
  column(12,tags$p(style="font-size: 15px;","Download ",tags$a(href='https://github.com/kuppal2/xmsPANDA/tree/master/examples_and_manual/Example_feature_table_and_classlabels',target="_blank","example data")," from Github."))
  
)