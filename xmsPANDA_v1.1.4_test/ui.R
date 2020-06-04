library(shiny)
library(shinyBS)
library(V8)

source("R/introduction_page.R")
source("R/workflow1/workflow1_page.R")
source("R/workflow2/workflow2_page.R")
#source("R/additional_analysis.R")
source("R/help_page.R")


ui<-fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(script='js/css.js'),
  tags$head(
    tags$meta(charset="utf-8"),
    tags$meta(name="description",content="Free Web tutorials"),
    tags$title("xmsPANDA - v1.0.7"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
  ),
  includeCSS("www/mystyle.css"),
  column(12,
         tags$div(
           h3(tags$img(style="float:left;margin-right:15px;",src="images/xmsPANDA_log.png",height='80px',width="80px"),"xmsPANDA - R pacakge for biomarker discovery, supervised and unsupervised learning, and network analysis (v1.1)")),
         tabsetPanel(
           tabPanel("Introduction", introduction_page), 
           navbarMenu("Statistical Analysis", tabPanel("workflow I (feature selection using ANOVA, multivariate, time-series, regression, and other methods)",workflow1_page),
                                              tabPanel("workflow II (nested feature selection and predictive modeling)",workflow2_page)
                      ), 
           #tabPanel("Additional Analysis", additional_analysis_page), 
           tabPanel("Help and Support", help_page),
           type ="tabs"
         )
  ),
  column(style="padding-top:0px;padding-bottom:0px;",12,tags$hr(style="margin-top:0px;margin-bottom:15px;border-top: 0.5px solid #ccccb3;")),
  column(12,  tags$div(style="margin-center",tags$footer(align="center",color="white",style="font-weight:normal;font-size:95%;color:black","Maintained by Chunyu Ma (",tags$a(href="mailto:machunyu4402@hotmail.com","machunyu4402@hotmail.com"),") and Karan Uppal (",tags$a(href="mailto:kuppal2@emory.edu","kuppal2@emory.edu"),") at Emory University, Atlanta, GA, USA")))
)