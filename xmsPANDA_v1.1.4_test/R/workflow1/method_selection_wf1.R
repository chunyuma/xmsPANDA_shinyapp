library(shiny)
library(shinyWidgets)

method_selection_wf1<-fluidRow(
  tags$div(
    id="maindiv",
    column(width=12, 
           id="inputarea",
           column(width=6,selectInput(width="350px","analysismode_wf1","Select analysis mode:",c("classification","regression"))),
           column(width=6,radioButtons("pairedanalysis_wf1", "Is this a repeated-measurement design?", inline=TRUE,c(True = "TRUE",False = "FALSE"),selected = "FALSE")),
           column(width=12,
                  conditionalPanel(
                    condition = "input.analysismode_wf1 == 'classification' && input.pairedanalysis_wf1 == 'FALSE'",
                    column(width=12,
                           div(style="display: inline-block;vertical-align:top",tags$p(style="margin:0;padding-top:4px;padding-bottom:4px;font-weight:bold","Choose feature selection methods:  ")),
                           div(style="display: inline-block;vertical-align:top",actionButton("methodbuttoni_wf1", "View")),
                           bsModal("method_modali", "Method List", "methodbuttoni_wf1", size = "large",
                                   checkboxGroupInput("featselmethodi_wf1", "", inline = FALSE,
                                                      choiceNames =list('limma (one-way ANOVA using LIMMA)',
                                                                        'pls (partial least squares)',
                                                                        'limma2way (two-way ANOVA using LIMMA)',
                                                                        'lm1wayanova (one-way ANOVA using linear model)',
                                                                        'lm2wayanova (two-way ANOVA using linear model)',
                                                                        'lmreg (linear regression)',
                                                                        'logitreg (logistic regression)',
                                                                        'rfesvm (recursive feature elimination SVM)',
                                                                        'ttest (simple t-test)',
                                                                        'wilcox (wilcoxon test)',
                                                                        'RF (random forest using boruta algorithm)',
                                                                        'MARS (multiple adaptive regression splines)',
                                                                        'spls (sparse partial least squares)',
                                                                        'o1pls (orthogonal partial least squares)',
                                                                        'pamr (microarrays algorithm based on the nearest shrunked centroid method)'),
                                                      choiceValues =list('limma','pls','limma2way','lm1wayanova','lm2wayanova','lmreg',
                                                                         'logitreg','rfesvm','ttest','wilcox','RF','MARS','spls','o1pls','pamr'),
                                                      selected=c('limma','pls'),
                                                      width='800px'
                                   )
                           )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.analysismode_wf1 == 'classification' && input.pairedanalysis_wf1 == 'TRUE'",
                    column(width=12,
                           div(style="display: inline-block;vertical-align:top",tags$p(style="margin:0;padding-top:4px;padding-bottom:4px;font-weight:bold","Choose feature selection methods:  ")),
                           div(style="display: inline-block;vertical-align:top",actionButton("methodbuttonii_wf1", "View")),
                           bsModal("method_modalii", "Method List", "methodbuttonii_wf1", size = "large",
                                   checkboxGroupInput("featselmethodii_wf1", "", inline = FALSE,
                                                      choiceNames =list('limma1wayrepeat (one-way ANOVA repeated measures using LIMMA)',
                                                                        'limma2wayrepeat (two-way ANOVA repeated measures using LIMMA)',
                                                                        'lm1wayanovarepeat (one-way ANOVA repeated measures using linear model)',
                                                                        'lm2wayanovarepeat (two-way ANOVA repeated measures using linear model)',
                                                                        'spls1wayrepeat (one-way ANOVA repeated measures using sparse partial least squares)',
                                                                        'spls2wayrepeat (two-way ANOVA repeated measures using sparse partial least squares)',
                                                                        'ttestrepeat (simple t-test repeated measures)',
                                                                        'wilcoxrepeat (wilcoxon test repeated measures)'),
                                                      choiceValues=list('limma1wayrepeat','limma2wayrepeat','lm1wayanovarepeat','lm2wayanovarepeat','spls1wayrepeat','spls2wayrepeat','ttestrepeat','wilcoxrepeat'),
                                                      selected=c('lm1wayanovarepeat'),
                                                      width='800px'
                                   )
                           )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.analysismode_wf1 == 'regression' && input.pairedanalysis_wf1 == 'FALSE'",
                    column(width=12,
                           div(style="display: inline-block;vertical-align:top",tags$p(style="margin:0;padding-top:4px;padding-bottom:4px;font-weight:bold","Choose feature selection methods:  ")),
                           div(style="display: inline-block;vertical-align:top",actionButton("methodbuttoniii_wf1", "View")),
                           bsModal("method_modaliii", "Method List", "methodbuttoniii_wf1", size = "large",
                                   checkboxGroupInput("featselmethodiii_wf1", "", inline = FALSE,
                                                      choiceNames =list('lmreg (linear regression)',
                                                                        'RF (random forest using boruta algorithm)',
                                                                        'MARS (multiple adaptive regression splines)',
                                                                        'pls (partial least squares)',
                                                                        'spls (sparse partial least squares)',
                                                                        'o1pls (orthogonal partial least squares)'),
                                                      choiceValues=list('lmreg','RF','MARS','pls','spls','o1pls'),
                                                      selected=c('lmreg'),
                                                      width='800px'
                                   ) 
                           )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.analysismode_wf1 == 'regression' && input.pairedanalysis_wf1 == 'TRUE'",
                    tags$p(style="color:red;font-size:15px;font-weight:bold;","Temporally xmsPANDA doesn't provide methods for 'regression' mode with repeated-measurement design. Please select 'classification' mode 
                           for your study.")
                    )
                  ),
           
           column(width=12,style="margin-top:10px",conditionalPanel(
             condition = "output.checkpvalue_wf1 || output.checkvip_wf1",
             column(width=6,style='padding-left:0;',numericInput(width="350px","pvalue_thresh_wf1", "P-value threshold:", 0.05, min = 0, max = 1)),
             column(width=6,selectInput(width="350px","fdr_method_wf1","Choose FDR correction method:",choices=c("BH (Benjamini-Hochberg 1995)"="BH","ST (Storey & Tibshi- rani 2001)"="ST","Strimmer (Strimmer 2008)"='Strimmer',"No FDR calculation"="none"))),
             column(width=6,style='padding-left:0;',numericInput(width="350px","fdrthresh_wf1", "False discovery threshold:", 1, min = 0, max = 1)),
             column(width=6,style='padding-left:10;',
                    tags$label("Network based feature ranking:", `for` = "netbasedfeatranking_switch_wf1"),
                    div(style="width: 100px;", switchInput(inputId = "netbasedfeatranking_switch_wf1",value = FALSE))
             ),
             column(width=6,style='padding-left:0;margin-top:25px;',actionButton("argumentbutton_wf1", "More arguments")),
             bsModal("argument_modal1", "More arguments for feature selection", "argumentbutton_wf1", size = "large",
                     tags$div(
                       width=12,
                       style="height:500px",
                       column(width=6,numericInput(width="350px","foldchangethresh_wf1", "Fold change threshold (0-100 limit):", 0, min = 0, max = 100)),
                       column(width=6,numericInput(width="350px","kfold_wf1", "k for k-fold Cross Validation (1-10000 limit):", 10, min = 1, max = 10000)),
                       column(width=6,numericInput(width="350px","pls_vip_thresh_wf1", "VIP threshold (1-100 limit):", 2, min = 1, max = 100)),
                       bsTooltip("pls_vip_thresh_wf1", "This argument only works for PLS or O1PLS models","bottom", options = list(container = "body")),
                       column(width=6,numericInput(width="350px","pls_ncomp_wf1", "Max number of components to consider:", 5, min = 1, max = 100000)),
                       bsTooltip("pls_ncomp_wf1", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       column(width=6,numericInput(width="350px","max_comp_sel_wf1", "Number of components to use for VIP selection:", 1, min = 1, max = 100000)),
                       bsTooltip("max_comp_sel_wf1", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       column(width=6,selectInput(width="350px","optselect_wf1","Find optimal number of components:",c("TRUE","FALSE"))),
                       bsTooltip("optselect_wf1", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       column(width=6,selectInput(width="350px","pls_vip_selection_wf1","VIP summarization across multiple components:",c("max","mean"))),
                       bsTooltip("pls_vip_selection_wf1", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       column(width=6,
                              tags$label("Number of permutations for calculating p-values:", `for` = "permu_switch_wf1"),
                              div(style="display: inline-block;vertical-align:top; width: 100px;", switchInput(inputId = "permu_switch_wf1",value = FALSE)),
                              div(style="display: inline-block;vertical-align:top; width: 250px;", numericInput(inputId = "pls_permut_count_wf1", label = NULL, value = 1000, min = 1, max = 100000))
                       ),
                       bsTooltip("pls_permut_count_wf1", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       column(width=12,
                              column(width=6,style="padding-left:0;",conditionalPanel(
                                condition = "output.checkmax_varsel_wf1",
                                numericInput(width="350px","max_varsel_wf1", "Max number of variables to be used:", 100, min = 1, max = 100000),
                                bsTooltip("max_varsel_wf1", "This argument only works for sPLS, spls1wayrepeat, spls2wayrepeat, rfesvm, and Random Forest","bottom", options = list(container = "body"))
                              )),
                              column(width=6,conditionalPanel(
                                condition = "output.checkaggregationmethod_wf1",
                                selectInput(width="350px","aggregation_method_wf1","Methods for aggregating results:",choices=c("No aggregation"="None","Probability-based"="consensus","RankAggreg (cross entropy)"="RankAggreg","RankAggregGA (genetic algorithm)"="RankAggregGA"))
                              ))
                       )
                     )
             )
           ))
           )
    
  )
  )
