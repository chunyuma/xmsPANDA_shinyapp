library(shiny)
library(shinyWidgets)

method_selection_wf2<-fluidRow(
  tags$div(
    id="maindiv",
    column(width=12, 
           id="inputarea",
           column(width=12,
                  div(style="display: inline-block;vertical-align:top",tags$p(style="margin:0;padding-top:4px;padding-bottom:4px;font-weight:bold","Choose feature selection methods:  ")),
                  div(style="display: inline-block;vertical-align:top",actionButton("methodbuttoni_wf2", "View")),
                  bsModal("method_modali2", "Method List", "methodbuttoni_wf2", size = "large",
                          checkboxGroupInput("featselmethodi_wf2", "", inline = FALSE,
                                             choiceNames =list('limma (one-way ANOVA using LIMMA)',
                                                               'pls (partial least squares)',
                                                               'lmreg (linear regression)',
                                                               'lmregrobust (linear regression using robust sandwich estimator)',
                                                               'logitreg (logistic regression)',
                                                               'logitregrobust (logistic regression using robust sandwich estimator)',
                                                               'rferadial (recursive feature elimination SVM with RBF kernel)',
                                                               'rfe (recursive feature elimination SVM with linear kernel)',
                                                               't.test (simple t-test)',
                                                               'wilcox.test (wilcoxon test)',
                                                               'welch.test (welch test)',
                                                               'f.test (f test)',
                                                               'kruskal.test (kruskal-wallis test)',
                                                               'rf (random forest using the randomForest package)',
                                                               'rfboruta (random forest using Boruta algorithm)',
                                                               'lasso',
                                                               'elasticnet',
                                                               'spls (sparse partial least squares)',
                                                               'o1pls (orthogonal partial least squares)'),
                                             choiceValues =list('limma','pls','lmreg', 'lmregrobust',
                                                                'logitreg','logitregrobust','rferadial','rfe','t.test','wilcox.test','welch.test','f.test','kruskal.test','rf','rfboruta','lasso','elasticnet','spls','o1pls'),
                                             selected=c('limma','rfe','rf','pls','lasso'),
                                             width='800px'
                          )
                  )
                           
            ),
           column(width=12,style="margin-top:10px",conditionalPanel(
             condition = "output.checkpvalue_wf2 || output.checkvip_wf2",
             column(width=6,numericInput(width="370px","train_pct_wf2", "Proportion of samples to use for training:",  0.7, min = 0, max = 1)),
             column(width=6,selectInput(width="370px","split_train_test_wf2", "Randmly split the input data into training and test sets:", c("TRUE","FALSE"))),
             column(width=6,selectInput(width="370px","learningsetmethod_wf2", "Method to generate learning sets:", choices=c("Cross-validation"="CV","Monte-carlo cross-validation"="MCCV","Bootstrap"="bootstrap"))),
             column(width=6,selectInput(width="370px","balance_classes_wf2", "Balance classes by generating synthetic samples:", choices=c("FALSE","TRUE"))),
             column(width=6,numericInput(width="370px","kfold_wf2", "k for k-fold Cross Validation (1-10000 limit):", 10, min = 1, max = 10000)),
             column(width=6,style='margin-top:25px;',actionButton("argumentbutton_wf2", "More arguments")),
             bsModal("argument_modal2", "More arguments for feature selection", "argumentbutton_wf2", size = "large",
                     tags$div(
                       width=12,
                       style="height:500px",
                       column(width=6,numericInput(width="370px","pvalue_thresh_wf2", "P-value threshold:", 0.05, min = 0, max = 1)),
                       column(width=6,selectInput(width="370px","fdr_method_wf2","Choose FDR correction method:",choices=c("BH (Benjamini-Hochberg 1995)"="BH","ST (Storey & Tibshi- rani 2001)"="ST","Strimmer (Strimmer 2008)"='Strimmer',"No FDR calculation"="none"))),
                       column(width=6,numericInput(width="370px","fdrthresh_wf2", "False discovery threshold:", 1, min = 0, max = 1)),
                       column(width=6,numericInput(width="370px","pls_vip_thresh_wf2", "VIP threshold (1-100 limit):", 2, min = 1, max = 100)),
                       bsTooltip("pls_vip_thresh_wf2", "This argument only works for PLS or O1PLS models","bottom", options = list(container = "body")),
                       column(width=6,conditionalPanel(
                         condition = "output.checkaggregationmethod_wf2",
                         selectInput(width="370px","aggregation_method_wf2","Methods for aggregating results:",choices=c("Probability-based"="consensus","RankAggreg (cross entropy)"="RankAggreg","RankAggregGA (genetic algorithm)"="RankAggregGA"))
                       )),
                       column(width=6,conditionalPanel(
                              condition = "output.checkaggregationmethod_wf2 && input.aggregation_method_wf2=='consensus'",
                              numericInput(width="370px","prop_select_thresh_wf2", "Min. proportion of subsets in which a variable is selected:", 0.7, min = 0, max = 1),
                              bsTooltip("prop_select_thresh_wf2", "This argument only works for probability-based aggregation","bottom", options = list(container = "body"))
                       )),
                       column(width=6,conditionalPanel(
                         condition = "output.checkmax_varsel_wf2",
                         numericInput(width="370px","max_var_sel_wf2", "Max number of variables to be used:", 100, min = 1, max = 100000),
                         bsTooltip("max_var_sel_wf2", "This argument only works for rferadial, rfe, rf, rfboruta, and spls","bottom", options = list(container = "body"))
                       ))
                       #column(width=6,numericInput(width="350px","pls_ncomp_wf2", "Max number of components to consider:", 5, min = 1, max = 100000)),
                       #bsTooltip("pls_ncomp_wf2", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       #column(width=6,numericInput(width="350px","max_comp_sel_wf2", "Number of components to use for VIP selection:", 1, min = 1, max = 100000)),
                       #bsTooltip("max_comp_sel_wf2", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       #column(width=6,selectInput(width="350px","optselect_wf2","Find optimal number of components:",c("TRUE","FALSE"))),
                       #bsTooltip("optselect_wf2", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       #column(width=6,selectInput(width="350px","pls_vip_selection_wf2","VIP summarization across multiple components:",c("max","mean"))),
                       #bsTooltip("pls_vip_selection_wf2", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       #column(width=6,
                      #        tags$label("Number of permutations for calculating p-values:", `for` = "permu_switch_wf2"),
                       #       div(style="display: inline-block;vertical-align:top; width: 100px;", switchInput(inputId = "permu_switch_wf2",value = FALSE)),
                      #        div(style="display: inline-block;vertical-align:top; width: 250px;", numericInput(inputId = "pls_permut_count_wf2", label = NULL, value = 1000, min = 1, max = 100000))
                       #),
                       #bsTooltip("pls_permut_count_wf2", "This argument only works for PLS, sPLS, or O1PLS models","bottom", options = list(container = "body")),
                       
                     )
             )
           ))
                  
        )
    )
)
