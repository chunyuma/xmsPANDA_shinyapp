library(shiny)

graphical_options_wf1<-fluidRow(
  tags$div(
    id="maindiv",
    column(width=12,
           column(width=6,selectInput(width="350px","heatmap_color_scheme_wf1","Heatmap color palettes/scheme:",c("redblue","yellowblue","redyellowgreen","yellowwhiteblue","redwhiteblue","topo","heat"))),
           column(width=6,numericInput(width="350px","pca_cex_val_wf1", "Size of points on PCA plots (1-20 limit):", 4, min = 1, max = 20))
    ),
    column(width=12,
           column(width=6,numericInput(width="350px","ellipse_conf_level_wf1", "Confidence interval for PCA ellipses (0-1 limit):", 0.95, min = 0, max = 1)),
           column(width=6,selectInput(width="350px","pca_ellipse_wf1","Should ellipse be plotted on PCA plots?",c("TRUE","FALSE")))
    ),
    column(width=12,
           column(width=6,selectInput(width="350px","boxplot_type_wf1", "Boxplot type:", c("ggplot","simple"))),
           column(width=6,selectInput(width="350px","boxplot_jitter_wf1", "Add jitter to boxplots:", c("TRUE","FALSE")))
    ),
    column(width=12,
           column(width=6,selectInput(width="350px","timeseries_lineplots_wf1", "Plot time series lineplots (for time-series data):",c("FALSE","TRUE"))),
           column(width=6,selectInput(width="350px","alphabetical_order_wf1", "Plot classes on the x-axis in alphabetical order:",c("TRUE","FALSE")))
    ),
    column(width=12,
           column(width=6,textInput(width="350px","ylabel_text_wf1", "Label for y-axis in boxplots, barplots, and lineplots:","Abundance",placeholder="Default: Abundance")),
           column(width=6,style='margin-top:25px;',actionButton("graph_argumentbutton_wf1", "More arguments")),
           bsModal("graph_argument_modal", "More arguments for generating graphs", "graph_argumentbutton_wf1", size = "large",
                   tags$div(
                     width=12,
                     style="height:200px",
                     column(width=6,textInput(width="350px","boxplot_color_theme_wf1", "Color theme or options for boxplots:","journal",placeholder="Default: journal")),
                     column(width=6,textInput(width="350px","barplot_color_theme_wf1", "Color theme or options for barplots:","journal",placeholder="Default: journal")),
                     column(width=6,textInput(width="350px","sample_color_theme_wf1", "Color theme or options for samples:","journal",placeholder="Default: journal")),
                     column(width=6,textInput(width="350px","lineplot_color_theme_wf1", "Color theme or options for lineplots:","journal",placeholder="Default: journal")),
                     column(width=12,tags$br()),
                     column(width=12,tags$p("Note: the common options for above arguments include heat, rainbow, red, grey57."))
                   )
           )
    )
  )
)
