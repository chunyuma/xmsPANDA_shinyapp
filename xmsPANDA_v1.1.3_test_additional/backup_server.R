options(shiny.maxRequestSize=100*1024^2)
options(shiny.sanitize.errors=FALSE)
# Server logic
source("R/source_codes/xmsPANDA_v1.0.8.38.R")

server <- function(input, output, session) {
  ##################################  Additional Analysis Page ##########################
  
  check1 <- reactiveValues(count = 1)
  search <- reactiveValues(count = 0)
  id2 <- NULL
  #update1 <- reactiveValues(count = 0)
  off <- reactiveVal(0)
  observeEvent(input$feat_inte,{check1$count=1})
  observeEvent(input$classlabel_inte,{check1$count=1})
  #observeEvent(input$mzvalue,{search$count=0})
  #observeEvent(input$timevalue,{search$count=0})
  
  
  observeEvent(input$start1, 
               {
                 output$nText3 <- renderText({shiny::validate(
                   need(input$feat_inte, "No feature table provided. Please upload your feature table'."),
                   need(input$feat_inte$type=="text/csv" || input$feat_inte$type=="text/plain", "The format of feature table is not correct. Please upload the file with correct format."),
                   need(input$classlabel_inte, "No class label file provided. Please upload class label file in 'Choose Files'."),
                   need(input$classlabel_inte$type=="text/csv" || input$classlabel_inte$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format.")
                 )})
                 shiny::validate(
                   need(input$feat_inte, "No feature table provided. Please upload your feature table'."),
                   need(input$feat_inte$type=="text/csv" || input$feat_inte$type=="text/plain", "The format of feature table is not correct. Please upload the file with correct format."),
                   need(input$classlabel_inte, "No class label file provided. Please upload class label file in 'Choose Files'."),
                   need(input$classlabel_inte$type=="text/csv" || input$classlabel_inte$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format.")
                 )
                 check1$count=0
                 id2 <<- showNotification("Scatter plot is generating now.", duration=NULL)
               })
  
  
  feat_inte <- reactive({
    if(input$start1!=0  & check1$count==0 & !is.null(input$feat_inte$name) ){
      
      if((input$feat_inte$type=="text/csv" || input$feat_inte$type=="text/plain")){
        req(input$feat_inte)
        if(input$feat_inte$type=="text/plain"){
          feature_table_file <- read.delim(input$feat_inte$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$feat_inte$type=="text/csv"){
            feature_table_file <- read.csv(input$feat_inte$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        feature_table_file 
        
      }
    }
  })
  
  classlabel_inte <- reactive({
    if(input$start1!=0  & check1$count==0 & !is.null(input$classlabel_inte$name)){
      
      if((input$classlabel_inte$type=="text/csv" || input$classlabel_inte$type=="text/plain")){
        req(input$classlabel_inte)
        if(input$classlabel_inte$type=="text/plain"){
          class_labels_file <- read.delim(input$classlabel_inte$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$classlabel_inte$type=="text/csv"){
            class_labels_file <- read.csv(input$classlabel_inte$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        class_labels_file 
      }
    }
  })
  
  plot1 <- reactive({
    
    if(input$start1!=0  & check1$count==0 & !is.null(feat_inte()) & !is.null(classlabel_inte())){
      
      feat=feat_inte()
      colnames(feat)<- gsub('^X','',colnames(feat))
      
      if(isolate(input$diagramtype)=='manhattan'){
        
        
        if(isolate(input$yaxislabel)=='pvalue'){
            
            manhattan=feat[,c('Name','P.value','adjusted.P.value','max.fold.change.log2')]
            manhattan=manhattan[grep("_",manhattan$Name),]
            rownames(manhattan)=1:nrow(manhattan)
            manhattan = manhattan %>% separate(Name,c("mz","time"),remove=FALSE,sep="_")
            manhattan$mz=as.numeric(manhattan$mz)
            manhattan$time=as.numeric(manhattan$time)

                   
          if(isolate(input$adjdashline)=='yes'){
            
            if(isolate(input$psignif)=='pvalue'){
              
              cutoff <- isolate(input$pvaluecutoff)
            }else{
              
              cutoff <- max(manhattan[manhattan$P.value< isolate(input$pvaluecutoff) & manhattan$adjusted.P.value< isolate(input$adjpvaluecutoff),'P.value'])
            }
            
            adjdashline <- geom_hline(yintercept=-log10(max(manhattan[manhattan$P.value< isolate(input$pvaluecutoff) & manhattan$adjusted.P.value< isolate(input$adjpvaluecutoff),'P.value'])),linetype='dotted',color= input$dottedlinecol)
            
          }else{
            
            cutoff <- isolate(input$pvaluecutoff)
            adjdashline <- NULL
            
          }
          
          
          if(isolate(input$labelexpression)=='yes'){
            
            manhattan[manhattan$P.value<cutoff & manhattan$max.fold.change.log2>0,'color']<- input$poscol
            manhattan[manhattan$P.value<cutoff & manhattan$max.fold.change.log2<0,'color']<- input$negcol
          }else{
            
            manhattan[manhattan$P.value<cutoff,'color']<- input$sigcol
          }
          
          manhattan[manhattan$P.value>=cutoff,'color']<- input$insigcol
          manhattan[manhattan$color== input$insigcol,'size']=0.2
          manhattan[!(manhattan$color== input$insigcol),'size']=1.5
          pal <- levels(factor(manhattan$color))
          names(pal)<-pal
          
          if(isolate(input$plottype)=='type1'){
            
            xincrement=input$x_axis_spacing_type2
            xmin_val <- min(0,manhattan$mz)
            xmax_val <- max(manhattan$mz)
            xlabel <- xlab('mass-to-charge (m/z)')
            maintitle <- ggtitle('Type 1 manhattan plot (-log10p vs mz)')
            gplotline <- ggplot(manhattan, aes(x=mz, y=-log10(P.value), color=color, text=manhattan$Name))
            
          }else{
            
            xincrement=input$x_axis_spacing_type2 #round_any(max(manhattan$time)/10,10,f=floor)
            xmin_val <- min(0,manhattan$time)
            xmax_val <- max(manhattan$time)
            xlabel <- xlab('Retention time (s)')
            maintitle <- ggtitle('Type 2 manhattan plot (-log10p vs time)')
            gplotline <- ggplot(manhattan, aes(x=time, y=-log10(P.value), color=color, text=manhattan$Name))
          }
          
          yvec <- -log10(manhattan$P.value)
          if(max(yvec)>20){yvec[yvec>20]<-20}
          ymax_val <- max(yvec)
          ymax=floor(max(yvec))+1
          ylabel <- ylab('-log10p')
          dashline <- geom_hline(yintercept=-log10(isolate(input$pvaluecutoff)),linetype='dashed',color= input$dashedlinecol)
          
        }else{
          
          manhattan=feat[,c('Name','VIP', 'max.fold.change.log2')]
          manhattan=manhattan[grep("_",manhattan$Name),]
          rownames(manhattan)=1:nrow(manhattan)
          manhattan = manhattan %>% separate(Name,c("mz","time"),remove=FALSE,sep="_")
          manhattan$mz=as.numeric(manhattan$mz)
          manhattan$time=as.numeric(manhattan$time)
          
          if(isolate(input$labelexpression)=='yes'){
            
            manhattan[manhattan$VIP > isolate(input$vipcutoff) & manhattan$max.fold.change.log2>0,'color']<- input$poscol
            manhattan[manhattan$VIP > isolate(input$vipcutoff) & manhattan$max.fold.change.log2<0,'color']<- input$negcol
          }else{
            
            manhattan[manhattan$VIP > isolate(input$vipcutoff),'color']<- input$sigcol
          }
          
          manhattan[manhattan$VIP<= isolate(input$vipcutoff),'color']<- input$insigcol
          manhattan[manhattan$color== input$insigcol,'size']=0.2
          manhattan[!(manhattan$color== input$insigcol),'size']=1.5
          pal <- levels(factor(manhattan$color))
          names(pal)<-pal
          
          if(isolate(input$plottype)=='type1'){
            
            xincrement= input$x_axis_spacing_type1
            xmin_val <- min(0,manhattan$mz)
            xmax_val <- max(manhattan$mz)
            xlabel <- xlab('mass-to-charge (m/z)')
            maintitle <- ggtitle('Type 1 manhattan plot (VIP vs mz)')
            gplotline <- ggplot(manhattan, aes(x=mz, y=VIP, color=color, text=rownames(manhattan)))
            
          }else{
            
            xincrement=round_any(max(manhattan$time)/10,10,f=floor)
            xmin_val <- min(0,manhattan$time)
            xmax_val <- max(manhattan$time)
            xlabel <- xlab('Retention time (s)')
            maintitle <- ggtitle('Type 2 manhattan plot (VIP vs time)')
            gplotline <- ggplot(manhattan, aes(x=time, y=VIP, color=color, text=rownames(manhattan)))
          }
          
          yvec <- manhattan[,3]
          if(max(yvec)>20){yvec[yvec>20]<-20}
          ymax_val <- max(yvec)
          ymax=floor(max(yvec))+1
          ylabel <- ylab('VIP')
          dashline <- geom_hline(yintercept= isolate(input$vipcutoff),linetype='dashed',color= input$dashedlinecol)
          
        }
        
        p1 <- gplotline +
          geom_point(show.legend=F,size=manhattan$size,alpha=0.7) + 
          xlabel + ylabel +
          scale_color_manual(values=pal) +
          scale_x_continuous(breaks = seq(xmin_val, xmax_val, by = xincrement)) +
          scale_y_continuous(breaks = seq(0, (ymax_val + 2), by = input$y_axis_spacing), limits = c(0,ymax)) +
          dashline + adjdashline +
          maintitle +
          theme(axis.title=element_text(size=12), 
                axis.text =element_text(size=10),
                axis.text.x = element_text(angle=0),
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                plot.title = element_text(size=14, hjust = 0.5),
                legend.position = "none")
        
        p1
        
      }else{
        
        plotdata=feat[,c('Name','P.value','adjusted.P.value','max.fold.change.log2')]
        
        colnames(plotdata)[which(colnames(plotdata)=="max.fold.change.log2")] <- "log2foldchange"
        plotdata$log10pvalue <- -log10(plotdata$P.value)
        
        
        plotdata[plotdata$log2foldchange > log2(isolate(input$volcanorfc)) & plotdata$log10pvalue > -log10(isolate(input$volcanopcutoff)), 'color'] <- input$volcanoposcol
        plotdata[plotdata$log2foldchange < log2(isolate(input$volcanolfc)) & plotdata$log10pvalue > -log10(isolate(input$volcanopcutoff)), 'color'] <- input$volcanonegcol
        plotdata[is.na(plotdata$color),'color'] <- input$volcanoinsigcol
        plotdata[plotdata$color== input$volcanoinsigcol,'size']=0.2
        plotdata[!(plotdata$color== input$volcanoinsigcol),'size']=1.5
        pal <- levels(factor(plotdata$color))
        names(pal)<-pal
        
        p1 <- ggplot(data=plotdata,aes(x=plotdata$log2foldchange, y=log10pvalue, color=color, text=rownames(plotdata))) +
          geom_point(show.legend=F,size=plotdata$size,alpha=0.7) + 
          scale_y_continuous(name="-log10(p-value)") +
          scale_x_continuous(name="log2(Fold Change)")+
          geom_hline(yintercept = -log10(isolate(input$volcanopcutoff)), linetype=2, colour= input$volcanopcutoffdashed) + 
          geom_vline(xintercept = log2(isolate(input$volcanolfc)), linetype=2, colour= input$volcanolfcdashed) + 
          geom_vline(xintercept = log2(isolate(input$volcanorfc)), linetype=2, colour= input$volcanorfcdashed) +
          scale_color_manual(values=pal) +
          theme(axis.title=element_text(size=12), 
                axis.text =element_text(size=10),
                axis.text.x = element_text(angle=0),
                axis.line = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                plot.title = element_text(size=14, hjust = 0.5),
                legend.position = "none")
        
        
        p1
        
      }
      
    }
    
  })
  
  
  pplot1 <- reactive({
    
    if(!is.null(plot1())){
      
      ggp1 <- ggplotly(plot1(),tooltip = c("text"),source="scatterplot") 
      
      #%>% config(displayModeBar = F)
      ggp1
      
    }
  })
  
  
  
  output$scatterplot <- renderPlotly({
    
    if(!is.null(pplot1())){
      
      if (!is.null(id2)){
        removeNotification(id2)
        id2 <<- showNotification("Click each point on scatter plot to show its corresponding boxplot.", duration=NULL)
      }
      pplot1()
      
    }
    
  })
  
  
  plot2 <- reactive({
    
    if(!is.null(pplot1())){
      
      d <- event_data('plotly_click',source = 'scatterplot')
      if(!is.null(d)){
        feat=feat_inte()
        colnames(feat)<- gsub('^X','',colnames(feat))
        x1<-as.numeric(d['curveNumber']+1);x2<-as.numeric(d['pointNumber']+1)
        text <- pplot1()$x$data[[x1]]$text[x2]
        mz <- unlist(strsplit(unlist(strsplit(text,split = ' '))[1],split = ':'))[2]
        time <- unlist(strsplit(unlist(strsplit(text,split = ' '))[2],split = ':'))[2]
        if(!is.na(mz) & !is.na(time)){
          group <- names(table(classlabel_inte()[,2]))
          boxplotdata <- data.frame(matrix(NA,ncol=2,nrow=1))
          colnames(boxplotdata) <- c('group','intensity')
          for(i in 1:length(group)){
            #print('yes1')
            #print(feat[feat$mz==mz & feat$time==time,classlabel_inte()[classlabel_inte()[,2]==group[i],1]])
            tmp2 <- as.numeric(feat[feat$mz==mz & feat$time==time,classlabel_inte()[classlabel_inte()[,2]==group[i],1]])
            tmp1 <- rep(group[i],length(tmp2))
            tmpdata <- cbind(tmp1,tmp2)
            colnames(tmpdata) <- c('group','intensity')
            boxplotdata <- rbind(boxplotdata,tmpdata)
          }
          boxplotdata <- boxplotdata[-1,]
          boxplotdata$intensity <- as.numeric(boxplotdata$intensity)
          if(isolate(input$boxplotcolor)=='yes'){
            p2 <- ggboxplot(boxplotdata, x = "group", y = "intensity", color = "group", bxp.errorbar = TRUE, bxp.errorbar.width=0.5, size = 0.5) +
              ggtitle(text) + ylab('log2(intensity)') +
              theme(axis.title=element_text(size=12), 
                    axis.text =element_text(size=10),
                    axis.text.x = element_text(angle=0),
                    axis.line = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    plot.title = element_text(size=14, hjust = 0.5))
          }else{
            p2 <- ggboxplot(boxplotdata, x = "group", y = "intensity", bxp.errorbar = TRUE, bxp.errorbar.width=0.5, size = 0.5) +
              ggtitle(text) + ylab('log2(intensity)') +
              theme(axis.title=element_text(size=12), 
                    axis.text =element_text(size=10),
                    axis.text.x = element_text(angle=0),
                    axis.line = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    plot.title = element_text(size=14, hjust = 0.5))
          }
          p2
        }else{
          NULL 
        }
      } 
    }
    
  })
  
  
  observeEvent(input$search,{search$count=1})
  
  plot3 <- reactive({
    
    if(!is.null(pplot1())){
      if(input$search!=0 & search$count==1){
        if(!isolate(input$mzvalue)=="" & !isolate(input$timevalue)==""){
          feat=feat_inte()
          colnames(feat)<- gsub('^X','',colnames(feat))
          mz <- isolate(input$mzvalue)
          time <- isolate(input$timevalue)
          group <- names(table(classlabel_inte()[,2]))
          boxplotdata <- data.frame(matrix(NA,ncol=2,nrow=1))
          colnames(boxplotdata) <- c('group','intensity')
          for(i in 1:length(group)){
            tmp2 <- as.numeric(feat[feat$mz==mz & feat$time==time,classlabel_inte()[classlabel_inte()[,2]==group[i],1]])
            tmp1 <- rep(group[i],length(tmp2))
            tmpdata <- cbind(tmp1,tmp2)
            colnames(tmpdata) <- c('group','intensity')
            boxplotdata <- rbind(boxplotdata,tmpdata)
          }
          boxplotdata <- boxplotdata[-1,]
          boxplotdata$intensity <- as.numeric(boxplotdata$intensity)
          text <- paste('mz:',mz,' time:',time,sep='')
          if(isolate(input$boxplotcolor)=='yes'){
            p3 <- ggboxplot(boxplotdata, x = "group", y = "intensity", color = "group", bxp.errorbar = TRUE, bxp.errorbar.width=0.5, size = 0.5) +
              ggtitle(text) + ylab('log2(intensity)') +
              theme(axis.title=element_text(size=12), 
                    axis.text =element_text(size=10),
                    axis.text.x = element_text(angle=0),
                    axis.line = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    plot.title = element_text(size=14, hjust = 0.5))
          }else{
            
            p3 <- ggboxplot(boxplotdata, x = "group", y = "intensity", bxp.errorbar = TRUE, bxp.errorbar.width=0.5, size = 0.5) +
              ggtitle(text) + ylab('log2(intensity)') +
              theme(axis.title=element_text(size=12), 
                    axis.text =element_text(size=10),
                    axis.text.x = element_text(angle=0),
                    axis.line = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                    plot.title = element_text(size=14, hjust = 0.5))
          }
          p3
        }
      } 
    }
    
  })
  
  observeEvent({if(!is.null(event_data('plotly_click',source = 'scatterplot'))) TRUE else return()},{off(0)})
  observeEvent(input$search,{off(1)})
  #observeEvent({if(!input$mzvalue=="" || !input$timevalue=="") TRUE else return()},{off(3)})
  
  plot4 <- reactive({
    
    if(!is.null(plot3()) & off()==1){
      plot3()
    }else if(!is.null(plot2()) & off()==0){
      plot2()
    }else{
      NULL
    }
    
  })
  
  
  pplot2 <- reactive({
    
    if(!is.null(plot4())){
      pp2 <- ggplotly(plot4()) %>% config(displayModeBar = F)
      pp2   
    }
    
  })
  
  
  output$boxplot <- renderPlotly({
    
    if(!is.null(pplot2())){
      if (!is.null(id2)){
        removeNotification(id2)
        id2 <<- NULL
      }
      pplot2()
    }
  })
  
  output$checkplot1 <- reactive({!is.null(pplot1())})
  outputOptions(output, "checkplot1", suspendWhenHidden = FALSE)
  output$checkplot2 <- reactive({!is.null(pplot2())})
  outputOptions(output, "checkplot2", suspendWhenHidden = FALSE)
  
  output$downloadPlot1 <- downloadHandler(
    
    filename <- "scatterplot.png",
    content = function(file) {
      ggsave(file, plot = plot1(), device = "png")
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    
    filename <- "boxplot.png",
    content = function(file) {
      ggsave(file, plot = plot4(), device = "png")
    }
  )
  
  comparison_table <- reactive({
    
    if(!is.null(pplot1()) & !is.null(plot4())){
      
      plot4data <- plot4()$data
      rownames(plot4data) <- 1:dim(plot4data)[1]
      group <- as.character(unique(plot4data[,1]))
      comparison_table <- as.data.frame(t(combn(group,2)))
      colnames(comparison_table) <- c('group1','group2')
      for(i in 1:dim(comparison_table)[1]){
        group1=plot4data[plot4data[,1]==as.character(comparison_table$group1[i]),2]
        group2=plot4data[plot4data[,1]==as.character(comparison_table$group2[i]),2]
        comparison_table[i,'t.test.pvalue'] <- round(t.test(group1,group2)$p.value,5)
        comparison_table[i,'wilcoxon.test.pvalue'] <- round(wilcox.test(group1,group2)$p.value,5)
      }
      
      comparison_table
      
    }
    
  })
  
  
  
  output$boxplottbl <- renderDT(comparison_table(),options = list(lengthChange = FALSE, dom='t'), rownames=FALSE)
  
  output$tblen <- reactive({paste('height:',(dim(comparison_table())[1]-1)*40 + 55,'px',sep='')})
  outputOptions(output, "tblen", suspendWhenHidden = FALSE)
  
  
  ########################
  
  check2 <- reactiveValues(count = 0)
  #update2 <- reactiveValues(count = 0)
  done2 <- reactiveValues(cluster_table = matrix())
  id3 <- NULL
  #off <- reactiveVal(0)
  #observeEvent(input$clusterinput,{update2$count=1;start2$count=0})
  #observeEvent(input$kegg_species_code,{update2$count=1;start2$count=0})
  #observeEvent(input$database,{update2$count=1;start2$count=0})
  #observeEvent(input$type.statistic,{update2$count=1;start2$count=0})
  observeEvent(input$start2,{check2$count=0})
  
  
  observeEvent(input$start2, 
               {
                 output$nText4 <- renderText({shiny::validate(
                   need(input$clusterinput, "No data file was provided. Please upload your data file."),
                   need(input$clusterinput$type=="text/csv" || input$clusterinput$type=="text/plain", "The format of data file is not correct. Please upload the file with correct format.")
                 )})
                 shiny::validate(
                   need(input$clusterinput, "No data file was provided. Please upload your data file."),
                   need(input$clusterinput$type=="text/csv" || input$clusterinput$type=="text/plain", "The format of data file is not correct. Please upload the file with correct format.")
                 )
                 check2$count=1
                 id3 <<- showNotification("Data is processing now.", duration=NULL)
               })
  
  
  cluster_metab_data <- reactive({
    
    if(input$start2!=0  & check2$count==1){
      
      if(input$clusterinput$type=="text/plain"){
        metab_data <- read.delim(input$clusterinput$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
      }else{
        if(input$clusterinput$type=="text/csv"){
          metab_data <- read.csv(input$clusterinput$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          
          metab_data <- NULL
        }
      }
      metab_data
    }
    
  })
  
  output$nText5 <- renderText({
    
    if(input$start2!=0  & check2$count==1){
      
      kegg_species_code <- switch(isolate(input$kegg_species_code), "Homo sapiens(default)" = "hsa",
                                  "Mus musculus" = "mmu",
                                  "Pan troglodytes" = "ptr",
                                  "Macaca mulatta" = "mcc",
                                  "Bos taurus" = "bta",
                                  "Rattus norvegicus" = "rno",
                                  "Danio rerio"= "dre",
                                  "C. elegans"= "cel",
                                  "Drosophila melanogaster"= "dme"
      )
      
      if(isolate(input$database)=='pathway(default)'){
        database="pathway"
      }else{
        database="module"
      }
      
      if(isolate(input$type.statistic)=='TRUE'){
        type.statistic="p-value"
      }else{
        type.statistic="other"
      }
      
      
      #print("yes1")
      #print(head(cluster_metab_data()))
      #print(c(kegg_species_code,database,type.statistic))
      #done2$cluster_table <- ctable_module
      done2$cluster_table <-get_fcs(metab_data=cluster_metab_data(),kegg_species_code=kegg_species_code,database=database,type.statistic=type.statistic)
      NULL
      #print(dim(done2$cluster_table))
      #print("yes2")
      
    }else{
      
      NULL
    }
    
  })
  
  
  observeEvent({if(dim(done2$cluster_table)[2]==4) TRUE else return()},{
    if (!is.null(id3)){
      removeNotification(id3)
      id3 <<- NULL
    }
  })
  
  
  output$pathwaytb <- renderDT(
    
    if(dim(done2$cluster_table)[2]==4){
      
      table = done2$cluster_table
      table[,1] <- as.character(table[,1])
      
      for(i in 1:dim(table)[1]){
        
        if(isolate(input$database)=='pathway(default)'){
          table[i,1]=paste("<a target='_blank' href='https://www.genome.jp/dbget-bin/www_bget?",table[i,1],"'>",table[i,1],"</a>",sep="")
        }else{
          
          moduleid = strsplit(table[i,1],split = ":")[[1]][length(strsplit(table[i,1],split = ":")[[1]])]
          table[i,1]=paste("<a target='_blank' href='https://www.kegg.jp/kegg-bin/show_module?",moduleid,"'>",table[i,1],"</a>",sep="")
        }
        
      }
      
      all=dim(table)[1]
      if(all>1000){
        lines=c(5,10,50,100,500,1000,all)
      }else{
        lines=c(5,10,50,100,500,1000)
      }
      
      datatable(table,options = list(dom='lrtip', lengthMenu = lines), rownames=FALSE, escape = FALSE)
    }
    
  )
  
  output$downloadtableData <- downloadHandler(
    filename = function() {
      if(input$database=='pathway(default)'){
        paste("pathway_table", ".csv", sep = "")
      }else{
        paste("module_table", ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(done2$cluster_table, file, row.names = FALSE)
    }
  )
  # output$downloadPlot3 <- downloadHandler(
  #   
  #   filename <- "barplot",
  #   content = function(file) {
  #     
  #     if(is.na(input$figurewidth)){
  #       width=10
  #     }else{
  #       width=input$figurewidth
  #     }
  #     
  #     if(is.na(input$figureheight)){
  #       height=6
  #     }else{
  #       height=input$figureheight
  #     }
  #     ggsave(file, plot = plot5(), device = "png", width = width, height = height, units='in')
  #   }
  # )
  
  
  #output$checkplot3 <- reactive({!is.null(pplot3())})
  #outputOptions(output, "checkplot3", suspendWhenHidden = FALSE)
  output$checktable1 <- reactive({input$start2!=0  & check2$count==1 & dim(done2$cluster_table)[2]==4})
  outputOptions(output, "checktable1", suspendWhenHidden = FALSE)
  
  
  output$downloadbutton <- renderUI({
    column(12,style="margin-top:20px;text-align:right;",
           downloadButton(style = "background-color:#417ee0;color:#ffffff;",outputId = "downloadtableData", label = "Download Table")
    )
  })
  
  #output$pathwaytb <- renderTable({pathway_table()[,-1]}, striped = TRUE) 
  
  #output$hover <- renderPrint(list(input$figurewidth,input$figureheight))
  
  ########################
  
  check3 <- reactiveValues(count = 0)
  done3 <- reactiveValues(count = 0)
  id4 <- NULL
  #off <- reactiveVal(0)
  observeEvent(input$start3,{check3$count=0})
  
  
  observeEvent(input$start3, 
               {
                 output$nText6 <- renderText({shiny::validate(
                   need(input$featuretable_file, "No feature table provided. Please upload your feature table."),
                   need(input$featuretable_file$type=="text/csv" || input$featuretable_file$type=="text/plain", "The format of feature table is not correct. Please upload the file with correct format."),
                   need(input$classlabel_file, "No class label file was provided. Please upload your class label file."),
                   need(input$classlabel_file$type=="text/csv" || input$classlabel_file$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format."),
                   need(input$ref_meta_file, "No standard metabolite library file was provided. Please upload your data file."),
                   need(input$ref_meta_file$type=="text/csv" || input$ref_meta_file$type=="text/plain", "The format of standard metabolite library file is not correct. Please upload the file with correct format."),
                   need(any(c(input$step1,input$step2,input$step3)),"Please select at least one step."),
                   if(input$step3){
                     need(input$foldchange_file, "No fold change file was provided. Please upload your data file.")
                   },
                   if(input$step3){
                     need(input$foldchange_file$type=="text/csv" || input$foldchange_file$type=="text/plain", "The format of fold change file is not correct. Please upload the file with correct format.")
                   }
                 )})
                 shiny::validate(
                   need(input$featuretable_file, "No feature table provided. Please upload your feature table."),
                   need(input$featuretable_file$type=="text/csv" || input$featuretable_file$type=="text/plain", "The format of feature table is not correct. Please upload the file with correct format."),
                   need(input$classlabel_file, "No class label file was provided. Please upload your class label file."),
                   need(input$classlabel_file$type=="text/csv" || input$classlabel_file$type=="text/plain", "The format of class label file is not correct. Please upload the file with correct format."),
                   need(input$ref_meta_file, "No standard metabolite library file was provided. Please upload your data file."),
                   need(input$ref_meta_file$type=="text/csv" || input$ref_meta_file$type=="text/plain", "The format of standard metabolite library file is not correct. Please upload the file with correct format."),
                   need(any(c(input$step1,input$step2,input$step3)),"Please select at least one step."),
                   if(input$step3){
                     need(input$foldchange_file, "No fold change file was provided. Please upload your data file.")
                   },
                   if(input$step3){
                     need(input$foldchange_file$type=="text/csv" || input$foldchange_file$type=="text/plain", "The format of fold change file is not correct. Please upload the file with correct format.")
                   }
                 )
                 check3$count=1
                 id4 <<- showNotification("Data is processing now.", duration=NULL)
               })
  
  
  featuretable_file <- reactive({
    if(input$start3!=0  & check3$count==1 & !is.null(input$featuretable_file$name) ){
      
      if((input$featuretable_file$type=="text/csv" || input$featuretable_file$type=="text/plain")){
        req(input$featuretable_file)
        if(input$featuretable_file$type=="text/plain"){
          featuretable_file <- read.delim(input$featuretable_file$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$featuretable_file$type=="text/csv"){
            featuretable_file <- read.csv(input$featuretable_file$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        featuretable_file 
        
      }
    }
  })
  
  classlabel_file <- reactive({
    if(input$start3!=0 & check3$count==1 & !is.null(input$classlabel_file$name) ){
      
      if((input$classlabel_file$type=="text/csv" || input$classlabel_file$type=="text/plain")){
        req(input$classlabel_file)
        if(input$classlabel_file$type=="text/plain"){
          classlabel_file <- read.delim(input$classlabel_file$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$classlabel_file$type=="text/csv"){
            classlabel_file <- read.csv(input$classlabel_file$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        classlabel_file 
        
      }
    }
  })
  
  ref_meta_file <- reactive({
    if(input$start3!=0 & check3$count==1 & !is.null(input$ref_meta_file$name) ){
      
      if((input$ref_meta_file$type=="text/csv" || input$ref_meta_file$type=="text/plain")){
        req(input$ref_meta_file)
        if(input$ref_meta_file$type=="text/plain"){
          ref_meta_file <- read.delim(input$ref_meta_file$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$ref_meta_file$type=="text/csv"){
            ref_meta_file <- read.csv(input$ref_meta_file$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        ref_meta_file 
        
      }
    }
  })
  
  foldchange_file <- reactive({
    if(input$start3!=0 & check3$count==1 & input$step3 & !is.null(input$foldchange_file$name) ){
      
      if((input$foldchange_file$type=="text/csv" || input$foldchange_file$type=="text/plain")){
        req(input$foldchange_file)
        if(input$foldchange_file$type=="text/plain"){
          foldchange_file <- read.delim(input$foldchange_file$datapath,header=TRUE,sep="\t",stringsAsFactors=FALSE,check.names=FALSE)
        }else{
          if(input$foldchange_file$type=="text/csv"){
            foldchange_file <- read.csv(input$foldchange_file$datapath,header=TRUE,sep=",",stringsAsFactors=FALSE,check.names=FALSE)
          }
        }
        foldchange_file 
        
      }
    }else{
      
      NA
    }
  })
  
  session_outloc_quant <- reactive({
    if(input$start3!=0 & check3$count==1){
      cur_date<-Sys.time()
      cur_date<-gsub(x=cur_date,pattern="-",replacement="")
      cur_date<-gsub(x=cur_date,pattern=":",replacement="")
      cur_date<-gsub(x=cur_date,pattern=" ",replacement="")
      outloc<-paste('~/metabolite_quantification_analysis_results',cur_date,sep="")
      outloc
    }else{
      NULL
    }
  })
  
  
  output$nText7 <- renderText({
    
    if(input$start3!=0  & check3$count==1){
      
      isolate({
        steps=""
        if(input$step1){
          steps=paste(steps,"1",sep="")
          if(input$groupcheck=="TRUE"){
            groupcheck=TRUE
          }else{
            groupcheck=FALSE
          }
          if(input$targetID==""){
            targetID=NA
          }else{
            targetID=unlist(strsplit(input$targetID,split=","))
          }
        }else{
          groupcheck=FALSE
          targetID=NA
        }
        if(input$step2){
          steps=paste(steps,"2",sep="")
        }
        if(input$step3){
          steps=paste(steps,"3",sep="")
          highcolor=input$highcolor
          lowcolor=input$lowcolor
          if(!is.na(input$minhit)){
            minhit <- input$minhit
          }else{
            stop("Please enter the correct value for 'Minimum #metablites hitted in KEGG map'.")
          }
        }else{
          minhit=3
          highcolor="red"
          lowcolor="blue"
        }
        
        if(input$summarize_replicates=="TRUE"){
          summarize_replicates=TRUE
        }else{
          summarize_replicates=FALSE
        }
        
        if(!is.na(input$num_replicate2)){
          num_replicate2 <- input$num_replicate2
        }else{
          stop("Please enter the correct value for 'Number of technical replicates'.")
        }
        
        if(!is.na(input$rep_max_missing_thresh)){
          rep_max_missing_thresh <- input$rep_max_missing_thresh
        }else{
          stop("Please enter the correct value for 'Maximum missing value ratio'.")
        }
        
        if(!is.na(input$mass_error)){
          mass_error <- input$mass_error
        }else{
          stop("Please enter the correct value for 'Mass-to-charge tolerance'.")
        }
        
        if(!is.na(input$time_error)){
          time_error <- input$time_error
        }else{
          stop("Please enter the correct value for 'Retention time tolerance'.")
        }
        
        quant(Xmat=featuretable_file(),Ymat=classlabel_file(),Wmat=ref_meta_file(),Zmat=foldchange_file(),
              feature_table=NA,class_file=NA,ref_list=NA,foldchange_list=NA,
              outloc=session_outloc_quant(),
              num_replicates=num_replicate2,
              summarize_replicates=summarize_replicates,
              rep.max.missing.thresh=rep_max_missing_thresh,
              summary.method=input$summary_method,
              mass_error= mass_error,
              time_error= time_error,
              percent_node=1,
              steps=steps,
              min_num_nonmissing=3,
              targetID=targetID,
              minhit=minhit,
              groupcheck=groupcheck,
              highcolor=highcolor,
              lowcolor=lowcolor) 
        
        
        done3$count=1
        #file.copy(paste(getwd(),'matrix_centrality.txt',sep='/'),session_outloc())
        setwd(session_outloc_quant())
        zip(zipfile=paste(basename(session_outloc_quant()),'zip',sep='.'), files='.')
        print("Processing complete. Please click on download button to save the results.")
        
      })  
      
    }else{
      
      NULL
    }
    
  })
  
  
  observeEvent({if(done3$count==1) TRUE else return()},{
    if (!is.null(id4)){
      removeNotification(id4)
      id4 <<- NULL
    }
  })
  
  output$downloadQdata <- downloadHandler(
    
    #if(input$go!=0 && input$featselmethod!="-" && input$feature_table_file!="" && input$class_labels_file!=""){
    
    filename <- function() {
      paste(basename(session_outloc_quant()), "zip", sep=".")
    },
    content <- function(file) {
      fname1<-paste(session_outloc_quant(),"/",basename(session_outloc_quant()), ".zip", sep="")
      file.copy(fname1, file)
    },
    contentType = "application/zip"
    #}
  )
  
  output$done <- reactive({done3$count==1})
  outputOptions(output, "done", suspendWhenHidden = FALSE)
} 
