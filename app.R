library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shinycssloaders)
library(smacof)
library(gplots)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(readxl)
library(goeveg)
library(ggConvexHull)
library(qlcMatrix)
library(vegan)
library(ggpubr)
library(ggplot2)
library(plotly)
library("ggplotify")
library(ggfortify)
library(grid)

dataSets <- function(){
  a <- data(package ="smacof")
  a <- as.data.frame(a$results)
  a <- a$Item
  
  return(as.character(a))
}


# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
  dashboardHeaderPlus(title = "MDS"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Data",tabName = "data"),
      menuItem("Explore",tabName = "explore"),
      menuItem("Diagnostic",tabName = "diagnostics"),
      menuItem("Final Model",tabName = "final")
      
      )
    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      
      tabItem(tabName = "data",
              boxPlus(
                tags$h4("Data Selection"),
                tags$hr(),
                checkboxInput("upload","I will upload"),
                checkboxInput("select","I will select"),
                conditionalPanel(
                  condition = "input.upload == true ",
                  fileInput("dataInput","Upload your data")
                  
                ),
                conditionalPanel(
                  condition = "input.select == true",
                  selectInput("dataSelect","Select Data Set",choices = dataSets())
                  
                ),
                
                tags$h4("Type"),
                tags$hr(),
                checkboxInput("direct","Direct"),
                checkboxInput("indirect","Indirect"),
                conditionalPanel(condition = "input.indirect == true",
                                 tags$hr(),
                                 tags$h3("Indirect Types"),
                                 tags$hr(),
                                 checkboxInput("nominal","Nominal"),
                                 conditionalPanel(
                                   condition = "input.nominal == true", 
                                   tags$hr(),
                                   selectInput("nominMethod","Select Method",c("chuprov", "g",  "mutual", "variation")),
                                   tags$hr()
                                 ),
                                 
                                 
                                 checkboxInput("distance","Distance"),
                               
                                 conditionalPanel(
                                   condition = "input.distance==true",
                                   tags$hr(),
                                   
                                  
                                   checkboxInput("disBin","Binary?"),
                                   selectInput("disMtd","Select Method",c("manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis")),
                                   
                                   tags$hr(),
                                   checkboxInput("disCol","Column"),
                                   checkboxInput("disRow","Row"),
                                   tags$hr()
                                 ),
                                 checkboxInput("similarity","Similarity"),
                                 conditionalPanel(
                                   condition = "input.similarity==true",
                                   tags$hr(),
                                   selectInput("simMtd","Select Method",c("pearson", "kendall", "spearman")),
                                   checkboxInput("simCol","Column"),
                                   checkboxInput("simRow","Row"),
                                   tags$hr()
                                   
                                   
                                 ),
                                 checkboxInput("gravity","Gravity")
                                 
                ),
                actionButton("screeGo","Scree Plot"),
                tags$hr(),
                tags$h4("IC Plot"),
                selectInput("di","Enter The Dim Number",c(2,3)),
                selectInput("ty","TY",c("ordinal","interval", "ratio" , "mspline")),
                selectInput("tie","Ties",c("primary","secondary","tertiary")),
                actionButton("go","Go")
                
                
              ),#end of box
              boxPlus(
                title = "Plot",
                withSpinner(
                  plotOutput("plot0")),
                withSpinner(
                  plotOutput("plot1")),
                footer = "Lorem Ipsum"
              )
              
              
              
      ),#endof data tab
      tabItem(tabName = "explore",
              box(
                tags$h4("Model"),
                numericInput("confNo","Select Conf",""),
                checkboxInput("mds","MDS"),
                checkboxInput("indscal","Indscal"),
                checkboxInput("idioscal","Idioscal"),
                tags$hr(),
                
                selectInput("dimSelect","Select Comparison Dimention",c("D2","D3")),
                checkboxInput("external","External groups?",value = T),
                conditionalPanel(
                  condition = "input.external == false",
                  textInput("noOfCluster","Enter number of clusters")
                ),
                checkboxInput("externalGroups","External scales?"),
                conditionalPanel(
                  condition = "input.externalGroups == true && input.di == 2",
                  textInput("scale1","Enter Scale Name"),
                  textInput("scale2","Enter Scale Name")
                  
                ),
                
                conditionalPanel(
                  condition = "input.externalGroups == true && input.di == 3",
                  textInput("scale11","Enter Scale Name"),
                  textInput("scale22","Enter Scale Name"),
                  textInput("scale33","Enter Scale Name")
                ),
                
               
                
                actionButton("modelGo","Go")),
              boxPlus(
                title = "Plot",
                withSpinner(
                  plotOutput("plot4")),
                footer = "Lorem Ipsum"
              )
              
              
              
              
              
              
      ),#end Of Explore Tab
      tabItem(tabName = "diagnostics",
              boxPlus(title = "Options",
                      checkboxInput("torgerson","Torgerson"),
                      checkboxInput("random","Random"),
                      actionButton("shepGo","Shepard Go"),
                      tags$br(),
                      tags$br(),
                      
                      actionButton("stressGo","Stress Go"),
                      tags$br(),
                      tags$br(),
                      actionButton("stabilGo","Stability Go")
                      
                      
              ),
              boxPlus(title = "shepard plot",plotOutput("shepardPlot") %>% withSpinner(color="#0dc5c1"),footer = "Lorem Ipsum"),
              
              boxPlus(title = " stress per point",plotOutput("stressPlot") %>% withSpinner(color="#0dc5c1"),footer = "Lorem Ipsum"),
              conditionalPanel(
                condition = "input.mds == true",
                boxPlus(title = "stability",plotOutput("stabilityPlot") %>% withSpinner(color="#0dc5c1"),footer = "Lorem Ipsum")
              )
              
              
              
              
      ),#end of diag
      tabItem(tabName = "final",
              boxPlus(checkboxInput("finalTorgerson","Torgerson"),
                      checkboxInput("finalRandom","Random"),
                      textInput("modelTitle","Enter the title of the model"),
                      actionButton("finalGo","Go"),
                      footer = "Lorem Ipsum",
                      conditionalPanel(
                        condition = "input.di == 3",
                        plotlyOutput("final3d") %>% withSpinner(color="#0dc5c1"))
                      ),
             
              boxPlus(
                conditionalPanel(
                  condition = "input.di == 3",
                  
                  plotOutput("final2d",width = "auto",height = 950) %>% withSpinner(color="#0dc5c1")
                  
                ),
                conditionalPanel(
                  condition = "input.di == 2",
                  plotOutput("finalModel")%>% withSpinner(color="#0dc5c1"))
              )
              
              
              
      )
      
      
      
      
    )  
  )#end of the body
  
  
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  readData <- reactive({
    
    
    if(input$select == T){
      dat <- input$dataSelect
      
      
      
    }
    else{
      dat <-read.csv(input$dataInput$datapath)
    }
    dat
  })
  
  direct <- reactive({
    dd <<- readData()
    #does your data contain additional 
    #if yes
    exx <<-c(na.omit(dd$exx))
    exy <<-c(na.omit(dd$exy))
    exz <<-c(na.omit(dd$exz))
    groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
    groups <- as.factor(groups$grp)
    dd <-dd%>%dplyr::select(-c("grp", "exx","exy", "exz"))
    sbj <-rep(1:(nrow(dd)/ncol(dd)), each = ncol(dd))
    zz <-with(dd, split(dd, list(sbj)))
    df <-Reduce(`+`, zz)/length(zz) #this takes an average of a list
    zz <<-zz
    rownames(df) <-names(df)
    df
    
    
    
  })
  
  nomin <- reactive({
    mtd <- input$nominMethod
    library(qlcMatrix)
    dd <<-readData()%>% dplyr::select(-c("rexx", "rexy","rexz", "rgrp"))
    exx <<-c(na.omit(dd$exx))
    exy <<-c(na.omit(dd$exy))
    exz <<-c(na.omit(dd$exz))
    
    groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
    groups <-as.factor(groups$grp)
    dd <-dd%>%dplyr::select(-c("grp", "exx","exy","exz", "row.name"))
    zz <-with(dd, split(dd, list(subject)))
    gg <-lapply(zz, function(x) x[, c(2:ncol(dd))])
    zz <-gg %>% map(~as.matrix(sim2diss( sim.att((.x),method = mtd))))
    df <-data.frame(Reduce(`+`, zz)/length(zz)) #this takes an average of a list
    zz <<- zz
    
    df
    
    
  })
  
  distan <- reactive({
    mtd <<- input$disMtd
    bi <<- input$disBin
    if(input$disCol == T){
      
      
      library(vegan)
      dd <<-readData()%>% dplyr::select(-c("rexx", "rexy",  "rexz", "rgrp"))
      exx <<-c(na.omit(dd$exx))
      exy <<-c(na.omit(dd$exy))
      exz <<-c(na.omit(dd$exz))
      
      groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
      groups <-as.factor(groups$grp)
      dd <-dd%>%dplyr::select(-c("grp", "exx","exy", "exz", "row.name"))
      zz <-with(dd, split(dd, list(subject)))
      gg <-lapply(zz, function(x) x[, c(2:ncol(dd))])
      zz <-gg %>% map(~as.matrix(vegdist(t(.x), method = mtd, binary =bi, diag=T, upper=T)))
      nms <- names(gg[[1]])
      
      for (i in seq_along(zz)){
        colnames(zz[[i]]) <- nms
      }
      for (i in seq_along(zz)){
        rownames(zz[[i]]) <- nms
      }
      
      df <-data.frame(Reduce(`+`, zz)/length(zz)) #this takes an average of a list
      
      zz <<- zz
      
      
      
    }
    if(input$disRow == T){
      
      
      library(vegan)
      dd <<-readData()%>% dplyr::select(-c("exx", "exy", "exz","grp"))
      rnms<-levels(factor(dd$row.name, exclude = ""))
      #does your data contain additional 
      #if yes
      exx <<-c(na.omit(dd$rexx))
      exy <<-c(na.omit(dd$rexy))
      exz <<-c(na.omit(dd$rexz))
      
      groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("rgrp"))
      groups <-as.factor(groups$rgrp)
      dd <-dd%>%dplyr::select(-c("rgrp", "rexx","rexy","rexz",  "row.name"))
      zz <-with(dd, split(dd, list(subject)))
      gg <-lapply(zz, function(x) x[, c(2:ncol(dd))])
      zz <-gg %>% map(~as.matrix(vegdist((.x), method = mtd, binary =bi, diag=T, upper=T)))
      
      for (i in seq_along(zz)){
        colnames(zz[[i]]) <- rnms
      }
      for (i in seq_along(zz)){
        rownames(zz[[i]]) <- rnms
      }
      
      df <-data.frame(Reduce(`+`, zz)/length(zz)) #this takes an average of a list
      zz <<- zz
    }
    df
  })
  
  simil <- reactive({
    
    mtd <<- input$simMtd
    if(input$simCol == T){
      
      
      dd <<-readData()%>% dplyr::select(-c("rexx", "rexy", "rexz", "rgrp"))
      #does your data contain additional 
      #if yes
      exx <<-c(na.omit(dd$exx))
      exy <<-c(na.omit(dd$exy))
      exz <<- c(na.omit(dd$exz))
      groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
      groups <-as.factor(groups$grp)
      dd <-dd%>%dplyr::select(-c("grp", "exx","exy", "exz", "row.name"))
      zz <-with(dd, split(dd, list(subject)))
      gg <-lapply(zz, function(x) x[, c(2:ncol(dd))])
      zz <-gg %>%map(~sim2diss(cor(.x, method=mtd)))
      nms<- names(gg[[1]])
      for (i in seq_along(zz)){
        colnames(zz[[i]]) <- nms
      }
      for (i in seq_along(zz)){
        rownames(zz[[i]]) <- nms
      }
      
      df <-data.frame(Reduce(`+`, zz)/length(zz)) #this takes an average of a list
      zz <<- zz
      
      
    }
    if(input$simRow == T){
      
      dd <<-readData()%>% dplyr::select(-c("exx", "exy",  "exz", "grp"))
      rnms<-levels(factor(dd$row.name, exclude = ""))
      #does your data contain additional 
      #if yes
      exx <<-c(na.omit(dd$rexx))
      exy <<-c(na.omit(dd$rexy))
      exz <<-c(na.omit(dd$rexz))
      groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("rgrp"))
      groups <-as.factor(groups$rgrp)
      dd <-dd%>%dplyr::select(-c("rgrp", "rexx","rexy", "rexz","row.name"))
      zz <-with(dd, split(dd, list(subject)))
      gg <-lapply(zz, function(x) x[, c(2:ncol(dd))])
      zz <-gg %>%map(~sim2diss(cor(t(.x), method=mtd)))
      
      for (i in seq_along(zz)){
        colnames(zz[[i]]) <- rnms
      }
      for (i in seq_along(zz)){
        rownames(zz[[i]]) <- rnms
      }
      
      df <-data.frame(Reduce(`+`, zz)/length(zz)) #this takes an average of a list
      zz <<- zz
      
      
    }
    
    df
    
  })
  
  grav <- reactive({
    dd <<-readData()%>% dplyr::select(-c("rexx", "rexy","rexz", "rgrp"))
    exx <<-c(na.omit(dd$exx))
    exy <<-c(na.omit(dd$exy))
    exz <<-c(na.omit(dd$exz))
    
    groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
    groups <-as.factor(groups$grp)
    dd <-dd%>%dplyr::select(-c("grp", "exx","exy", "exz", "row.name"))
    zz <-with(dd, split(dd, list(subject)))
    gg <-lapply(zz, function(x) x[, c(2:ncol(dd))])
    
    zz<-gg %>% map(~as.matrix(gravity(as.matrix(.x), lambda =2)$gravdiss))
    
    df <-data.frame(Reduce(`+`, zz)/length(zz)) #this takes an average of a list
    zz <<- zz
    df
    
  })
  
  scree <- eventReactive(input$screeGo,{
    
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    
    xa <- mds(df, ndim = 1, type=input$ty, ties=input$tie)$stress
    xb <- mds(df, ndim = 2, type=input$ty, ties=input$tie)$stress
    xc <-mds(df, ndim = 3, type=input$ty, ties=input$tie)$stress
    xd <-mds(df, ndim = 4, type=input$ty, ties=input$tie)$stress
    xe <-mds(df, ndim = 5, type=input$ty, ties=input$tie)$stress
    xf <-mds(df, ndim = 6, type=input$ty, ties=input$tie)$stress
    x <- c(xa,xb,xc,xd,xe,xf)
    x1 <- min(randomstress(n = ncol(df), ndim = 1, nrep
                           =20, type = input$ty))
    x2 <-  min(randomstress(n = ncol(df), ndim = 2, nrep
                            =20, type = input$ty))
    x3 <-  min(randomstress(n = 15, ndim = 3, nrep
                            =20, type = input$ty))
    x4 <- min(randomstress(n = ncol(df), ndim = 4, nrep
                           =20, type = input$ty))
    x5 <-  min(randomstress(n = ncol(df), ndim = 5, nrep
                            =20, type = input$ty))
    x6 <-  min(randomstress(n = ncol(df), ndim = 6, nrep
                            =20, type = input$ty))
    y <- c(x1,x2,x3,x4,x5,x6)
    ccc <-cbind(data.frame(x),y)
    library(latticeExtra)
    p <-xyplot(ccc$x+ccc$y~as.numeric(rownames(ccc)), type="l", xlab="Dimensions",
               ylab="Stress") + as.layer(xyplot(ccc$x+ccc$y~as.numeric(rownames(ccc)), type="p"))
    
    
    
 p
    
    
    
  })
  
  ic <- eventReactive(input$go,{
    
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    
    nr <- 20
    
    res0 <<- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
    #confi <<-res0$mdsfit[[input$confNo]]$conf
    p2 <-as.grob(~plot(res0, cex.scale = 16) )
    title1=textGrob("Figure 2. Initial parameters", gp=gpar(fontface="bold"))
    grid.arrange(p2)
    
  })
  
  
  
  
  model1 <- reactive({
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    
    if(input$mds == T){
      
      res <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000,init = "torgerson")
      
      DF1 <<-res$conf 
      
      
    }
    
    if(input$indscal == T){
      res <- indscal(zz, ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 100000)
      DF1 <<-res$gspace
      names(res$spp)<- names(df)
      Wk <- res$cweights # Weight matrices, Wk (k=1,...,K)
      
      W <-unlist(Wk %>%map(~diag(.)))
      W<<-data.frame(matrix(W, ncol=ncol(DF1), nrow=nlevels(dd$subject),byrow=TRUE))
      
      
      
    }
    if(input$idioscal == T){
      res <- idioscal(zz,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 100000)
      
      DF1 <<-res$gspace
      Wk <- res$cweights # Weight matrices, Wk (k=1,...,K)
      
      W <-unlist(Wk %>%map(~diag(.)))
      W<<-data.frame(matrix(W, ncol=ncol(DF1), nrow=nlevels(dd$subject),byrow=TRUE))
      
    }
    
    
    DF1
    
    
  })
  
  model2 <- reactive({
    
    
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    
    
    nr <- 20
    res0 <- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
    
    
    confi <-res0$mdsfit[[input$confNo]]$conf
    if(input$mds == T){
      
      
      
      
      res2 <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000, init = confi)
      
      DF2 <<- res2$conf
      
      
    }
    
    if(input$indscal == T){
      
      
      res2 <-indscal(zz, ndim =as.integer(input$di), type = input$ty, ties=input$tie, init = confi, itmax = 100000)
      names(res2$spp)<- names(df)
      DF2 <<-res2$gspace
      
      
      
      
    }
    if(input$idioscal == T){
      
      
      resind2 <-idioscal(zz, ndim =as.integer(input$di), type = input$ty, ties=input$tie,init = confi,itmax = 100000)
      
      DF2 <<-resind2$gspace
      
    }
    
    
    DF2
    
    
  })
  
  output$plot0 <- renderPlot({
    
    scree()
    
    
  })
  
  output$plot1 <- renderPlot({
    
    ic()
    
    
  })
  
  dispModel <- eventReactive(input$modelGo,{
    
    DF1 <- model1()
    DF2 <- model2()
    
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    res <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000,init = "torgerson")
    nr <- 20
    res0 <- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
    confi <-res0$mdsfit[[input$confNo]]$conf
    res2 <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000, init = confi)
    
    
    dm <- input$dimSelect
    if(input$di == 2){
    aa <- input$scale1
    ac <- input$scale2
    ad <- input$scale3
    }
    if(input$di == 3){
      aa <- input$scale11
      ac <- input$scale22
      ad <- input$scale33
      
    }
    if(input$external == T)
    {
      groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
      groups <-as.factor(groups$grp)
      groups <- groups
      cc <- data.frame(DF1)
      tit <- "Torgerson"
      
      fitbi <- biplotmds(res, cbind(exx, exy,exz))
      ab <-fitbi$coefficients
      
      
      sp <-ggscatter(cc, x = "D1", y = dm, 
                     label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                     repel = TRUE)+ ggtitle(tit) 
      
      
      library(ggConvexHull)
      sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
      
      nn <- as.integer(dm %>% str_replace("D", ""))
      
      
      rx <- round(fitbi$R2vec[1], 2)
      ry<- round(fitbi$R2vec[nn],2) 
      if(input$externalGroups == T){
      r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
      r2<-paste(names(exy), ac, "(", as.character(ry), ")")
      r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
      xxx <-ifelse(nn==2, r2, r3)
      sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
        geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
        annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
        annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
      }
      
      sp1 <-sp
      
      
      
      cc <- data.frame(DF2)
      tit <- "Random"
      fitbi <- biplotmds(res2, cbind(exx, exy,exz))
      ab <-fitbi$coefficients
      sp <-ggscatter(cc, x = "D1", y = dm, 
                     label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                     repel = TRUE)+ ggtitle(tit) 
      
      
      library(ggConvexHull)
      sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
      
      nn <- as.integer(dm %>% str_replace("D", ""))
      
      
      rx <- round(fitbi$R2vec[1], 2)
      ry<- round(fitbi$R2vec[nn],2) 
      
      if(input$externalGroups == T){
        r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
        r2<-paste(names(exy), ac, "(", as.character(ry), ")")
        r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
        xxx <-ifelse(nn==2, r2, r3)
        sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
          geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
          annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
          annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
      }
      
      sp2 <- sp
      
      sss <-grid.arrange(sp1, sp2, nrow = 1)
      
    }
    if(input$external == F){
      
      cc <- data.frame(DF1)
      cl <- input$noOfCluster
      set.seed(123466)
      library(cluster)
      if(input$gravity == F){
      clust <- kmeans(df, cl)$cluster %>%
        as.factor()
      }# Compute fuzzy clustering with k = 2
      if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
      #change D2 w D3
      groups <- clust
      
      tit <- "Torgerson"
      fitbi <- biplotmds(res, cbind(exx, exy,exz))
      ab <-fitbi$coefficients
      
      
      sp <-ggscatter(cc, x = "D1", y = dm, 
                     label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                     repel = TRUE)+ ggtitle(tit) 
      
      
      library(ggConvexHull)
      sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
      
      nn <- as.integer(dm %>% str_replace("D", ""))
      
      
      rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
      
      if(input$externalGroups == T){
        r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
        r2<-paste(names(exy), ac, "(", as.character(ry), ")")
        r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
        xxx <-ifelse(nn==2, r2, r3)
        sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
          geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
          annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
          annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
      }
      
      
      sp1 <-sp
      
      
      
      cc <- data.frame(DF2)
      tit <- "Random"
      fitbi <- biplotmds(res2, cbind(exx, exy,exz))
      ab <-fitbi$coefficients
      sp <-ggscatter(cc, x = "D1", y = dm, 
                     label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                     repel = TRUE)+ ggtitle(tit) 
      
      
      library(ggConvexHull)
      sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
      
      nn <- as.integer(dm %>% str_replace("D", ""))
      
      
      rx <- round(fitbi$R2vec[1], 2)
      ry<- round(fitbi$R2vec[nn],2) 
      
      if(input$externalGroups == T){
        r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
        r2<-paste(names(exy), ac, "(", as.character(ry), ")")
        r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
        xxx <-ifelse(nn==2, r2, r3)
        sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
          geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
          annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
          annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
      }
      
      sp2 <- sp
      
      
      sss <- grid.arrange(sp1, sp2, nrow = 1)
      
      
    }
    
    sss
    
  })
  
  output$plot4 <- renderPlot({
    dispModel()
    
  })
  
  selectFit <- reactive({
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    
    if(input$torgerson == T){
      di <<- as.integer(input$di)
      typ <<- input$ty
      tay <<- input$tie 
      
      if(input$mds == T){   fit <<- mds(df,ndim =di, type = typ, ties=tay,  itmax = 10000,init = "torgerson")}
      if(input$indscal == T){ fit <<- indscal(zz, ndim = di, type=typ, ties=tay,  itmax = 100000)}
      if(input$ idioscal == T){fit <<- idioscal(zz, ndim = di, type=typ, ties=tay,  itmax = 100000)}
      
   
    }
    if(input$random == T){
      nr <- 20 
      res0 <- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
      confi <<-res0$mdsfit[[input$confNo]]$conf
      
      di <<- as.integer(input$di)
      typ <<- input$ty
      tay <<- input$tie
      
     
      if(input$mds == T){   fit <<- mds(df,ndim =di, type = typ, ties=tay,  itmax = 10000,init = confi)}
      if(input$indscal == T){ fit <<- indscal(zz, type=typ, ties=tay, ndim = di, init = confi, itmax = 100000)}
      if(input$ idioscal == T){fit <<- idioscal(zz, type=typ, ties=tay, ndim = di, init = confi, itmax = 100000)}
      
      
    }
    fit
  })
  
  shepard <- eventReactive(input$shepGo,{
    fit <- selectFit()
    if(input$idioscal == T || input$indscal == T){
  
    txt <- paste("stress= ",round(fit$stress, digits=3 ) , sep = "")
    plot(fit, plot.type = "Shepard", main = "Shepard Diagram", sub=txt,cex=1.5 )
    }
    if(input$mds == T){
    op <- par(mfrow = c(1,2))
    par(cex=1.2)
    txt <- paste("stress= ",round(fit$stress, digits=3 ) , sep = "")
    plot(fit, plot.type = "Shepard", main = "Shepard Diagram", sub=txt,cex=1.5 )
    
    set.seed(1234)
    res.perm <- permtest(fit, nrep =200, verbose = FALSE)
    plot(res.perm)
    par(op)
    }
  })
  
  output$shepardPlot <- renderPlot({
    
    shepard()
    
    
  })
  
  stress <- eventReactive(input$stressGo,{
    
    fit <<- selectFit()
    par(cex=1.2)
    p1 <-as.grob(~plot(fit, plot.type = "stressplot"))
    
    if(input$mds == T){
      RepErr <<-as.matrix((fit$dhat-fit$confdist)^2)
    }
    if(input$idioscal == T || input$indscal == T){
      
      if(input$torgerson == T){
        di <<- as.integer(input$di)
        typ <<- input$ty
        tay <<- input$tie 
        
        if(input$mds == T){   res <- mds(df,ndim =di, type = typ, ties=tay,  itmax = 10000,init = "torgerson")}
        if(input$indscal == T){ res <- indscal(zz, ndim = di, type=typ, ties=tay,  itmax = 100000)}
        if(input$ idioscal == T){res <- idioscal(zz, ndim = di, type=typ, ties=tay,  itmax = 100000)}
        
        
      }
      if(input$random == T){
        nr <- 20 
        res0 <- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
        confi <<-res0$mdsfit[[input$confNo]]$conf
        
        di <<- as.integer(input$di)
        typ <<- input$ty
        tay <<- input$tie
        
        
        if(input$mds == T){   res <- mds(df,ndim =di, type = typ, ties=tay,  itmax = 10000,init = confi)}
        if(input$indscal == T){ res <- indscal(zz, type=typ, ties=tay, ndim = di, init = confi, itmax = 100000)}
        if(input$ idioscal == T){res <- idioscal(zz, type=typ, ties=tay, ndim = di, init = confi, itmax = 100000)}
      }
        
      dh <-fit$dhat
      dh <-dh %>% map(~as.matrix(.x))
      dha <-data.frame(Reduce(`+`, dh)/length(dh)) #this takes an average of a list
      dco <-fit$confdist
      dco <-dco %>% map(~as.matrix(.x))
      dco <-data.frame(Reduce(`+`, dco)/length(dco)) #this takes an average of a list
      rnms<<-rownames(res$gspace)
      RepErr <<-as.matrix(unlist((dha-dco)^2))
      colnames(RepErr) <- rnms
      rownames(RepErr) <- rnms
      
    }
    yr <<- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)
    p2 <-as.grob(~heatmap.2(RepErr, cellnote=round(RepErr,2), Rowv = NA, Colv = "Rowv", lhei=c(0.05, .15),
                            margins = c(8, 8), key=FALSE, notecol = "black", 
                            trace = "none", col = yr, symm = TRUE, dendrogram = "none") )
    grid.arrange(p1, p2, nrow = 1)
    
    
  })
  
  output$stressPlot <- renderPlot({
    
    
    stress()
    
  })
  
  stabil <- eventReactive(input$stabilGo,{
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    fit <- selectFit()
    op <- par(mfrow = c(1,2))
    JK <-jackknife(fit)
    plot(JK)
    set.seed(123)
    resboot <- bootmds(fit, data=df, method.dat="euclidean", nrep=500)
    plot(resboot, main="", xlab="", ylab="", col.axis = "white", ell=list(lty=1,
                                                                          col="black", cex=2, label.conf=list(label=TRUE, pos=3, col=1, cex=1.5)))
    
    par(op)
    
    
  })
  output$stabilityPlot <- renderPlot({
    
    stabil()
    
    
  })
  
  finalModelDraw <- eventReactive(input$finalGo,{
    DF1 <- model1()
    DF2 <- model2()
    title1 <- input$modelTitle
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    res <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000,init = "torgerson")
    nr <- 20
    res0 <- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
    confi <-res0$mdsfit[[input$confNo]]$conf
    res2 <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000, init = confi)
    
    
    dm <- input$dimSelect
    if(input$di == 2){
      aa <- input$scale1
      ac <- input$scale2
      ad <- input$scale3
    }
    if(input$di == 3){
      aa <- input$scale11
      ac <- input$scale22
      ad <- input$scale33
      
    }
    
    if(input$mds == T){
      if(input$finalTorgerson == T){
        
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF1)
          tit <- ""
          
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF1)
          tit<-""
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          
          sp <-ggscatter(cc, x = "D1", y = input$dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
        }
        
        }
      
      if(input$finalRandom == T){
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF2)
          tit <- ""
          
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF2)
          tit<-""
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          
          sp <-ggscatter(cc, x = "D1", y = input$dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
        }
        
      }
    }
    if(input$idioscal == T || input$indscal == T){
      if(input$finalTorgerson == T){
        
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF1)
          tit <- ""
          
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
          p <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sp <-  grid.arrange(sp, p, nrow = 2, top=title1)
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF1)
          tit<-""
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          
          sp <-ggscatter(cc, x = "D1", y = input$dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
          p <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sp <-  grid.arrange(sp, p, nrow = 2, top=title1)
          
        }
        
        }
      
      if(input$finalRandom == T){
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF2)
          tit <- ""
          
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
          p <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sp <-  grid.arrange(sp, p, nrow = 2, top=title1)
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF2)
          tit<-""
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          sp <-ggscatter(cc, x = "D1", y = input$dm, 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
          p <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sp <-  grid.arrange(sp, p, nrow = 2, top=title1)
          
        }
        
      }
      
      
      
    }
    sp
    
  })
  
  
  TDModel <- eventReactive(input$finalGo,{
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    library(plotly)
    
    t <- list(
      family = "sans serif",
      size = 14)
    DF1 <- model1()
    cc <- data.frame(DF1)
    p3 <-plot_ly(cc, x = ~D1, y = ~D2, z=~D3, text = rownames(df)) %>%
      add_markers() %>%
      add_text(textfont = t, textposition = "top right") %>%
      layout(xaxis = list(range = c(1.6, 3.2)),
             showlegend = FALSE)
    
    p3
    
    
  })
  
  TTModel <- eventReactive(input$finalGo,{
    
    
    DF1 <- model1()
    DF2 <- model2()
    title1 <- input$modelTitle
    if(input$direct == T){df <- direct()}
    if(input$nominal == T) {df <- nomin()}
    if(input$distance == T){df <-distan()}
    if(input$similarity == T){df <- simil()}
    if(input$gravity == T){df <- grav()}
    res <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000,init = "torgerson")
    nr <- 20
    res0 <- icExplore(df, ndim =as.integer(input$di), type = input$ty, ties=input$tie, nrep = nr, returnfit = TRUE, itmax = 10000)
    confi <-res0$mdsfit[[input$confNo]]$conf
    res2 <- mds(df,ndim =as.integer(input$di), type = input$ty, ties=input$tie,  itmax = 10000, init = confi)
    
    
    dm <- input$dimSelect
    if(input$di == 2){
      aa <- input$scale1
      ac <- input$scale2
      ad <- input$scale3
    }
    if(input$di == 3){
      aa <- input$scale11
      ac <- input$scale22
      ad <- input$scale33
      
    }
    
    if(input$mds == T){
      if(input$finalTorgerson == T){
        
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF1)
          tit <- ""
          
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          sss <- grid.arrange(sp1, sp2, nrow = 1, top=title1)
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF1)
          tit<-""
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          sss <- grid.arrange(sp1, sp2, nrow = 1, top=title1)
        }
        
      }
      
      
      if(input$finalRandom == T){
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF2)
          tit <- ""
          
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          sss <- grid.arrange(sp1, sp2, nrow = 1, top=title1)
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF2)
          tit<-""
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          sss <- grid.arrange(sp1, sp2, nrow = 1, top="tit")
        }
        
      }
    }
    if(input$idioscal == T || input$indscal == T){
      if(input$finalTorgerson == T){
        
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF1)
          tit <- ""
          
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp 
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer("D3" %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          
          sp2 <- sp
          p1 <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          p2 <-ggplot(W, aes(X1, X3)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sss <- grid.arrange(sp1, sp2, p1, p2, nrow = 2, top=title1)
        }
        
        if(input$external == F){
          cc <- data.frame(DF1)
          tit<-""
          fitbi <- biplotmds(res, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
          
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2);ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          
          p1 <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          p2 <-ggplot(W, aes(X1, X3)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sss <- grid.arrange(sp1, sp2, p1, p2, nrow = 2, top=title1)
          
        }
        
        }
      
      if(input$finalRandom == T){
        if(input$external == T)
        {
          groups <-dd %>% na.omit() %>%dplyr::filter_(!is.na("grp"))
          groups <-as.factor(groups$grp)
          groups <- groups
          cc <- data.frame(DF2)
          tit <- ""
          
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          
          
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = groups)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          
          
          p1 <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          p2 <-ggplot(W, aes(X1, X3)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sss <- grid.arrange(sp1, sp2, p1, p2, nrow = 2, top=title1)
          
        }
        
        if(input$external == F){
          cc <- data.frame(DF2)
          tit<-""
          fitbi <- biplotmds(res2, cbind(exx, exy,exz))
          ab <-fitbi$coefficients
          cl <- input$noOfCluster
          
          set.seed(123466)
          library(cluster)
         
          if(input$gravity == F){
            clust <- kmeans(df, cl)$cluster %>%
              as.factor()
          }# Compute fuzzy clustering with k = 2 # Compute fuzzy clustering with k = 2
          #change D2 w D3
          
          if(input$gravity == T){clust <- kmeans(cc, cl)$cluster %>% as.factor() }
          sp <-ggscatter(cc, x = "D1", y = "D2", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp1 <- sp
          
          sp <-ggscatter(cc, x = "D1", y = "D3", 
                         label = names(df), size =max(res$spp)-res$spp,alpha=.3, legend="none",
                         repel = TRUE)+ ggtitle(tit) 
          
          
          library(ggConvexHull)
          sp <-sp+geom_convexhull(alpha = 0.2,aes(fill = clust)) 
          
          nn <- as.integer(dm %>% str_replace("D", ""))
          
          
          rx <- round(fitbi$R2vec[1], 2)
          ry<- round(fitbi$R2vec[nn],2) 
          
          if(input$externalGroups == T){
            r1 <-paste(names(exx),aa ,"(", as.character(rx), ")"  )
            r2<-paste(names(exy), ac, "(", as.character(ry), ")")
            r3 <-paste(names(exz), ad, "(", as.character(ry), ")")
            xxx <-ifelse(nn==2, r2, r3)
            sp <-sp + geom_abline(slope = ab[nn,1]/ab[1,1], color="blue",linetype="dashed", size=1)+
              geom_abline(slope =ab[nn,nn]/ab[1,nn], color="red",linetype="dashed", size=1) +
              annotate("text", x=.2, y=(ab[nn,1]/ab[1,1])*.2, label = r1, color="blue",  fontface =4) + 
              annotate("text", x=.2 , y =(ab[nn,nn])/(ab[1,nn])*.2,label = xxx,color="red",  fontface =4) +theme(legend.position = "none")
          }
          sp2 <- sp
          
          
          p1 <-ggplot(W, aes(X1, X2)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          p2 <-ggplot(W, aes(X1, X3)) +
            geom_label_repel(label=levels(dd$subject), size=5)
          sss <- grid.arrange(sp1, sp2, p1, p2, nrow = 2, top=title1)
          
        }
        
      }
      
      }
    sss
    
    })
  
  output$finalModel <- renderPlot({
    
    finalModelDraw()
  
    })
  
  output$final3d <- renderPlotly({
    
    TDModel()
    
  })
  
  output$final2d <- renderPlot({
    
    TTModel()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
