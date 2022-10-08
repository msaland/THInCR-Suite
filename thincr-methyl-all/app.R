library(DT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)

gene.orient <- readRDS("gene-orient.rds")
pre.autofill <- sort(readRDS("list_options.rds")$V1)
autofill <- intersect(unlist(gene.orient[1]), pre.autofill)
autofill2 <- vector()
my.env <- new.env()

ui <- fluidPage(title = "THInCR - Probe Methylation vs. Genomic Loci", theme = shinytheme("cosmo"), useShinyjs(), 
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("THInCR - The HPV Induced Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Other Links:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://thincr.ca/thincr-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="THInCR2:EE", tabPanel(a(href="https://thincr.ca/thincr2-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr2-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr2-methyl-all/", "Cellular loci vs. Methylation Status")))),
  tags$head( tags$style(HTML(" .shiny-output-error-validation { color: red; } ")) ),
  titlePanel("THInCR - Correlation of Cellular Loci and Probe Methylation by HPV Status"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to compare genomic methylation across the loci of candidate cellular genes. Using this tool, researchers can easily investigate the available TCGA DNA methylation data at individual CpGs in relation to their precise genomic location, while identifying differences in DNA methylation between HPV+, HPV- and normal control tissue from the TCGA cervical (CESC) and head & neck cancer (HNSC) cohorts. Select the candidate cellular gene by typing the name in the input box on the top right. The slider allows modification of the genomic region under investigation. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Images of graphs can be downloaded as a PNG file and a statistical analysis comparing relative methylation for all probes in the defined region is generated to allow 'at a glance' identification of probes demonstrating statistically significant methylation. Individual box plots comparing relative methylation for a given probe can be downloaded as PNG files and the information necessary to reproduce the box plots can be downloaded as a CSV file. Level 3 Infinium HumanMethylation450 BeadChip array data for the TCGA HNSC and CESC cohorts was downloaded from the Broad Genome Data Analysis Centers Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". HPV status was annotated as described in Gameiro et al, Viruses 2017", a(href="https://www.mdpi.com/1999-4915/9/9/252", "DOI: 10.3390/v9090252")),
  p(style="text-align: justify;", strong("Download the Differentially Methylated Probe (DMP) Analysis: "), a(href="DMP-methylation.xlsx", "Click Here", download=NA, target="_blank")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v5", "Click Here")),
  p(style="text-align: justify;", strong("Citing THInCR: "),"Salnikov et al., 2022, The HPV Induced Cancer Resource (THInCR): a Suite of Tools for Investigating HPV-Dependent Human Carcinogenesis, mSphere, In Press."),
  sidebarLayout(sidebarPanel( 
      fluidRow(column(4, selectizeInput(inputId = 'searchGene', label = HTML('Select or Type Gene Name'), choices = autofill, selected = character(0), options = list(placeholder = 'Please select a gene from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
              column(8, sliderInput("rangeBP", label = "Selection Range (bp)", min = 0, max = 0, value = c(0,0), step = 1))),
      fluidRow(column(6, selectizeInput(inputId = 'searchProbe', label = HTML('Select Probe associated with Gene'), choices = autofill2, selected = character(0), options = list(placeholder = 'Please select a probe from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE))),
      fluidRow(column(12,dataTableOutput(outputId ="signifProbe"))),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadData', 'Download Table'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      width=6),
    mainPanel(width = 6,
      jqui_resizable(plotOutput(outputId = "linePlotCESC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'CESCdownloadLineP', 'Download CESC Lineplot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'CESCdownloadLineV', 'Download CESC Lineplot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "linePlotHNSCC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'HNSCCdownloadLineP', 'Download HNSC Lineplot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'HNSCCdownloadLineV', 'Download HNSC Lineplot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "boxCESC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'CESCdownloadBoxP', 'Download CESC Boxplot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'CESCdownloadBoxV', 'Download CESC Boxplot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "boxHNSCC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'HNSCCdownloadBoxP', 'Download HNSC Boxplot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'HNSCCdownloadBoxV', 'Download HNSC Boxplot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))))),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  output$CESCdownloadLineP <- downloadHandler(
    filename = function() {paste("CESC-lineplot-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.lineplotCESC, width = 5*input$linePlotCESC_size$width, height = 5*input$linePlotCESC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$HNSCCdownloadLineP <- downloadHandler(
    filename = function() {paste("HNSC-lineplot-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.lineplotHNSCC, width = 5*input$linePlotHNSCC_size$width, height = 5*input$linePlotHNSCC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$CESCdownloadBoxP <- downloadHandler(
    filename = function() {paste("CESC-boxplot-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.boxplotCESC, width = 5*input$boxCESC_size$width, height = 5*input$boxCESC_size$height, dpi = 300, units = "px", device = "png")
    })
  
  output$HNSCCdownloadBoxP <- downloadHandler(
    filename = function() {paste("HNSC-boxplot-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.boxplotHNSCC, width = 5*input$boxHNSCC_size$width, height = 5*input$boxHNSCC_size$height, dpi = 300, units = "px", device = "png")
    })
  
  output$CESCdownloadLineV <- downloadHandler(
    filename = function() {paste("CESC-lineplot-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.lineplotCESC, width = 5*input$linePlotCESC_size$width, height = 5*input$linePlotCESC_size$height, dpi = 300, units = "px", device = "pdf")
    })
  
  output$HNSCCdownloadLineV <- downloadHandler(
    filename = function() {paste("HNSC-lineplot-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.lineplotHNSCC, width = 5*input$linePlotHNSCC_size$width, height = 5*input$linePlotHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
    })
  
  output$CESCdownloadBoxV <- downloadHandler(
    filename = function() {paste("CESC-boxplot-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.boxplotCESC, width = 5*input$boxCESC_size$width, height = 5*input$boxCESC_size$height, dpi = 300, units = "px", device = "pdf")
    })
  
  output$HNSCCdownloadBoxV <- downloadHandler(
    filename = function() {paste("HNSC-boxplot-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.boxplotHNSCC, width = 5*input$boxHNSCC_size$width, height = 5*input$boxHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste("Methylation-Data-", my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.tablePR, filename, row.names=FALSE)
  })
  
  observeEvent(input$rangeBP,{
    req(input$searchGene)
    part.data.cesc=data.frame(my.env$g.data.active.cesc[my.env$g.data.active.cesc[4] > input$rangeBP[1] & my.env$g.data.active.cesc[4] < input$rangeBP[2],])
    assign("g.part.data.cesc", part.data.cesc, envir = my.env)
    part.data.hnscc=data.frame(my.env$g.data.active.hnscc[my.env$g.data.active.hnscc[4] > input$rangeBP[1] & my.env$g.data.active.hnscc[4] < input$rangeBP[2],])
    assign("g.part.data.hnscc", part.data.hnscc, envir = my.env)
    autofill2=as.vector(my.env$g.part.data.cesc[1])
    updateSelectizeInput(session, "searchProbe", choices = autofill2)
    rm(part.data.cesc)
    rm(part.data.hnscc)
    
    if(nrow(my.env$g.part.data.cesc)<2 & nrow(my.env$g.part.data.hnscc)<2){
      hide("CESCdownloadLineP")
      hide("CESCdownloadLineV")
      hide("HNSCCdownloadLineP")
      hide("HNSCCdownloadLineV")
      hide("downloadData")
    } else{
      show("CESCdownloadLineP")
      show("CESCdownloadLineV")
      show("HNSCCdownloadLineP")
      show("HNSCCdownloadLineV")
      show("downloadData")
    }
  })
  
  observeEvent(input$searchGene,{
    hide("CESCdownloadLineP")
    hide("CESCdownloadLineV")
    hide("HNSCCdownloadLineP")
    hide("HNSCCdownloadLineV")
    hide("CESCdownloadBoxV")
    hide("CESCdownloadBoxP")
    hide("HNSCCdownloadBoxV")
    hide("HNSCCdownloadBoxP")
    hide("downloadData")
    hide("renderTable")
    req(input$searchGene)
    hide("CESCdownloadLineP")
    hide("CESCdownloadLineV")
    hide("HNSCCdownloadLineP")
    hide("HNSCCdownloadLineV")
    show("downloadData")
    show("renderTable")
    assign("g.gene", input$searchGene, envir = my.env)
    gene <- input$searchGene
    data.active.cesc <- readRDS(paste("../thincr-data/gene-segments/cesc-methyl-",gene,".rds",sep=""))
    data.active.hnscc <- readRDS(paste("../thincr-data/gene-segments/hnsc-methyl-",gene,".rds",sep=""))
    assign("g.data.active.cesc", data.active.cesc, envir = my.env)
    assign("g.data.active.hnscc", data.active.hnscc, envir = my.env)
    maxVal=as.numeric(gene.orient[gene.orient[1]==gene][5])
    minVal=as.numeric(gene.orient[gene.orient[1]==gene][4])
    updateSliderInput(session, "rangeBP", label = "Selection Range (bp)", max=maxVal+100000, min=minVal-100000,value=c(minVal, maxVal))
    part.data.cesc=data.frame(my.env$g.data.active.cesc[my.env$g.data.active.cesc[4] > minVal & my.env$g.data.active.cesc[4] < maxVal,])
    assign("g.part.data.cesc", part.data.cesc, envir = my.env)
    part.data.hnscc=data.frame(my.env$g.data.active.hnscc[my.env$g.data.active.hnscc[4] > minVal & my.env$g.data.active.hnscc[4] < maxVal,])
    assign("g.part.data.hnscc", part.data.hnscc, envir = my.env)
    rm(part.data.cesc)
    rm(part.data.hnscc)
  })
  
  observeEvent(input$searchProbe,{
    req(input$searchGene, input$rangeBP, input$searchProbe)
    assign("g.probe", input$searchProbe, envir = my.env)
  })
  
  output$boxHNSCC <- renderPlot({
    hide("CESCdownloadBoxV")
    hide("CESCdownloadBoxP")
    hide("HNSCCdownloadBoxV")
    hide("HNSCCdownloadBoxP")
    req(input$searchProbe)
    show("CESCdownloadBoxV")
    show("CESCdownloadBoxP")
    show("HNSCCdownloadBoxV")
    show("HNSCCdownloadBoxP")
    probe=input$searchProbe
    plot.data <- dataBox(my.env$g.part.data.hnscc[my.env$g.part.data.hnscc[1]==probe,])    
    plot.data$Status <- factor(plot.data$Status , levels=c("Positive", "Negative", "Normal"))
    my.plot <- ggplot(plot.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("HPV Positive","HPV Negative","Normal Control"), name="HPV Status") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      ylab(paste("Methylation (Beta Values)")) +
      ggtitle(paste("HNSC Data - ",input$searchProbe,sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    assign("g.boxplotHNSCC", my.plot, envir = my.env)
    my.plot
   })
  
  output$boxCESC <- renderPlot({
    req(input$searchProbe)
    probe=input$searchProbe
    plot.data <- dataBox(my.env$g.part.data.cesc[my.env$g.part.data.cesc[1]==probe,])
    plot.data$Status <- factor(plot.data$Status , levels=c("Positive", "Negative", "Normal"))
    my.plot <- ggplot(plot.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("HPV Positive","HPV Negative","Normal Control"), name="HPV Status") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      ylab(paste("Methylation (Beta Values)")) +
      ggtitle(paste("CESC Data - ",probe,sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    assign("g.boxplotCESC", my.plot, envir = my.env)
    my.plot
  })
  
  dataBox <- function(g.probe.data){
    hpv.pos <- data.frame(g.probe.data[6],g.probe.data[7], g.probe.data[8], g.probe.data[9], g.probe.data[10], "Positive")
    colnames(hpv.pos) <- c("Median", "Min", "Max", "One", "Three", "Status")
    hpv.neg <- data.frame(g.probe.data[12],g.probe.data[13], g.probe.data[14], g.probe.data[15], g.probe.data[16], "Negative")
    colnames(hpv.neg) <- c("Median", "Min", "Max", "One", "Three", "Status")
    hpv.nrm <- data.frame(g.probe.data[18],g.probe.data[19], g.probe.data[20], g.probe.data[21], g.probe.data[22], "Normal")
    colnames(hpv.nrm) <- c("Median", "Min", "Max", "One", "Three", "Status")
    plot.data <- rbind(hpv.pos, hpv.neg, hpv.nrm)
    return(plot.data)
  }
  
  dataLinePlot <- function(g.part.data){
    if(nrow(g.part.data) > 1){
      hpv.pos <- g.part.data[c(4,5)]
      hpv.pos["Status"]="Positive"
      colnames(hpv.pos) = c("GenCoord", "MeanMethyl", "Status")
      hpv.neg <- g.part.data[c(4,11)]
      hpv.neg["Status"]="Negative"
      colnames(hpv.neg) = c("GenCoord", "MeanMethyl", "Status")
      hpv.nrm <- g.part.data[c(4,17)]
      hpv.nrm["Status"]="Normal"
      colnames(hpv.nrm) = c("GenCoord", "MeanMethyl", "Status")
      plot.data = rbind(hpv.pos, hpv.neg, hpv.nrm)
      return(plot.data)
    }
    return(NULL)
  }
  
  output$linePlotCESC <- renderPlot({
    req(input$searchGene, input$rangeBP[1]>0)
    validate(need(nrow(my.env$g.part.data.cesc)>1, "No probes in selected region."))
    plot.data <- dataLinePlot(my.env$g.part.data.cesc)
    numVals = my.env$g.part.data.cesc[4]
    maxVal=max(numVals)
    minVal=min(numVals)
    gene=input$searchGene
    maxVal.gene=as.numeric(gene.orient[gene.orient[1]==gene][5])
    minVal.gene=as.numeric(gene.orient[gene.orient[1]==gene][4])
    numVals.segment = input$rangeBP[c(1,2)]
    maxVal.segment=max(numVals.segment)
    minVal.segment=min(numVals.segment)
    if (ncol(my.env$g.part.data.cesc>2)){
      my.plot <- ggplot(data=plot.data, aes(x=GenCoord, y=MeanMethyl, color=Status)) +
        geom_line(size=1.25) +
        scale_x_continuous(sec.axis=sec_axis(~.,breaks=unname(unlist(my.env$g.part.data.cesc[4])), labels=unname(unlist(my.env$g.part.data.cesc[1]))), limits = c(input$rangeBP[1],input$rangeBP[2])) +
        ylim(0,1) + xlab("Genomic Coordinates") + ylab("Mean Methylation (Beta Values)") + ggtitle("CESC Data - Mean Methylation by Probe") +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'), axis.text.x.top = element_text(size=10, color='black', angle = 45, hjust = 0)) +
        geom_point() 
      name <- my.env$g.data.active.cesc[grepl(my.env$g.gene, unlist(my.env$g.data.active.cesc[2])),1][1]
      if (maxVal.segment >= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.gene, y = 0, xend = maxVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (maxVal.segment < maxVal.gene & minVal.segment > minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.segment, y = 0, xend = maxVal.segment, yend = 0), col='black')
      } else if (maxVal.segment <= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.gene, y = 0, xend = maxVal.segment, yend = 0), col='black')
      } else if (minVal.segment >= minVal.gene & maxVal.segment >= maxVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.segment, y = 0, xend = maxVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if ((minVal > maxVal.gene | maxVal < minVal.gene) & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        print("")
      } else if (maxVal.segment >= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.gene, y = 0, xend = minVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (maxVal.segment < maxVal.gene & minVal.segment > minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.segment, y = 0, xend = minVal.segment, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      }  else if (maxVal.segment <= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.segment, y = 0, xend = minVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (minVal.segment >= minVal.gene & maxVal.segment >= maxVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.gene, y = 0, xend = minVal.segment, yend = 0), col='black')
      } else if ((minVal > maxVal.gene | maxVal < minVal.gene) & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        print("")
      }
      assign("g.lineplotCESC", my.plot, envir = my.env, )
      my.plot
    }
  })
  
  output$linePlotHNSCC <- renderPlot({
    req(input$searchGene, input$rangeBP[1]>0)
    validate(need(nrow(my.env$g.part.data.hnscc)>1, "No probes in selected region."))
    plot.data <- dataLinePlot(my.env$g.part.data.hnscc)
    numVals = my.env$g.part.data.hnscc[4]
    maxVal=max(numVals)
    minVal=min(numVals)
    gene=input$searchGene
    maxVal.gene=as.numeric(gene.orient[gene.orient[1]==gene][5])
    minVal.gene=as.numeric(gene.orient[gene.orient[1]==gene][4])
    numVals.segment = input$rangeBP[c(1,2)]
    maxVal.segment=max(numVals.segment)
    minVal.segment=min(numVals.segment)
    if (ncol(my.env$g.part.data.hnscc>2)){
      my.plot <- ggplot(data=plot.data, aes(x=GenCoord, y=MeanMethyl, color=Status)) +
        geom_line(size=1.25) +
        scale_x_continuous(sec.axis=sec_axis(~.,breaks=unname(unlist(my.env$g.part.data.hnscc[4])), labels=unname(unlist(my.env$g.part.data.hnscc[1]))), limits = c(input$rangeBP[1],input$rangeBP[2])) +
        ylim(0,1) + xlab("Genomic Coordinates") + ylab("Mean Methylation (Beta Values)") + ggtitle("HNSCC Data - Mean Methylation by Probe") +
        theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'), axis.text.x.top = element_text(size=10, color='black', angle = 45, hjust = 0)) +
        geom_point() 
      name <- my.env$g.data.active.hnscc[grepl(my.env$g.gene, unlist(my.env$g.data.active.hnscc[2])),1][1]
      if (maxVal.segment >= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.gene, y = 0, xend = maxVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (maxVal.segment < maxVal.gene & minVal.segment > minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.segment, y = 0, xend = maxVal.segment, yend = 0), col='black')
      } else if (maxVal.segment <= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.gene, y = 0, xend = maxVal.segment, yend = 0), col='black')
      } else if (minVal.segment >= minVal.gene & maxVal.segment >= maxVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        my.plot <- my.plot + geom_segment(aes(x = minVal.segment, y = 0, xend = maxVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if ((minVal > maxVal.gene | maxVal < minVal.gene) & gene.orient[gene.orient[1]==input$searchGene][6] == "plus"){
        print("")
      } else if (maxVal.segment >= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.gene, y = 0, xend = minVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (maxVal.segment < maxVal.gene & minVal.segment > minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.segment, y = 0, xend = minVal.segment, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      }  else if (maxVal.segment <= maxVal.gene & minVal.segment <= minVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.segment, y = 0, xend = minVal.gene, yend = 0), arrow = arrow(length = unit(0.5, "cm")), col='black')
      } else if (minVal.segment >= minVal.gene & maxVal.segment >= maxVal.gene & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        my.plot <- my.plot + geom_segment(aes(x = maxVal.gene, y = 0, xend = minVal.segment, yend = 0), col='black')
      } else if ((minVal > maxVal.gene | maxVal < minVal.gene) & gene.orient[gene.orient[1]==input$searchGene][6] == "minus"){
        print("")
      }
      assign("g.lineplotHNSCC", my.plot, envir = my.env, )
      my.plot
    }
  })
  
  output$signifProbe <- renderDT({
    req(input$searchGene, input$rangeBP[1]>0)
    gene <- input$searchGene
    tablePRfull <- readRDS(paste("../thincr-data/methyl-stats/methyl-", gene, ".rds", sep=""))
    tablePR <- tablePRfull[tablePRfull[4] >= input$rangeBP[1] & tablePRfull[4] <= input$rangeBP[2],]
    validate(need(nrow(tablePR)>1, "No probes in selected region."))
    cesc.pval.PosNeg=c()
    cesc.pval.PosNrm=c()
    cesc.pval.NegNrm=c()
    hnsc.pval.PosNeg=c()
    hnsc.pval.PosNrm=c()
    hnsc.pval.NegNrm=c()
    for (i in 1:length(unlist(tablePR[1]))){
      q <- as.numeric(tablePR[i,6])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      cesc.pval.PosNeg <- c(cesc.pval.PosNeg, p.text)
      q <- as.numeric(tablePR[i,8])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      cesc.pval.PosNrm <- c(cesc.pval.PosNrm, p.text)
      q <- as.numeric(tablePR[i,10])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      cesc.pval.NegNrm <- c(cesc.pval.NegNrm, p.text)
      q <- as.numeric(tablePR[i,12])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      hnsc.pval.PosNeg <- c(hnsc.pval.PosNeg, p.text)
      q <- as.numeric(tablePR[i,14])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      hnsc.pval.PosNrm <- c(hnsc.pval.PosNrm, p.text)
      q <- as.numeric(tablePR[i,16])
      if (is.na(q) | q > 0.1){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      hnsc.pval.NegNrm <- c(hnsc.pval.NegNrm, p.text)
    }
    tableDraw=data.frame(tablePR[1],tablePR[2],tablePR[3],tablePR[4],tablePR[5],tablePR[6],cesc.pval.PosNeg,tablePR[7],tablePR[8],cesc.pval.PosNrm,tablePR[9],tablePR[10],cesc.pval.NegNrm,tablePR[11],tablePR[12],hnsc.pval.PosNrm,tablePR[13],tablePR[14],hnsc.pval.PosNrm,tablePR[15],tablePR[16],hnsc.pval.NegNrm)
    colnames(tableDraw)=c("Probe", "Gene", "Chromosome", "Coordinate", "CESC HPV(+) vs. HPV(-) p-Val.", "CESC HPV(+) vs. HPV(-) q-Val. (FDR=0.1)", "CESC HPV(+) vs. HPV(-) Signif.", "CESC HPV(+) vs. Normal p-Val.", "CESC HPV(+) vs. Normal q-Val. (FDR=0.1)", "CESC HPV(+) vs. Normal Signif.", "CESC HPV(-) vs. Normal p-Val.", "CESC HPV(-) vs. Normal q-Val. (FDR=0.1)", "CESC HPV(-) vs. Normal Signif.", "HNSC HPV(+) vs. HPV(-) p-Val.", "HNSC HPV(+) vs. HPV(-) q-Val. (FDR=0.1)", "HNSC HPV(+) vs. HPV(-) Signif.", "HNSC HPV(+) vs. Normal p-Val.", "HNSC HPV(+) vs. Normal q-Val. (FDR=0.1)", "HNSC HPV(+) vs. Normal Signif.", "HNSC HPV(-) vs. Normal p-Val.", "HNSC HPV(-) vs. Normal q-Val. (FDR=0.1)", "HNSC HPV(-) vs. Normal Signif.")
    assign("g.tablePR", tableDraw, envir = my.env)
    datatable(my.env$g.tablePR, options=list(scrollX=TRUE, bFilter=0, pageLength = 15, bLengthChange=0, bInfo=0, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = 0:21))), rownames= FALSE) %>%  formatStyle(columns = c('CESC HPV(+) vs. HPV(-) Signif.', 'HNSC HPV(+) vs. HPV(-) Signif.', 'CESC HPV(+) vs. Normal Signif.', 'HNSC HPV(+) vs. Normal Signif.', 'CESC HPV(-) vs. Normal Signif.', 'HNSC HPV(-) vs. Normal Signif.'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212'))) %>% formatSignif(columns=c(5,6,8,9,11,12,14,15,17,18,20,21), digits=3)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)