library(DT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)
library(survival)
library(survminer)
library(tidyverse)

my.env <- new.env()
autofill <- sort(readRDS("../thincr-data/autofill-mRNA.rds"))
optionsCompare <- c(2, 3, 4)
cesc.survival <- readRDS("../thincr-data/cesc-survival.rds")
colnames(cesc.survival) <- gsub("\\.", "-", colnames(cesc.survival))
hnscc.survival <- readRDS("../thincr-data/hnsc-survival.rds")
colnames(hnscc.survival) <- gsub("\\.", "-", colnames(hnscc.survival))

ui <- fluidPage(title="THInCR - mRNA vs. OS", theme = shinytheme("cosmo"), useShinyjs(),
      fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("THInCR - The HPV Induced Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
      navbarPage(title="Other Links:", tabPanel(title="☲"), 
                 navbarMenu(title="THInCR", tabPanel(a(href="https://thincr.ca/thincr-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
                 navbarMenu(title="THInCR2:EE", tabPanel(a(href="https://thincr.ca/thincr2-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr2-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr2-methyl-all/", "Cellular loci vs. Methylation Status")))),
      titlePanel("THInCR - Correlation of Cellular mRNA Expression and Overal Survival by HPV Status"),
      p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between expression of a candidate mRNA and patient overall survival (OS) between HPV+ and HPV- cervical (CESC) and head & neck cancers (HNSC). Select the candidate mRNA by typing the name in the input box on the top right. Patients can be stratified into 2, 3 or 4 comparison groups based on expression levels of the target mRNA using the adjacent input box. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Kaplan-Meier plots can be downloaded as PNG files. Survival data was extracted from Liu et al, Cell 2018",a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6066282/", "DOI: 10.1016/j.cell.2018.02.052")),
      p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v4", "Click Here")),
      p(style="text-align: justify;", strong("Citing THInCR: "),"Salnikov et al., 2022, The HPV Induced Cancer Resource (THInCR): a Suite of Tools for Investigating HPV-Dependent Human Carcinogenesis, mSphere, In Press."),
      sidebarLayout( sidebarPanel(
      fluidRow(column(8, align="left", offset = 0, selectizeInput(
        inputId = 'search', label = HTML('Type the Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
        choices = autofill, selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
        column(4, align="left", offset = 0, selectizeInput(
           inputId = 'compare', label = HTML('<br/>', 'Select # of Comparison Groups'),
           choices = optionsCompare, options = list(openOnFocus = FALSE), multiple = FALSE))), width=6, dataTableOutput(outputId = 'table'),
      dataTableOutput(outputId = 'tablePQ'), tags$div(HTML('<br/>')),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadPQ', 'Download Data'),
                      tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>'))
      ), 
    mainPanel(
      jqui_resizable(plotOutput(outputId = "survPosCESC", height=500)),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownload1P', 'Download HPV(+) CESC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownload1V', 'Download HPV(+) CESC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "survNegCESC", height=500)), 
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownload2P', 'Download HPV(-) CESC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownload2V', 'Download HPV(-) CESC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "survPosHNSCC", height=500)), 
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownload1P', 'Download HPV(+) HNSCC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownload1V', 'Download HPV(+) HNSCC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "survNegHNSCC", height=500)),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownload2P', 'Download HPV(-) HNSCC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownload2V', 'Download HPV(-) HNSCC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
    HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  output$downloadPQ <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-pq.csv", sep="")},
    content = function(filename){write.csv(my.env$g.tablePQ, filename, row.names=FALSE)
  })
  
  output$cescdownload1P <- downloadHandler(
    filename = function() {paste("CESC-HPV-Pos-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot1[[1]], width = 3*input$survPosCESC_size$width, height = 3*input$survPosCESC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$cescdownload2P <- downloadHandler(
    filename = function() {paste("CESC-HPV-Neg-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot2[[1]], width = 3*input$survNegCESC_size$width, height = 3*input$survNegCESC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$hnsccdownload1P <- downloadHandler(
    filename = function() {paste("HNSC-HPV-Pos-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plot1[[1]], width = 3*input$survPosHNSCC_size$width, height = 3*input$survPosHNSCC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$hnsccdownload2P <- downloadHandler(
    filename = function() {paste("HNSC-HPV-Neg-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plot2[[1]], width = 3*input$survNegHNSCC_size$width, height = 3*input$survNegHNSCC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$cescdownload1V <- downloadHandler(
    filename = function() {paste("CESC-HPV-Pos-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot1[[1]], width = 3*input$survPosCESC_size$width, height = 3*input$survPosCESC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$cescdownload2V <- downloadHandler(
    filename = function() {paste("CESC-HPV-Neg-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot2[[1]], width = 3*input$survNegCESC_size$width, height = 3*input$survNegCESC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$hnsccdownload1V <- downloadHandler(
    filename = function() {paste("HNSC-HPV-Pos-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plot1[[1]], width = 3*input$survPosHNSCC_size$width, height = 3*input$survPosHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$hnsccdownload2V <- downloadHandler(
    filename = function() {paste("HNSC-HPV-Neg-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plot2[[1]], width = 3*input$survNegHNSCC_size$width, height = 3*input$survNegHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$tablePQ <- renderDT({
    hide("downloadPQ")
    req(input$search, input$compare)
    show("downloadPQ")
    gene <- input$search
    compare <- as.numeric(input$compare)
    assign("g.gene", gene, envir = my.env)
    if (compare == 2){
      temp <- readRDS("pq-2group-mRNA.rds")
    } else if (compare == 3){
      temp <- readRDS("pq-3group-mRNA.rds")
    } else if (compare == 4){
      temp <- readRDS("pq-4group-mRNA.rds")
    }
    data=temp[gene,]
    len=length(data)
    cesc.plot.data <- makePlotData(gene, "CESC")
    hnsc.plot.data <- makePlotData(gene, "HNSCC")
    percent1 <- 100*colSums(cesc.plot.data[1] == 0)/nrow(cesc.plot.data)
    percent2 <- 100*colSums(hnsc.plot.data[1] == 0)/nrow(hnsc.plot.data)
    truthy1=TRUE
    truthy2=TRUE
    if (percent1 > 50){truthy1=FALSE}
    if (percent2 > 50){truthy2=FALSE}
    pval=c()
    for (i in seq(2, len, 2)){
      q <- as.numeric(data[i])
      if (is.na(q) | q > 0.1 | i <= len/2 & truthy1==FALSE | i > len/2 & truthy2==FALSE){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      pval <- c(pval, p.text)
    }
    if (compare == 2){
      names <- c("CESC HPV(+) High vs. Low", "CESC HPV(-) High vs. Low", "HNSC HPV(+) High vs. Low","HNSC HPV(-) High vs. Low")
      pvals <- c(data[1], data[3], data[5], data[7])
      qvals <- c(data[2], data[4], data[6], data[8])
    } else if (compare == 3){
      names <- c("CESC HPV(+) Mid vs. Low", "CESC HPV(-) Mid vs. Low", "CESC HPV(+) High vs. Low", "CESC HPV(-) High vs. Low", "CESC HPV(+) High vs. Mid", "CESC HPV(-) High vs. Mid", "HNSC HPV(+) - Mid vs. Low", "HNSC HPV(-) Mid vs. Low", "HNSC HPV(+) High vs. Low", "HNSC HPV(-) High vs. Low", "HNSC HPV(+) High vs. Mid", "HNSC HPV(-) High vs. Mid")
      pvals <- c(data[1], data[3], data[5], data[7], data[9], data[11], data[13], data[15], data[17], data[19], data[21], data[23])
      qvals <- c(data[2], data[4], data[6], data[8], data[10], data[12], data[14], data[16], data[18], data[20], data[22], data[24])
    } else if (compare == 4){
      names <- c("CESC HPV(+) Mid-Low vs. Low", "CESC HPV(-) Mid-Low vs. Low", "CESC HPV(+) High-Mid vs. Low", "CESC HPV(-) High-Mid vs. Low", "CESC HPV(+) High vs. Low", "CESC HPV(-) High vs. Low", "CESC HPV(+) High-Mid vs. Mid-Low", "CESC HPV(-) High-Mid vs. Mid-Low", "CESC HPV(+) High vs. Mid-Low", "CESC HPV(-) High vs. Mid-Low", "CESC HPV(+) High vs. Mid-High", "CESC HPV(-) High vs. Mid-High", "HNSC HPV(+) Mid-Low vs. Low", "HNSC HPV(-) Mid-Low vs. Low", "HNSC HPV(+) High-Mid vs. Low", "HNSC HPV(-) High-Mid vs. Low", "HNSC HPV(+) High vs. Low", "HNSC HPV(-) High vs. Low", "HNSC HPV(+) High-Mid vs. Mid-Low", "HNSC HPV(-) High-Mid vs. Mid-Low", "HNSC HPV(+) High vs. Mid-Low", "HNSC HPV(-) High vs. Mid-Low", "HNSC HPV(+) High vs. Mid-High", "HNSC HPV(-) High vs. Mid-High")
      pvals <- c(data[1], data[3], data[5], data[7], data[9], data[11], data[13], data[15], data[17], data[19], data[21], data[23], data[25], data[27], data[29], data[31], data[33], data[35], data[37], data[39], data[41], data[43], data[45], data[47])
      qvals <- c(data[2], data[4], data[6], data[8], data[10], data[12], data[14], data[16], data[18], data[20], data[22], data[24], data[26], data[28], data[30], data[32], data[34], data[36], data[38], data[40], data[42], data[44], data[46], data[48])
    }
    tablePQ <- data.frame(cbind(names, pvals, qvals, pval))
    tablePQ=data.frame(lapply(tablePQ, as.character), stringsAsFactors=FALSE)
    colnames(tablePQ) <- c("Comparison Group", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.tablePQ", tablePQ, envir = my.env)
    datatable(tablePQ, options=list(scrollX=TRUE, bFilter=0, paginate=FALSE, bLengthChange=0, bInfo=0, autoWidth=FALSE, columnDefs=list(list(className = 'dt-center', targets = 0:3))), rownames=FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212'))) %>% formatSignif(columns=c(2, 3), digits=3)
  })
  
  drawPlot <- function(plot.data, gene, titles, mode, compare.G) {
    if (mode == "Positive"){
      pos.data <- plot.data[which(plot.data$Status == "HPV"), ]
      plot.data.mut <- pos.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    } else if (mode == "Negative"){
      neg.data <- plot.data[which(plot.data$Status == "Negative"), ]
      plot.data.mut <- neg.data %>% mutate(quantile = ntile(Value, as.numeric(compare.G)))
    }
    surv.plot.data <- survival::survfit(formula = survival::Surv(time = OS_time/30, event = OS) ~ quantile, data = plot.data.mut, conf.type = "log-log")
    if (as.numeric(compare.G) == 2){
      my.plot <- ggsurvplot(surv.plot.data, data = plot.data.mut,  risk.table = TRUE, xlim = c(0,60), break.time.by = 12,  
                            xlab = "Time in months", ylab = "Percent Survival", legend.labs = c(paste("LOW", gsub("-.*","",gene), sep=" "), paste("HIGH", gsub("-.*","",gene), sep=" ")), title = titles, font.legend = list(size = 12, color = "black"))
    } else if (as.numeric(compare.G) == 3){
      my.plot <- ggsurvplot(surv.plot.data, data = plot.data.mut,  risk.table = TRUE, xlim = c(0,60), break.time.by = 12,  
                            xlab = "Time in months", ylab = "Percent Survival", legend.labs = c(paste("LOW", gsub("-.*","",gene), sep=" "), paste("MID", gsub("-.*","",gene), sep=" "), paste("HIGH", gsub("-.*","",gene), sep=" ")), title = titles, font.legend = list(size = 12, color = "black"),)
    } else if (as.numeric(compare.G) == 4){
      my.plot <- ggsurvplot(surv.plot.data, data = plot.data.mut,  risk.table = TRUE, xlim = c(0,60), break.time.by = 12,  
                            xlab = "Time in months", ylab = "Percent Survival", legend.labs = c(paste("LOW", gsub("-.*","",gene), sep=" "), paste("MID-LOW", gsub("-.*","",gene), sep=" "), paste("HIGH-MID", gsub("-.*","",gene), sep=" "), paste("HIGH", gsub("-.*","",gene), sep=" ")), title = titles, font.legend = list(size = 12, color = "black"))
    }
    return(my.plot)
  }
  
  output$survPosCESC <- renderPlot({
    hide("cescdownload1P")
    hide("hnsccdownload1P")
    hide("cescdownload2P")
    hide("hnsccdownload2P")
    hide("cescdownload1V")
    hide("hnsccdownload1V")
    hide("cescdownload2V")
    hide("hnsccdownload2V")
    req(input$search, input$compare)
    show("cescdownload1P")
    show("hnsccdownload1P")
    show("cescdownload2P")
    show("hnsccdownload2P")
    show("cescdownload1V")
    show("hnsccdownload1V")
    show("cescdownload2V")
    show("hnsccdownload2V")
    gene <- input$search
    assign("g.gene", gene, envir = my.env)
    plot.data <- makePlotData(gene, "CESC")
    cesc.plot <- drawPlot(plot.data, gene, "CESC Dataset - HPV Positive", "Positive", input$compare)
    assign("g.cesc.plot1", cesc.plot, envir = my.env)
    cesc.plot
  })
  
  output$survNegCESC <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "CESC")
    cesc.plot <- drawPlot(plot.data, gene, "CESC Dataset - HPV Negative", "Negative", input$compare)
    assign("g.cesc.plot2", cesc.plot, envir = my.env)
    cesc.plot
  })
  
  output$survPosHNSCC <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "HNSCC")
    hnscc.plot <- drawPlot(plot.data, gene, "HNSC Dataset - HPV Positive", "Positive", input$compare)
    assign("g.hnscc.plot1", hnscc.plot, envir = my.env)
    hnscc.plot
  })
  
  output$survNegHNSCC <- renderPlot({
    req(input$search, input$compare)
    gene <- input$search
    plot.data <- makePlotData(gene, "HNSCC")
    hnscc.plot <- drawPlot(plot.data, gene, "HNSC Dataset - HPV Negative", "Negative",input$compare)
    assign("g.hnscc.plot2", hnscc.plot, envir = my.env)
    hnscc.plot
  })
  
  makePlotData <- function(gene, mode) {
    if (mode == "CESC"){
      cesc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/CESC-", gene, ".rds", sep=""))
      cesc.combined <- merge(cesc.mRNA, cesc.survival, by="TCGA-Call-Number")
      assign("g.cesc.combined", cesc.combined, envir = my.env)
      pos_hpv <- data.frame(cesc.combined[cesc.combined["HPV_Status.x"] == "HPV16" | cesc.combined["HPV_Status.x"] == "HPV33" | cesc.combined["HPV_Status.x"] == "HPV35", c(gene,"OS", "OS_time")])
      neg_hpv <- data.frame(cesc.combined[cesc.combined$HPV_Status.x =="Negative", c(gene,"OS", "OS_time")])
    } else if (mode == "HNSCC"){
      hnscc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/HNSC-", gene, ".rds", sep=""))
      hnscc.combined <- merge(hnscc.mRNA, hnscc.survival, by="TCGA-Call-Number")
      assign("g.hnscc.combined", hnscc.combined, envir = my.env)
      pos_hpv <- data.frame(hnscc.combined[hnscc.combined["HPV_Status.x"] == "HPV16" | hnscc.combined["HPV_Status.x"] == "HPV33" | hnscc.combined["HPV_Status.x"] == "HPV35", c(gene,"OS", "OS_time")])
      neg_hpv <- data.frame(hnscc.combined[hnscc.combined$HPV_Status.x=="Negative", c(gene,"OS", "OS_time")])
    }
    pos_hpv['Status'] = "HPV"
    names(pos_hpv)[1] = "Value"
    neg_hpv['Status'] = "Negative"
    names(neg_hpv)[1] = "Value"
    plot.data <- rbind(pos_hpv, neg_hpv)
    return(plot.data)
  } 
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui = ui, server = server)
