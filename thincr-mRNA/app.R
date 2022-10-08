library(DT)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)

my.env <- new.env()
autofill <- sort(readRDS("../thincr-data/autofill-mRNA.rds"))
pqvalues <- readRDS("pq-values-mRNA.rds")

ui <- fluidPage(title = "THInCR - mRNA vs. HPV Status", theme = shinytheme("cosmo"), useShinyjs(), 
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("THInCR - The HPV Induced Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Other Links:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://thincr.ca/thincr-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="THInCR2:EE", tabPanel(a(href="https://thincr.ca/thincr2-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr2-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr2-methyl-all/", "Cellular loci vs. Methylation Status")))),
  fluidRow(style="margin-left: 1px;", titlePanel("THInCR - Correlation of Cellular mRNA Expression and HPV Status")),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to compare expression of candidate cellular mRNAs between HPV+, HPV- and normal control tissue from the TCGA cervical (CESC) and head & neck cancer (HNSC) cohorts. Select the candidate mRNA by typing the name in the input box on the top right. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Box plots can be downloaded as PNG files and the information necessary to reproduce the box plots can be downloaded as a CSV file. Level 3 mRNA expression data from the TCGA HNSC and CESC cohorts was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". HPV status was annotated as described in Gameiro et al, Viruses 2017", a(href="https://www.mdpi.com/1999-4915/9/9/252", "DOI: 10.3390/v9090252")),
  p(style="text-align: justify;", strong("Download the Differentially Expressed Gene (DEG) Analysis: "), a(href="DEG-combined-mRNA.xlsx", "Click Here", download=NA, target="_blank")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v1", "Click Here")),
  p(style="text-align: justify;", strong("Citing THInCR: "),"Salnikov et al., 2022, The HPV Induced Cancer Resource (THInCR): a Suite of Tools for Investigating HPV-Dependent Human Carcinogenesis, mSphere, In Press."),
  sidebarLayout( sidebarPanel(width=6, selectizeInput(
    inputId = 'search', label = HTML('Type the Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
    choices = autofill,selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE),
    dataTableOutput(outputId = 'tablePQ'), tags$div(HTML('<br/>')),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadPQ', 'Download Data'),
            tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>')),
    dataTableOutput(outputId = 'tableStats'), tags$div(HTML('<br/>')),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadCSV', 'Download Data'),
            tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))),
    mainPanel(
      jqui_resizable(plotOutput(outputId = "boxPlotCESC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadP', 'Download CESC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadV', 'Download CESC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "boxPlotHNSCC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownloadP', 'Download HNSC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownloadV', 'Download HNSC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  output$downloadPQ <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-pq.csv", sep="")},
    content = function(filename){write.csv(my.env$g.tablePQ, filename, row.names=FALSE)
    })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {paste(my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.tableStats, filename, row.names=FALSE)
    })
  
  output$cescdownloadP <- downloadHandler(
    filename = function() {paste("CESC-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot, width = 5*input$boxPlotCESC_size$width, height = 5*input$boxPlotCESC_size$height, dpi = 300, units = "px", device = "png")
    })
  
  output$hnsccdownloadP <- downloadHandler(
    filename = function() {paste("HNSC-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plot, width = 5*input$boxPlotHNSCC_size$width, height = 5*input$boxPlotHNSCC_size$height, dpi = 300, units = "px", device = "png")
    })
  
  output$cescdownloadV <- downloadHandler(
    filename = function() {paste("CESC-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot, width = 5*input$boxPlotCESC_size$width, height = 5*input$boxPlotCESC_size$height, dpi = 300, units = "px", device = "pdf")
    })
  
  output$hnsccdownloadV <- downloadHandler(
    filename = function() {paste("HNSC-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plot, width = 5*input$boxPlotHNSCC_size$width, height = 5*input$boxPlotHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
    })
  
  output$tablePQ <- renderDT({
    hide("cescdownloadP")
    hide("cescdownloadV")
    hide("hnsccdownloadP")
    hide("hnsccdownloadV")
    hide("downloadCSV")
    hide("downloadPQ")
    req(input$search)
    show("cescdownloadP")
    show("cescdownloadV")
    show("hnsccdownloadP")
    show("hnsccdownloadV")
    show("downloadCSV")
    show("downloadPQ")
    gene <- input$search
    assign("g.gene", gene, envir = my.env)
    hnscc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/HNSC-", gene, ".rds", sep=""))
    assign("g.hnscc.mRNA", hnscc.mRNA, envir = my.env)
    cesc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/CESC-", gene, ".rds", sep=""))
    assign("g.cesc.mRNA", cesc.mRNA, envir = my.env)
    temp <- pqvalues[pqvalues$Gene == gene,]
    pval <- c(temp[2], temp[4], temp[6], temp[8], temp[10], temp[12])
    pval <- signif(as.numeric(pval), digits = 3)
    qval <- c(temp[3], temp[5], temp[7], temp[9], temp[11], temp[13])
    qval <- signif(as.numeric(qval), digits = 3)
    pval.s <- c()
    cesc.plot.data <- makePlotData(gene, "CESC")
    hnsc.plot.data <- makePlotData(gene, "HNSC")
    percent1 <- 100*colSums(cesc.plot.data[1] == 0)/nrow(cesc.plot.data)
    percent2 <- 100*colSums(hnsc.plot.data[1] == 0)/nrow(hnsc.plot.data)
    truthy1=TRUE
    truthy2=TRUE
    if (percent1 > 50){truthy1=FALSE}
    if (percent2 > 50){truthy2=FALSE}
    for (i in 1:length(pval)){
      q <- as.numeric(qval[i])
      if (is.na(q) | q > 0.1 | i<4 & truthy1==FALSE | i>3 & truthy2==FALSE){
        p.text <- "NO"
      } else{
        p.text <- "YES"
      }
      pval.s <- c(pval.s, p.text)
    }
    names <- c("CESC - HPV(+) vs. HPV(-)", "CESC - HPV(+) vs. Normal", "CESC - HPV(-) vs. Normal", "HNSC - HPV(+) vs. HPV(-)", "HNSC - HPV(+) vs. Normal", "HNSC - HPV(-) vs. Normal")
    tablePQ <- data.frame(cbind(names, pval, qval, pval.s))
    colnames(tablePQ) <- c("Comparison Group", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.tablePQ", tablePQ, envir = my.env)
    datatable(tablePQ, options=list(scrollX=TRUE, bFilter=0, paginate=FALSE, bLengthChange=0, bInfo=0, autoWidth=FALSE, columnDefs=list(list(className = 'dt-center', targets = 0:3))), rownames=FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212')))
  })
  
  output$tableStats <- renderDT({
    req(input$search)
    gene <- input$search
    assign("g.gene", gene, envir = my.env)
    cesc.mRNA <- my.env$g.cesc.mRNA
    hnscc.mRNA <- my.env$g.hnscc.mRNA
    cesc.hpv <- data.frame(cesc.mRNA[cesc.mRNA["HPV_Status"] == "HPV16" | cesc.mRNA["HPV_Status"] == "HPV33" | cesc.mRNA["HPV_Status"] == "HPV35", gene])
    names(cesc.hpv)[1] = "Value"
    cesc.hpv.summary <- c("CESC - HPV(+)", as.numeric(count(cesc.hpv)$n), round(as.numeric(min(cesc.hpv)), digits = 3), round(as.numeric(max(cesc.hpv)), digits = 3), round(mean(unlist(cesc.hpv)), digits = 3), round(as.numeric(summarize_all(cesc.hpv, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.hpv, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.hpv, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    cesc.neg <- data.frame(cesc.mRNA[cesc.mRNA$HPV_Status=="Negative", gene])
    names(cesc.neg)[1] = "Value"
    cesc.neg.summary <- c("CESC - HPV(-)", as.numeric(count(cesc.neg)$n), round(as.numeric(min(cesc.neg)), digits = 3), round(as.numeric(max(cesc.neg)), digits = 3), round(mean(unlist(cesc.neg)), digits = 3), round(as.numeric(summarize_all(cesc.neg, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.neg, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.neg, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    cesc.norm <- data.frame(cesc.mRNA[cesc.mRNA$HPV_Status=="Normal", gene])
    names(cesc.norm)[1] = "Value"
    cesc.norm.summary <- c("CESC - Normal", as.numeric(count(cesc.norm)$n), round(as.numeric(min(cesc.norm)), digits = 3), round(as.numeric(max(cesc.norm)), digits = 3), round(mean(unlist(cesc.norm)), digits = 3), round(as.numeric(summarize_all(cesc.norm, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.norm, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.norm, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.hpv <- data.frame(hnscc.mRNA[hnscc.mRNA["HPV_Status"] == "HPV16" | hnscc.mRNA["HPV_Status"] == "HPV33" | hnscc.mRNA["HPV_Status"] == "HPV35", gene])
    names(hnscc.hpv)[1] = "Value"
    hnscc.hpv.summary <- c("HNSC - HPV(+)", as.numeric(count(hnscc.hpv)$n), round(as.numeric(min(hnscc.hpv)), digits = 3), round(as.numeric(max(hnscc.hpv)), digits = 3), round(mean(unlist(hnscc.hpv)), digits = 3), round(as.numeric(summarize_all(hnscc.hpv, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.hpv, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.hpv, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.neg <- data.frame(hnscc.mRNA[hnscc.mRNA$HPV_Status=="Negative", gene])
    names(hnscc.neg)[1] = "Value"
    hnscc.neg.summary <- c("HNSC - HPV(-)", as.numeric(count(hnscc.neg)$n), round(as.numeric(min(hnscc.neg)), digits = 3), round(as.numeric(max(hnscc.neg)), digits = 3), round(mean(unlist(hnscc.neg)), digits = 3), round(as.numeric(summarize_all(hnscc.neg, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.neg, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.neg, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.norm <- data.frame(hnscc.mRNA[hnscc.mRNA$HPV_Status=="Normal", gene])
    names(hnscc.norm)[1] = "Value"
    hnscc.norm.summary <- c("HNSC - Normal", as.numeric(count(hnscc.norm)$n), round(as.numeric(min(hnscc.norm)), digits = 3), round(as.numeric(max(hnscc.norm)), digits = 3), round(mean(unlist(hnscc.norm)), digits = 3), round(as.numeric(summarize_all(hnscc.norm, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.norm, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.norm, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    tableStats <- rbind(cesc.hpv.summary, cesc.neg.summary, cesc.norm.summary, hnscc.hpv.summary, hnscc.neg.summary, hnscc.norm.summary)
    colnames(tableStats) <- c("Dataset", "# of Patients", "Min", "Max", "Mean", "1st Quartile", "Median", "3rd Quartile")
    row.names(tableStats) <- c("CESC - HPV (+)", "CESC - HPV (-)", "CESC - Normal", "HNSC - HPV (+)", "HNSC - HPV (-)", "HNSC - Normal")
    assign("g.tableStats", tableStats, envir = my.env)
    tableStats
  }, options=list(scrollX = TRUE, bFilter=0, bPaginate=0, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:7))), rownames= FALSE)
  
  drawPlot <- function(plot.data, gene, titles) {
    pos=unlist(plot.data[plot.data[2]=="HPV",][1])
    neg=unlist(plot.data[plot.data[2]=="Negative",][1])
    nrm=unlist(plot.data[plot.data[2]=="Normal",][1])
    transform.pos=data.frame(signif(max(pos),3), signif(min(pos),3), signif(median(pos),3), quantile(pos, c(0.25,0.75))[1], quantile(pos, c(0.25,0.75))[2], "Positive")
    colnames(transform.pos)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.neg=data.frame(signif(max(neg),3), signif(min(neg),3), signif(median(neg),3), quantile(neg, c(0.25,0.75))[1], quantile(neg, c(0.25,0.75))[2], "Negative")
    colnames(transform.neg)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.nrm=data.frame(signif(max(nrm),3), signif(min(nrm),3), signif(median(nrm),3), quantile(nrm, c(0.25,0.75))[1], quantile(nrm, c(0.25,0.75))[2], "Normal")
    colnames(transform.nrm)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.data=rbind(transform.pos, transform.neg, transform.nrm)
    transform.data$Status <- factor(transform.data$Status , levels=c("Positive", "Negative", "Normal"))
    my.plot <- ggplot(transform.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("HPV Positive","HPV Negative","Normal Control"), name="HPV Status") +
      ylab(paste("mRNA expression levels of", gsub("-.*","",gene), sep=" ")) +
      ggtitle(titles) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    return(my.plot)
  }
  
  output$boxPlotCESC <- renderPlot({
    req(input$search)
    gene <- input$search
    plot.data <- makePlotData(gene, "CESC")
    cesc.plot <- drawPlot(plot.data, gene, "CESC Dataset")
    assign("g.cesc.plot", cesc.plot, envir = my.env)
    cesc.plot
  })
  
  output$boxPlotHNSCC <- renderPlot({
    req(input$search)
    gene <- input$search
    plot.data <- makePlotData(gene, "HNSC")
    hnscc.plot <- drawPlot(plot.data, gene, "HNSC Dataset")
    assign("g.hnscc.plot", hnscc.plot, envir = my.env)
    hnscc.plot
  })
  
  makePlotData <- function(gene, mode) {
    if(mode == "CESC") {
      cesc.mRNA <- my.env$g.cesc.mRNA
      hpv <- data.frame(cesc.mRNA[cesc.mRNA["HPV_Status"] == "HPV16" | cesc.mRNA["HPV_Status"] == "HPV33" | cesc.mRNA["HPV_Status"] == "HPV35", gene])
      neg <- data.frame(cesc.mRNA[cesc.mRNA$HPV_Status=="Negative", gene])
      norm <- data.frame(cesc.mRNA[cesc.mRNA$HPV_Status=="Normal", gene])
    } else if(mode == "HNSC") {
      hnscc.mRNA <- my.env$g.hnscc.mRNA
      hpv <- data.frame(hnscc.mRNA[hnscc.mRNA["HPV_Status"] == "HPV16" | hnscc.mRNA["HPV_Status"] == "HPV33" | hnscc.mRNA["HPV_Status"] == "HPV35", gene])
      neg <- data.frame(hnscc.mRNA[hnscc.mRNA$HPV_Status=="Negative", gene])
      norm <- data.frame(hnscc.mRNA[hnscc.mRNA$HPV_Status=="Normal", gene])
    }
    hpv['Status'] = "HPV"
    names(hpv)[1] = "Value"
    neg['Status'] = "Negative"
    names(neg)[1] = "Value"
    norm['Status'] = "Normal"
    names(norm)[1] = "Value"
    plot.data <- rbind(hpv, neg, norm)
    return(plot.data)
  }
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)