library(corrplot)
library(DT)
library(dplyr)
library(gdata)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(reshape2)
library(shiny)
library(shinyjqui)
library(shinyjs)
library(shinythemes)

my.env <- new.env()
autofillCell <- sort(readRDS("../thincr-data/autofill-mRNA.rds"))
cesc.hpv.genes <- readRDS("../thincr-data/cesc-hpv-genes.rds")
colnames(cesc.hpv.genes) <- gsub("\\.", "-", colnames(cesc.hpv.genes))
hnsc.hpv.genes <- readRDS("../thincr-data/hnsc-hpv-genes.rds")
colnames(hnsc.hpv.genes) <- gsub("\\.", "-", colnames(hnsc.hpv.genes))
cesc.pqr <- readRDS("pqr-cesc-mRNA.rds")
hnsc.pqr <- readRDS("pqr-hnsc-mRNA.rds")

ui <- fluidPage(title = "THInCR - HPV mRNA vs. Cellular mRNA", theme = shinytheme("cosmo"), useShinyjs(),
  fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("THInCR - The HPV Induced Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
  navbarPage(title="Other Links:", tabPanel(title="☲"), 
             navbarMenu(title="THInCR", tabPanel(a(href="https://thincr.ca/thincr-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
             navbarMenu(title="THInCR2:EE", tabPanel(a(href="https://thincr.ca/thincr2-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr2-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr2-methyl-all/", "Cellular loci vs. Methylation Status")))),
  titlePanel("THInCR - Correlation of HPV mRNA and Cellular mRNA Expression by HPV Status"),
  p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between viral gene mRNA expression and expression of candidate cellular mRNAs in HPV16, HPV33, and HPV35 positive cervical (CESC) and head & neck cancers (HNSC). Select the candidate cellular mRNAs by typing the name in the input box on the top right to perform a Spearman's correlation analysis. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Heat maps can be downloaded as PNG files and the significance matrix data can be downloaded as a CSV file. Level 3 mRNA expression data from the TCGA HNSC and CESC cohorts was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". HPV16/33/35 status and viral mRNA expression was extracted from Ren et al, Oncogene 2020", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7529583/", "DOI: 10.1038/s41388-020-01431-8")),
  p(style="text-align: justify;", strong("Download the Correlation Master List: "), a(href="CML_HPV-combined-mRNA.xlsx", "Click Here", download=NA, target="_blank")),
  p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v3", "Click Here")),
  p(style="text-align: justify;", strong("Citing THInCR: "),"Salnikov et al., 2022, The HPV Induced Cancer Resource (THInCR): a Suite of Tools for Investigating HPV-Dependent Human Carcinogenesis, mSphere, In Press."),
  sidebarLayout( sidebarPanel( 
    selectizeInput(inputId = 'searchCell', label = HTML('Type the Cellular Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
                   choices = autofillCell, selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE), width=6, 
    tags$div(HTML('<br/>')), tags$div(id="p1",tags$strong("Pairwise comparisons for CESC Data:")), tags$div(HTML('<br/>')), dataTableOutput(outputId = 'tableCESC'), br(), 
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'CESCdownloadCSV', 'Download CESC Data'),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), 
    tags$div(HTML('<br/>')), tags$div(id="p2",tags$strong("Pairwise comparisons for HNSC Data:")), tags$div(HTML('<br/>')), dataTableOutput(outputId = 'tableHNSC'), br(),
    fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'HNSCdownloadCSV', 'Download HNSC Data'),
                    tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
    ),
    mainPanel(
      fluidRow(column(6, align="center", offset = 3, dataTableOutput(outputId = 'equiv'))), br(),
      jqui_resizable(plotOutput(outputId = "corrCESC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadP', 'Download CESC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadV', 'Download CESC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
      jqui_resizable(plotOutput(outputId = "corrHNSC")),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnscdownloadP', 'Download HNSC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
      fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnscdownloadV', 'Download HNSC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
  HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  output$CESCdownloadCSV <- downloadHandler(
    filename = function() {paste("CESC-", my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.CESC.table, filename, row.names=FALSE)
  })
  
  output$HNSCdownloadCSV <- downloadHandler(
    filename = function() {paste("HNSC-", my.env$g.gene, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.HNSC.table, filename, row.names=FALSE)
  })
  
  output$cescdownloadP <- downloadHandler(
    filename = function() {paste("CESC-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot, width = 5*input$corrCESC_size$width, height = 5*input$corrCESC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$hnscdownloadP <- downloadHandler(
    filename = function() {paste("HNSC-", my.env$g.gene, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnsc.plot, width = 5*input$corrHNSC_size$width, height = 5*input$corrHNSC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$cescdownloadV <- downloadHandler(
    filename = function() {paste("CESC-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plot, width = 5*input$corrCESC_size$width, height = 5*input$corrCESC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$hnscdownloadV <- downloadHandler(
    filename = function() {paste("HNSC-", my.env$g.gene, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnsc.plot, width = 5*input$corrHNSC_size$width, height = 5*input$corrHNSC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  cormatF <- function(plot.data){
    cormat <- round(cor(select(plot.data, 2:2, 4:11), method = "spearman"), 2)
    melted_cormat <- melt(cormat)
    upper_tri <- get_upper_tri(cormat)
    colnames(upper_tri)[1] <- my.env$g.gene
    rownames(upper_tri)[1] <- my.env$g.gene
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    melted_cormat["p.val"] = "n.s."
    for (i in 1:nrow(melted_cormat)){
      var1=levels(drop.levels(melted_cormat[i,][[1]]))
      var2=levels(drop.levels(melted_cormat[i,][[2]]))
      if(var1 == my.env$g.gene){var1="ValueCell"}
      if(var2 == my.env$g.gene){var2="ValueCell"}
      vals = cor.test(as.numeric(unlist(na.omit(plot.data[var1]))), as.numeric(unlist(na.omit(plot.data[var2]))), method = "spearman", conf.level = 0.95, exact = FALSE)
      if(var1 == var2){melted_cormat[i,]$p.val="N/A"
      }else {
        if (vals$p.value <= 0.0001){
          melted_cormat[i,]$p.val="****"
        } else if (vals$p.value <= 0.001) {
          melted_cormat[i,]$p.val="***"
        } else if (vals$p.value <= 0.01) {
          melted_cormat[i,]$p.val="**"        
        } else if (vals$p.value <= 0.05) {
          melted_cormat[i,]$p.val="*"
        } else{
          melted_cormat[i,]$p.val=="n.s."
        }
      }
    }
    return(melted_cormat)
  }
  
  output$equiv <- renderDT({
    hide("CESCdownloadCSV")
    hide("HNSCdownloadCSV")
    hide("cescdownloadP")
    hide("hnscdownloadP")
    hide("cescdownloadV")
    hide("hnscdownloadV")
    hide("p1")
    hide("p2")
    req(input$searchCell)
    show("CESCdownloadCSV")
    show("HNSCdownloadCSV")
    show("cescdownloadP")
    show("hnscdownloadP")
    show("cescdownloadV")
    show("hnscdownloadV")
    show("equiv")
    show("p1")
    show("p2")
    temp = readRDS("equiv.rds")
    colnames(temp)=c("Symbol", "p-Value Equivalent")
    datatable(temp, options=list(scrollX=TRUE, bFilter=0, paginate=0, pageLength = 5, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:1))), rownames= FALSE) 
  })
  
  output$tableCESC <- renderDT({
    req(input$searchCell)
    geneCell <- input$searchCell
    assign("g.gene", geneCell, envir = my.env)
    temp=cesc.pqr[geneCell, ]
    n=c(paste(gsub("-.*","",geneCell), " vs. E1", sep=""), paste(gsub("-.*","",geneCell), " vs. E2", sep=""), paste(gsub("-.*","",geneCell), " vs. E4", sep=""), paste(gsub("-.*","",geneCell), " vs. E5", sep=""), paste(gsub("-.*","",geneCell), " vs. E6", sep=""), paste(gsub("-.*","",geneCell), " vs. E7", sep=""), paste(gsub("-.*","",geneCell), " vs. L1", sep=""), paste(gsub("-.*","",geneCell), " vs. L2", sep=""))
    r=c(temp[1], temp[4], temp[7],temp[10], temp[13], temp[16], temp[19], temp[22])
    p=c(temp[2], temp[5], temp[8],temp[11], temp[14], temp[17], temp[20], temp[23])
    q=c(temp[3], temp[6], temp[9],temp[12], temp[15], temp[18], temp[21], temp[24])
    plot.data <- makePlotData(geneCell, "CESC")
    melted_cormat <- cormatF(plot.data)
    melted_cormat<-melted_cormat[c(2,4,7,11,16,22,29,37),] 
    s=c()
    for (i in 1:8){
      if (is.na(q[i]) | q[i] > 0.1 | melted_cormat$p.val[i] =="N/A"){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      s <- c(s, p.text)
    }
    data=cbind(n, r, p, q, s)
    colnames(data) <- c("Comparison Group", "Spearman ρ", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.CESC.table", data, envir = my.env)
    datatable(data, options=list(scrollX=TRUE, bFilter=0, paginate=0, pageLength = 15, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))), rownames= FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212'))) %>% formatSignif(columns=c(2, 3, 4), digits=3)
  })
  
  output$tableHNSC <- renderDT({
    req(input$searchCell)
    geneCell <- input$searchCell
    assign("g.gene", geneCell, envir = my.env)
    temp=hnsc.pqr[geneCell, ]
    n=c(paste(gsub("-.*","",geneCell), " vs. E1", sep=""), paste(gsub("-.*","",geneCell), " vs. E2", sep=""), paste(gsub("-.*","",geneCell), " vs. E4", sep=""), paste(gsub("-.*","",geneCell), " vs. E5", sep=""), paste(gsub("-.*","",geneCell), " vs. E6", sep=""), paste(gsub("-.*","",geneCell), " vs. E7", sep=""), paste(gsub("-.*","",geneCell), " vs. L1", sep=""), paste(gsub("-.*","",geneCell), " vs. L2", sep=""))
    r=c(temp[1], temp[4], temp[7],temp[10], temp[13], temp[16], temp[19], temp[22])
    p=c(temp[2], temp[5], temp[8],temp[11], temp[14], temp[17], temp[20], temp[23])
    q=c(temp[3], temp[6], temp[9],temp[12], temp[15], temp[18], temp[21], temp[24])
    plot.data <- makePlotData(geneCell, "HNSC")
    melted_cormat <- cormatF(plot.data)
    melted_cormat<-melted_cormat[c(2,4,7,11,16,22,29,37),] 
    s=c()
    for (i in 1:8){
      if (is.na(q[i]) | q[i] > 0.1 | melted_cormat$p.val[i] =="N/A"){
        p.text <- "NO"
      } else {
        p.text <- "YES"
      } 
      s <- c(s, p.text)
    }
    data=cbind(n, r, p, q, s)
    colnames(data) <- c("Comparison Group", "Spearman ρ", "p-Value", "q-Value (FDR=0.1)", "Significance")
    assign("g.HNSC.table", data, envir = my.env)
    datatable(data, options=list(scrollX=TRUE, bFilter=0, paginate=0, pageLength = 15, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))), rownames= FALSE) %>%  formatStyle(columns = c('Significance'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212'))) %>% formatSignif(columns=c(2, 3, 4), digits=3)
  })
  
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  drawPlot <- function(plot.data, titles, geneCell, mode) {
    melted_cormat <- cormatF(plot.data)
    my.plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_y_discrete(labels=c(gsub("-.*","",geneCell), "E1", "E2", "E4", "E5", "E6", "E7", "L1", "L2")) +
      scale_x_discrete(labels=c(gsub("-.*","",geneCell), "E1", "E2", "E4", "E5", "E6", "E7", "L1", "L2")) +
      geom_text(aes(Var2, Var1, label = value), color="black", size=4, vjust = -0.5) +
      geom_text(aes(label=p.val), color="black", size=4, vjust = 0.7) + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") + theme_minimal() + 
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.direction = "horizontal", plot.title = element_text(hjust = 0.5), axis.text.y=element_text(size=12, color='black'), axis.text.x=element_text(size=12, color='black', angle=45, hjust=1, vjust=1)) +
      ggtitle(titles) + guides(fill = guide_colorbar(barwidth = 6, barheight = 1,  title.position = "top", title.hjust = 0.5)) + 
      coord_fixed()
    return(my.plot)
  }
  
  output$corrCESC <- renderPlot({
    req(input$searchCell)
    geneCell <- input$searchCell
    plot.data <- makePlotData(geneCell, "CESC")
    cesc.plot <- drawPlot(plot.data, "CESC Dataset", geneCell, "CESC")
    assign("g.cesc.plot", cesc.plot, envir = my.env)
    cesc.plot
  })
  
  output$corrHNSC <- renderPlot({
    req(input$searchCell)
    geneCell <- input$searchCell
    plot.data <- makePlotData(geneCell, "HNSC")
    hnsc.plot <- drawPlot(plot.data, "HNSC Dataset", geneCell, "HNSC")
    assign("g.hnsc.plot", hnsc.plot, envir = my.env)
    hnsc.plot
  })
  
  makePlotData <- function(geneCell, mode) {
    if (mode == "CESC") {
      cesc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/CESC-", geneCell, ".rds", sep=""))
      assign("g.cesc.mRNA", cesc.mRNA, envir = my.env)
      hpvCell <- data.frame(cesc.mRNA[cesc.mRNA["HPV_Status"] == "HPV16" | cesc.mRNA["HPV_Status"] == "HPV33" | cesc.mRNA["HPV_Status"] == "HPV35", geneCell])
      hpvCell['TCGA-Call-Number'] = cesc.mRNA[cesc.mRNA["HPV_Status"] == "HPV16" | cesc.mRNA["HPV_Status"] == "HPV33" | cesc.mRNA["HPV_Status"] == "HPV35", "TCGA-Call-Number"]
      names(hpvCell)[1] = "ValueCell"
      hpvPreTot <- data.frame(cesc.hpv.genes[cesc.hpv.genes["HPV_Status"] == "HPV16" | cesc.hpv.genes["HPV_Status"] == "HPV33" | cesc.hpv.genes["HPV_Status"] == "HPV35", ])
    } else if (mode == "HNSC") {
      hnsc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/HNSC-", geneCell, ".rds", sep=""))
      assign("g.hnscc.mRNA", hnsc.mRNA, envir = my.env)
      hpvCell <- data.frame(hnsc.mRNA[hnsc.mRNA["HPV_Status"] == "HPV16" | hnsc.mRNA["HPV_Status"] == "HPV33" | hnsc.mRNA["HPV_Status"] == "HPV35", geneCell])
      hpvCell['TCGA-Call-Number'] = hnsc.mRNA[hnsc.mRNA["HPV_Status"] == "HPV16" | hnsc.mRNA["HPV_Status"] == "HPV33" | hnsc.mRNA["HPV_Status"] == "HPV35", "TCGA-Call-Number"]
      names(hpvCell)[1] = "ValueCell"
      hpvPreTot <- data.frame(hnsc.hpv.genes[hnsc.hpv.genes["HPV_Status"] == "HPV16" | hnsc.hpv.genes["HPV_Status"] == "HPV33" | hnsc.hpv.genes["HPV_Status"] == "HPV35", ])
    }
    colnames(hpvPreTot) <- gsub("\\.", "-", colnames(hpvPreTot))
    hpvTotal <- merge(hpvCell, hpvPreTot, by="TCGA-Call-Number")
    return(hpvTotal)
  }
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)