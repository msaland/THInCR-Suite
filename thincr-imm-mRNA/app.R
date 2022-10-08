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
autofill1 <- sort(readRDS("../thincr-data/autofill-mRNA.rds"))
cesc.imm <- readRDS("../thincr-data/cesc-imm.rds")
autofill2 <- sort(gsub("\\.", " ", colnames(cesc.imm[c(6:8,10:33,38:60,63:ncol(cesc.imm))])))
colnames(cesc.imm) <- gsub("\\.", "-", colnames(cesc.imm))
hnscc.imm <- readRDS("../thincr-data/hnsc-imm.rds")
colnames(hnscc.imm) <- gsub("\\.", "-", colnames(hnscc.imm))
descript <- readRDS("../thincr-data/description-imm.rds")

ui <- fluidPage(title = "THInCR - mRNA vs. Immune Function", theme = shinytheme("cosmo"), useShinyjs(),
        fluidRow(column(11.5, style = "background-image: linear-gradient(to bottom, #4F2683 , #807F83); margin-left:15px; margin-right:15px;",  align="middle", div(style = "margin-left:10px; margin-right:10px; padding-top:5px; padding-bottom:5px; display:inline-block;", img(src="logo.png", width="175px", heigth="175px")), div(style = "display:inline-block; vertical-align:middle;", h1("THInCR - The HPV Induced Cancer Resource", style="font-weight: bold; color: white; font-size: 50px;")))),
        navbarPage(title="Other Links:", tabPanel(title="☲"), 
                   navbarMenu(title="THInCR", tabPanel(a(href="https://thincr.ca/thincr-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr-mrna/", "Cellular mRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-mirna/", "Cellular miRNA Expression vs. HPV Status")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mrna/", "Cellular mRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-correlation-mirna/", "Cellular miRNA vs. HPV mRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr-methyl-all/", "Cellular loci vs. Methylation Status"))),
                   navbarMenu(title="THInCR2:EE", tabPanel(a(href="https://thincr.ca/thincr2-home/", "Home")), tabPanel(a(href="https://thincr.ca/thincr2-mrna/", "Cellular mRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-mirna/", "Cellular miRNA Expression vs. EBV Status")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mrna/", "Cellular mRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-correlation-mirna/", "Cellular miRNA vs. EBV miRNA Expression")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mrna/", "Cellular mRNA Expression vs. Immune Function")), tabPanel(a(href="https://thincr.ca/thincr2-imm-mirna/", "Cellular miRNA Expression vs. Immune Function")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mrna/", "Cellular mRNA Expression vs. Overall Survival")),tabPanel(a(href="https://thincr.ca/thincr2-survival-mirna/", "Cellular miRNA Expression vs. Overall Survival")), tabPanel(a(href="https://thincr.ca/thincr2-methyl-all/", "Cellular loci vs. Methylation Status")))),
        titlePanel("THInCR - Correlation of Cellular mRNA Expression and Immune Function by HPV Status"),
        p(style="text-align: justify;", strong("Description: "), "The purpose of this analysis is to explore relationships between candidate cellular mRNA expression and various calculated characteristics related to the tumor immune landscape in HPV positive cervical (CESC) and head & neck cancers (HNSC). Select the candidate cellular mRNAs by typing the name in the input box on the top right. Select the immune function/factor to analyze by selecting the desired option in the adjacent dropdown. Generated figures can be resized via dragging the right or bottom figure edges, or via the grey triangle in the bottom right edge of the figure. Correlation plots can be downloaded as PNG files and the information necessary to reproduce the box and scatter plots can be downloaded as a CSV file. Level 3 mRNA expression data from the TCGA head & neck cancer (HNSC) and cervical carcinoma (CESC) cohorts was downloaded from the Broad Genome Data Analysis Center’s Firehose server", a(href="https://gdac.broadinstitute.org/", "Link"), ". HPV status was annotated as described in Gameiro et al, Viruses 2017", a(href="https://www.mdpi.com/1999-4915/9/9/252", "DOI: 10.3390/v9090252"), ". Immune landscape data was extracted from Thorsson et al, Immunity 2018", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5982584/", "DOI: 10.1016/j.immuni.2018.03.023")),
        p(style="text-align: justify;", strong("Immune Landscape Features: "), "A master list of all 53 immune landscape features, put together according to Thorsson et al, Immunity 2018", a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5982584/", "DOI: 10.1016/j.immuni.2018.03.023"), "can be found", a(href="description-imm.txt", "here"), "."),
        p(style="text-align: justify;", strong("Video Guide: "), a(href="https://thincr.ca/thincr-home/#v2", "Click Here")),
        p(style="text-align: justify;", strong("Citing THInCR: "),"Salnikov et al., 2022, The HPV Induced Cancer Resource (THInCR): a Suite of Tools for Investigating HPV-Dependent Human Carcinogenesis, mSphere, In Press."),
        sidebarLayout( sidebarPanel(width=6, 
            fluidRow(column(7, selectizeInput(inputId = 'searchGene', label = HTML('Type the Gene ID or Name', '<br/>', 'Selection Format: (Gene Name)-(Gene ID)'),
                                              choices = autofill1, selected = character(0), options = list(placeholder = 'Please type in the gene name/ID or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE)),
                     column(5, selectizeInput(inputId = 'searchFactor', label = HTML('<br/>', 'Factor Selection'),
                                              choices = autofill2, selected = character(0), options = list(placeholder = 'Please type in the factor or select from the list', onInitialize = I('function() { this.setValue(""); }'), openOnFocus = FALSE), multiple = FALSE))),
              dataTableOutput(outputId = 'tableFactor'),
              fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadFactorCSV', 'Download Factor Data'),
                              tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), tags$div(HTML('<br/>')),
              tags$div(id="p1", tags$strong("P-values for CESC Boxplot Data:")), htmlOutput(outputId = "textCESC1B"), tags$div(HTML('<br/>')),
              tags$div(id="p2",tags$strong("P-values for HNSC Boxplot Data:")), htmlOutput(outputId = "textHNSCC1B"), tags$div(HTML('<br/>')),
              dataTableOutput(outputId = 'table'), tags$div(HTML('<br/>')),
              fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'downloadCSV', 'Download Data'),
                 tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")))),
          mainPanel(
            tags$div(id="d", tags$strong("Description:")), htmlOutput(outputId = "description"), tags$div(HTML('<br/>')),
            jqui_resizable(plotOutput(outputId = "scatterPlotCESC")),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadScatterP', 'Download CESC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadScatterV', 'Download CESC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
            jqui_resizable(plotOutput(outputId = "boxPlotCESC")),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadBoxP', 'Download CESC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'cescdownloadBoxV', 'Download CESC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),
            jqui_resizable(plotOutput(outputId = "scatterPlotHNSCC")),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownloadScatterP', 'Download HNSC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))),  br(),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownloadScatterV', 'Download HNSC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), 
            jqui_resizable(plotOutput(outputId = "boxPlotHNSCC")),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownloadBoxP', 'Download HNSC Plot (PNG)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), br(),
            fluidRow(column(6, align="center", offset = 3, downloadButton(outputId = 'hnsccdownloadBoxV', 'Download HNSC Plot (PDF)'), tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"))), width = 6)),
        HTML('<br>'), column(12, style="background-color:#4F2683;", div(style="margin-left:1px;", img(src='uwo.gif', align = "left", width="150px", heigth="75px")))
)

server <- function(input, output, session) {
  output$downloadCSV <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-", my.env$g.factor, ".csv", sep="")},
    content = function(filename){write.csv(my.env$g.table, filename, row.names=FALSE)
  })
  
  output$downloadFactorCSV <- downloadHandler(
    filename = function() {paste(my.env$g.gene, "-all_factors.csv", sep="")},
    content = function(filename){write.csv(my.env$g.tableFactor, filename, row.names=FALSE)
  })
  
  output$cescdownloadScatterP <- downloadHandler(
    filename = function() {paste("CESC-scatter-", my.env$g.gene, "-", my.env$g.factor, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plotScatter, width = 5*input$scatterPlotCESC_size$width, height = 5*input$scatterPlotCESC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$cescdownloadBoxP <- downloadHandler(
    filename = function() {paste("CESC-box-", my.env$g.gene, "-", my.env$g.factor, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plotBox, width = 5*input$boxPlotCESC_size$width, height = 5*input$boxPlotCESC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$hnsccdownloadScatterP <- downloadHandler(
    filename = function() {paste("HNSC-scatter-", my.env$g.gene, "-", my.env$g.factor, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plotScatter, width = 5*input$scatterPlotHNSCC_size$width, height = 5*input$scatterPlotHNSCC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$hnsccdownloadBoxP <- downloadHandler(
    filename = function() {paste("HNSC-box-", my.env$g.gene, "-", my.env$g.factor, ".png", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plotBox, width = 5*input$boxPlotHNSCC_size$width, height = 5*input$boxPlotHNSCC_size$height, dpi = 300, units = "px", device = "png")
  })
  
  output$cescdownloadScatterV <- downloadHandler(
    filename = function() {paste("CESC-scatter-", my.env$g.gene, "-", my.env$g.factor, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plotScatter, width = 5*input$scatterPlotCESC_size$width, height = 5*input$scatterPlotCESC_size$height, dpi = 300, units = "px", device = "pdf")
  })

  output$cescdownloadBoxV <- downloadHandler(
    filename = function() {paste("CESC-box-", my.env$g.gene, "-", my.env$g.factor, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.cesc.plotBox, width = 5*input$boxPlotCESC_size$width, height = 5*input$boxPlotCESC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$hnsccdownloadScatterV <- downloadHandler(
    filename = function() {paste("HNSC-scatter-", my.env$g.gene, "-", my.env$g.factor, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plotScatter, width = 5*input$scatterPlotHNSCC_size$width, height = 5*input$scatterPlotHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$hnsccdownloadBoxV <- downloadHandler(
    filename = function() {paste("HNSC-box-", my.env$g.gene, "-", my.env$g.factor, ".pdf", sep="")},
    content = function(filename){ggsave(filename, my.env$g.hnscc.plotBox, width = 5*input$boxPlotHNSCC_size$width, height = 5*input$boxPlotHNSCC_size$height, dpi = 300, units = "px", device = "pdf")
  })
  
  output$table <- renderDT({
    hide("cescdownloadScatterP")
    hide("cescdownloadBoxP")
    hide("hnsccdownloadScatterP")
    hide("hnsccdownloadBoxP")
    hide("cescdownloadScatterV")
    hide("cescdownloadBoxV")
    hide("hnsccdownloadScatterV")
    hide("hnsccdownloadBoxV")
    hide("downloadCSV")
    hide("p1")
    hide("p2")
    hide("d")
    req(input$searchGene, input$searchFactor)
    show("cescdownloadScatterP")
    show("cescdownloadBoxP")
    show("hnsccdownloadScatterP")
    show("hnsccdownloadBoxP")
    show("cescdownloadScatterV")
    show("cescdownloadBoxV")
    show("hnsccdownloadScatterV")
    show("hnsccdownloadBoxV")
    show("downloadCSV")
    show("downloadFactorCSV")
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    assign("g.factor", factor, envir = my.env)
    cesc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/CESC-", gene, ".rds", sep=""))
    cesc.combined <- merge(cesc.mRNA, cesc.imm, by="TCGA-Call-Number")
    assign("g.cesc.combined", cesc.combined, envir = my.env)
    hnscc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/HNSC-", gene, ".rds", sep=""))
    hnscc.combined <- merge(hnscc.mRNA, hnscc.imm, by="TCGA-Call-Number")
    assign("g.hnscc.combined", hnscc.combined, envir = my.env)
    cesc.hpv.gene <- data.frame(cesc.combined[cesc.combined["HPV_Status.x"] == "HPV16" | cesc.combined["HPV_Status.x"] == "HPV33" | cesc.combined["HPV_Status.x"] == "HPV35", gene])
    names(cesc.hpv.gene)[1] = "Value"
    cesc.hpv.gene.summary <- c(paste("CESC HPV(+) -", gsub("-.*","",gene), sep=" "), as.numeric(count(cesc.hpv.gene)$n), round(as.numeric(min(cesc.hpv.gene)), digits = 3), round(as.numeric(max(cesc.hpv.gene)), digits = 3), round(mean(unlist(cesc.hpv.gene)), digits = 3), round(as.numeric(summarize_all(cesc.hpv.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.hpv.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.hpv.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    cesc.neg.gene <- data.frame(cesc.combined[cesc.combined$HPV_Status.x=="Negative", gene])
    names(cesc.neg.gene)[1] = "Value"
    cesc.neg.gene.summary <- c(paste("CESC HPV(-) -", gsub("-.*","",gene), sep=" "), as.numeric(count(cesc.neg.gene)$n), round(as.numeric(min(cesc.neg.gene)), digits = 3), round(as.numeric(max(cesc.neg.gene)), digits = 3), round(mean(unlist(cesc.neg.gene)), digits = 3), round(as.numeric(summarize_all(cesc.neg.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.neg.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.neg.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    cesc.hpv.factor <- data.frame(na.omit(cesc.combined[cesc.combined["HPV_Status.x"] == "HPV16" | cesc.combined["HPV_Status.x"] == "HPV33" | cesc.combined["HPV_Status.x"] == "HPV35", factor]))
    names(cesc.hpv.factor)[1] = "Value"
    cesc.hpv.factor.summary <- c(paste("CESC HPV(+) -", gsub("-"," ",factor), sep=" "), as.numeric(count(cesc.hpv.factor)$n), round(as.numeric(min(cesc.hpv.factor)), digits = 3), round(as.numeric(max(cesc.hpv.factor)), digits = 3), round(mean(unlist(cesc.hpv.factor)), digits = 3), round(as.numeric(summarize_all(cesc.hpv.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.hpv.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.hpv.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    cesc.neg.factor <- data.frame(na.omit(cesc.combined[cesc.combined$HPV_Status.x=="Negative", factor]))
    names(cesc.neg.factor)[1] = "Value"
    cesc.neg.factor.summary <- c(paste("CESC HPV(-) -", gsub("-"," ",factor), sep=" "), as.numeric(count(cesc.neg.factor)$n), round(as.numeric(min(cesc.neg.factor)), digits = 3), round(as.numeric(max(cesc.neg.factor)), digits = 3), round(mean(unlist(cesc.neg.factor)), digits = 3), round(as.numeric(summarize_all(cesc.neg.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(cesc.neg.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(cesc.neg.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.hpv.gene <- data.frame(hnscc.combined[hnscc.combined["HPV_Status.x"] == "HPV16" | hnscc.combined["HPV_Status.x"] == "HPV33" | hnscc.combined["HPV_Status.x"] == "HPV35", gene])
    names(hnscc.hpv.gene)[1] = "Value"
    hnscc.hpv.gene.summary <- c(paste("HNSC HPV(+) - ", gsub("-.*","",gene), sep=" "), as.numeric(count(hnscc.hpv.gene)$n), round(as.numeric(min(hnscc.hpv.gene)), digits = 3), round(as.numeric(max(hnscc.hpv.gene)), digits = 3), round(mean(unlist(hnscc.hpv.gene)), digits = 3), round(as.numeric(summarize_all(hnscc.hpv.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.hpv.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.hpv.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.neg.gene <- data.frame(hnscc.combined[hnscc.combined$HPV_Status.x=="Negative", gene])
    names(hnscc.neg.gene)[1] = "Value"
    hnscc.neg.gene.summary <- c(paste("HNSC HPV(+) - ", gsub("-.*","",gene), sep=" "), as.numeric(count(hnscc.neg.gene)$n), round(as.numeric(min(hnscc.neg.gene)), digits = 3), round(as.numeric(max(hnscc.neg.gene)), digits = 3), round(mean(unlist(hnscc.neg.gene)), digits = 3), round(as.numeric(summarize_all(hnscc.neg.gene, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.neg.gene, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.neg.gene, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.hpv.factor <- data.frame(na.omit(hnscc.combined[hnscc.combined["HPV_Status.x"] == "HPV16" | hnscc.combined["HPV_Status.x"] == "HPV33" | hnscc.combined["HPV_Status.x"] == "HPV35", factor]))
    names(hnscc.hpv.factor)[1] = "Value"
    hnscc.hpv.factor.summary <- c(paste("HNSC HPV(+) -", gsub("-"," ",factor), sep=" "), as.numeric(count(hnscc.hpv.factor)$n), round(as.numeric(min(hnscc.hpv.factor)), digits = 3), round(as.numeric(max(hnscc.hpv.factor)), digits = 3), round(mean(unlist(hnscc.hpv.factor)), digits = 3), round(as.numeric(summarize_all(hnscc.hpv.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.hpv.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.hpv.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    hnscc.neg.factor <- data.frame(na.omit(hnscc.combined[hnscc.combined$HPV_Status.x=="Negative", factor]))
    names(hnscc.neg.factor)[1] = "Value"
    hnscc.neg.factor.summary <- c(paste("HNSC HPV(-) -", gsub("-"," ",factor), sep=" "), as.numeric(count(hnscc.neg.factor)$n), round(as.numeric(min(hnscc.neg.factor)), digits = 3), round(as.numeric(max(hnscc.neg.factor)), digits = 3), round(mean(unlist(hnscc.neg.factor)), digits = 3), round(as.numeric(summarize_all(hnscc.neg.factor, ~ quantile(.x, probs = 0.25))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.neg.factor, ~ quantile(.x, probs = 0.5))$Value), digits = 3), round(as.numeric(summarize_all(hnscc.neg.factor, ~ quantile(.x, probs = 0.75))$Value), digits = 3))
    table <- rbind(cesc.hpv.gene.summary, cesc.neg.gene.summary, cesc.hpv.factor.summary, cesc.neg.factor.summary, hnscc.hpv.gene.summary, hnscc.neg.gene.summary, hnscc.hpv.factor.summary, hnscc.neg.factor.summary)
    colnames(table) <- c("Dataset", "# of Patients", "Min", "Max", "Mean", "1st Quartile", "Median", "3rd Quartile")
    row.names(table) <- c(paste("CESC - HPV (+) -", gsub("-.*","",gene), sep=" "), paste("CESC - HPV (-) -", gsub("-.*","",gene), sep=" "), paste("CESC - HPV (+) -", gsub("-"," ",factor), sep=" "), paste("CESC - HPV (-) -", gsub("-"," ",factor), sep=" "), paste("HNSCC - HPV (+) - ", gsub("-.*","",gene), sep=" "), paste("HNSCC - HPV (+) - ", gsub("-.*","",gene), sep=" "), paste("HNSCC - HPV (+) - ", gsub("-"," ",factor), sep=" "), paste("HNSCC - HPV (+) - ", gsub("-"," ",factor), sep=" "))
    assign("g.table", table, envir = my.env)
    table
  }, options=list(scrollX=TRUE, bFilter=0, bPaginate=0, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:7))), rownames= FALSE)
  
  output$tableFactor <- renderDT({
    hide("cescdownloadScatterP")
    hide("cescdownloadBoxP")
    hide("hnsccdownloadScatterP")
    hide("hnsccdownloadBoxP")
    hide("cescdownloadScatterV")
    hide("cescdownloadBoxV")
    hide("hnsccdownloadScatterV")
    hide("hnsccdownloadBoxV")
    hide("downloadCSV")
    hide("downloadFactorCSV")
    hide("p1")
    hide("p2")
    hide("d")
    req(input$searchGene)
    show("downloadFactorCSV")
    gene <- input$searchGene
    cesc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/CESC-", gene, ".rds", sep=""))
    cesc.combined <- merge(cesc.mRNA, cesc.imm, by="TCGA-Call-Number")
    assign("g.cesc.combined", cesc.combined, envir = my.env)
    hnscc.mRNA <- readRDS(paste("../thincr-data/mrna-genes/HNSC-", gene, ".rds", sep=""))
    hnscc.combined <- merge(hnscc.mRNA, hnscc.imm, by="TCGA-Call-Number")
    assign("g.hnscc.combined", hnscc.combined, envir = my.env)
    listFactor = sort(gsub("\\.", "-", colnames(cesc.imm[c(6:8,10:33,38:ncol(cesc.imm))])))
    assign("g.gene", gene, envir = my.env)
    tablePR <- readRDS(paste("../thincr-data/imm-stats-mrna/", gene, ".rds", sep=""))
    pval.CP = c()
    pval.CN = c()
    pval.HP = c()
    pval.HN = c()
    for (i in 1:length(listFactor)){
      cesc.plot.data <- makePlotDataScatter(gene, listFactor[i], "CESC")
      hnsc.plot.data <- makePlotDataScatter(gene, listFactor[i], "HNSCC")
      percent1 <- 100*colSums(cesc.plot.data[1] == 0)/nrow(cesc.plot.data)
      percent2 <- 100*colSums(hnsc.plot.data[1] == 0)/nrow(hnsc.plot.data)
      truthy1=TRUE
      truthy2=TRUE
      if (percent1 > 50){truthy1=FALSE}
      if (percent2 > 50){truthy2=FALSE}
      q.CP <- as.numeric(tablePR[i,4])
      q.CN <- as.numeric(tablePR[i,7])
      q.HP <- as.numeric(tablePR[i,10])
      q.HN <- as.numeric(tablePR[i,13])
      if (is.na(q.CP) | q.CP > 0.1 | truthy1==FALSE){
        p.CP.text <- "NO"
      } else {
        p.CP.text <- "YES"
      } 
      if (is.na(q.CN) | q.CN > 0.1 | truthy1==FALSE){
        p.CN.text <- "NO"
      } else {
        p.CN.text <- "YES"
      } 
      if (is.na(q.HP) | q.HP > 0.1 | truthy2==FALSE){
        p.HP.text <- "NO"
      } else {
        p.HP.text <- "YES"
      } 
      if (is.na(q.HN) | q.HN > 0.1 | truthy2==FALSE){
        p.HN.text <- "NO"
      } else {
        p.HN.text <- "YES"
      }
      pval.CP = c(pval.CP, p.CP.text)
      pval.CN = c(pval.CN, p.CN.text)
      pval.HP = c(pval.HP, p.HP.text)
      pval.HN = c(pval.HN, p.HN.text)
    }
    tablePRN = data.frame(tablePR[1], tablePR[2], tablePR[3], tablePR[4], pval.CP, tablePR[5], tablePR[6], tablePR[7], pval.CN, tablePR[8], tablePR[9], tablePR[10], pval.HP, tablePR[11], tablePR[12], tablePR[13], pval.HN)
    colnames(tablePRN) <- c("Comparison Group", "CESC HPV(+) Spearman ρ", "CESC HPV(+) p-Val.", "CESC HPV(+) q-Val. (FDR=0.1)", "CESC HPV(+) Signif.", "CESC HPV(-) Spearman ρ", "CESC HPV(-) p-Val.", "CESC HPV(-) q-Val. (FDR=0.1)", "CESC HPV(-) Signif.", "HNSC HPV(+) Spearman ρ", "HNSC HPV(+) p-Val.","HNSC HPV(+) q-Val. (FDR=0.1)", "HNSC HPV(+) Signif.", "HNSC HPV(-) Spearman ρ", "HNSC HPV(-) p-Val.", "HNSC HPV(-) q-Val. (FDR=0.1)", "HNSC HPV(-) Signif.")
    assign("g.tableFactor", tablePRN, envir = my.env)
    datatable(tablePRN, options=list(scrollX=TRUE, bFilter=0, pageLength = 10, bLengthChange=0, bInfo=0, autoWidth = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:16))), rownames= FALSE) %>% formatSignif(columns=c(2,3,4,6,7,8,10,11,12,14,15,16), digits=3) %>%  formatStyle(columns = c('CESC HPV(+) Signif.', 'CESC HPV(-) Signif.', 'HNSC HPV(+) Signif.', 'HNSC HPV(-) Signif.'), color = styleEqual(c('NO', 'YES'), c('#000000', '#e61212')))
  })
  
  drawPlotScatter <- function(plot.data, gene, factor, titles) {
    my.plot <- ggplot(data = plot.data, aes(x=ValueFactor, y=ValueGene, color=Status)) + 
      geom_point() + 
      xlab(paste(gsub("-"," ",factor), sep="")) +
      ylab(paste("mRNA expression levels of", gsub("-.*","",gene), sep=" ")) +
      ggtitle(titles) +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_color_manual(labels = c("HPV(+)", "HPV(-)"), values = c("red", "blue")) +
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
      stat_cor(method = "spearman") +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=10, color='black'), axis.title.x = element_text(size=10, color='black'))
    return(my.plot)
  }
  
  drawPlotBox <- function(transform.data, factor, titles) {
    my.plot <- ggplot(transform.data,aes(x=Status,ymin=One-1.5*(Median-One),lower=One,middle=Median,upper=Three,ymax=Three+1.5*(Three-Median),fill=Status))+ 
      geom_errorbar(position=position_dodge(0.9), width=0.2) +
      geom_boxplot(stat="identity") +
      guides(fill="none") +
      scale_y_continuous(breaks=pretty_breaks(n = 10)) +
      scale_fill_brewer(palette="BuPu") + 
      scale_x_discrete(labels=c("HPV Positive","HPV Negative"), name="HPV Status") +
      ylab(paste("Measured levels of", gsub("-"," ",factor), sep=" ")) +
      ggtitle(titles) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=10, color='black'), axis.text.x = element_text(size=10, color='black'), axis.title.y = element_text(size=12, color='black'), axis.title.x = element_text(size=12, color='black'))
    return(my.plot)
  }
  
  output$scatterPlotCESC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    show("cescdownloadScatterP")
    show("cescdownloadBoxP")
    show("hnsccdownloadScatterP")
    show("hnsccdownloadBoxP")
    show("cescdownloadScatterV")
    show("cescdownloadBoxV")
    show("hnsccdownloadScatterV")
    show("hnsccdownloadBoxV")
    show("downloadCSV")
    show("p1")
    show("p2")
    show("d")
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataScatter(gene, factor, "CESC")
    cesc.plotScatter <- drawPlotScatter(plot.data, gene, factor, "CESC Dataset - Immune Function Correlations")
    assign("g.cesc.plotScatter", cesc.plotScatter, envir = my.env)
    cesc.plotScatter
  })
  
  output$boxPlotCESC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataBox(gene, factor, "CESC")
    pos=unlist(unname(plot.data[plot.data[2]=="HPV",][1]))
    neg=unlist(unname(plot.data[plot.data[2]=="Negative",][1]))
    transform.pos=data.frame(signif(max(pos, na.rm=TRUE),3), signif(min(pos, na.rm=TRUE),3), signif(median(pos, na.rm=TRUE),3), quantile(pos, c(0.25,0.75), na.rm=TRUE)[1], quantile(pos, c(0.25,0.75), na.rm=TRUE)[2], "Positive")
    colnames(transform.pos)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.neg=data.frame(signif(max(neg, na.rm=TRUE),3), signif(min(neg, na.rm=TRUE),3), signif(median(neg, na.rm=TRUE),3), quantile(neg, c(0.25,0.75), na.rm=TRUE)[1], quantile(neg, c(0.25,0.75), na.rm=TRUE)[2], "Negative")
    colnames(transform.neg)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.data=rbind(transform.pos, transform.neg)
    transform.data$Status <- factor(transform.data$Status , levels=c("Positive", "Negative"))
    cesc.plotBox <- drawPlotBox(transform.data, factor, "CESC Dataset - Immune Function")
    assign("g.cesc.plotBox", cesc.plotBox, envir = my.env)
    cesc.plotBox
  })
  
  output$scatterPlotHNSCC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataScatter(gene, factor, "HNSCC")
    hnscc.plotScatter <- drawPlotScatter(plot.data, gene, factor, "HNSC Dataset - Immune Function Correlations")
    assign("g.hnscc.plotScatter", hnscc.plotScatter, envir = my.env)
    hnscc.plotScatter
  })
  
  output$boxPlotHNSCC <- renderPlot({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataBox(gene, factor, "HNSCC")
    pos=unlist(unname(plot.data[plot.data[2]=="HPV",][1]))
    neg=unlist(unname(plot.data[plot.data[2]=="Negative",][1]))
    transform.pos=data.frame(signif(max(pos, na.rm=TRUE),3), signif(min(pos, na.rm=TRUE),3), signif(median(pos, na.rm=TRUE),3), quantile(pos, c(0.25,0.75), na.rm=TRUE)[1], quantile(pos, c(0.25,0.75), na.rm=TRUE)[2], "Positive")
    colnames(transform.pos)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.neg=data.frame(signif(max(neg, na.rm=TRUE),3), signif(min(neg, na.rm=TRUE),3), signif(median(neg, na.rm=TRUE),3), quantile(neg, c(0.25,0.75), na.rm=TRUE)[1], quantile(neg, c(0.25,0.75), na.rm=TRUE)[2], "Negative")
    colnames(transform.neg)=c("Max", "Min", "Median", "One", "Three", "Status")
    transform.data=rbind(transform.pos, transform.neg)
    transform.data$Status <- factor(transform.data$Status , levels=c("Positive", "Negative"))
    hnscc.plotBox <- drawPlotBox(transform.data, factor, "HNSC Dataset - Immune Function")
    assign("g.hnscc.plotBox", hnscc.plotBox, envir = my.env)
    hnscc.plotBox
  })
  
  makePlotDataScatter <- function(gene, factor, mode) {
    print(factor)
    print(gene)
    if (mode == "CESC"){
      cesc.combined <- my.env$g.cesc.combined
      hpv <- data.frame(cesc.combined[cesc.combined["HPV_Status.x"] == "HPV16" | cesc.combined["HPV_Status.x"] == "HPV33" | cesc.combined["HPV_Status.x"] == "HPV35", c(gene,factor)])
      neg <- data.frame(cesc.combined[cesc.combined$HPV_Status.x =="Negative", c(gene,factor)])
    } else if (mode == "HNSCC") {
      hnscc.combined <- my.env$g.hnscc.combined
      hpv <- data.frame(hnscc.combined[hnscc.combined["HPV_Status.x"] == "HPV16" | hnscc.combined["HPV_Status.x"] == "HPV33" | hnscc.combined["HPV_Status.x"] == "HPV35", c(gene,factor)])
      neg <- data.frame(hnscc.combined[hnscc.combined$HPV_Status.x=="Negative", c(gene,factor)])
    }
    hpv['Status'] = "HPV"
    names(hpv)[1] = "ValueGene"
    names(hpv)[2] = "ValueFactor"
    neg['Status'] = "Negative"
    names(neg)[1] = "ValueGene"
    names(neg)[2] = "ValueFactor"
    plot.data <- rbind(hpv, neg)
    return(plot.data)
  } 
  
  makePlotDataBox <- function(gene, factor, mode) {
    if (mode == "CESC"){
      cesc.combined <- my.env$g.cesc.combined
      hpv <- data.frame(cesc.combined[cesc.combined["HPV_Status.x"] == "HPV16" | cesc.combined["HPV_Status.x"] == "HPV33" | cesc.combined["HPV_Status.x"] == "HPV35", factor])
      neg <- data.frame(cesc.combined[cesc.combined$HPV_Status.x =="Negative", factor])
    } else if (mode == "HNSCC") {
      hnscc.combined <- my.env$g.hnscc.combined
      hpv <- data.frame(hnscc.combined[hnscc.combined["HPV_Status.x"] == "HPV16" | hnscc.combined["HPV_Status.x"] == "HPV33" | hnscc.combined["HPV_Status.x"] == "HPV35", factor])
      neg <- data.frame(hnscc.combined[hnscc.combined$HPV_Status.x=="Negative", factor])
    }
    hpv['Status'] = "HPV"
    names(hpv)[1] = "ValueFactor"
    neg['Status'] = "Negative"
    names(neg)[1] = "ValueFactor"
    plot.data <- rbind(hpv, neg)
    return(plot.data)
  } 
  
  selectPvalBox <- function(p.values, num, text,truthy) {
    if (is.na(p.values[num]) & truthy == FALSE | p.values[num] > 0.05) {
      rgbStr <- sprintf('rgb(%d,%d,%d)',0,0,0)
      div(HTML(sprintf("<text style='color:%s'> %s: %s</text>", rgbStr, text, signif(p.values[num], digits=3))))
    } else if (p.values[num] <= 0.0001 & truthy){
      rgbStr <- sprintf('rgb(%d,%d,%d)',230, 18, 18)
      div(HTML(sprintf("<text style='color:%s'> %s: %s</text>", rgbStr, text, signif(p.values[num], digits=3))))
    } else if (p.values[num] <= 0.001) {
      rgbStr <- sprintf('rgb(%d,%d,%d)',148, 27, 27)
      div(HTML(sprintf("<text style='color:%s'> %s: %s</text>", rgbStr, text, signif(p.values[num], digits=3))))
    } else if (p.values[num] <= 0.01) {
      rgbStr <- sprintf('rgb(%d,%d,%d)',115, 26, 26)
      div(HTML(sprintf("<text style='color:%s'> %s: %s</text>", rgbStr, text, signif(p.values[num], digits=3))))
    } else if (p.values[num] <= 0.05) {
      rgbStr <- sprintf('rgb(%d,%d,%d)',87, 28, 28)
      div(HTML(sprintf("<text style='color:%s'> %s: %s</text>", rgbStr, text, signif(p.values[num], digits=3))))
    } 
  }
  
  output$textCESC1B <- renderUI({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataBox(gene, factor, "CESC")
    sub1 <- data.frame(na.omit(plot.data[plot.data["Status"] == "HPV", "ValueFactor"]))
    sub2 <- data.frame(na.omit(plot.data[plot.data["Status"] == "Negative", "ValueFactor"]))
    percent1 <- 100*colSums(sub1[1] == 0)/nrow(sub1)
    percent2 <- 100*colSums(sub2[1] == 0)/nrow(sub2)
    truthy = TRUE
    if (percent1 > 50 | percent2 > 50){truthy=FALSE}
    p.values <- compare_means(ValueFactor ~ Status,  data = plot.data)$p.adj
    selectPvalBox(p.values, 1, "HPV Positive vs. HPV Negative", truthy)
  })
  
  output$textHNSCC1B <- renderUI({
    req(input$searchGene, input$searchFactor)
    gene <- input$searchGene
    factor <- gsub(" ","-",input$searchFactor)
    plot.data <- makePlotDataBox(gene, factor, "HNSCC")
    sub1 <- data.frame(na.omit(plot.data[plot.data["Status"] == "HPV", "ValueFactor"]))
    sub2 <- data.frame(na.omit(plot.data[plot.data["Status"] == "Negative", "ValueFactor"]))
    percent1 <- 100*colSums(sub1[1] == 0)/nrow(sub1)
    percent2 <- 100*colSums(sub2[1] == 0)/nrow(sub2)
    truthy = TRUE
    if (percent1 > 50 | percent2 > 50){truthy=FALSE}
    p.values <- compare_means(ValueFactor ~ Status,  data = plot.data)$p.adj
    selectPvalBox(p.values, 1, "HPV Positive vs. HPV Negative", truthy)
  })
  
  output$description <- renderText({
    req(input$searchFactor)
    show("d")
    factor <- input$searchFactor
    HTML(unlist(descript[descript[1]==factor,][2]))
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)