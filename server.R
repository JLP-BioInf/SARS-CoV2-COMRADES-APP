library(shiny)
library(plotly)
library(RColorBrewer)
library(DT)
library(RRNA)
library(shinydashboard)
library(shinythemes)


# cache computation of the correlation matrix
load("./data/dataForShiny2.RData")
load("./data/clusterPositionsWithStructures.RData")
myCol = c("#000000","#000000","#000000","#000000",colorRampPalette(brewer.pal(8,"YlOrRd"))(40))



linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- navbarPage(theme = shinytheme("superhero"),
  title = "SARS-CoV2 COMRADES",
                 tabPanel("SARS2 MERS Compairson",
                          h4("This Shiny app allows you to explore the results from the publication: The short- and long-range RNA-RNA Interactome of SARS-CoV-2"),
                          uiOutput("moCell"),
                          h4("https://doi.org/10.1016/j.molcel.2020.11.004"),
                          h4("This tab allows you to explore and download the clusters of SARS-CoV-2 and MERS structural data."),
                          h1("Contact maps of SARS-CoV2 and MERS"),
                          h4("The heatmaps show the clusters of duplex reads for SARS-CoV-2 and MERS. They are interactive, by zooming in one one of the heatmaps the other will zoom to the same co-ordinates. Double click to zoom out again.  "),
                          
    
                          fluidRow(
                            splitLayout(cellWidths = "550px", 
                            plotlyOutput("heatSample",width = "500px", height = "450px"),#)),
                            #h1("Contact maps o f MERS"),
                            plotlyOutput("heatControl",width = "500px", height = "450px") )),
                          linebreaks(5),
                          hr(),
                          hr(),
                          h1("SARS-Cov-2 Clusters Shown in Contact Heatmap Above:"),
                          dataTableOutput("clusterTable"),
                          linebreaks(5),
                          hr(),
                          hr(),
                          h1("Sequence from cluster, select row from above table"),
                          htmlOutput("Sequences"),
                          linebreaks(5),
                          hr(),
                          hr(),
                          plotOutput("structure2", width = "600px", height = "400px")
                          
                          
                 ),#end of tab1
                 tabPanel("Data Download",
                          h1("All raw reads can be downloaded from the SRA:"),
                          uiOutput("sraURL"),
          
                          h1("Processed duplexes can be downloaded from GEO: "),
                          uiOutput("geoURL"),
             
                          
                          ), #end of tab 2
                tabPanel("SARS vs Host RNA",
                         h1("This tab allows the exploration and download of data for the interaction of SARS-CoV-2 and the host RNA in Vero Cells"),
                         h3("Table below shows each hostRNA that duplexes were obtained for clicking on a row will produce plots and tables below for that RNA"),
                         dataTableOutput("geneTable"),
                         plotOutput("hostRNAPlot"),
                         h3("Below is a table containing the raw data for the RNA that was selected in the first table"),
                         dataTableOutput("rawHostTable"),
                         h3("Below is plot showing where on the SARS-CoV-2 genome this RNA binds"),
                         plotlyOutput("hostBar"),
                         h3("Below is plot showing which part of the selected RNA binds to the SARS-CoV-2 genome"),
                         plotlyOutput("hostBar2"),
           
           
                )
                 
                 
                 
)
