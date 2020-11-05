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
                          h4("https://www.cell.com/molecular-cell/fulltext/S1097-2765(20)30782-6"),
                          h4("https://doi.org/10.1016/j.molcel.2020.11.004"),
                          h1("Contact maps of SARS-CoV2 and MERS"),
                          h4("The heatmaps show the clusters of duplex reads for SARS-CoV-2 and MERS. They are interactive, by zooming in one one of the heatmaps the other will zoom to the same co-ordinates. Double click to zoom out again.  "),
                          
    
                          fluidRow(
                            splitLayout(cellWidths = "550px", 
                            plotlyOutput("heatSample",width = "500px", height = "450px"),#)),
                            #h1("Contact maps of MERS"),
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
                         # h1("Download all Chimeras"),
                          #dataTableOutput("rawTable"),   
                          )
                 
                 
                 
)
