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
                          h1("Contact maps of SARS-CoV2                                  Contact maps of MERS"),
    
                          fluidRow(
                            splitLayout(cellWidths = "550px", 
                            plotlyOutput("heatSample",width = "500px", height = "450px"),#)),
                            #h1("Contact maps of MERS"),
                            plotlyOutput("heatControl",width = "500px", height = "450px") )),
                          linebreaks(5),
                          hr(),
                          hr(),
                          h1("Clusters Shown in Contact Heatmaps Above:"),
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
                 )
                 
                 
                 
)
