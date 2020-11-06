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
server <- function(input, output, session) {
  
  
  
  ######################################################
  # Heatmaps - sorting out the zoom of both
  ######################################################
  output$heatSample <- renderPlotly({
    plot_ly(source = "heat_plot") %>%
      add_heatmap(x = row.names( combinedMatListScaleNoReps[["sars"]][["sample"]]), 
                  y = colnames( combinedMatListScaleNoReps[["sars"]][["sample"]]),
                  z = log2( combinedMatListScaleNoReps[["sars"]][["sample"]]+1),
                  colors = myCol)
  })
  
  output$heatControl <- renderPlotly({
    
    # if there is no click data, render the normal plot
    clickData <- event_data("plotly_relayout", source = "heat_plot")
    clickDataOld = clickData
    
    if (is.null(clickData)){
      clickData = list()
      clickData$`xaxis.range[0]` = 0
      clickData$`xaxis.range[1]` = 1000
      clickData$`yaxis.range[0]` = 0
      clickData$`yaxis.range[1]` = 1000
      # return(NULL)
    }else{
      # must change the coordinates to indexes
      factor = 1000 /29883
      clickData$`xaxis.range[0]` = round(clickData$`xaxis.range[0]` * factor)
      clickData$`xaxis.range[1]` = round(clickData$`xaxis.range[1]` * factor)
      clickData$`yaxis.range[0]` = round(clickData$`yaxis.range[0]` * factor)
      clickData$`yaxis.range[1]` = round(clickData$`yaxis.range[1]` * factor)
    }
    print(clickDataOld)
    print( clickData)
    control = combinedMatListScaleNoReps[["mers"]][["sample"]][clickData$`yaxis.range[0]`:clickData$`yaxis.range[1]`,
                                                               clickData$`xaxis.range[0]`: clickData$`xaxis.range[1]` ]
    # scatterplot with fitted line
    plot_ly() %>%
      add_heatmap(x = row.names(control),
                  y = colnames(control),
                  z = log2(control+1),
                  colors = myCol)
  })
  
  ######################################################
  # END - Heatmaps
  ######################################################
  
  
  ######################################################
  # Render Data Table 
  ######################################################
  output$clusterTable <- renderDataTable({
    
    #get zoom parameters
    clickData <- event_data("plotly_relayout", source = "heat_plot")
    clickDataOld = clickData
    if (is.null(clickData)){
      clickData = list()
      clickData$`xaxis.range[0]` = 0
      clickData$`xaxis.range[1]` = 30000
      clickData$`yaxis.range[0]` = 0
      clickData$`yaxis.range[1]` = 30000
    }
    # return(NULL)
    t = clusterPositionsListTrimmed[["sars"]][["sample"]]
                        
    
    row.names(t) = NULL
    
    t = t[t$ls >= clickData$`xaxis.range[0]` & t$le <= clickData$`xaxis.range[1]` & t$rs >= clickData$`yaxis.range[0]` & t$re <= clickData$`yaxis.range[1]`,]  
    
    t[order(t$size.x, decreasing = T),]
    
  },
  
  extensions = 'Buttons',
  
  options = list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'tB',
    buttons = c('copy', 'csv', 'excel'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'color': '#fff'});",
      "}")
  ),
  
  class = "display")
  
  ######################################################
  # END - data table 
  ######################################################

  
  
  
  
  
  ######################################################
  # Sequence
  ######################################################
  
  
  output$Sequences <- renderUI({
    clickData <- event_data("plotly_relayout", source = "heat_plot")
    clickDataOld = clickData
    if (is.null(clickData)){
      clickData = list()
      clickData$`xaxis.range[0]` = 0
      clickData$`xaxis.range[1]` = 30000
      clickData$`yaxis.range[0]` = 0
      clickData$`yaxis.range[1]` = 30000
    }
    # return(NULL)
    t = clusterPositionsListTrimmed[["sars"]][["sample"]]

    row.names(t) = NULL
    t = t[t$ls >= clickData$`xaxis.range[0]` &
            t$le <= clickData$`xaxis.range[1]` &
            t$rs >= clickData$`yaxis.range[0]` &
            t$re <= clickData$`yaxis.range[1]`,]  
    t = t[order(t$size.x, decreasing = T),]
    
    index = input$clusterTable_row_last_clicked
    index = t[index,]
    print(index$ls)
    #print(index)
    #now get the sequence that relates to the chosen cluster
    seq = rnaRefs[["sars"]]
    seq1 = paste(seq[[1]][index$ls:index$le], collapse = "")
    name1 = paste(">",names(seq),"-",index$id,"-",index$ls,"-",index$le,sep="")
    seq2 = paste(seq[[1]][index$rs:index$re], collapse = "")
    name2 = paste(">",names(seq),"-",index$id,"-",index$rs,"-",index$re,sep="")
    seq3 = paste(seq[[1]][index$ls:index$re], collapse = "")
    name3 = paste(">",names(seq),"-",index$id,"-",index$ls,"-",index$re,"-","FULLSEQ",sep="")
    
    HTML(paste(name1, toupper(seq1),name2,toupper(seq2),name3,toupper(seq3), sep = '<br/>'))
    
  })
  
  
  output$structure <- renderImage({
    clickData <- event_data("plotly_relayout", source = "heat_plot")
    clickDataOld = clickData
    if (is.null(clickData)){
      clickData = list()
      clickData$`xaxis.range[0]` = 0
      clickData$`xaxis.range[1]` = 30000
      clickData$`yaxis.range[0]` = 0
      clickData$`yaxis.range[1]` = 30000
    }
    # return(NULL)
    t = clusterPositionsListTrimmedSarsCombinedWithStructures
    
    row.names(t) = NULL
    t = t[t$ls >= clickData$`xaxis.range[0]` &
            t$le <= clickData$`xaxis.range[1]` &
            t$rs >= clickData$`yaxis.range[0]` &
            t$re <= clickData$`yaxis.range[1]`,]  
    t = t[order(t$size.x, decreasing = T),]
    
    index = input$clusterTable_row_last_clicked
    index = t[index,]
    print(index$ls)
    
    outfile <- tempfile(fileext = '.vienna')
    writeLines(c(">x",paste(index[,"seq1new"],index[,"seq2new"], sep ="  "),
                 sub("&","  ",index[,"vienna"])), "./programs/vienna.vienna")
    
    print(c(">x",paste(index[,"seq1new"],index[,"seq2new"], sep =""),
            sub("&","",index[,"vienna"])))
    annotString = paste("-annotations ",'"',(index[,"ls"]+9),":anchor=",10,';',index[,"re"],":anchor=",nchar(sub("&","  ",index[,"vienna"])),'"', sep = "")
    command = paste("java -cp ./programs/VARNAv3-93.jar fr.orsay.lri.varna.applications.VARNAcmd -i ./programs/vienna.vienna -o ./programs/output.svg", annotString)
    print(command)
    x = system(command,intern = T)
    
    list(src = "./programs/output.svg",
         contentType = "image/svg+xml",
         height = 1600,
         width = 1200)
    
  }, deleteFile = F)
  
  output$structure2 <- renderPlot({
    clickData <- event_data("plotly_relayout", source = "heat_plot")
    clickDataOld = clickData
    if (is.null(clickData)){
      clickData = list()
      clickData$`xaxis.range[0]` = 0
      clickData$`xaxis.range[1]` = 30000
      clickData$`yaxis.range[0]` = 0
      clickData$`yaxis.range[1]` = 30000
    }
    # return(NULL)
    t = clusterPositionsListTrimmedSarsCombinedWithStructures
    
    row.names(t) = NULL
    t = t[t$ls >= clickData$`xaxis.range[0]` &
            t$le <= clickData$`xaxis.range[1]` &
            t$rs >= clickData$`yaxis.range[0]` &
            t$re <= clickData$`yaxis.range[1]`,]  
    t = t[order(t$size.x, decreasing = T),]
    
    index = input$clusterTable_row_last_clicked
    index = t[index,]
    print(index$ls)
    

    
    ct=makeCt( seq = paste(index[,"seq1new"],index[,"seq2new"], sep =""),
              struct = sub("&","",index[,"vienna"])
    )
    
    
    dat=ct2coord(ct)
    RNAPlot(dat,nt = T,tsize = 0.6)

    
  })

  
  output$moCell <- renderUI({
    mocellURL = a("Publication Link (Molecular Cell)", href = "https://www.cell.com/molecular-cell/fulltext/S1097-2765(20)30782-6")
    tagList(mocellURL)
  })
  
  output$geoURL <- renderUI({
    geoURL = a("Processed Duplexes (GEO)", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE154662")
    tagList(geoURL)
  })
  

  output$sraURL <- renderUI({
    sraURL = a("Raw Reads (SRA)", href = "https://www.ncbi.nlm.nih.gov/sra?term=SRP272408")
    tagList(sraURL)
  })
  
  
}

#shinyApp(ui = ui, server = server)

