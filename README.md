# R Shiny App for exporation and download of SARS-CoV2 COMRADES data

You can now explore the structure of SARS-CoV-2 and the interactions with the host RNA in Vero cells!

This is an ongoing development and will be updated soon. More things to come. 

## Required R Libraries

```
library(shiny)
library(plotly)
library(RColorBrewer)
library(DT)
library(RRNA)
library(shinydashboard)
library(shinythemes)
library(reshape2)
library(ggplot2)
library(stringr)
```

## Run the app from R with:
```
library(shiny)

shiny::runGitHub('SARS-CoV2-COMRADES-APP','JLP-BioInf')
```
