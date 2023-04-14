suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))
library(shiny)
library(shinyWidgets)
library(shinyjs)
suppressPackageStartupMessages(library(DT))
library(rclipboard)


ui <- fluidPage(
  rclipboardSetup(),
  shinyjs::useShinyjs(),
  titlePanel('MultiplePrimerInput'),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  tabPanel('Generic', br(),
                           fluidRow(column(12,HTML('<strong><big>Options</strong></big>'))),HTML('<hr style="margin-top: 2px;"/>'),
                           fluidRow(column(12,
                                           HTML('<strong>Delimiter between name and sequence for the output text:</strong>'),
                                           textInput('delim_out', NULL, width = '20%', value = '\\t')
                           ))),
                  tabPanel('Merck', br(),
                           fluidRow(column(12,HTML('<strong><big>Options</strong></big>'))),HTML('<hr style="margin-top: 2px;"/>'),
                           fluidRow(column(12,
                                           sliderTextInput(inputId = 'scale',label =  'Scale', grid = TRUE, choices = c(0.025, 0.05, 0.2, 1, 10, 15), selected = 0.025, hide_min_max = TRUE, force_edges = TRUE),
                                           sliderTextInput(inputId = 'purification',label =  'Purification', grid = TRUE, choices = c("Desalt", "Cartridge", "HPLC", "PAGE"), selected = "Desalt", hide_min_max = TRUE, force_edges = TRUE),
                                           sliderTextInput(inputId = 'format',label =  'Format', grid = TRUE, choices = c("Dry", "In Solution (Water)", "In Solution (TE)", "In Solution (TRIS)"), selected = "Dry", hide_min_max = TRUE, force_edges = TRUE),
                                           shinyjs::disabled(sliderTextInput(inputId = 'conc', grid = TRUE,label =  'Concentration', choices = c(20, 50, 100, 200, 500), selected = 100, hide_min_max = TRUE, force_edges = TRUE))
                           ))),
                  tabPanel('IDT',  br(),
                           fluidRow(column(12,HTML('<strong><big>Options</strong></big>'))),HTML('<hr style="margin-top: 2px;"/>'),
                           fluidRow(column(12,
                                           sliderTextInput(inputId = 'scaleIDT',label =  'Scale', grid = TRUE, choices = c("25nm","100nm","250nm","1um","2um","5um","10um","4nmU","20nmU","PU","25nmS"), selected = "25nm", hide_min_max = TRUE, force_edges = TRUE),
                                           sliderTextInput(inputId = 'purificationIDT',label =  'Purification', grid = TRUE, choices = c("STD","PAGE","HPLC","IEHPLC","RNASE","DUALHPLC","PAGEHPLC"), selected = "STD", hide_min_max = TRUE, force_edges = TRUE),
                           )))),
      htmlOutput('delim_in_text'),
      textInput('delim', NULL, width = '20%', value = '\\t')
      ),
    mainPanel(
      fluidRow(
        column(12,
               textAreaInput("seqs", NULL, width = '100%', height= '46vh', placeholder = 'Enter name + primer sequences'),
               hidden(div(id = 'hidehr', 
                          uiOutput('clip'),
                          HTML('<hr style="margin-bottom: 0px; border-color: black;"/>'))),
               DTOutput('out'),br()
        )
      )
    )
  )
)