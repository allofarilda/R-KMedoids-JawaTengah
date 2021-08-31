library(shiny)
library(shinydashboard)
library(markdown)


shinyUI(
  navbarPage("GUI",
          tabPanel("Dataset",
          sidebarLayout(
          sidebarPanel(
                                        
        #Input: Select a file ---
        fileInput("file1", "Choose Choose File",
                  multiple = FALSE,
                  accept = c("text/xlsx",
                             "text/comma-separated-values.text/plain",
                             ".csv")),
        
        #Horizontal line ---
        tags$hr(),
        
        #Input: Checkbox if file has header ---
        checkboxInput("header", "Header", TRUE),
        
        #Input: Select separator ---
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        #Input: Select quotes ---
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        #Horizontal line ---
        tags$hr(),
        
        #Input: Select number of rows to display ---
        radioButtons("disp","Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head")
                                      ),
        mainPanel(
          tableOutput("contents")
        )
                                    )
                           ),
        tabPanel("Summary",
                 verbatimTextOutput("summ")
        ),
        tabPanel("Standarisasi Data",
                 verbatimTextOutput("standarisasi")
        ),
        tabPanel("Nilai Korelasi",
                 verbatimTextOutput("korelasi")
        ),
        tabPanel("Hasil PCA",
                 verbatimTextOutput("pca")
        ),
        tabPanel("Ukuran Kemiripan",
                 verbatimTextOutput("euclidian")
        ),
        tabPanel("Hasil Clustering K-Medoids",
                 mainPanel(
                   tableOutput("cluster")
                 )),
        tabPanel("Plot Cluster",
                 mainPanel(
                   plotOutput("plot1")
                 )),
        tabPanel("Nilai Rata-Rata Cluster",
                 mainPanel(
                   tableOutput("interpretasi")
                 )),
        tabPanel("Validasi Cluster",
                 mainPanel(
                   plotOutput("plot2")
                )),
        tabPanel("Pemetaan",
                 mainPanel(
                   plotOutput("plot3")
                 ))

                 ))