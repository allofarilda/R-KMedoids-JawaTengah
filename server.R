# Define server logic to read selected file ----
server <- function(input, output,session) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$summ<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    summary(df[,c(-1)])
  })
  
  output$standarisasi<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
      )
    
    scale(df[,c(-1)])
  })
  
  output$korelasi<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    cor(scale(df[,c(-1)]))
  })

  output$standarisasi<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    scale(df[,c(-1)])
  })
  

  output$pca<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    library(factoextra)
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    new_Data
  })  
  
  output$euclidian<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    library(cluster)
    library(fpc)
    options(max.print = 10000)
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    pam.result <- pam(new_Data,2)
    pam.result$diss
  }) 

  output$cluster<-renderTable({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    pam.result <- pam(new_Data, 2)
    Kabupaten.Kota <- df$KabupatenKota
    Cluster <- pam.result$cluster
    df.cluster = data.frame(Kabupaten.Kota, Cluster)
    df.cluster
  })  
  
  output$plot1<-renderPlot({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    pam.result <- pam(new_Data,2)
    fviz_cluster(pam.result)
  })  
  
  output$interpretasi<-renderTable({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    library(tidyverse)
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    df[,c(-1)] %>%
      mutate(Cluster = pam(new_Data,2)$cluster) %>%
      group_by(Cluster) %>%
      summarise_all("mean")
  })
  
  output$plot2<-renderPlot({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    distance <- dist(new_Data)
    plot(silhouette(pam(new_Data, 2)$cluster, dist(new_Data)))
  })
  
  output$plot3<-renderPlot({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    library(rgdal)
    library(raster)
    library(prettymapr)
    new_Data <- prcomp(scale(df[,c(-1)]), scale. = TRUE)$x[,1:3]
    Cluster <- pam(new_Data,2)$cluster
    jawa <- readOGR(dsn ='C:/data/Map of Jawa (original)', layer='jawa')
    head(jawa@data)
    data.jawatengah <- subset(jawa, jawa$NAMA_PROP=="Jawa Tengah")
    data.jawatengah@data
    datkabjateng <- data.frame(data.jawatengah$NAMA_KAB, Cluster)
    plot(data.jawatengah, col = c("mintcream","lightblue")[datkabjateng$Cluster], axes = TRUE, cex = 0.25,border = "black")
    text(data.jawatengah, data.jawatengah$NAMA_KAB, cex = 0.5)
    legend("bottomright", legend = c("Cluster 1", "Cluster 2"), col = c("mintcream","lightblue"),
           inset=.02, fill= c("mintcream","lightblue"), cex = 0.7, bty = "n")
    addnortharrow(pos = "topright",scale = 0.5,padin=c(0.55,0.15))
  })
}
