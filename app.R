library(shiny)
#library(data.table)


# # setwd("C:/Users/John L/Desktop/hackathon/scExprViz")
# #cellExpr = fread("human_sub.csv")
# # save(cellExpr, file="cellExpr.RData")
# # load(file="cellExpr.RData")
# clusterID = as.numeric(read.csv("kmeans_10clusters_ADTclr_filtered.csv", header=FALSE))
# subsetcolNum = ncol(cellExpr) #subsetcolNum = 5000 # samples
# subsetrowNum = nrow(cellExpr) #subsetrowNum = 20 # rows
# geneNames = cellExpr[,V1]
# geneNames = geneNames[1:subsetrowNum]
# # cellExpr = cellExpr[1:10,1:10]
# row.names(cellExpr) = cellExpr[,V1]
# # cellExpr = cellExpr[1:10,2:10]
# cellExpr = cellExpr[1:subsetrowNum, 2:subsetcolNum]
# cellExpr = as.data.frame(t(cellExpr))
# colnames(cellExpr) <- geneNames
# clusterID = clusterID[1:(subsetcolNum-1)]#c(rep(1,5), 2:(subsetrowNum-5))#clusterID = 1:(subsetcolNum-1)
# cellExpr=cbind(cellExpr, clusterID)
# save(cellExpr, file="cellExpr.RData")
load(file = "cellExpr.RData")


server <- function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        cellExpr[cellExpr$clusterID == as.numeric(input$clustNum)-1, input$xcol] # requires get() because of data.table syntax
        #cellExpr$clusterID == as.numeric(input$clustNum)
    })
    
    
    output$plot1 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        hist(selectedData(), breaks=seq(min(selectedData())-0.5, max(selectedData())+0.5, by=1), main= "",  
             xlab = "Transcript Count", ylab="Number of Cells",
             col="blue", border="black") # 
        
    })
    
}

ui <- fluidPage(
pageWithSidebar(
    titlePanel("Visualizing Cluster-Specific Gene Expression"),
    #headerPanel('Gene Expression'),
    sidebarPanel(
        p("Pick a gene transcript and cluster to see the distribution of cluster-specific gene expression."),
        selectInput('xcol', 'Transcript of Interest', colnames(cellExpr),
                    selected=colnames(cellExpr)[[6]], selectize=FALSE,size=3),
        selectInput('clustNum', 'Cluster ID Number', as.character(sort(unique(cellExpr$clusterID))+1),
                    selected="1")
        # numericInput('clusters', 'Cluster count', 3,
                     # min = 1, max = 9)
        
        
    ),
    mainPanel(
        plotOutput('plot1'),
        img(src = "tsneFig.png", height = 200, width = 400),
        p("The above figure shows a tSNE plot made from single cell gene expression data for around 8000 cells (human cord blood mononuclear cells). Clusters were assigned by kmeans clustering of surface marker values for those same cells."),
        p("This interactive figure was made as part of a two day hackathon at the University of Virginia by members of the Biomedical Data Sciences Training Grant. Data was drawn from the following study that generated single cell RNA and surface marker data: https://www.nature.com/nmeth/journal/v14/n9/full/nmeth.4380.html.")
        
    )
)
)

shinyApp(ui = ui, server = server)

# Code initially based on Rshiny Kmeans example
# License: MIT
# Author: Joe Cheng <joe@rstudio.com>
#     AuthorUrl: http://www.rstudio.com/
#     Tags: getting-started kmeans plotoutput sliderinput numericinput reactivity
# DisplayMode: Showcase
# Type: Shiny