library(shiny)
library(shinyjs)
library(DT)
library(FactoMineR)
library(factoextra)
library(ggbiplot)
library(plotly)
library(htmltools)
library(shinythemes)
library(summarytools)
library(ggcorrplot)
library(stringr)
library(ggvis)






ui <- shinyUI(
  tagList(
    useShinyjs(),
    #tags$nav("navbar navbar-expand-lg navbar-dark bg-dark"),
    navbarPage(
      theme = bslib::bs_theme(version = 5, bootswatch = "quartz"),
      inverse = TRUE,
      "楊皓丞的 PCA 作業",
      tabPanel(
        "PCA",
        tabsetPanel(
          tabPanel(
            "PCA Plot",
            br(),
            column(
              11,
              fluidRow(
                tags$h2("PCA"),
              ),
              br(),
              fluidRow(
                column(
                  3,
                  fluidRow(
                    tags$h4("x-axis"),
                  ),
                  fluidRow(),
                  fluidRow(
                    uiOutput("x_PC_choice"),
                  ),
                  br(),
                  fluidRow(
                    tags$h4("y-axis"),
                  ),
                  fluidRow(),
                  fluidRow(
                    uiOutput("y_PC_choice"),
                  ),          
                ),
                column(
                  9,
                  plotlyOutput(outputId = "pcaPlot")
                )
              )
            )
          ),
          tabPanel("result data",
            fluidRow(
              column(3,
                tags$h2("result data")
              ),
              column(9,
                DT::dataTableOutput("result_data")
              )
            )
          ),
          tabPanel("input data (log)",
            fluidRow(
              column(3,
                tags$h2("input data (log)")
              ),
              column(9,
                DT::dataTableOutput("input_data_log")
              )
            )
          ),
          tabPanel("extended result",
            fluidRow(
              column(3,
                tags$h2("extended result"),
              ),
              column(9, 
                fluidRow(
                  column(2, tags$h4("sdev"), tableOutput("sdev_table")),
                  column(2, tags$h4("center"), tableOutput("center_table")),
                  column(2, tags$h4("scale"), tableOutput("scale_table")),
                  column(6, tags$h4("rotation"), plotlyOutput("pca_rotation")),
                ),
                fluidRow(
                  column(12, tags$h4("PCA Result Summary:"), verbatimTextOutput("pca_result_summary")),
                ),
              ),
            ),
          ),
        ),
      ),
      tabPanel(
        "CA",
        tabsetPanel(
          tabPanel(
            "CA Plot",
            br(),
            fluidRow(
              column(3,
                fluidRow(
                  tags$h2("CA")
                ),
                br(),
                fluidRow(
                  sliderInput("n", "Number of points", value = 150, min = 6, max = 150),
                )
              ),
              column(9,
                plotOutput(outputId = "caPlot"),
                br(),
                verbatimTextOutput("ca_result_summary")
              )
            )
          ),
          # tabPanel(
          #   "extended result",
          #   br(),
          #   fluidRow(
          #     column(3,
          #       fluidRow(
          #         tags$h2("CA(use kmean)")
          #       ),
          #       br(),
          #       fluidRow(
          #         sliderInput("centers_k", "centers(k)", value = 3, min = 3, max = 10),
          #       ),
          #     ),
          #     column(9,
          #       #DT::dataTableOutput("result_data")
          #     )
          #   ),
          # )
        ),
      ),
      tabPanel("iris data",
        fluidRow(
          column(3,
            fluidRow(tags$h2("iris data"),),
            fluidRow(htmlOutput("photo")),
            fluidRow(htmlOutput("iris_data_help"))
          ),
          column(8,
            tabsetPanel(
              tabPanel("Raw Data",
                br(),
                DT::dataTableOutput("iris_raw")
              ),
              tabPanel("Summary",
                br(),
                htmlOutput("summary_of_raw_data_output"),
              ),
              tabPanel("Correlations",
                br(),
                tags$h4("Correlations"),
                plotlyOutput("correlations_of_raw_data_output"),
              ),
              tabPanel("Scatter Plot",
                br(),
                ggvisOutput("scatter_output"),
                uiOutput("scatter_plot_xy")
                
              ),
              #tabPanel("Box Plot",
              #  br(),
              #  plotOutput("box_plot")
              #)
            )
          )
        )
      ),
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  thematic::thematic_shiny()
  data(iris)
  # log transform
  log.ir <- log(iris[, 1:4])
  ir.species <- iris[, 5]
  # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
  ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  
  ir.ca <- reactive(CA(iris[1:input$n, 1:4], graph = FALSE))
  
  
  
  x_pc <- reactiveVal(1)
  y_pc <- reactiveVal(2)
  observeEvent(input$x_PC1, {x_pc(1)})
  observeEvent(input$x_PC2, {x_pc(2)})
  observeEvent(input$x_PC3, {x_pc(3)})
  observeEvent(input$x_PC4, {x_pc(4)})  
  observeEvent(input$y_PC1, {y_pc(1)})
  observeEvent(input$y_PC2, {y_pc(2)})  
  observeEvent(input$y_PC3, {y_pc(3)})
  observeEvent(input$y_PC4, {y_pc(4)})

  
  output$x_PC_choice <- renderUI({
    column(12,
      {if (y_pc() != 1) actionButton("x_PC1", "PC1", class = "btn-primary btn-sm bg-gradient")},
      {if (y_pc() != 2) actionButton("x_PC2", "PC2", class = "btn-info btn-sm bg-gradient")},
      {if (y_pc() != 3) actionButton("x_PC3", "PC3", class = "btn-success btn-sm bg-gradient")},
      {if (y_pc() != 4) actionButton("x_PC4", "PC4", class = "btn-warning btn-sm bg-gradient")},)
  })
  output$y_PC_choice <- renderUI({
    column(12,
      {if (x_pc() != 1) actionButton("y_PC1", "PC1", class = "btn-primary btn-sm bg-gradient")},
      {if (x_pc() != 2) actionButton("y_PC2", "PC2", class = "btn-info btn-sm bg-gradient")},
      {if (x_pc() != 3) actionButton("y_PC3", "PC3", class = "btn-success btn-sm bg-gradient")},
      {if (x_pc() != 4) actionButton("y_PC4", "PC4", class = "btn-warning btn-sm bg-gradient")},)
  })
  


  output$pcaPlot <- renderPlotly({
    g <- ggbiplot(ir.pca, choices = c(x_pc(), y_pc()), obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top', plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent', color=NA))
    ggplotly(g)
  })
  #, bg = "transparent"
  
  output$result_data <- renderDT({
    predict(ir.pca, log.ir)
  }, options = list(pageLength = 25))
  
  output$input_data_log <- renderDT({
    log.ir
  }, options = list(pageLength = 25))
  
  output$sdev_table <- renderTable({
    ir.pca$sdev
  })
  
  output$center_table <- renderTable({
    ir.pca$center
  })
  
  output$scale_table <- renderTable({
    ir.pca$scale
  })
  
  output$pca_rotation <- renderPlotly({
    fig <- plot_ly(z = t(as.matrix(ir.pca$rotation)), x = rownames(ir.pca$rotation), y = colnames(ir.pca$rotation), type = "heatmap")
    fig
  })
  
  output$pca_result_summary <- renderPrint({
    summary(ir.pca)
  })
  
  output$caPlot <- renderPlot({
    #CA(X = iris[1:input$n,1:4], graph = FALSE)
    #ir.ca <- kmeans(log.ir, centers = input$center_k)[["centers"]]
    fviz_ca_biplot(ir.ca(), repel = TRUE)
  })
  
  output$ca_result_summary <- renderPrint({
    #ir.ca <- CA(iris[1:input$n, 1:4], graph = FALSE)
    summary(ir.ca())
  })
  
  output$photo <- renderText({
    "<img src='https://miro.medium.com/v2/resize:fit:1400/format:webp/1*gwmXliaxIBkY4NQBhoe9JQ.png' width='300'>"
  })
  
  output$iris_data_help <- renderText({
    HTML("<p>The Iris dataset was used in R.A. Fisher's classic 1936 paper, The Use of Multiple Measurements in Taxonomic Problems, and can also be found on the UCI Machine Learning Repository.</p> <p>It includes three iris species with 50 samples each as well as some properties about each flower. One flower species is linearly separable from the other two, but the other two are not linearly separable from each other.</p> (from <a href='https://www.kaggle.com/uciml/iris'>Kaggle - iris dataset</a> )")
  })
  
  output$iris_raw <- renderDT({
    iris
  }, options = list(pageLength = 25))
  
  output$summary_of_raw_data_output <- renderPrint({
    summarytools::view(dfSummary(iris), method = "render")
  })
  
  output$correlations_of_raw_data_output <- renderPlotly({
    ggcorrplot(cor(iris[,1:4]))
  })
  
  
  iris %>% ggvis(input_select(names(iris), map = as.name), input_select(names(iris), map = as.name)) %>% layer_points(fill = ~Species) %>% bind_shiny("scatter_output", "scatter_plot_xy")
  
  
  output$box_plot <- renderPlot({
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


# data(iris)
# # log transform
# log.ir <- log(iris[, 1:4])
# ir.species <- iris[, 5]
# # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
# ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
# library(ggbiplot)
# g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
# g <- g + scale_color_discrete(name = '')
# g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
# print(g)


# 
# library(shiny)
# 
# ui <- fluidPage(
#   
# )
# 
# server <- function(input, output, session) {
#   
# }
# 
# shinyApp(ui, server)
