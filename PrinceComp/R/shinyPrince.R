#' Shiny Application for PCA
#'
#' Runs shiny app that explores PCA on a given dataset
#'
#' This shiny app allows users to manipulate data that's inputted into PCA function, obtaining immediate visual results in the form of a screeplot and loadings info.
#' @param X a matrix of data
#'
#' @return opens a shiny server
#' @import shiny
#' @import shinydashboard
#' @export

shinyPrince <- function(X){ ## X is a dataset

  # make input list for ui
  choice <- list()
  for(i in 1:ncol(X)){
    choice[i] <- colnames(X)[i]
  }

  ui <- dashboardPage(
    dashboardHeader(title = "Shiny Prince PCA" , titleWidth = 500),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
      fluidPage(

        column(1,

               ##widgets for editing data preferences

               checkboxGroupInput("checkGroup", label = h3("Variables"),
                                  choices = choice,
                                  selected = choice[c(2,3)]),

               selectInput("method", "PCA Method:",
                           choices = list("Correlation", "Covariance"),
                           selected = "Correlation"),


        ), ## column

        column(11,
               ##page output
               tabBox(

                 tabPanel("Data Table",  tableOutput("data")),
                 tabPanel("Scree Plot", plotOutput("plot2"),
                          br(),
                          "PCA Loadings",
                          br(),
                          verbatimTextOutput("pca_sum"))
               )
        ) ## col 2


      ) ## fluidPage
    ) ## dash body
  ) ## ui function



  server <- function(input, output) {


    output$data <- renderTable({
      X[, input$checkGroup, drop = FALSE]
    }, rownames = TRUE)


    output$plot2 <- renderPlot({

      X <- X[,  input$checkGroup, drop = FALSE]

      screeplot <- function(data, method){

        if (method == "Covariance") {## Cov method
          pca <- prince(X, cor = F)

        } else if (method == "Correlation") {## cor method
          pca <- prince(X, cor = T)
        }

        plot(pca)
      }

      ## Run Screeplot making fn
      screeplot(data = X, method = input$method)
    })

    output$pca_sum <- renderPrint({

      X <- X[,  input$checkGroup, drop = FALSE]

      pcasum <- function(data, method){

        if (method == "Covariance") { ## Cov method
          pca <- prince(X, cor = F)

        } else if (method == "Correlation") { ## cor method
          pca <- prince(X, cor = T)
        }
        list(pca$Eigenvalues, pca$Eigenvectors, pca$Loadings)
      }

      ## Run PCA summary making function
      pcasum(data = X, method = input$method)
    })

  }

  shinyApp(ui, server)

}
