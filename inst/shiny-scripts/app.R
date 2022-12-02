library(shiny)

ui <- fluidPage(

  # App title
  titlePanel("MethylBioPaths: MethylOverrep & methylMutationAssociation"),

  # Sidebar to display inputs and outputs
  sidebarLayout(

    # Sidepar panel - inputs
    sidebarPanel(

      tags$p("Plot results of overrepresentation analysis on DNA methylation
             data and the association of DNA methylation with specific
             mutations."),
      tags$p("Upload RDS files containing the methylation data belonging to a
             sample representative of disease being studied, the methylation
             data belonging to a sample representative of baseline methylation,
             and the gene expression data belonging to a sample representative
             of the disease being studied. To view the mutation-methylation
             plot, upload an RDS file containing a list of methylation driver
             genes organized based on the category of their mutation"),

      br(),

      # Accepting files as input
      uiOutput("tab1"),
      uiOutput("tab2"),
      uiOutput("tab3"),
      uiOutput("tab4"),
      fileInput(inputId =  "METCancer",
                label = "Choose an RDS file of DNA methylation data
                representative of disease of interest",
                accept = ".rds"),
      fileInput(inputId =  "METNormal",
                label =  "Choose an RDS file of DNA methylation data
                representative of baseline methylation",
                accept = ".rds"),
      fileInput(inputId = "GECancer",
                label = "Choose an RDS file of gene expression representative
                of disease of interest",
                accept = ".rds"),
      fileInput(inputId = "mutationData",
                label = "Choose an RDS file of list organizing methylation
                driver genes based on the category of their mutation",
                accept = ".rds"),

      # Accepting inputs for p-value and q-value
      textInput(inputId = "pValue",
                label = "Enter p-value for overrepresentation test (0 - 1)",
                "0.05"),
      textInput(inputId = "qValue",
                label = "Enter q-value for overrepresentation test (0 - 1)",
                "0.10"),
    ),

    # Main panel - outputs
    mainPanel(

      # Output plots
      tabsetPanel(type = "tabs",
                  tabPanel("Summary of Overrepresentation Analysis Results",
                           plotOutput("overrep")),
                  tabPanel("Association Between Methylation and Mutation",
                           plotOutput("mutAssoc"))
                  )

    )
  )
)

server <- function(input, output, session) {

  # Plotting overrep results
  output$overrep <- renderPlot({

    methylationCancerFile <- input$METCancer
    methylationNormalFile <- input$METNormal
    geneCancerFile <- input$GECancer

    #do not proceed before all inputs have been added
    if (is.null(methylationCancerFile) | is.null(methylationNormalFile) |
        is.null(geneCancerFile)) {
      return(NULL)
    }

    METcancer <- readRDS(methylationCancerFile$datapath)
    METnormal <- readRDS(methylationNormalFile$datapath)
    GEcancer <- readRDS(geneCancerFile$datapath)
    pval <- as.numeric(input$pValue)
    qval <- as.numeric(input$qValue)

    # can't proceed if inputs with no default are NULL
    if (is.null(METcancer) | is.null(METnormal) | is.null(GEcancer)) {
      return(NULL)
    }

    MethylBioPaths::methylOverrep(methylationCancer = METcancer,
                                  methylationNormal = METnormal,
                                  geneCancer = GEcancer,
                                  pValue = pval,
                                  qValue = qval)
  })

  # Plotting association results
  output$mutAssoc <- renderPlot({

    methylationCancerFile <- input$METCancer
    methylationNormalFile <- input$METNormal
    geneCancerFile <- input$GECancer
    mutDataFile <- input$mutationData

    #do not proceed before all inputs have been added
    if (is.null(methylationCancerFile) | is.null(methylationNormalFile) |
        is.null(geneCancerFile) | is.null(mutDataFile)) {
      return(NULL)
    }

    METcancer <- readRDS(methylationCancerFile$datapath)
    METnormal <- readRDS(methylationNormalFile$datapath)
    GEcancer <- readRDS(geneCancerFile$datapath)
    mutData <- readRDS(mutDataFile$datapath)

    # can't proceed if inputs with no default are NULL
    if (is.null(METcancer) | is.null(METnormal) | is.null(GEcancer) |
        is.null(mutData)) {
      return(NULL)
    }

    MethylBioPaths::methylMutationAssociation(methylationCancer = METcancer,
                                              methylationNormal = METnormal,
                                              geneCancer = GEcancer,
                                              mutationData = mutData)
  })

  url1 <- a("Example Dataset 1",
            href="https://raw.githubusercontent.com/Juan-De-Los-Rios/
            MethylBioPaths/master/inst/extdata/METcancer.rds")
  output$tab1 <- renderUI({
    tagList("Download:", url1)
  })

  url2 <- a("Example Dataset 2",
            href="https://raw.githubusercontent.com/Juan-De-Los-Rios/
            MethylBioPaths/master/inst/extdata/METnormal.rds")
  output$tab2 <- renderUI({
    tagList("Download:", url2)
  })

  url3 <- a("Example Dataset 3",
            href="https://raw.githubusercontent.com/Juan-De-Los-Rios/
            MethylBioPaths/master/inst/extdata/GEcancer.rds")
  output$tab3 <- renderUI({
    tagList("Download:", url3)
  })

  url4 <- a("Example Dataset 4",
            href="https://raw.githubusercontent.com/Juan-De-Los-Rios/
            MethylBioPaths/master/inst/extdata/mutationExample.rds")
  output$tab4 <- renderUI({
    tagList("Download:", url4)
  })

}

shinyApp(ui, server)

# [END]
