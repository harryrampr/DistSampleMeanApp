# Outside resources used:
# https://getbootstrap.com/docs/3.4/css/
# https://bootswatch.com/3/cerulean/
#
library(shiny)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("www/general.css"),
    fluidRow(
        column(9,
               titlePanel("Sampling Distribution of the Sample Mean App")),
        column(1,
               actionButton(inputId = "helpBtn",
                            label = "Help",
                            class = "btn-primary btn-sm"))),
    sidebarLayout(
        sidebarPanel(
            h4("Population Distribution"),
            selectInput(inputId = "popuDist",
                        label = "Shape:",
                        c("Normal", "Uniform", "T","F", "Chisquare", "Exponential")),
            numericInput(inputId = "popuDistN",
                         label = "N:",
                         value = 10000,
                         min = 1),

            # Only show for normal distribution
            conditionalPanel("input.popuDist == 'Normal'",
                             numericInput(inputId = "popuDistMean",
                                          label = "Mean:",
                                          value = 0),
                             numericInput(inputId = "popuDistStdDev",
                                          label = "Std. Deviation:",
                                          value = 1,
                                          min = 0)),

            # Only show for uniform distribution
            conditionalPanel("input.popuDist == 'Uniform'",
                             numericInput(inputId = "popuDistMin",
                                          label = "Min",
                                          value = 0),
                             numericInput(inputId = "popuDistMax",
                                          label = "Max",
                                          value = 1)),

            # Only show for T distribution
            conditionalPanel("input.popuDist == 'T'",
                             numericInput(inputId = "popuDistDf",
                                          label = "Degrees of Freedom",
                                          value = 10)),

            # Only show for F distribution
            conditionalPanel("input.popuDist == 'F'",
                             numericInput(inputId = "popuDistDf1",
                                          label = "Degrees of Freedom 1",
                                          value = 15),
                             numericInput(inputId = "popuDistDf2",
                                          label = "Degrees of Freedom 2",
                                          value = 60)),

            # Only show for chisquare distribution
            conditionalPanel("input.popuDist == 'Chisquare'",
                             numericInput(inputId = "popuDistDf",
                                          label = "Degrees of Freedom",
                                          value = 5)),

            # Only show for exponential distribution
            conditionalPanel("input.popuDist == 'Exponential'",
                             numericInput(inputId = "popuDistRate",
                                          label = "Rate",
                                          value = 1)),

            h4("Sample Settings", style = "margin-top: 50px;"),
            numericInput(inputId = "sampSize",
                         label = "Size:",
                         value = 30,
                         min = 1),
            numericInput(inputId = "sampRep",
                         label = "Repeats:",
                         value = 1,
                         min = 1),
            actionButton(inputId = "sampDraw",
                         label = "Draw Samples",
                         class = "btn-success"),
            actionButton(inputId = "sampReset",
                         label = "Reset",
                         class = "btn-warning"),

            h4("Optional Settings", style = "margin-top: 50px;"),
            checkboxInput(inputId = "setRandSeed",
                          label = "Set Random Seed",
                          value = FALSE),
            conditionalPanel("input.setRandSeed == true",
                             numericInput(inputId = "randSeed",
                                          label = "Random Seed:",
                                          value = 4567)),
            textInput(inputId = "varName",
                      label = "Variable Name:",
                      value = "X"),
            numericInput(inputId = "summDecimals",
                         label = "Summary Decimals:",
                         value = 3),
            width = 3),

        mainPanel(
            fluidRow(
                column(8,
                       div(class = "mebmargin",
                           plotOutput("popuHist", height = 305))),
                column(4,
                       wellPanel(h4("Population Summary", class = "nomargin"),
                                 class = "well-sm"),
                       tableOutput("popuStats1"),
                       tableOutput("popuStats2"))
            ),
            fluidRow(
                column(8,
                       div(class = "mebmargin",
                           plotOutput("sampHist", height = 305))),
                column(4,
                       wellPanel(h4("Sample Summary", class = "nomargin"),
                                 class = "well-sm"),
                       tableOutput("sampStats1"),
                       tableOutput("sampStats2"))
            ),
            fluidRow(
                column(8,
                       div(class = "mebmargin",
                           plotOutput("sampMeansHist", height = 305))),
                column(4,
                       wellPanel(h4("Sample Mean Summary", class = "nomargin"),
                                 class = "well-sm"),
                       tableOutput("sampMeansStats1"),
                       tableOutput("sampMeansStats2"),
                       wellPanel(h4("Theorical Values", class = "nomargin"),
                                 class = "well-sm"),
                       tableOutput("sampMeansTheoric"))
            ),
            br(),
            width = 9)
    )
)
