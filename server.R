library(shiny)

server <- function(input, output, session) {
    sampData <- reactiveValues(Values = numeric(), Means = numeric())
    popuData <- reactiveValues(Values = numeric())
    rangeHist <- numeric()
    maxSummDecimals <- 10

    # View help information
    observeEvent(input$helpBtn, {
        showModal(modalDialog(
            title = "Application Help",
            includeHTML("www/help_content.html"),
            size = "m",
            easyClose = TRUE,
            footer = NULL
        ))
    })

    # Generate population data
    observe({
        sampData$Values = numeric()
        sampData$Means = numeric()
        popuData$Values = numeric()
        if(input$setRandSeed) {
            req(is.integer(input$randSeed),
                input$randSeed >= 0)
            set.seed(input$randSeed)
        }
        req(is.integer(input$popuDistN),
            input$popuDistN >= 1)
        if(input$popuDist == "Normal") {
            req(is.numeric(input$popuDistMean),
                is.numeric(input$popuDistStdDev),
                input$popuDistStdDev >= 0)
            popuData$Values <- rnorm(input$popuDistN,
                                     input$popuDistMean,
                                     input$popuDistStdDev)
        } else if(input$popuDist == "Uniform") {
            req(is.numeric(input$popuDistMin),
                is.numeric(input$popuDistMax),
                input$popuDistMin < input$popuDistMax)
            popuData$Values <- runif(input$popuDistN,
                                     min = input$popuDistMin,
                                     max = input$popuDistMax)
        } else if(input$popuDist == "T") {
            req(is.numeric(input$popuDistDf),
                input$popuDistDf > 0)
            popuData$Values <- rt(input$popuDistN,
                                  df = input$popuDistDf)
        } else if(input$popuDist == "F") {
            req(is.numeric(input$popuDistDf1),
                is.numeric(input$popuDistDf2),
                input$popuDistDf1 > 0,
                input$popuDistDf2 > 0)
            popuData$Values <- rf(input$popuDistN,
                                  df1 = input$popuDistDf1,
                                  df2 = input$popuDistDf2)
        } else if(input$popuDist == "Chisquare") {
            req(is.numeric(input$popuDistDf),
                input$popuDistDf > 0)
            popuData$Values <- rchisq(input$popuDistN,
                                      df = input$popuDistDf)
        } else if(input$popuDist == "Exponential") {
            req(is.numeric(input$popuDistRate),
                input$popuDistRate > 0)
            popuData$Values <- rexp(input$popuDistN,
                                    rate = input$popuDistRate)
        } else {
            popuData$Values <- numeric()
        }

        # Calc and save main histogram x axis scale for secondary histograms
        if(length(popuData$Values) > 0) {
            temp <- hist(popuData$Values, plot = FALSE)
            rangeHist <<- range(temp$breaks)
        }
    })

    # Reset sample data
    observeEvent(input$sampReset, {
        sampData$Values = numeric()
        sampData$Means = numeric()
    })

    # Draw samples
    observeEvent(input$sampDraw, {
        if(length(sampData$Values) == 0 & input$setRandSeed) {
            set.seed(input$randSeed)
        }
        newSamples <- list()
        req(is.integer(input$sampSize),
            is.integer(input$sampRep),
            input$sampSize * input$sampRep >= 1)
        for(i in 1:input$sampRep){
            newSamples[[i]] <- sample(x = popuData$Values,
                                      size = input$sampSize,
                                      replace = TRUE)
        }
        sampData$Values = c(sampData$Values, unlist(newSamples))
        newMeans <- list()
        for(i in 1:input$sampRep){
            newMeans[[i]] <- mean(newSamples[[i]], na.rm = TRUE)
        }
        sampData$Means = c(sampData$Means, unlist(newMeans))
    })

    # Show population characteristics
    output$popuHist <- renderPlot({
        if(length(popuData$Values) > 0) {
            par(mar = c(4,4,2,1))
            hist(popuData$Values,
                 main = "Population Histogram",
                 xlab = paste0(input$varName, " Population"),
                 col = "#317EACDA",
                 border = "#BBBBBB")
        }
    })

    output$popuStats1 <- renderTable({
        t <- data.frame(N = length(popuData$Values),
                        Median = median(popuData$Values),
                        Mean = ifelse(length(popuData$Values) > 0,
                                      mean(popuData$Values, na.rm = TRUE),
                                      numeric()),
                        SD = sd(popuData$Values, na.rm = TRUE))
        req(is.integer(input$summDecimals),
            input$summDecimals >= 0,
            input$summDecimals <= maxSummDecimals)
        t[, -1] <- round(t[, -1], input$summDecimals)
        format(t, digits = input$summDecimals)},
        digits = maxSummDecimals)

    output$popuStats2 <- renderTable({
        q <- quantile(popuData$Values, probs = c(0, 0.25, 0.75, 1))
        t <- data.frame(Min = q[1],
                        Q25 = q[2],
                        Q75 = q[3],
                        Max = q[4])
        req(is.integer(input$summDecimals),
            input$summDecimals >= 0,
            input$summDecimals <= maxSummDecimals)
        t <- round(t, input$summDecimals)
        format(t, digits = input$summDecimals)},
        digits = maxSummDecimals)

    # Show sample characteristics
    output$sampHist <- renderPlot({
        if(length(sampData$Values) > 0) {
            par(mar = c(4,4,2,1))
            hist(sampData$Values,
                 xlim = rangeHist,
                 main = "Sample Histogram",
                 xlab = paste0(input$varName, " Sample"),
                 col = "#73A839DA",
                 border = "#BBBBBB")
        }
    })

    output$sampStats1 <- renderTable({
        t <- data.frame(N = length(sampData$Values),
                        Median = median(sampData$Values),
                        Mean = ifelse(length(sampData$Values) > 0,
                                      mean(sampData$Values, na.rm = TRUE),
                                      numeric()),
                        SD = sd(sampData$Values, na.rm = TRUE))
        req(is.integer(input$summDecimals),
            input$summDecimals >= 0,
            input$summDecimals <= maxSummDecimals)
        t[, -1] <- round(t[, -1], input$summDecimals)
        format(t, digits = input$summDecimals)
        }, digits = maxSummDecimals)

    output$sampStats2 <- renderTable({
        q <- quantile(sampData$Values, probs = c(0, 0.25, 0.75, 1))
        t <- data.frame(Min = q[1],
                        Q25 = q[2],
                        Q75 = q[3],
                        Max = q[4])
        req(is.integer(input$summDecimals),
            input$summDecimals >= 0,
            input$summDecimals <= maxSummDecimals)
        t <- round(t, input$summDecimals)
        format(t, digits = input$summDecimals)},
        digits = maxSummDecimals)

    # Show sample mean characteristics
    output$sampMeansHist <- renderPlot({
        if(length(sampData$Means) > 0) {
            par(mar = c(4,4,2,1))
            hist(sampData$Means,
                 xlim = rangeHist,
                 main = "Sample Mean Histogram",
                 xlab = paste0(input$varName, " Sample Means"),
                 col = "#DD5600DA",
                 border = "#BBBBBB")
        }
    })

    output$sampMeansStats1 <- renderTable({
        t <- data.frame(N = length(sampData$Means),
                        Median = median(sampData$Means),
                        Mean = ifelse(length(sampData$Means) > 0,
                                      mean(sampData$Means, na.rm = TRUE),
                                      numeric()),
                        SD = sd(sampData$Means, na.rm = TRUE))
        req(is.integer(input$summDecimals),
            input$summDecimals >= 0,
            input$summDecimals <= maxSummDecimals)
        t[, -1] <- round(t[, -1], input$summDecimals)
        format(t, digits = input$summDecimals)},
        digits = maxSummDecimals)

    output$sampMeansTheoric <- renderTable({
        req(length(popuData$Values) > 0,
            is.integer(input$sampSize),
            input$sampSize > 0)
        t <- data.frame(Mean = mean(popuData$Values, na.rm = TRUE),
                        SD = sd(popuData$Values, na.rm = TRUE) / sqrt(input$sampSize))
        req(is.integer(input$summDecimals),
            input$summDecimals >= 0,
            input$summDecimals <= maxSummDecimals)
        t <- round(t, input$summDecimals)
        format(t, digits = input$summDecimals)},
        digits = maxSummDecimals)
}
