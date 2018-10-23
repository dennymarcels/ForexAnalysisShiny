library(shiny)
library(dplyr)
library(data.table)
library(DT)
library(lubridate)
library(plotly)
library(shinyWidgets)

shinyServer(function(input, output) {

# Declaring a function for loading and pre-formating each uploaded file
     
readAndFormat <- function(file, name){
    table <- read.csv(file)
    name <- sub("\\..*", "", name)
    table$EA <- (name)
    names(table) <- gsub("\\.", "", names(table))
    table <- if(length(which(table$CloseDate == "")) == 0) {table} else {table[1:which(table$CloseDate == "")[1] -1, ]}
    table$CloseDate <- as.POSIXct(table$CloseDate, format = "%m/%d/%Y %H:%M")
    table <- table %>% arrange(table$CloseDate)
    table$CloseDate <- as.Date(table$CloseDate, format = "%m/%d/%Y")
    table$Comment <- tolower(table$Comment)
    table$Comment <- gsub("\\[sl]", "", table$Comment)
    table <- table[
        table$Comment != "cancelled", 
        c('EA', 'Symbol', 'MagicNumber', 'CloseDate', 'Pips', 'Comment')
        ]
    table$EA_MN_S <- paste(table$EA, table$MagicNumber, table$Symbol, sep = " | ")
    table
}
     
# Pre-processing and row-binding all uploaded files

preProc <- reactive({
    preProc <- rbindlist(
        as.data.frame(
            mapply(
                readAndFormat, input$files$datapath, input$files$name, USE.NAMES = FALSE
            )
        )
    )
})

# Filtering dataframe based on date criteria

dateFilt <- reactive({
    dateFilt <- preProc()[preProc()$CloseDate %in% input$dateRange[1]:input$dateRange[2],]
    dateFilt$EA <- sub("_.*", "", dateFilt$EA)
    dateFilt$CloseDate <- format(dateFilt$CloseDate, format = "%Y/%m")
    colnames(dateFilt)[colnames(dateFilt) == "CloseDate"] <- "YearMonth"
    dateFilt
})

# Filtering dataframe based on magic number criteria

magicNumberFilt <- reactive({
    dateFilt()[dateFilt()$EA_MN_S %in% input$magicNumberSelection, ]
})

# Including a column for cumulative sum of Pips by EA/MagicNumber/Symbol

withNetPips <- reactive({
     withNetPips <- magicNumberFilt() %>% group_by(EA_MN_S) %>% mutate(CumPips = cumsum(Pips))
     
     # Including a column for color scheme by EA/MagicNumber/Symbol
     
     withNetPips$Color <- factor(
         withNetPips$EA_MN_S, labels = rainbow(
             length(
                 unique(
                     withNetPips$EA_MN_S
                 )
             )
         )
     )
     
     withNetPips
})

# Including a column for drawdown by EA/MagicNumber/Symbol

withDrawdown <- reactive({
     withDrawdown <- data.frame()
     for(eamns in unique(withNetPips()$EA_MN_S)){
         temp <- withNetPips() %>% filter(EA_MN_S == eamns)
         maxpips <- 0
         cumdd <- 0
         cumddvector <- vector()
         for(row in 1:nrow(temp)){
             dd <- -(temp$CumPips[row] - maxpips)
             cumdd <- ifelse(dd>cumdd, dd, cumdd)
             if(temp$CumPips[row] > maxpips){
                 maxpips <- temp$CumPips[row]
                 cumdd <- 0
             }
             cumddvector[row] <- cumdd
         }
         temp$CumDD <- round(cumddvector, digits = 1)
         withDrawdown <- rbindlist(list(withDrawdown, as.data.frame(temp)))
     }
     withDrawdown
})

# Defining a complete summary table for the period

byPeriod0 <- reactive({ 
    withDrawdown() %>% 
        group_by(EA, MagicNumber, Symbol, EA_MN_S) %>%
        summarise(Comment = Comment[1],
                  FirstTrade = min(YearMonth),
                  NMonths = length(unique(YearMonth)),
                  NTrades = n(), 
                  NetWins = round(sum(Pips[Pips > 0]), digits = 1), 
                  NetLosses = round(sum(Pips[Pips < 0]), digits = 1), 
                  NetPips = round(sum(Pips), digits = 1), 
                  DD = max(CumDD), 
                  Note = round(NetPips/DD, digits = 2))
})

# Final summary table for the period, excluding based on note

byPeriod <- reactive({
    byPeriod <- byPeriod0()[byPeriod0()$Note >= input$minNote,] %>% arrange(desc(Note))
    byPeriod$EA <- as.factor(byPeriod$EA)
    byPeriod$MagicNumber <- as.factor(byPeriod$MagicNumber)
    byPeriod$FirstTrade <- as.factor(byPeriod$FirstTrade)
    byPeriod
})

# Defining a summary table by month

byMonth0 <- reactive({
    withDrawdown() %>%
        filter(EA_MN_S %in% byPeriod()$EA_MN_S) %>%
        group_by(EA_MN_S, Color, YearMonth) %>%
        summarise(NetPips = sum(Pips))
})

# Completing the empty months in the previous table, for the plot

byMonth <- reactive({
    monSeq <- unique(
        format(
            seq.Date(input$dateRange[1], input$dateRange[2], by = 15), 
            format = "%Y/%m"
        )
    )
    
    byMonth <- data.frame()
    for(eamns in unique(byMonth0()$EA_MN_S)){
        temp <- as.data.frame(
            byMonth0()[byMonth0()$EA_MN_S == eamns,]
        )
        months <- unique(temp$YearMonth)
        monthsToAdd <- monSeq[!(monSeq %in% months)]
        if(length(monthsToAdd) != 0){
            temp <- rbindlist(list(temp, data.frame(
                EA_MN_S = eamns, 
                Color = temp$Color[1],
                YearMonth = monthsToAdd,
                NetPips = 0)))
        }
        byMonth <- rbindlist(list(byMonth, temp)) 
    }
    byMonth$YearMonth <- as.character(byMonth$YearMonth)
    byMonth <- byMonth %>% arrange(YearMonth)
    byMonth
})

# Generating a table for the plot

byMonthForPlot <- reactive({
    rows_selected <- sort(input$finalTable_rows_selected)
    EA_MN_SForPlot <- sort(byPeriod()$EA_MN_S[rows_selected])
    byMonth()[byMonth()$EA_MN_S %in% EA_MN_SForPlot,]
})

# Rendering input UIs

output$dateRange <- renderUI({
     req(input$files)
     min <- min(preProc()$CloseDate)
     max <- max(preProc()$CloseDate)
     dateRangeInput(
          "dateRange", 
          "Choose the date range", 
          min = min, 
          max = max, 
          start = max %m-% months(6), 
          end = max)
})

output$magicNumberSelection <- renderUI({
     req(input$files)
     magicNumbers <- sort(unique(dateFilt()$EA_MN_S))
     pickerInput(
          "magicNumberSelection",
          "Choose the < EA | Magic Number | Symbol > to be included in the report",
          choices = magicNumbers,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
     )
})

output$minNote <- renderUI({
     req(input$files, input$magicNumberSelection)
     byPeriodValid <- byPeriod0()$Note
     numericInput(
          "minNote",
          "Choose the minimum note acceptable",
          min = ifelse(min(byPeriodValid) > 0, 0, min(byPeriodValid)),
          max = ifelse(max(byPeriodValid) < 0, 0, max(byPeriodValid)),
          step = 0.1,
          value = 0
     )
})

# Render output UIs

output$filesSummary <- renderTable({
     req(input$files)
     temp <- data.frame(FileName = input$files$name, FirstTrade = 0, LastTrade = 0)
     for(row in 1:dim(temp)[1]){
          name <- sub("\\..*", "", temp$FileName)
          temp$FirstTrade[row] <- format(
               min(
                    preProc()$CloseDate[preProc()$EA == name[row]]
                    ), format = "%d/%m/%Y"
               )
          temp$LastTrade[row] <- format(
               max(
                    preProc()$CloseDate[preProc()$EA == name[row]]
               ), format = "%d/%m/%Y"
          )
     }
     temp
     },
     spacing = "xs",
     caption = "Dates covered on each file", 
     caption.placement = getOption("xtable.caption.placement", "top")
)

output$entries <- renderText({
    req(input$files, input$magicNumberSelection)
    c("Number of entries with the selected parameters: ", nrow(byPeriod()))
})

output$finalTable <- renderDT({
     req(input$files, input$magicNumberSelection)
     byPeriod()[, -which(names(byPeriod()) == "EA_MN_S")]
     },
     caption = "**Select rows from table below to display monthly performance of desired strategies (MagicNumbers) in a graph above**",
     # server = FALSE,
     filter = "top",
     options = list(
        columnDefs = list(list(className = 'dt-right', targets = "_all")),
        pageLength = 100
       )
     #### options = list(pageLength = 100,
     ####                htmlwidgets.TOJSON_ARGS = list(na = 'string'))
)

output$plot <- renderPlotly({
    req(input$files, input$magicNumberSelection, input$finalTable_rows_selected)
    
    valuesSumPips <- byMonth()$NetPips
    rangeY <- c(min(valuesSumPips), max(valuesSumPips))
    
    EA_MN_SForPlot <- unique(byMonthForPlot()$EA_MN_S)
    temp <- byMonthForPlot()[byMonthForPlot()$EA_MN_S == EA_MN_SForPlot[1], ]
    
    p <- plot_ly(temp,
                 x = temp$YearMonth,
                 y = temp$NetPips,
                 type = "bar",
                 marker = list(color = temp$Color[1]),
                 name = temp$EA_MN_S[1]
                 )

    for(eamns in EA_MN_SForPlot[-1]){
        temp2 <- byMonthForPlot()[byMonthForPlot()$EA_MN_S == eamns,]
        p <- add_trace(p,
                       x = temp2$YearMonth,
                       y = temp2$NetPips,
                       marker = list(color = temp2$Color[1]),
                       name = temp2$EA_MN_S[1]
        )
    }

    p %>%
        layout(title = "Net Pips per Month",
               barmode = "group",
               bargap = 0.2,
               showlegend = TRUE,
               xaxis = list(title = "Month",
                            showgrid = TRUE,
                            tickangle = 270),
               yaxis = list(title = "Pips",
                            range = rangeY
               )
        )
    })

})