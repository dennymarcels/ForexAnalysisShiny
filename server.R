library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(shinyWidgets)

shinyServer(function(input, output) {

# Declaring a function for loading and pre-formating each uploaded file
     
readAndFormat <- function(file, name){
     table <- read.csv(file)
     name <- sub("\\..*", "", name)
     table$EA <- (name)
     names(table) <- gsub("\\.", "", names(table))
     table <- table[1:which(table$CloseDate == "")[1]-1,]
     table$CloseDate <- as.POSIXct(table$CloseDate, format = "%m/%d/%Y %H:%M")
     table <- table %>% arrange(table$CloseDate)
     table$CloseDate <- as.Date(table$CloseDate, format = "%m/%d/%Y")
     table <- table[
          table$Comment != "cancelled", 
          c('EA', 'Symbol', 'MagicNumber', 'CloseDate', 'Pips')
          ]
}
     
# Pre-processing and row-binding all uploaded files

preProc <- reactive({
     preProc <- data.table::rbindlist(
     as.data.frame(
          mapply(
               readAndFormat, input$files$datapath, input$files$name, USE.NAMES = FALSE
               )
          )
     )
     preProc
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
     dateFilt()[dateFilt()$MagicNumber %in% input$magicNumberSelection, ]
})

# Including a column for cumulative sum of Pips by MagicNumber

withNetPips <- reactive({
     withNetPips <- magicNumberFilt() %>% group_by(MagicNumber) %>% mutate(CumPips = cumsum(Pips))
     
     # Including a column for color scheme by MagicNumber
     
     withNetPips$Color <- factor(
          withNetPips$MagicNumber, labels = rainbow(
               length(
                    unique(
                         withNetPips$MagicNumber
                    )
                    )
               )
     )
     
     withNetPips
})

# Including a column for drawdown by MagicNumber

withDrawdown <- reactive({
     withDrawdown <- NULL
     for(magicnumber in unique(withNetPips()$MagicNumber)){
          temp <- withNetPips() %>% filter(MagicNumber == magicnumber)
          maxpips <- 0
          cumdd <- 0
          cumddvector <- vector()
          for(row in 1:dim(temp)[1]){
               dd <- -(temp$CumPips[row] - maxpips)
               cumdd <- ifelse(dd>cumdd, dd, cumdd)
               if(temp$CumPips[row] > maxpips){
                    maxpips <- temp$CumPips[row]
                    cumdd <- 0
               }
               cumddvector[row] <- cumdd
          }
          temp$CumDD <- round(cumddvector, digits = 1)
          withDrawdown <- rbind(withDrawdown, as.data.frame(temp))
     }
     withDrawdown
})

# Defining a complete summary table for the period

byPeriod0 <- reactive({ 
     withDrawdown() %>% 
     group_by(EA, MagicNumber, Symbol) %>%
     summarise(FirstTrade = min(YearMonth),
               NMonths = length(unique(YearMonth)),
               NTrades = n(), 
               NetWins = round(sum(Pips[Pips > 0]), digits = 1), 
               NetLosses = round(sum(Pips[Pips < 0]), digits = 1), 
               NetPips = round(sum(Pips), digits = 1), 
               DD = max(CumDD), 
               Note = round(NetPips/(DD+1), digits = 2))
})

# Final summary table for the period, excluding based on note

byPeriod <- reactive({
    byPeriodTemp <- byPeriod0()[byPeriod0()$Note >= input$minNote,] %>% arrange(desc(Note))
    byPeriodTemp$EA <- as.factor(byPeriodTemp$EA)
    byPeriodTemp$MagicNumber <- as.factor(byPeriodTemp$MagicNumber)
    byPeriodTemp$FirstTrade <- as.factor(byPeriodTemp$FirstTrade)
    byPeriodTemp
})

# Defining a summary table by month

byMonth0 <- reactive({
     withDrawdown() %>%
     group_by(YearMonth, EA, MagicNumber, Color) %>%
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
     byMonth <- NULL
     for(magicnumber in unique(byMonth0()$MagicNumber)){
          temp <- as.data.frame(
               byMonth0()[byMonth0()$MagicNumber == magicnumber,]
          )
          months <- unique(temp$YearMonth)
          monthsToAdd <- monSeq[!(monSeq %in% months)]
          if(length(monthsToAdd) != 0){
               temp <- rbind(temp, data.frame(
                    YearMonth = monthsToAdd,
                    EA = temp$EA[1],
                    MagicNumber = magicnumber, 
                    Color = temp$Color[1], 
                    NetPips = 0))
          }
          byMonth <- rbind(byMonth, temp) 
     }
     byMonth %>% arrange(YearMonth)
})

# Generating a table for the plot

byMonthForPlot <- reactive({
     rows_selected <- sort(input$finalTable_rows_selected)
     magicNumbersForPlot <- sort(byPeriod()$MagicNumber[rows_selected])
     byMonth()[byMonth()$MagicNumber %in% magicNumbersForPlot,]
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
          start = max - 182, 
          end = max)
})

output$magicNumberSelection <- renderUI({
     req(input$files)
     magicNumbers <- sort(unique(dateFilt()$MagicNumber))
     pickerInput(
          "magicNumberSelection",
          "Choose the Magic Numbers to be included in the report",
          choices = magicNumbers,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
     )
})

output$minNote <- renderUI({
     req(input$files, input$magicNumberSelection)
     # byPeriodValid <- byPeriod0()$Note[byPeriod0()$DD != 0]
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
    c("Number of entries with the selected parameters: ", dim(byPeriod())[1])
})

output$finalTable <- renderDT({
     req(input$files, input$magicNumberSelection)
     byPeriod()
     },
     caption = "**Select rows from table below to display monthly performance of desired strategies (MagicNumbers) in a graph above**",
     # server = FALSE,
     filter = "top",
     options = list(
        columnDefs = list(list(className = 'dt-right', targets = "_all")),
        pageLength = 100
        )
     # options = list(pageLength = 100,
     #                htmlwidgets.TOJSON_ARGS = list(na = 'string'))
)

output$plot <- renderPlotly({
     req(input$files, input$magicNumberSelection, input$finalTable_rows_selected)
     magicNumbersToInclude <- byPeriod0()[byPeriod0()$Note >= input$minNote,]$MagicNumber
     valuesSumPips <- byMonth()[byMonth()$MagicNumber %in% magicNumbersToInclude,]$NetPips
     rangeY <- c(min(valuesSumPips), max(valuesSumPips))
     rows_selected <- input$finalTable_rows_selected
     magicNumbersForPlot <- sort(byPeriod()$MagicNumber[rows_selected])
     temp <- byMonthForPlot()[byMonthForPlot()$MagicNumber == magicNumbersForPlot[1],]

     p <- plot_ly(temp,
                  x = temp$YearMonth,
                  y = temp$NetPips,
                  type = "bar",
                  marker = list(color = temp$Color[1]),
                  name = temp$MagicNumber[1]
                  )

     for(magicnumber in magicNumbersForPlot[-1]){
          temp2 <- byMonthForPlot()[byMonthForPlot()$MagicNumber == magicnumber,]
          p <- add_trace(p,
                         x = temp2$YearMonth,
                         y = temp2$NetPips,
                         marker = list(color = temp2$Color[1]),
                         name = temp2$MagicNumber[1]
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
     }
)

})