library(shiny)
library(DT)
library(plotly)
library(shinycssloaders)

shinyUI(fluidPage(
  
  titlePanel("EA Analysis"),
  
  sidebarLayout(
    sidebarPanel(
       fileInput("files", "Choose the csv files to be processed", multiple = TRUE),
       tableOutput("filesSummary"),
       uiOutput("dateRange"),
       uiOutput("magicNumberSelection") %>% withSpinner,
       uiOutput("minNote")
    ),
    
    mainPanel(
         tabsetPanel(
              tabPanel("Results",
                       plotlyOutput("plot") %>% withSpinner,
                       hr(),
                       textOutput("entries"),
                       DTOutput("finalTable") %>% withSpinner,
                       uiOutput("downloadButton")
              ),
              tabPanel("Instructions",
                       br(),
                       tags$ol(
                            tags$li(HTML(paste0("Go to the MyFxBook page you want to analyse, then click on ", strong("Export "), "(top right of the first graph), then ", strong("CSV"), ". Save the file with the ", em("name of the respective EA"), " (the name given will be used by the app to generate the summaries)."))),
                            tags$li(HTML(paste0("Repeat the process above for as many EAs as you wish to analyse. If you are downloading more than one file per EA (for example, same EA but more than one MyFxBook account), the file name should be in the format ", code("nameOfEA_index"), (", where "), em("index"), " is any text string. ", strong("Do not forget the underscore symbol! "), ("This is necessary to make sure the app functions as designed; all text after the underscore symbol will be stripped off when generating the summaries.")))),
                            tags$li(HTML(paste0("At the EA Analysis page (this one you are looking at!), click on ", strong("Browse... "), "on the left panel, choose the CSV file(s) you just downloaded."))),
                            tags$li(HTML(paste0("After the files are loaded, you will get a short summary with file names and the initial and final trade dates on each file, for your reference."))),
                            tags$li(HTML(paste0("Below the file/date summary, you can choose the ", em("period you wish to analyse. "), "By default, the period is set to the ", em("last 6 months "), "available in the datasets."))),
                            tags$li(HTML(paste0("Below the date selection menu, you can ", em("filter strategies by ", strong("EA | MagicNumber | Symbol")), "."))),
                            tags$li(HTML(paste0("After you picked the desired EA/MagicNumber/Symbol(s), below their selection menu, you can ", em("filter strategies by their ", strong("minimum Note")), ". The note is defined as the sum of pips that the strategy reached ", em("(NetPips) "), "divided by the drawdown ", em("(DD)"), ", in the period. Negative notes correspond to strategies with negative performance. Zero corresponds to strategies with no net pips. One, for example, corresponds to strategies whose performance was equal to the drawdown. This input box is set to ", em("zero"), " by default."))),
                            tags$li(HTML(paste0("The table in the ", em("Results "), "tab will show ", em("performance indicators for each EA/MagicNumber/Symbol chosen from the provided files"), ", according to the dates and the minimum note chosen."))),
                            tags$li(HTML(paste0("Select rows from this table to see a ", em("graph showing the monthly performance (Net Pips) of the desired EA/MagicNumber/Symbol(s)."))))
                            ),
                       br(),
                       div(
                            HTML(paste0("Credits: ", em("Denny MC"), " (2018/10/23)")))
                       ),
              tabPanel("Instruções",
                       br(),
                       tags$ol(
                            tags$li(HTML(paste0("Vá à página do MyFxBook que você quer analisar, então clique em ", strong("Export "), "(canto superior direito do primeiro gráfico), depois ", strong("CSV"), ". Salve o arquivo com o ", em("nome do respectivo EA"), " (o nome dado será usado pelo aplicativo para gerar os resumos)."))),
                            tags$li(HTML(paste0("Repita o processo acima para todos os EAs que você gostaria de analisar. Se estiver salvando mais que um arquivo por EA (por exemplo, mesmo EA mas mais de uma conta no MyFxBook), o nome do arquivo deve ser no formato ", code("nomeDoEA_índice"), (", onde "), em("índice"), " é qualquer texto desejado. ", strong("Não se esqueça do símbolo de underline!"), " Isso é necessário para garantir que o aplicativo funcionará como projetado; todo o texto depois do símbolo de underline será descartado durante a geração dos resumos."))),
                            tags$li(HTML(paste0("Na página EA Analysis (essa que você está olhando!), clique em ", strong("Browse... "), "no painel à esquerda, depois escolha os arquivos CSV que você acabou de fazer o download."))),
                            tags$li(HTML(paste0("Depois que os arquivos tenham sido carregados, um breve resumo vai aparecer mostrando os nomes dos arquivos (FileName) e as datas iniciais e finais de trade (FirstTrade e LastTrade, respectivamente) de cada arquivo, para sua referência."))),
                            tags$li(HTML(paste0("Abaixo do resumo de arquivos/datas, você pode escolher o ", em("período que gostaria de analisar. "), "Por padrão, o período está programado para os ", em("últimos 6 meses "), "disponíveis nos arquivos de dados."))),
                            tags$li(HTML(paste0("Abaixo do menu de seleção de datas, você pode ", em("filtrar estratégias por ", strong("EA | MagicNumber | Symbol")), "."))),
                            tags$li(HTML(paste0("Depois de escolher o(s) EA/MagicNumber/Symbol(s) desejado(s), abaixo do menu correspondente, você pode ", em("filtrar estratégias por sua ", strong("Nota mínima (minimum Note)")), ". A nota é definida como a soma de pips que a estratégia alcançou ", em("(NetPips) "), "dividida pelo drawdown ", em("(DD)"), ", no período. Notas negativas correspondem às estratégias com desempenho negativo. Zero corresponde às estratégias sem saldo de pips. Um, por exemplo, corresponde às estratégias cuja performance foi igual ao drawdown. O valor padrão dessa caixa de entrada de texto é ", em("zero.")))),
                            tags$li(HTML(paste0("A tabela na aba ", em("Results (Resultados)"), " mostrará os ", em("indicadores de desempenho para cada EA/MagicNumber/Symbol escolhido a partir dos arquivos fornecidos"), ", de acordo com as datas e a nota mínima escolhidas."))),
                            tags$li(HTML(paste0("Selecione linhas dessa tabela para ver um ", em("gráfico mostrando o desempenho mensal do saldo de pips (Net Pips) do(s) EA/MagicNumber/Sumbol(s) desejados."))))
                       ),
                       br(),
                       div(
                            HTML(paste0("Créditos: ", em("Denny MC"), " (2018/10/23)")))
                       )
              )
         )
    )
  ))