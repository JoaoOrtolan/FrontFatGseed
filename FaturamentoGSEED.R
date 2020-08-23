library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmltools)
library(plotly)
library(shinyDND)
Caminho <- "C:/Users/Joao Ortolan/Desktop/"


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Faturamento - GSEED"),
  dashboardSidebar(
    sidebarMenu(
      dateInput("DateInputSidebar", "Anomes Visão", format = "yyyymm", language = "pt-br", startview = "year"),
      hr(),
      menuItem("Visão Geral", tabName = "SidebarVisaoGeral", icon = icon("fas fa-file")),
      menuItem("Controles", tabName = "SidebarControles", icon = icon("search", lib="glyphicon"),
        menuSubItem("Eleição", tabName = "SubSidebarEleicao"),
        menuSubItem("BC", tabName = "SubSidebarBC"),
        menuSubItem("Modelo", tabName = "SubSidebarModelo"),
        menuSubItem("Bacen", tabName = "SubSidebarBacen"),
        menuSubItem("Pisos", tabName = "SubSidebarPisos"),
        menuSubItem("Sinal Vital", tabName = "SubSidebarSinalVital")),
      menuItem("Consulta CNPJ", tabName = "SidebarConsultaCNPJ", icon = icon("fas fa-building"), 
                numericInput("NumInputCNPJ", "CNPJ", 0),
                dateInput("DateInputCNPJ", "Anomes Faturamento", format = "yyyymm", language = "pt-br", startview = "year"),
                radioButtons("radioButtonsCNPJ", "Modelo", c("Eleição", "Fat. Não Corr.", "Fat. Corr."), selected="Eleição"),
                actionButton("ActionButtonCNPJ", "Buscar"),
                helpText("Só mês e ano serão considerados."),
                helpText("Apartir do dia 20, temos m+1")))),
  dashboardBody(useShinyjs(), 
                tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),tabItems(
    tabItem(tabName = "SidebarVisaoGeral",
      fluidRow(infoBoxOutput("InfoBoxPisos"),infoBoxOutput("InfoBoxBC"),infoBoxOutput("InfoBoxBacen")),
      fluidRow(infoBoxOutput("InfoBoxModelo"),infoBoxOutput("InfoBoxSinalVital"),infoBoxOutput("InfoBoxEleição"))),
    tabItem(tabName = "SubSidebarEleicao",
      titlePanel("Controle eleição"),
      fluidRow(box("Filtros de público", width=9, collapsible=TRUE,
                   fluidRow(column(3,wellPanel(checkboxGroupInput("CBGISegmentoEleicao", "Segmento", c("EMP2", "EMP3"), selected=c("EMP2")))),
                            column(3,wellPanel(checkboxGroupInput("CBGIMetodoEleicao", "Método", c("G", "B"), selected=c("G")))))),
               box(width=3, collapsible=TRUE,
                   checkboxGroupInput("CBGIRefsEleicao", "Referências consideradas"))),
      fluidRow(box(width = 12, plotlyOutput("PlotOutputGraficoEleicao"))),
      fluidRow(box(width = 3, selectInput("SelectInputPlotEleicao", "Eixo Y Gráfico", c())),
               box(width = 3, selectInput("SelectInputLSICEleicao", "Limite Superior Banda", c())),
               box(width = 3, selectInput("SelectInputLIICEleicao", "Limite Inferior Banda", c())),
               box(width = 3, textOutput("TextOutput1Eleicao"),
                              textOutput("TextOutput2Eleicao"),
                              textOutput("TextOutput3Eleicao"),
                              actionButton("AtualizaGraficosEleicao", "Atualiza Gráfico")))),
    tabItem(tabName = "SubSidebarBC",
      fluidRow(box("Controle BC"))),
    tabItem(tabName = "SubSidebarModelo",
      fluidRow(box("Controle Modelo"))),
    tabItem(tabName = "SubSidebarBacen",
      fluidRow(box("Controle Bacen"))),
    tabItem(tabName = "SubSidebarPisos",
      fluidRow(box("Controle Pisos"))),
    tabItem(tabName = "SubSidebarSinalVital",
      fluidRow(box("Controle Sinal Vital")))
)))

server <- function(session, input, output) {
  Dados_Eleicao <- read.csv(paste0(Caminho,"Cubo_Eleição.csv"), sep=";")
  
  updateCheckboxGroupInput(session, "CBGIRefsEleicao", choices = unique(Dados_Eleicao[,"Referencia"]), selected = unique(Dados_Eleicao[,"Referencia"]))
  updateSelectInput(session, "SelectInputPlotEleicao", choices = c(colnames(Dados_Eleicao)[!colnames(Dados_Eleicao) %in% c("ï..Segmento", "Metodo_Eleito", "Referencia", "Qtd")]), selected = "Mediana_Faturamento")
  updateSelectInput(session, "SelectInputLSICEleicao", choices = c("3x Desvio Padrão", colnames(Dados_Eleicao)[!colnames(Dados_Eleicao) %in% c("ï..Segmento", "Metodo_Eleito", "Referencia", "Qtd")]), selected = "3x Desvio Padrão")
  updateSelectInput(session, "SelectInputLIICEleicao", choices = c("3x Desvio Padrão", colnames(Dados_Eleicao)[!colnames(Dados_Eleicao) %in% c("ï..Segmento", "Metodo_Eleito", "Referencia", "Qtd")]), selected = "3x Desvio Padrão")
  
  observeEvent(input$AtualizaGraficosEleicao, {
    output$TextOutput1Eleicao <- renderText(paste0("Eixo Y: ", as.character(input$SelectInputPlotEleicao)))
    output$TextOutput2Eleicao <- renderText(paste0("Limite Sup. Banda: ", as.character(input$SelectInputLSICEleicao)))
    output$TextOutput3Eleicao <- renderText(paste0("Limite Inf. Banda: ", as.character(input$SelectInputLIICEleicao)))
    
    Dados_Plot <- Dados_Eleicao[Dados_Eleicao[,"ï..Segmento"] %in% input$CBGISegmentoEleicao &
                                Dados_Eleicao[,"Metodo_Eleito"] %in% input$CBGIMetodoEleicao,]
    output$PlotOutputGraficoEleicao <- renderPlotly({Roda_Grafico_IC(Dados_Plot, "Intervalo de confiança", "#5ab4ac","Referencia",as.numeric(input$CBGIRefsEleicao), input$SelectInputLIICEleicao, input$SelectInputLSICEleicao, input$SelectInputPlotEleicao, input$SelectInputPlotEleicao, "Faturamento")})
  }, ignoreNULL = FALSE)
  
  output$InfoBoxPisos <- renderInfoBox({infoBox(tags$strong("Pisos"), 
                                                paste0("Não processada"), 
                                                subtitle = "Data limite: 12º dia útil", 
                                                icon = icon("list"),
                                                color = "light-blue",
                                                fill = TRUE)})
  output$InfoBoxBC <- renderInfoBox({infoBox(tags$strong("BC"), 
                                             paste0("Não processada"), 
                                             subtitle = "Data limite: 12º dia útil", 
                                             icon = icon("list"), 
                                             color = "light-blue",
                                             fill = TRUE)})
  output$InfoBoxBacen <- renderInfoBox({infoBox(tags$strong("Bacen"), 
                                                a(paste0("Processada: 14/08/2020"), 
                                                  onclick = "openTab('SubSidebarBacen')",
                                                  href = "#"),
                                                subtitle = "Data limite: 6º dia útil", 
                                                icon = icon("list"), 
                                                color = "lime",
                                                fill = TRUE)})
  output$InfoBoxModelo <- renderInfoBox({infoBox(tags$strong("Modelo"), 
                                                 a(paste0("Processada: 12/08/2020"), 
                                                   onclick = "openTab('SubSidebarModelo')",
                                                   href = "#"),
                                                 subtitle = "Data limite: 8º dia útil", 
                                                 icon = icon("list"), 
                                                 color = "red",
                                                 fill = TRUE)})
  output$InfoBoxSinalVital <- renderInfoBox({infoBox(tags$strong("Sinal Vital"), 
                                                     a(paste0("Processada: 13/08/2020"), 
                                                       onclick = "openTab('SubSidebarSinalVital')",
                                                       href = "#"), 
                                                     subtitle = "Data limite: 5º dia útil", 
                                                     icon = icon("fas fa-heartbeat"), 
                                                     color = "yellow",
                                                     fill = TRUE)})
  output$InfoBoxEleição <- renderInfoBox({infoBox(tags$strong("Eleição"),
                                                  a(paste0("Processada: 19/08/2020"), 
                                                    onclick = "openTab('SubSidebarEleicao')",
                                                    href = "#"),
                                                  subtitle = "Data limite: 15º dia útil", 
                                                  icon = icon("fas fa-money-check-alt"), 
                                                  color = "red",
                                                  fill = TRUE)})

  
}

shinyApp(ui, server)