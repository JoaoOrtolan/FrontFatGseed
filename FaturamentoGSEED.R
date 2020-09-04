library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmltools)
library(plotly)
library(shinyDND)
library(shinyBS)
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
  dashboardBody(useShinyjs(),tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")), 
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
      fluidRow(infoBoxOutput("InfoBoxModelo"),infoBoxOutput("InfoBoxSinalVital"),infoBoxOutput("InfoBoxEleicao"))),
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
  Dados_Tela_Inicial <- read.csv(paste0(Caminho,"Referencias_Tela_Inicial.csv"), sep=";", encoding ="UTF-8")
  
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
  
  
  
  
  
  
  
  
  observeEvent(input$DateInputSidebar, {
    Dados_Tela_Inicial_Filt <- Dados_Tela_Inicial[trimws(as.character(Dados_Tela_Inicial[,"Anomes"]))==substr(gsub("-", "", input$DateInputSidebar),1,6),]
    
    if(nrow(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="PISOS",])==0) {
      output$InfoBoxPisos <- renderInfoBox({infoBox(tags$strong("Pisos"), 
                                                    paste0("Não processada"), 
                                                    subtitle = "Data limite: 12º dia útil", 
                                                    icon = icon("list"),
                                                    color = "light-blue",
                                                    fill = TRUE)})
    }else {
      Comentario_Pisos <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="PISOS","Comentario"])
      Data_exec_aux <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="PISOS","Data_Exec"])
      Data_exec_Pisos <- paste0(substr(Data_exec_aux, 7,8), "/", substr(Data_exec_aux, 5,6), "/", substr(Data_exec_aux, 1,4))
      output$InfoBoxPisos <- renderInfoBox({infoBox(tags$strong("Pisos"), 
                                                    a(paste0("Processada: ", Data_exec_Pisos), 
                                                      onclick = "openTab('SubSidebarPisos')",
                                                      href = "#"),
                                                    subtitle = "Data limite: 12º dia útil", 
                                                    icon = icon("list"), 
                                                    color = as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="PISOS","Farol"]),
                                                    fill = TRUE)})}
    
    if(nrow(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BC",])==0) {
    output$InfoBoxBC <- renderInfoBox({infoBox(tags$strong("BC"), 
                                               paste0("Não processada"), 
                                               subtitle = "Data limite: 6º dia útil", 
                                               icon = icon("list"), 
                                               color = "light-blue",
                                               fill = TRUE)})
    }else {
    Comentario_BC <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BC","Comentario"])
    Data_exec_aux <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BC","Data_Exec"])
    Data_exec_BC <- paste0(substr(Data_exec_aux, 7,8), "/", substr(Data_exec_aux, 5,6), "/", substr(Data_exec_aux, 1,4))
    output$InfoBoxBC <- renderInfoBox({infoBox(tags$strong("BC"), 
                                               a(paste0("Processada: ", Data_exec_BC), 
                                                 onclick = "openTab('SubSidebarBC')",
                                                 href = "#"),
                                               subtitle = "Data limite: 6º dia útil", 
                                               icon = icon("list"), 
                                               color = as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BC","Farol"]),
                                               fill = TRUE)})}
    
    if(nrow(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BACEN",])==0) {
    output$InfoBoxBacen <- renderInfoBox({infoBox(tags$strong("Bacen"), 
                                                  paste0("Não processada"), 
                                                  subtitle = "Data limite: 6º dia útil", 
                                                  icon = icon("list"),
                                                  color = "light-blue",
                                                  fill = TRUE)})
    }else {
    Comentario_Bacen <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BACEN","Comentario"])
    Data_exec_aux <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BACEN","Data_Exec"])
    Data_exec_Bacen <- paste0(substr(Data_exec_aux, 7,8), "/", substr(Data_exec_aux, 5,6), "/", substr(Data_exec_aux, 1,4))
    output$InfoBoxBacen <- renderInfoBox({infoBox(tags$strong("Bacen"), 
                                                  a(paste0("Processada: ", Data_exec_Bacen), 
                                                    onclick = "openTab('SubSidebarBacen')",
                                                    href = "#"),
                                                  subtitle = "Data limite: 6º dia útil", 
                                                  icon = icon("list"), 
                                                  color = as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="BACEN","Farol"]),
                                                  fill = TRUE)})}
    
    if(nrow(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="MODELO",])==0) {
    output$InfoBoxModelo <- renderInfoBox({infoBox(tags$strong("Modelo"), 
                                                   paste0("Não processada"), 
                                                   subtitle = "Data limite: 6º dia útil", 
                                                   icon = icon("list"),
                                                   color = "light-blue",
                                                   fill = TRUE)})
    }else {
    Comentario_Modelo <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="MODELO","Comentario"])
    Data_exec_aux <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="MODELO","Data_Exec"])
    Data_exec_Modelo <- paste0(substr(Data_exec_aux, 7,8), "/", substr(Data_exec_aux, 5,6), "/", substr(Data_exec_aux, 1,4))
    output$InfoBoxModelo <- renderInfoBox({infoBox(tags$strong("Modelo"), 
                                                   a(paste0("Processada: ", Data_exec_Modelo), 
                                                     onclick = "openTab('SubSidebarModelo')",
                                                     href = "#"),
                                                   subtitle = "Data limite: 6º dia útil", 
                                                   icon = icon("list"), 
                                                   color = as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="MODELO","Farol"]),
                                                   fill = TRUE)})}
    
    if(nrow(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="SINAL VITAL",])==0) {
    output$InfoBoxSinalVital <- renderInfoBox({infoBox(tags$strong("Sinal Vital"), 
                                                       paste0("Não processada"), 
                                                       subtitle = "Data limite: 5º dia útil", 
                                                       icon = icon("list"),
                                                       color = "light-blue",
                                                       fill = TRUE)})
    }else {
    Comentario_Sinal_Vital <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="SINAL VITAL","Comentario"])
    Data_exec_aux <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="SINAL VITAL","Data_Exec"])
    Data_exec_Sinal_Vital <- paste0(substr(Data_exec_aux, 7,8), "/", substr(Data_exec_aux, 5,6), "/", substr(Data_exec_aux, 1,4))
    output$InfoBoxSinalVital <- renderInfoBox({infoBox(tags$strong("Sinal Vital"), 
                                                       a(paste0("Processada: ", Data_exec_Sinal_Vital), 
                                                         onclick = "openTab('SubSidebarSinalVital')",
                                                         href = "#"),
                                                       subtitle = "Data limite: 5º dia útil", 
                                                       icon = icon("list"), 
                                                       color = as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="SINAL VITAL","Farol"]),
                                                       fill = TRUE)})}
    
    if(nrow(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="ELEIÇÃO",])==0) {
    output$InfoBoxEleicao <- renderInfoBox({infoBox(tags$strong("Eleição"),
                                                    paste0("Não processada"), 
                                                    subtitle = "Data limite: 12º dia útil", 
                                                    icon = icon("list"),
                                                    color = "light-blue",
                                                    fill = TRUE)})
    }else {
    Comentario_Eleicao <<- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="ELEIÇÃO","Comentario"])
    Data_exec_aux <- as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="ELEIÇÃO","Data_Exec"])
    Data_exec_Eleicao <- paste0(substr(Data_exec_aux, 7,8), "/", substr(Data_exec_aux, 5,6), "/", substr(Data_exec_aux, 1,4))
    output$InfoBoxEleicao <- renderInfoBox({infoBox(tags$strong("Eleição"),
                                                    a(paste0("Processada: ", Data_exec_Eleicao), 
                                                      onclick = "openTab('SubSidebarEleicao')",
                                                      href = "#"),
                                                    subtitle = "Data limite: 12º dia útil", 
                                                    icon = icon("list"), 
                                                    color = as.character(Dados_Tela_Inicial_Filt[Dados_Tela_Inicial_Filt[,1]=="ELEIÇÃO","Farol"]),
                                                    fill = TRUE)})
    if(!identical(Comentario_Eleicao, character(0))){}}}
    
    
    
    )}

shinyApp(ui, server)