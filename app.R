# HidroTelemetria

library(shiny)
library(shinydashboard)
library(leaflet)
library(XML)
library(plotly)
library(tidyr)
library(readr)
library(dplyr)
library(forecast)


Telemetricas <- read_csv2("dados/Telemetricas.csv")
Inventario <- read_delim("dados/Inventario.csv", delim = ";", locale = locale(encoding = "latin1", dec = ",")) %>%
    filter(Codigo %in% Telemetricas$Codigo,
           OperadoraSigla %in% c("CPRM", "ESBR"))
NiveisReferencia <- read_csv2("dados/Est_SACE.csv")

# Define barra lateral
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapa Iterativo", tabName = "mapa", icon = icon("map")),
        menuItem("Tabela Dinâmica", icon = icon("table"), tabName = "data"),
        menuItem("Series Históricas", icon = icon("table"), tabName = "series"),
        menuItem("Sobre o aplicativo", icon = icon("info"), tabName = "info")
        )
    )

# Define corpo do dashboard
body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "mapa",
                div(class = "outer",
                    tags$head(
                        includeCSS("styles.css"),
                        includeScript("gomap.js")
                    ),
                    
                    leafletOutput("mymap", width = "100%", height = "100%"),
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 100, left = "auto",
                                  right = 80, bottom = "auto",
                                  width = 430, height = "auto",
                                  
                                  h1("Estações telemétricas da Rede Hidrometerológica Nacional"))
                    )
                
                ),
        
        tabItem(tabName = "data",
                box(width = 12, title = "Aplicação de filtros",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                        box(width = 12, title = "Selecione dados para visualização da tabela",
                                status = "info", solidHeader = TRUE, collapsible = TRUE,
                                checkboxGroupInput("show_vars", "Dados para visualização:",
                                                   names(Inventario)[!c(names(Inventario) %in%
                                                                            c("Codigo", "Estacao", "EstadoNome", "SubBacia", "Municipio","ResponsavelSigla", "Action"))],
                                                   inline = TRUE)
                                )
                            ),
                    
                    fluidRow(
                        box(width = 12, title = "Aplique filtros",
                            status = "info", solidHeader = TRUE, collapsible = TRUE,
                            column(4,
                                   selectInput("estados", strong("Estados"),
                                               choices = unique(Inventario$EstadoNome),
                                               multiple = TRUE)
                            ),
                            
                            column(4,
                                   selectInput("subbacia", strong("Sub-Bacia"),
                                               choices = unique(Inventario$SubBacia),
                                               multiple = TRUE)
                            ),
                            column(4,
                                   selectInput("municipio", strong("Municipio"),
                                               choices = unique(Inventario$Municipio),
                                               multiple = TRUE)
                            )
                        )
                    ),
                        
                     fluidRow(
                         box(width = 12, title = "Tabela dinâmica",
                             status = "info", solidHeader = TRUE, 
                             DT::dataTableOutput("inventario")
                             )
                         )
                    )
                ),
        
        tabItem(tabName = "series",
                box(width = 12, title = "Visualização de dados",
                    status = "primary", solidHeader = TRUE,
                    
                    fluidRow(box(width = 12, title = "Parâmetros de entrada",
                                 status = "info", solidHeader = TRUE,  collapsible = TRUE,
                                 
                                 column(4,selectInput("Codigo", strong("Código da estação"),
                                                      choices = unique(Inventario$Codigo))),
                                 
                                 column(4,dateRangeInput("Periodo", strong("Período de dados"),
                                                         start = Sys.Date()-7)),
                                 
                                 actionButton("gera", "Buscar dados!")
                                 ),
                             ),
                    fluidRow(
                        box(width = 4, title = "Tabela de dados",
                            status = "info", solidHeader = TRUE,  collapsible = TRUE,
                            DT::dataTableOutput("dt_tabela"),
                            tags$hr(),
                            downloadButton("downloadData", 'Baixar série')
                            ),
                        
                        box(width = 8, title = "Gráfico",
                            status = "info", solidHeader = TRUE,  collapsible = TRUE,
                            plotlyOutput("grafico"))
                        
                        ),
                    fluidRow(
                        box(width = 4, title = "Previsão de níveis pela tendência",
                            status = "info", solidHeader = TRUE,  collapsible = TRUE,
                            
                            selectInput("Modelo", strong("Modelo de Previsão"),
                                                 choices = c("ARIMA","Exponencial")),
                            
                            actionButton("preve", "Roda previsão!"),
                            
                            tags$hr(),
                            
                            tableOutput("Resultado")
                            ),
                        
                        box(width = 8, title = "Gráfico de previsão",
                            status = "info", solidHeader = TRUE,  collapsible = TRUE,
                            
                            plotlyOutput("grafPrev")
                            )
                        )
                    )
                ),
        tabItem(tabName = "info",
                box(width = 12, title = "Informações sobre o aplicativo",
                    status = "primary", solidHeader = TRUE,
                    includeHTML("info_text.html")
                    )
        )
        
    )
)


ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = span(img(src = "logomarcacprmhorizontal_v2.jpg",
                                                     height = 40, width = 40*5),"\t",
                                                 ". Séries Hidrológicas Telemétricas/SGB"),
                                    titleWidth = 650),
                    sidebar,
                    body
)

server <- function(input, output, session) {
    
    tabela <- reactive({
        return(Inventario)
    })
    
    showStationPopup <- function(codigo, lat, lng) {
        tabela <- tabela()
        selectedCode <- tabela[tabela$Codigo == codigo,]
        content <- as.character(tagList(
            tags$h4("Estação:", selectedCode$Estacao), tags$br(),
            sprintf("Codigo da estação: %s", selectedCode$Codigo), tags$br(),
            sprintf("Estado: %s", selectedCode$EstadoNome), tags$br(),
            sprintf("Municipio: %s", selectedCode$Municipio), tags$br(),
            sprintf("Area de drenagem: %s km2", selectedCode$AreaDrenagem), tags$br(),
            sprintf("Responsavel: %s ", selectedCode$ResponsavelSigla), tags$br(),
            sprintf("Operador: %s ", selectedCode$OperadoraSigla)
        ))
        leafletProxy("mymap") %>% 
            addPopups(lng = lng, lat = lat, content, layerId = codigo)
    }
    
    observe({
        leafletProxy("mymap") %>% clearPopups()
        event <- input$mymap_marker_click
        if (is.null(event))
            return()
        
        isolate({
            showStationPopup(event$id, event$lat, event$lng)
        })
    })
    
    output$mymap <- renderLeaflet({
        leaflet(data = tabela()) %>%
            addProviderTiles("Esri.WorldTopoMap") %>%
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                             color = rgb(0.3,0.1,0.8), radius = 7,
                             #clusterOptions = markerClusterOptions(),
                             label = ~as.character(Codigo),
                             stroke = FALSE, fillOpacity = 0.4,
                             layerId = ~Codigo)
    })
    
    output$inventario <- DT::renderDataTable({
        df <- tabela() %>%
            filter(is.null(input$estados) | EstadoNome %in% input$estados) %>%
            filter(is.null(input$subbacia) | SubBacia %in% input$subbacia) %>%
            filter(is.null(input$municipio) | Municipio %in% input$municipio) %>%
            mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-codigo="', Codigo, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
            dplyr::select(c("Codigo", "Estacao", "EstadoNome", "SubBacia", "Municipio", input$show_vars, "Action"))
        
        action <- DT::dataTableAjax(session, df, outputId = "Codigo")
        
        DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
        
        
    })
    
    url_add <- function(Estacao,Inicio,Fim){
        paste0(
            "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/DadosHidrometeorologicosGerais?CodEstacao=",
            Estacao,
            "&DataInicio=",Inicio,"&DataFim=",Fim)
    }
    
    serieHistorica <- eventReactive(input$gera, {
        
        Url <- url_add(input$Codigo,format(input$Periodo[1], "%d/%m/%Y"),format(input$Periodo[2], "%d/%m/%Y"))
        doc <- xmlTreeParse(Url, useInternal = TRUE)
        rootNode <- xmlRoot(doc)
        
        if(length(xpathSApply(rootNode[[2]][[1]],"//NivelSensor",xmlValue)) == 0){
            return("Estação sem dados disponíveis para o período selecionado")
        } else {
            Dados <- data.frame("Codigo" = xpathSApply(rootNode[[2]][[1]],"//CodEstacao",xmlValue),
                                "DataHora" = xpathSApply(rootNode[[2]][[1]],"//DataHora",xmlValue),
                                "ChuvaHoraria" = xpathSApply(rootNode[[2]][[1]],"//ChuvaFinal",xmlValue),
                                "Nivel" = xpathSApply(rootNode[[2]][[1]],"//NivelSensor",xmlValue),
                                "Vazao" = xpathSApply(rootNode[[2]][[1]],"//VazaoFinal",xmlValue)) %>%
                mutate(DataHora = as.POSIXct(DataHora, tzone = "GMT") - 2*60*60,
                       ChuvaHoraria = as.numeric(as.character(ChuvaHoraria)),
                       Nivel = as.numeric(as.character(Nivel)),
                       Vazao = as.numeric(as.character(Vazao))) %>%
                filter(complete.cases(Nivel)) %>%
                dplyr::arrange(desc(DataHora))
            return(Dados)
        }
        
        
        
    })
    
    output$dt_tabela <- DT::renderDataTable({
        serieHistorica()
    })
    
    output$grafico <- renderPlotly({
        
        tryCatch({
            df <- serieHistorica()
            Codigo <- input$Codigo
            paleta <- c(rgb(0.8,0.2,0.2,0.8), rgb(1,0.5,0,0.8), rgb(0.9,0.9,0,0.8), rgb(0,0.6,0,0.8))
            
            Niveis <- NiveisReferencia[NiveisReferencia$Codigo == Codigo,]
            if(nrow(Niveis) == 0){
                
                plot_ly(x = df$DataHora, y = df$Nivel,
                        type = "scatter", mode = "lines", name = "Dado observado",
                        hoverinfo = "text",
                        text = ~paste("</br> Data:", df$DataHora,
                                      "</br> Cota: ", df$Nivel, "cm")) %>%
                    layout(title = paste("Niveis em", as.character(Inventario[Inventario$Codigo == Codigo,"Estacao"])),
                           xaxis = list(title = "Data"),
                           yaxis = list (title = "Nível (cm)"))
                
            } else {
                
                plot_ly(x = df$DataHora, y = df$Nivel,
                        type = "scatter", mode = "lines", name = "Dado observado",
                        hoverinfo = "text",
                        text = ~paste("</br> Data:", df$DataHora,
                                      "</br> Cota: ", df$Nivel, "cm")) %>%
                    layout(title = paste("Niveis em", as.character(Inventario[Inventario$Codigo == Codigo,"Estacao"])),
                           xaxis = list(title = "Data"),
                           yaxis = list (title = "Nível (cm)")) %>%
                    add_segments(x = df$DataHora[1], xend = tail(df$DataHora,1),
                                 y = Niveis$Inundacao, yend = Niveis$Inundacao, name = "Inundação",
                                 color = I(paleta[1])) %>%
                    add_segments(x = df$DataHora[1], xend = tail(df$DataHora,1),
                                 y = Niveis$Alerta, yend = Niveis$Alerta, name = "Alerta",
                                 color = I(paleta[2])) %>%
                    add_segments(x = df$DataHora[1], xend = tail(df$DataHora,1),
                                 y = Niveis$Atencao, yend = Niveis$Atencao, name = "Atenção",
                                 color = I(paleta[3]))
                
                }
            },
            error = function(e) {
                stop(safeError(e))
            })
            
    })
    
    output$downloadData <- downloadHandler(
        filename = function() paste0(input$Codigo,".csv"),
        content = function(file) {
            write.csv2(serieHistorica(), file, row.names = FALSE)
        }
    )
    
    
    modelo <- eventReactive(input$preve, {
        df <- serieHistorica() %>%
            arrange(DataHora)
        
        if(input$Modelo == "Exponencial"){
            
            fit <- ets(df$Nivel) 
            
        } else {
            fit <- auto.arima(df$Nivel) 
        }
        
        return(fit)
    })
    
    output$Resultado <- renderTable({
        return(summary(modelo()))
    })
    
    output$grafPrev <- renderPlotly({
        Prev <- modelo() %>%
            forecast(h = 24)
        
        
        df <- serieHistorica() %>%
            arrange(DataHora)
        
        Codigo <- input$Codigo
        paleta <- c(rgb(0.8,0.2,0.2,0.8), rgb(1,0.5,0,0.8), rgb(0.9,0.9,0,0.8), rgb(0,0.6,0,0.8))
        
        Niveis <- NiveisReferencia[NiveisReferencia$Codigo == Codigo,]
        if(nrow(Niveis) == 0){
            
            plot_ly(x = max(df$DataHora) + c(0:24)*60*60, y = c(tail(df$Nivel,1),as.numeric(Prev$mean)),
                    type = "scatter", mode = "lines", name = "Dado observado") %>%
                add_trace(x = df$DataHora,
                          y = df$Nivel,
                          mode = 'lines',name = "Previsão") %>%
                add_ribbons(x = max(df$DataHora) + c(0:24)*60*60,
                            ymin = c(tail(df$Nivel,1),as.numeric(Prev$lower[,1])),
                            ymax = c(tail(df$Nivel,1),as.numeric(Prev$upper[,1])),
                            line = list(color = rgb(0.2,0.2,0.8,0.2)),
                            fillcolor = rgb(0.2,0.2,0.8,0.2),
                            name = "IP 80%") %>%
                add_ribbons(x = max(df$DataHora) + c(0:24)*60*60,
                            ymin = c(tail(df$Nivel,1),as.numeric(Prev$lower[,2])),
                            ymax = c(tail(df$Nivel,1),as.numeric(Prev$upper[,2])),
                            line = list(color = rgb(0.2,0.2,0.8,0.2)),
                            fillcolor = rgb(0.2,0.2,0.8,0.2),
                            name = "IP 95%") %>%
                layout(title = paste("Niveis em", as.character(Inventario[Inventario$Codigo == Codigo,"Estacao"])),
                       xaxis = list(title = "Data"),
                       yaxis = list (title = "Nível (cm)")) 
            
        } else {
            
            plot_ly(x = max(df$DataHora) + c(0:24)*60*60, y = c(tail(df$Nivel,1),as.numeric(Prev$mean)),
                    type = "scatter", mode = "lines", name = "Dado observado") %>%
                add_trace(x = df$DataHora,
                          y = df$Nivel,
                          mode = 'lines',name = "Previsão") %>%
                add_ribbons(x = max(df$DataHora) + c(0:24)*60*60,
                            ymin = c(tail(df$Nivel,1),as.numeric(Prev$lower[,1])),
                            ymax = c(tail(df$Nivel,1),as.numeric(Prev$upper[,1])),
                            line = list(color = rgb(0.2,0.2,0.8,0.2)),
                            fillcolor = rgb(0.2,0.2,0.8,0.2),
                            name = "IP 80%") %>%
                add_ribbons(x = max(df$DataHora) + c(0:24)*60*60,
                            ymin = c(tail(df$Nivel,1),as.numeric(Prev$lower[,2])),
                            ymax = c(tail(df$Nivel,1),as.numeric(Prev$upper[,2])),
                            line = list(color = rgb(0.2,0.2,0.8,0.2)),
                            fillcolor = rgb(0.2,0.2,0.8,0.2),
                            name = "IP 95%") %>%
                layout(title = paste("Niveis em", as.character(Inventario[Inventario$Codigo == Codigo,"Estacao"])),
                       xaxis = list(title = "Data"),
                       yaxis = list (title = "Nível (cm)")) %>%
                add_segments(x = df$DataHora[1], xend = tail(df$DataHora,1)+24*3600,
                             y = Niveis$Inundacao, yend = Niveis$Inundacao, name = "Inundação",
                             color = I(paleta[1])) %>%
                add_segments(x = df$DataHora[1], xend = tail(df$DataHora,1)+24*3600,
                             y = Niveis$Alerta, yend = Niveis$Alerta, name = "Alerta",
                             color = I(paleta[2])) %>%
                add_segments(x = df$DataHora[1], xend = tail(df$DataHora,1)+24*3600,
                             y = Niveis$Atencao, yend = Niveis$Atencao, name = "Atenção",
                             color = I(paleta[3]))
            
        }
        
        
    })
    

}

shinyApp(ui, server)