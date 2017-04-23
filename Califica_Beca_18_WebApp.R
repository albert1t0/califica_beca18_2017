### Web App para calificación Beca 18

#### Librerias ####

library(CTT)
library(xlsx)

#### Funciones de apoyo ####

califica.lec <- function( data, clave) {
      datos <- data[,2:21]
      clave <- clave[1:20]
      resultado <- score(datos,clave, ID = data[,1])
      return(resultado)
}

califica.red <- function( data, clave) {
      datos <- data[,22:41]
      clave <- clave[21:40]
      resultado <- score(datos,clave, ID = data[,1])
      return(resultado)
}

califica.mat <- function( data, clave) {
      datos <- data[,42:65]
      clave <- clave[41:64]
      resultado <- score(datos,clave, ID = data[,1])
      return(resultado)
}

#### Página de aplicación ####

ui <-  fluidPage(
      headerPanel("Califica Beca 18 - Convocatoria 2017"),
      hr(),
      
      navlistPanel(
            tabPanel("Instrucciones",
                  h3("Instrucciones"),
                  p("Este aplicación Web realiza la calificación siguiendo los siguientes paso:"),
                  tags$ol(list(
                        tags$li("Carga de los archivos de respuestas."),
                        tags$li("Verificación de los datos cargados."),
                        tags$li("Revisión de número de correctas"),
                        tags$li("Revisión de resultados finales"),
                        tags$li("Descarga de resultados")
                  ))
            ),
            tabPanel("1. Cargar",
                  h3("Carga de archivos"),
                  hr(),
                  fileInput("archivo", "Archivo de datos (.csv):",
                            multiple = FALSE, 
                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                  fileInput("clave", "Claves en formato (.csv):",
                            multiple = FALSE, 
                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                  fileInput("tabla", "Tabla de calificación (.csv):",
                            multiple = FALSE, 
                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                  actionButton("procesar", "Procesar", icon = icon("gear")),
                  hr(),
                  h4(textOutput("registrosCargados1")),
                  h4(textOutput("registrosCargados2")),
                  h4(textOutput("registrosCargados3"))
            ),
            tabPanel("2. Revisar Tablas",      
                  tabsetPanel(
                        tabPanel("Data",
                                 tableOutput("sobrearchivo1"),
                                 tableOutput("sobrearchivo2")
                                 ),
                        tabPanel("Claves",
                                 tableOutput("sobreclaves")
                                 ),
                        tabPanel("Tabla Calificación",
                                 tableOutput("sobretabla")
                                 )
                  )
            ),
            tabPanel("3. Correctas",
                     tableOutput("resultado")
                     ),
            tabPanel("4. Puntaje Final",
                     tableOutput("escala")
                     ),
            tabPanel("5. Descarga de Archivo de Resultados",
                     h3("Descargar archivo de resultados en formato XLSX"),
                     downloadButton("descargaResultado", "Descargar resultado", icon = icon("download"))
                     )
            
      )
)

#### Proceso de datos - Servidor ####

server <-  function(input, output, session) {
      
      datos <- eventReactive(
            input$procesar, {
            req(input$archivo)
            inFile <- input$archivo
            read.csv(inFile$datapath, as.is = TRUE, header = FALSE, colClasses = rep("character",66), stringsAsFactors = FALSE)
      })
      
      claves <- eventReactive(
            input$procesar, {
            req(input$clave)
            inFile <- input$clave
            read.csv(inFile$datapath, as.is = TRUE, header = TRUE)
      })
      
      tablas <- eventReactive(
            input$procesar, {
            req(input$tabla)
            inFile <- input$tabla
            read.csv(inFile$datapath, as.is = TRUE, header = TRUE)
      })
      
      output$registrosCargados1 <- renderText(paste0("Registros cargados: ", dim(datos())[1]))
      output$registrosCargados2 <- renderText(paste0("Número de claves: ", dim(claves())[1]))
      output$registrosCargados3 <- renderText(paste0("Número de registros Tabla (incluye el cero): ", dim(tablas())[1]))

      output$sobrearchivo1 <- renderTable({
            data <- datos()
            num.registros <- dim(data)[1]
            num.variables <- dim(data)[2]
            imp.registros <- ifelse (num.registros < 5, num.registros, 5)
            registros <- head(data, imp.registros)
            
            salida <- data.frame(
                  cbind(c("Número de Registros", "Número de ítems"), 
                        c(num.registros, num.variables-2))
            )
            names(salida) <- c("Característica","Valor")
            
            return(salida)
      })
      
      output$sobrearchivo2 <- renderTable({
            data <- datos()
            head(data,5)
      })
      
      output$sobreclaves <- renderTable({
            claves()
      })
      
      output$sobretabla <- renderTable({
            tablas()
      })
      
      correctas <- reactive({
            data <- datos()
            clave <- claves()
            resultado.lec <- califica.lec(data,clave$correcta)
            resultado.red <- califica.red(data,clave$correcta)
            resultado.mat <- califica.mat(data,clave$correcta)
            
            calificado <- data.frame(cbind(
                  data[,1],
                  resultado.lec$score,
                  resultado.red$score,
                  resultado.mat$score)
            )
            names(calificado) <- c("identificador", "correctas.lectura", "correctas.redacción", "correctas.matemática")
            return(calificado)
      })
      
      output$resultado <- renderTable({
            tabla <- correctas()
            return(tabla)
      })
      
      resultado.final <- reactive({
            req(input$archivo, input$clave, input$tabla)
            tabla <- correctas()
            califica <- tablas()
            tabla <- merge(tabla, califica[,c(1,3)], by.x = "correctas.lectura", by.y = "correctas")
            tabla <- merge(tabla, califica[,c(1,2)], by.x = "correctas.redacción", by.y = "correctas")
            tabla <- merge(tabla, califica[,c(1,4)], by.x = "correctas.matemática", by.y = "correctas")
            resultado <- tabla[,c(4,1:3,5:7)]
            resultado$puntaje.final <- as.integer(round(.5 * resultado$matemática + 
                                              .25 * resultado$redacción + 
                                              0.25 * resultado$lectura,0))
            return(resultado[order(resultado$puntaje.final, decreasing = TRUE),])            
      })
      
      output$escala <- renderTable({
            resultado.final()
      })
      
      output$descargaResultado <- downloadHandler(
            filename = function() {
                  paste("resultado-beca18-", Sys.Date(),".xlsx", sep = "") 
                  },
            content = function(file) {
                  write.xlsx(resultado.final(), file, row.names = FALSE)
            }
      )
}

shinyApp( ui = ui, server = server)


