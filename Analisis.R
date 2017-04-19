### Calificación de Beca 18 - 2017

### Función para aproximar el TRUE SCORE

true.score <- function( dif.items, hab.inf = -6, hab.sup = 6) {
      puntaje <- 0
      tabla <- NULL
      for(i in seq(hab.inf,hab.sup,0.001)) {
            score <- c(i,sum(exp(i-dif.items)/(1 + exp(i-dif.items))))
            if (puntaje == round( score[2] - 0.5, 0 )) {
                  tabla <- rbind(tabla, c(puntaje,score))
                  puntaje <- puntaje + 1
            }
      }
      tabla <- as.data.frame(tabla)
      names(tabla) <- c('Puntaje', 'Ligitos', 'Puntaje aproximado')
      tabla
}

### Carga de estructuras 

library(xlsx)
estructura.sabado <- read.xlsx2("V1_Sábado_PROVINCIA_Estructura_Beca_18_2017.xls", sheetIndex = 1, 
                               header = TRUE, colClasses = c(rep("character",4), rep("numeric",3), 
                                                             "integer","character"), stringsAsFactors = FALSE)
estructura.domingo <- read.xlsx2("V2_domingo_Prov_y_Lima_-_Estructura_Beca_18_2017.xls", sheetIndex = 1, 
                                header = TRUE, colClasses = c(rep("character",4), rep("numeric",3), 
                                                              "integer","character"), stringsAsFactors = FALSE)

### Tablas para sistema de calificación

tabla.sabado <-
      cbind(
            0:24,
            c(true.score(estructura.domingo[estructura.sabado$Competencia == "014", 6])[, 2] * 100 + 500, 1000, rep(NA, 4)),
            c(true.score(estructura.domingo[estructura.sabado$Competencia == "013", 6])[, 2] * 100 + 500, 1000, rep(NA, 4)),
            c(true.score(estructura.domingo[estructura.sabado$Competencia == "011", 6])[, 2] * 100 + 500, 1000)
      )
tabla.sabado[1,] <- rep(0,4)
tabla.sabado <- data.frame(tabla.sabado)
names(tabla.sabado) <- c("correctas", "redacción", "lectura", "matemática")

tabla.domingo <-
      cbind(
            0:24,
            c(true.score(estructura.domingo[estructura.domingo$Competencia == "014", 6])[, 2] * 100 + 500, 1000, rep(NA, 4)),
            c(true.score(estructura.domingo[estructura.domingo$Competencia == "013", 6])[, 2] * 100 + 500, 1000, rep(NA, 4)),
            c(true.score(estructura.domingo[estructura.domingo$Competencia == "011", 6])[, 2] * 100 + 500, 1000)
      )
tabla.domingo[1,] <- rep(0,4)
tabla.domingo <- data.frame(tabla.domingo)
names(tabla.domingo) <- c("correctas", "redacción", "lectura", "matemática")

write.xlsx(tabla.sabado, "puntajes_beca_18_sabado.xlsx", sheetName = "Beca 18 - sábado", col.names = TRUE, row.names = FALSE, showNA = FALSE)
write.xlsx(tabla.domingo, "puntajes_beca_18_domingo.xlsx", sheetName = "Beca 18 - domingo", col.names = TRUE, row.names = FALSE, showNA = FALSE)

write.csv(tabla.sabado, "puntajes_beca_18_sabado.csv", row.names = FALSE)
write.csv(tabla.domingo, "puntajes_beca_18_domingo.csv", row.names = FALSE)


#### Lectura de archivo de calificación




