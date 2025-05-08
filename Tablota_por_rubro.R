library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Función para crear la tabla descriptiva para un rubro específico
crear_tabla_rubro <- function(tabla_comparativa, rubro_base, deciles = c("Nacional", "I", "V", "X")) {
  
  # Identificar las columnas relacionadas con este rubro
  cols_rubro <- grep(paste0("^", rubro_base, "_"), names(tabla_comparativa), value = TRUE)
  
  # Identificar las columnas de gasto total
  cols_gasto_total <- grep("^Gasto corriente monetario_", names(tabla_comparativa), value = TRUE)
  
  # Filtrar las filas para los deciles seleccionados
  datos_filtrados <- tabla_comparativa %>%
    filter(Decil %in% deciles)
  
  # Crear una tabla vacía para almacenar los resultados
  resultado <- data.frame(
    Decil = datos_filtrados$Decil,
    Pesos_2018 = NA,
    Porcentaje_2018 = NA,
    Pesos_2020 = NA,
    Porcentaje_2020 = NA,
    Pesos_2022 = NA,
    Porcentaje_2022 = NA,
    Media = NA,
    Desviacion = NA
  )
  
  # Llenar los datos para cada año
  for (i in 1:nrow(resultado)) {
    decil_actual <- resultado$Decil[i]
    fila_datos <- datos_filtrados[datos_filtrados$Decil == decil_actual, ]
    
    # 2018
    valor_2018 <- fila_datos[[paste0(rubro_base, "_2018")]]
    gasto_total_2018 <- fila_datos[["Gasto corriente monetario_2018"]]
    resultado$Pesos_2018[i] <- valor_2018
    resultado$Porcentaje_2018[i] <- 100 * valor_2018 / gasto_total_2018
    
    # 2020
    valor_2020 <- fila_datos[[paste0(rubro_base, "_2020")]]
    gasto_total_2020 <- fila_datos[["Gasto corriente monetario_2020"]]
    resultado$Pesos_2020[i] <- valor_2020
    resultado$Porcentaje_2020[i] <- 100 * valor_2020 / gasto_total_2020
    
    # 2022
    valor_2022 <- fila_datos[[paste0(rubro_base, "_2022")]]
    gasto_total_2022 <- fila_datos[["Gasto corriente monetario_2022"]]
    resultado$Pesos_2022[i] <- valor_2022
    resultado$Porcentaje_2022[i] <- 100 * valor_2022 / gasto_total_2022
    
    # Estadísticas
    valores <- c(valor_2018, valor_2020, valor_2022)
    resultado$Media[i] <- mean(valores)
    resultado$Desviacion[i] <- sd(valores)
  }
  
  return(resultado)
}

# Crear tablas para los tres rubros principales
tabla_alimentos <- crear_tabla_rubro(tabla_comparativa, "Alimentos")
tabla_transporte <- crear_tabla_rubro(tabla_comparativa, "Transporte")
tabla_educacion <- crear_tabla_rubro(tabla_comparativa, "Educación y esparcimiento")

# Función para formatear una tabla individual
formatear_tabla <- function(tabla, titulo) {
  kable(tabla, 
        caption = titulo,
        col.names = c("Decil", 
                      "Pesos", "%", 
                      "Pesos", "%", 
                      "Pesos", "%", 
                      "Media", "Desv."),
        align = c('l', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r'),
        digits = c(0, 0, 1, 0, 1, 0, 1, 0, 0)) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  full_width = FALSE) %>%
    add_header_above(c(" " = 1, 
                       "2018" = 2, 
                       "2020" = 2, 
                       "2022" = 2, 
                       "Estadísticas" = 2))
}

# Formatear las tres tablas
tabla_alimentos_formateada <- formatear_tabla(tabla_alimentos, "Alimentos, bebidas y tabaco")
tabla_transporte_formateada <- formatear_tabla(tabla_transporte, "Transporte y comunicaciones")
tabla_educacion_formateada <- formatear_tabla(tabla_educacion, "Educación y esparcimiento")

# Mostrar las tablas
tabla_alimentos_formateada
tabla_transporte_formateada
tabla_educacion_formateada

# Para usar fuera de R Markdown, podríamos exportar a HTML
# library(htmltools)
# save_html <- function(kable_output, filename) {
#   html_content <- as.character(kable_output)
#   writeLines(html_content, filename)
# }
# 
# save_html(tabla_alimentos_formateada, "tabla_alimentos.html")
# save_html(tabla_transporte_formateada, "tabla_transporte.html")
# save_html(tabla_educacion_formateada, "tabla_educacion.html")