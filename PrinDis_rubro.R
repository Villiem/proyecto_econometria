library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)

# Función para calcular datos de un rubro específico
calcular_rubro <- function(tabla_comparativa, rubro_base, deciles = c("Nacional", "I", "V", "X")) {
  
  # Filtrar los deciles seleccionados
  datos_filtrados <- tabla_comparativa %>%
    filter(Decil %in% deciles)
  
  # Columnas de gasto total
  col_gasto_2018 <- "Gasto corriente monetario_2018"
  col_gasto_2020 <- "Gasto corriente monetario_2020"
  col_gasto_2022 <- "Gasto corriente monetario_2022"
  
  # Columnas del rubro específico
  col_rubro_2018 <- paste0(rubro_base, "_2018")
  col_rubro_2020 <- paste0(rubro_base, "_2020")
  col_rubro_2022 <- paste0(rubro_base, "_2022")
  
  # Crear tabla de resultados
  resultado <- datos_filtrados %>%
    select(Decil, 
           !!col_rubro_2018, !!col_gasto_2018,
           !!col_rubro_2020, !!col_gasto_2020,
           !!col_rubro_2022, !!col_gasto_2022) %>%
    mutate(
      # Valores en pesos
      pesos_2018 = !!sym(col_rubro_2018),
      pesos_2020 = !!sym(col_rubro_2020),
      pesos_2022 = !!sym(col_rubro_2022),
      
      # Porcentajes
      pct_2018 = 100 * !!sym(col_rubro_2018) / !!sym(col_gasto_2018),
      pct_2020 = 100 * !!sym(col_rubro_2020) / !!sym(col_gasto_2020),
      pct_2022 = 100 * !!sym(col_rubro_2022) / !!sym(col_gasto_2022),
      
      # Estadísticas
      media = round((!!sym(col_rubro_2018) + !!sym(col_rubro_2020) + !!sym(col_rubro_2022)) / 3),
      desviacion = round(sd(c(!!sym(col_rubro_2018), !!sym(col_rubro_2020), !!sym(col_rubro_2022)))),
      
      # Agregar nombre del rubro
      Rubro = rubro_base
    ) %>%
    select(Rubro, Decil, 
           pesos_2018, pct_2018, 
           pesos_2020, pct_2020, 
           pesos_2022, pct_2022, 
           media, desviacion)
  
  return(resultado)
}

# Procesar los cuatro rubros principales
tabla_alimentos <- calcular_rubro(tabla_comparativa, "Alimentos")
tabla_transporte <- calcular_rubro(tabla_comparativa, "Transporte")
tabla_educacion <- calcular_rubro(tabla_comparativa, "Educación y esparcimiento")
tabla_vivienda <- calcular_rubro(tabla_comparativa, "Vivienda")

# Combinar las cuatro tablas
tabla_combinada <- bind_rows(tabla_alimentos, tabla_transporte, tabla_educacion, tabla_vivienda)

# Orden específico de los rubros
orden_rubros <- c("Alimentos", "Transporte", "Educación y esparcimiento", "Vivienda")
tabla_combinada$Rubro <- factor(tabla_combinada$Rubro, levels = orden_rubros)

# Orden específico de los deciles
orden_deciles <- c("Nacional", "I", "V", "X")
tabla_combinada$Decil <- factor(tabla_combinada$Decil, levels = orden_deciles)

# Ordenar la tabla
tabla_combinada <- tabla_combinada %>%
  arrange(Rubro, Decil)

# Crear la tabla GT
tabla_gt <- tabla_combinada %>%
  gt(groupname_col = "Rubro") %>%
  tab_header(
    title = "Estadística descriptiva de los principales rubros de gasto (ENIGH 2018-2022)",
    subtitle = "Valores monetarios y porcentuales por decil de ingreso"
  ) %>%
  # Agregar encabezados de columnas
  cols_label(
    Decil = "Decil",
    pesos_2018 = "Pesos",
    pct_2018 = "%",
    pesos_2020 = "Pesos",
    pct_2020 = "%",
    pesos_2022 = "Pesos",
    pct_2022 = "%",
    media = "Media",
    desviacion = "Desv."
  ) %>%
  # Agregar encabezados de columnas agrupadas
  tab_spanner(
    label = "2018",
    columns = c(pesos_2018, pct_2018)
  ) %>%
  tab_spanner(
    label = "2020",
    columns = c(pesos_2020, pct_2020)
  ) %>%
  tab_spanner(
    label = "2022",
    columns = c(pesos_2022, pct_2022)
  ) %>%
  tab_spanner(
    label = "Estadísticas",
    columns = c(media, desviacion)
  ) %>%
  # Formatear los números
  fmt_number(
    columns = c(pesos_2018, pesos_2020, pesos_2022, media, desviacion),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_percent(
    columns = c(pct_2018, pct_2020, pct_2022),
    decimals = 1,
    scale_values = FALSE
  ) %>%
  # Estilos para la tabla
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_spanners()
  ) %>%
  # Colorear la columna de 2022 para destacarla
  tab_style(
    style = list(
      cell_fill(color = "#f0f5ff")
    ),
    locations = cells_body(
      columns = c(pesos_2022, pct_2022)
    )
  ) %>%
  # Agregar notas al pie
  tab_footnote(
    footnote = "Los valores monetarios están expresados en pesos del año correspondiente",
    locations = cells_column_labels(columns = c(pesos_2018, pesos_2020, pesos_2022))
  ) %>%
  tab_footnote(
    footnote = "Los porcentajes representan la proporción del gasto total destinada a cada rubro",
    locations = cells_column_labels(columns = c(pct_2018, pct_2020, pct_2022))
  ) %>%
  # Agregar fuente
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la ENIGH 2018-2022"
  ) %>%
  # Aplicar tema predefinido
  gt_theme_538()

# Mostrar la tabla
tabla_gt
