# Importar librerías necesarias
library(gt)
library(dplyr)

# Creamos una versión de la tabla sin la columna de gasto corriente monetario
tabla_sin_total <- tabla_deciles %>%
  select(-`Gasto corriente monetario`)

# Agregar la columna "Total" que será la suma de todos los rubros
tabla_con_totales <- tabla_sin_total %>%
  mutate(Total = rowSums(select(., -Decil)))

# Crear tabla de porcentajes (excluyendo el gasto total del cálculo)
tabla_porcentajes <- tabla_con_totales %>%
  mutate(across(-c(Decil, Total), ~.x/Total*100))

# Agregar la columna de 100% para representar que es el total del gasto
tabla_porcentajes <- tabla_porcentajes %>%
  mutate(`Gasto corriente monetario` = 100) %>%
  select(Decil, `Gasto corriente monetario`, everything(), -Total)

# Crear tabla formateada con gt
tabla_porcentajes_gt <- tabla_porcentajes %>%
  gt() %>%
  # Título y subtítulo
  tab_header(
    title = "Distribución porcentual del gasto corriente monetario por deciles de ingreso",
    subtitle = "Porcentaje del gasto total destinado a cada rubro, ENIGH 2022"
  ) %>%
  # Formato de números (un decimal)
  fmt_number(
    columns = -Decil,
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  # Estilo de las columnas
  cols_align(
    align = "center",
    columns = -Decil
  ) %>%
  # Colores por rangos de porcentaje
  data_color(
    columns = -c(Decil, `Gasto corriente monetario`),
    fn = scales::col_numeric(
      palette = c("#E8F4F9", "#D1E6F9", "#A4D1F9", "#63ADF0", "#3D8DD4"),
      domain = c(0, 28)  # Ajustado para el rango máximo de porcentajes
    )
  ) %>%
  # Color para la columna de gasto total
  tab_style(
    style = list(
      cell_fill(color = "#A0A0A0"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = `Gasto corriente monetario`)
  ) %>%
  # Estilo del encabezado
  tab_style(
    style = list(
      cell_fill(color = "#003366"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  # Estilo para la columna de deciles
  tab_style(
    style = list(
      cell_fill(color = "#E6F0FF"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = Decil)
  ) %>%
  # Línea divisoria después de la fila Nacional
  tab_style(
    style = list(
      cell_borders(sides = "bottom", color = "black", weight = px(2))
    ),
    locations = cells_body(rows = 1)
  ) %>%
  # Nota al pie
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la ENIGH 2022"
  ) %>%
  # Nota sobre la suma de porcentajes
  tab_footnote(
    footnote = "Los porcentajes suman 100% horizontalmente.",
    locations = cells_column_labels(columns = `Gasto corriente monetario`)
  )

# Mostrar la tabla
tabla_porcentajes_gt
