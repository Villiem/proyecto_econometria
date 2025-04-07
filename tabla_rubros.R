# Utilizar los datos ya calculados con formato de dinero
tabla_rubros_gt <- tabla_deciles %>%
  # Tomar solo la fila nacional que contiene los promedios para todos los rubros
  filter(Decil == "Nacional") %>%
  # Convertir de formato ancho a largo para tener una columna de rubros y otra de valores
  pivot_longer(
    cols = -Decil,
    names_to = "Rubro",
    values_to = "Valor"
  ) %>%
  # Eliminar la columna Decil que ya no necesitamos
  select(-Decil) %>%
  # Crear la tabla gt
  gt() %>%
  tab_header(
    title = "Gasto corriente monetario promedio trimestral por grandes rubros",
    subtitle = "ENIGH 2022 (Pesos)"
  ) %>%
  # Aplicar formato de dinero en pesos mexicanos
  fmt_currency(
    columns = Valor,
    currency = "MXN",
    decimals = 0
  ) %>%
  cols_label(
    Rubro = "Rubros de gasto",
    Valor = "Promedio (pesos)"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#003366"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la ENIGH 2022."
  )

# Mostrar la tabla
tabla_rubros_gt