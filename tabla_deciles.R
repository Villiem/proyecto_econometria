# Utilizar los datos ya calculados con formato de dinero
tabla_deciles_gt <- tabla_deciles %>%
  # Tomar solo la columna de Decil y Gasto corriente monetario
  select(Decil, `Gasto corriente monetario`) %>%
  # Renombrar para mayor claridad
  rename(Gasto = `Gasto corriente monetario`) %>%
  # Crear la tabla gt
  gt() %>%
  tab_header(
    title = "Gasto corriente monetario promedio trimestral por deciles de ingreso",
    subtitle = "ENIGH 2022 (Pesos)."
  ) %>%
  # Aplicar formato de dinero en pesos mexicanos
  fmt_currency(
    columns = Gasto,
    currency = "MXN",
    decimals = 0
  ) %>%
  cols_label(
    Decil = "Deciles de hogares",
    Gasto = "Promedio (pesos)"
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
  # Línea divisoria después de la fila Nacional
  tab_style(
    style = list(
      cell_borders(sides = "bottom", color = "black", weight = px(2))
    ),
    locations = cells_body(rows = 1)
  ) %>%
  tab_source_note(
    source_note = "Fuente: Elaboración propia con datos de la ENIGH 2022."
  )
