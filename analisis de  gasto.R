# Gasto corriente monetario trimestral por grandes rubros según tamaño de localidad (2022)

library(tidyverse)
library(foreign)
library(srvyr)

# Función para leer el archivo
leer_archivo <- function(ruta_datos, archivo = "/concentradohogar.sav") {
  rio::import(paste0(ruta_datos,archivo))
}

# Función para seleccionar variables de interés
seleccionar_variables <- function(datos) {
  datos |>
    select(
      folioviv, foliohog, tam_loc, factor, upm, est_dis,
      gasto_mon, alimentos, vesti_calz, vivienda, limpieza,
      salud, transporte, educa_espa, personales, transf_gas
    ) |>
    # Crear variables para tamaño de localidad
    mutate(
      # 1 = menos de 2500, 2 = más de 2500
      tam = ifelse(tam_loc <= 3, 1, 2),
      tam0 = 0  # Marcador para total nacional
    )
}

# Función para crear indicadores de gastos
crear_indicadores_gastos <- function(df) {
  df |>
    mutate(
      # Indicadores binarios para cada categoría de gasto (1 si hay gasto > 0)
      A00 = ifelse(gasto_mon > 0, 1, 0),
      A01 = ifelse(alimentos > 0, 1, 0),
      A05 = ifelse(vesti_calz > 0, 1, 0),
      A08 = ifelse(vivienda > 0, 1, 0),
      A13 = ifelse(limpieza > 0, 1, 0),
      A17 = ifelse(salud > 0, 1, 0),
      A18 = ifelse(transporte > 0, 1, 0),
      A24 = ifelse(educa_espa > 0, 1, 0),
      A28 = ifelse(personales > 0, 1, 0),
      A32 = ifelse(transf_gas > 0, 1, 0)
    ) |>
    # Renombrar variables de montos de gastos
    rename(
      B00 = gasto_mon, 
      B01 = alimentos,
      B05 = vesti_calz, 
      B08 = vivienda,
      B13 = limpieza, 
      B17 = salud,
      B18 = transporte, 
      B24 = educa_espa,
      B28 = personales, 
      B32 = transf_gas
    )
}

# Función para separar datos de indicadores y montos
separar_datos <- function(df) {
  # Seleccionar indicadores binarios
  df_binario <- df |>
    select(folioviv, foliohog, upm, est_dis, factor, tam, tam0, 
           starts_with("A"))
  
  # Seleccionar montos de gastos
  df_montos <- df |>
    select(folioviv, foliohog, upm, est_dis, factor, tam, tam0, 
           starts_with("B"))
  
  return(list(
    binarios = df_binario,
    montos = df_montos
  ))
}

# Función genérica para transformar a formato largo
convertir_a_largo <- function(df, prefijo = "A", col_nombre = "TIPO", col_valor = "VALOR") {
  df |>
    pivot_longer(
      cols = starts_with(prefijo),
      names_to = col_nombre,
      values_to = col_valor
    )
}

# Función para combinar los datos en formato largo
combinar_datos_largos <- function(df_binario_largo, df_montos_largo) {
  df_binario_largo |>
    bind_cols(
      df_montos_largo |> 
        select(all_of(c("I_TIPO", "ING")))
    )
}

# Función para crear diseño muestral
crear_disenio_muestral <- function(df) {
  # Definir opciones para PSU solitarios
  options(survey.lonely.psu = "adjust")
  
  # Crear objeto de diseño muestral usando srvyr
  df |>
    as_survey_design(
      ids = upm,
      strata = est_dis,
      weights = factor
    )
}

# Función genérica para calcular totales
calcular_totales <- function(disenio, var_suma, var_grupo, 
                             segundo_grupo = NULL, 
                             nombre_resultado = "total") {
  # Construir fórmula para group_by
  if (!is.null(segundo_grupo)) {
    disenio <- disenio |> 
      group_by(!!sym(var_grupo), !!sym(segundo_grupo))
  } else {
    disenio <- disenio |> 
      group_by(!!sym(var_grupo))
  }
  
  # Calcular los totales
  disenio |>
    summarize(
      !!sym(nombre_resultado) := survey_total(!!sym(var_suma), 
                                              vartype = c("se", "cv", "ci"))
    )
}

# Función para calcular todas las estadísticas necesarias
calcular_todas_estadisticas <- function(disenio) {
  # Calcular totales nacionales (hogares)
  hogares_nacional <- calcular_totales(
    disenio = disenio,
    var_suma = "HOG",
    var_grupo = "H_TIPO",
    segundo_grupo = "tam0",
    nombre_resultado = "total_hog"
  )
  
  # Calcular totales nacionales (gastos)
  ingresos_nacional <- calcular_totales(
    disenio = disenio,
    var_suma = "ING",
    var_grupo = "I_TIPO",
    segundo_grupo = "tam0",
    nombre_resultado = "total_ing"
  )
  
  # Calcular totales por tamaño de localidad (hogares)
  hogares_localidad <- calcular_totales(
    disenio = disenio,
    var_suma = "HOG",
    var_grupo = "H_TIPO",
    segundo_grupo = "tam",
    nombre_resultado = "total_hog"
  )
  
  # Calcular totales por tamaño de localidad (gastos)
  ingresos_localidad <- calcular_totales(
    disenio = disenio,
    var_suma = "ING",
    var_grupo = "I_TIPO",
    segundo_grupo = "tam",
    nombre_resultado = "total_ing"
  )
  
  return(list(
    ingresos_nacional = ingresos_nacional,
    hogares_nacional = hogares_nacional,
    ingresos_localidad = ingresos_localidad,
    hogares_localidad = hogares_localidad
  ))
}

# Función para preparar los datos de estadísticas
preparar_datos_estadisticas <- function(resultados_estadisticas) {
  # Función auxiliar para procesar un conjunto de datos
  procesar_conjunto <- function(datos, tipo, escala = 1) {
    datos |>
      mutate(
        tam = if_else(is.na(tam), tam0, tam),
        across(matches("total_.*$"), ~ . / escala)
      ) |>
      select(
        {{tipo}}, 
        tam, 
        starts_with("total_")
      )
  }
  
  # Combinar todos los resultados
  ingresos_combinados <- bind_rows(
    resultados_estadisticas$ingresos_nacional,
    resultados_estadisticas$ingresos_localidad
  ) |> 
    procesar_conjunto(I_TIPO, escala = 1000)
  
  hogares_combinados <- bind_rows(
    resultados_estadisticas$hogares_nacional,
    resultados_estadisticas$hogares_localidad
  ) |> 
    procesar_conjunto(H_TIPO)
  
  return(list(
    hogares = hogares_combinados,
    ingresos = ingresos_combinados
  ))
}

# Función para convertir los datos a formato ancho (para presentación)
# Función corregida para convertir los datos a formato ancho
convertir_a_formato_ancho <- function(datos_estadisticas) {
  # Convertir datos de hogares a formato ancho
  hogares_ancho <- datos_estadisticas$hogares %>%
    pivot_wider(
      id_cols = H_TIPO,
      names_from = tam,
      values_from = c(total_hog, total_hog_se, total_hog_cv, total_hog_low, total_hog_upp)
    )
  
  # Convertir datos de ingresos a formato ancho
  ingresos_ancho <- datos_estadisticas$ingresos %>%
    pivot_wider(
      id_cols = I_TIPO,
      names_from = tam,
      values_from = c(total_ing, total_ing_se, total_ing_cv, total_ing_low, total_ing_upp)
    )
  
  return(list(
    hogares = hogares_ancho,
    ingresos = ingresos_ancho
  ))
}

# Función para crear las tablas de resultados finales
# Función corregida para crear las tablas de resultados finales
crear_tablas_finales <- function(datos_ancho) {
  # Crear tablas finales para cada tipo de estadística
  estimaciones_combinadas <- data.frame(
    datos_ancho$hogares$total_hog_0,
    datos_ancho$ingresos$total_ing_0,
    datos_ancho$hogares$total_hog_2,
    datos_ancho$ingresos$total_ing_2,
    datos_ancho$hogares$total_hog_1,
    datos_ancho$ingresos$total_ing_1
  )
  
  error_estandar_combinado <- data.frame(
    datos_ancho$hogares$total_hog_se_0,
    datos_ancho$ingresos$total_ing_se_0,
    datos_ancho$hogares$total_hog_se_2,
    datos_ancho$ingresos$total_ing_se_2,
    datos_ancho$hogares$total_hog_se_1,
    datos_ancho$ingresos$total_ing_se_1
  )
  
  cv_combinado <- data.frame(
    datos_ancho$hogares$total_hog_cv_0,
    datos_ancho$ingresos$total_ing_cv_0,
    datos_ancho$hogares$total_hog_cv_2,
    datos_ancho$ingresos$total_ing_cv_2,
    datos_ancho$hogares$total_hog_cv_1,
    datos_ancho$ingresos$total_ing_cv_1
  )
  
  li_combinado <- data.frame(
    datos_ancho$hogares$total_hog_low_0,
    datos_ancho$ingresos$total_ing_low_0,
    datos_ancho$hogares$total_hog_low_2,
    datos_ancho$ingresos$total_ing_low_2,
    datos_ancho$hogares$total_hog_low_1,
    datos_ancho$ingresos$total_ing_low_1
  )
  
  ls_combinado <- data.frame(
    datos_ancho$hogares$total_hog_upp_0,
    datos_ancho$ingresos$total_ing_upp_0,
    datos_ancho$hogares$total_hog_upp_2,
    datos_ancho$ingresos$total_ing_upp_2,
    datos_ancho$hogares$total_hog_upp_1,
    datos_ancho$ingresos$total_ing_upp_1
  )
  
  # Categorías de gastos para etiquetas de filas
  categorias_gastos <- c(
    "GASTO CORRIENTE MONETARIO",
    "ALIMENTOS, BEBIDAS Y TABACO", 
    "VESTIDO Y CALZADO",
    "VIVIENDA Y SERVICIOS DE CONSERVACIÓN",
    "ARTÍCULOS Y SERVICIOS PARA LA LIMPIEZA, CUIDADOS DE LA CASA, ENSERES DOMÉSTICOS",
    "CUIDADOS DE LA SALUD",
    "TRANSPORTE; ADQUISICIÓN, MANTENIMIENTO, ACCESORIOS Y SERVICIOS",
    "SERVICIOS DE EDUCACIÓN, ARTÍCULOS EDUCATIVOS, ARTÍCULOS DE ESPARCIMIENTO",
    "CUIDADOS PERSONALES, ACCESORIOS Y EFECTOS", 
    "TRANSFERENCIAS DE GASTO"
  )
  
  # Nombres de columnas para las tablas
  nombres_columnas <- c(
    "TOTAL HOGARES", "TOTAL GASTO",
    "MÁS DE 2 500 HOGARES", "MÁS DE 2 500 GASTO",
    "MENOS DE 2 500 HOGARES", "MENOS DE 2 500 GASTO"
  )
  
  # Crear lista de tablas
  tablas <- list(
    estimaciones = estimaciones_combinadas,
    error_estandar = error_estandar_combinado,
    cv = cv_combinado,
    limite_inferior = li_combinado,
    limite_superior = ls_combinado
  )
  
  # Aplicar nombres a filas y columnas
  tablas <- lapply(tablas, function(tabla) {
    rownames(tabla) <- categorias_gastos
    colnames(tabla) <- nombres_columnas
    return(tabla)
  })
  
  return(tablas)
}

# Función principal para ejecutar todo el análisis
analizar_gastos_por_localidad <- function(df) {
  # Paso 1: Leer y preparar los datos
  datos <- df|> #leer_archivo(ruta_datos) |> 
    seleccionar_variables() |>
    crear_indicadores_gastos()
  
  # Paso 2: Separar y transformar datos
  datos_separados <- separar_datos(datos)
  
  # Paso 3: Convertir a formato largo
  datos_binario_largo <- convertir_a_largo(
    datos_separados$binarios, 
    prefijo = "A", 
    col_nombre = "H_TIPO", 
    col_valor = "HOG"
  )
  
  datos_montos_largo <- convertir_a_largo(
    datos_separados$montos, 
    prefijo = "B", 
    col_nombre = "I_TIPO", 
    col_valor = "ING"
  )
  
  # Paso 4: Combinar datos
  datos_combinados <- combinar_datos_largos(datos_binario_largo, datos_montos_largo)
  
  # Paso 5: Crear diseño muestral
  disenio_muestral <- crear_disenio_muestral(datos_combinados)
  
  # Paso 6: Calcular estadísticas
  resultados_estadisticas <- calcular_todas_estadisticas(disenio_muestral)
  
  # Paso 7: Preparar datos para tablas
  datos_estadisticas_procesados <- preparar_datos_estadisticas(resultados_estadisticas)
  
  # Paso 8: Convertir a formato ancho
  datos_formato_ancho <- convertir_a_formato_ancho(datos_estadisticas_procesados)
  
  # Paso 9: Crear tablas finales
  tablas_resultados <- crear_tablas_finales(datos_formato_ancho)
  
  # Devolver los resultados
  return(tablas_resultados)
}

# Ejecutar el análisis (descomentar para ejecutar)
resultados <- analizar_gastos_por_localidad(enigh)
# 
# # Mostrar resultados
round(resultados$estimaciones, 8)
round(resultados$error_estandar, 8)
round(resultados$cv, 8)
round(resultados$limite_inferior, 8)
round(resultados$limite_superior, 8)
resultados$estimaciones
