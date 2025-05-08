# 1. Importar los datos
importar_datos <- function(ruta, archivo) {
  datos <- rio::import(file.path(ruta, archivo))
  return(datos)
}

# 2. Obtener etiquetas dinámicamente con fallback para archivos sin etiquetas
obtener_etiquetas <- function(datos, variables) {
  # Mapeo de fallback por si no hay etiquetas
  etiquetas_fallback <- list(
    gasto_mon = "Gasto corriente monetario",
    alimentos = "Alimentos, bebidas y tabaco",
    transporte = "Transporte y comunicaciones",
    educa_espa = "Educación y esparcimiento",
    vivienda = "Vivienda y servicios",
    personales = "Cuidados personales",
    limpieza = "Limpieza y cuidados de la casa",
    vesti_calz = "Vestido y calzado",
    salud = "Salud",
    transf_gas = "Transferencias de gasto"
  )
  
  etiquetas <- sapply(variables, function(var) {
    if (var %in% names(datos)) {
      # Intentar obtener etiqueta
      etiqueta <- tryCatch({
        label <- sjlabelled::get_label(datos[[var]])
        if (length(label) == 0 || is.na(label) || label == "") {
          NULL
        } else {
          label
        }
      }, error = function(e) {
        NULL
      })
      
      # Si no hay etiqueta, usar fallback o nombre de variable
      if (is.null(etiqueta)) {
        if (var %in% names(etiquetas_fallback)) {
          return(etiquetas_fallback[[var]])
        } else {
          return(var)
        }
      } else {
        return(etiqueta)
      }
    } else {
      warning(paste("Variable", var, "no encontrada en los datos"))
      return(var)
    }
  })
  
  return(etiquetas)
}

# 3. Preparar los datos para el análisis
preparar_datos_basicos <- function(datos, variables_necesarias) {
  datos_filtrados <- datos %>%
    select(all_of(variables_necesarias)) %>%
    # Añadir variable para conteo de hogares
    mutate(uno = 1)
  
  return(datos_filtrados)
}

# 4. Función corregida para crear deciles de ingreso
crear_deciles <- function(datos, var_ingreso = "ing_cor") {
  # Asegurarse de que los datos estén ordenados por ingreso
  datos_ordenados <- datos %>%
    arrange(!!sym(var_ingreso))
  
  # Calcular la suma total del factor
  total_factor <- sum(datos_ordenados$factor)
  
  # Determinar el tamaño de cada decil (10% del total)
  tamano_decil <- total_factor / 10
  
  # Calcular la suma acumulada del factor
  datos_ordenados <- datos_ordenados %>%
    mutate(acumulado = cumsum(factor),
           decil = NA)  # Inicializar variable decil
  
  # Asignar deciles
  for (i in 1:10) {
    limite_superior <- tamano_decil * i
    if (i == 1) {
      datos_ordenados <- datos_ordenados %>%
        mutate(decil = ifelse(acumulado <= limite_superior, i, decil))
    } else if (i < 10) {
      datos_ordenados <- datos_ordenados %>%
        mutate(decil = ifelse(acumulado > tamano_decil * (i-1) & 
                                acumulado <= limite_superior, i, decil))
    } else {
      # El último decil incluye todo lo restante
      datos_ordenados <- datos_ordenados %>%
        mutate(decil = ifelse(acumulado > tamano_decil * (i-1), i, decil))
    }
  }
  
  return(datos_ordenados)
}

# 5. Crear el diseño muestral
crear_disenio <- function(datos, id = "upm", estrato = "est_dis", peso = "factor") {
  options(survey.lonely.psu = "adjust")
  
  disenio <- datos %>%
    as_survey_design(
      ids = sym(id),
      strata = sym(estrato),
      weights = sym(peso)
    )
  
  return(disenio)
}

# 6. Calcular un promedio por grupo
calcular_promedio_por_grupo <- function(disenio, numerador, grupo, denominador = "uno") {
  resultado <- disenio %>%
    group_by(!!sym(grupo)) %>%
    summarize(
      promedio = survey_ratio(
        numerator = !!sym(numerador),
        denominator = !!sym(denominador)
      )
    )
  
  return(resultado)
}

# 7. Extraer valores numéricos de resultados por grupo
extraer_valores_por_grupo <- function(resultado_survey, grupo = "decil") {
  # Asegurarse de que el resultado sea un data frame
  if (!is.data.frame(resultado_survey)) {
    stop("El resultado debe ser un data frame")
  }
  
  # Verificar que las columnas necesarias existan
  if (!(grupo %in% names(resultado_survey)) || !("promedio" %in% names(resultado_survey))) {
    print(names(resultado_survey))
    stop("El resultado no tiene las columnas esperadas")
  }
  
  # Convertir a formato ancho
  valores <- resultado_survey$promedio
  nombres <- resultado_survey[[grupo]]
  
  resultado <- setNames(valores, nombres)
  return(resultado)
}

# 8. Calcular promedios por decil para una variable
calcular_promedio_decil <- function(disenio, variable, denominador = "uno") {
  resultado <- calcular_promedio_por_grupo(
    disenio, 
    numerador = variable,
    grupo = "decil",
    denominador = denominador
  )
  
  valores <- extraer_valores_por_grupo(resultado)
  return(valores)
}

# 9. Calcular promedios por decil para varias variables
calcular_promedios_por_decil <- function(disenio, variables, denominador = "uno") {
  # Primero, calculamos el promedio nacional
  resultados_nacional <- list()
  valores_nacional <- numeric(length(variables))
  names(valores_nacional) <- variables
  
  # Calcular valor nacional para cada variable
  for (i in seq_along(variables)) {
    var <- variables[i]
    resultado <- disenio %>%
      summarize(
        promedio = survey_ratio(
          numerator = !!sym(var),
          denominator = !!sym(denominador)
        )
      )
    
    resultados_nacional[[var]] <- resultado
    valores_nacional[i] <- resultado$promedio
  }
  
  # Ahora calculamos los promedios por decil
  resultados_decil <- list()
  
  for (var in variables) {
    resultados_decil[[var]] <- calcular_promedio_decil(disenio, var, denominador)
  }
  
  return(list(
    nacional = valores_nacional,
    por_decil = resultados_decil
  ))
}

# 10. Crear tabla de resultados por decil
crear_tabla_deciles <- function(resultados, variable, etiqueta = NULL) {
  if (is.null(etiqueta)) {
    etiqueta <- variable
  }
  
  # Obtener valores nacional y por decil
  valor_nacional <- resultados$nacional[variable]
  valores_decil <- resultados$por_decil[[variable]]
  
  # Crear data frame con todos los valores
  tabla <- data.frame(
    Decil = c("Nacional", paste("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")),
    Valor = round(c(valor_nacional, valores_decil[1:10]))
  )
  
  return(tabla)
}

# 11. Función corregida para crear tabla completa con todos los resultados
crear_tabla_completa_deciles <- function(resultados) {
  # Obtener lista de variables
  variables <- names(resultados$nacional)
  
  # Crear tabla con la estructura correcta
  tabla_completa <- data.frame(
    Decil = c("Nacional", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"),
    stringsAsFactors = FALSE
  )
  
  # Para cada variable, añadir una columna con los valores
  for (var in variables) {
    # Obtener el valor nacional
    valor_nacional <- resultados$nacional[var]
    
    # Obtener valores por decil - están ordenados numéricamente
    valores_decil <- resultados$por_decil[[var]]
    
    # Combinar valor nacional y valores por decil
    todos_valores <- c(valor_nacional, valores_decil)
    
    # Añadir columna a la tabla
    tabla_completa[[var]] <- round(todos_valores)
  }
  
  return(tabla_completa)
}


# Ejemplo de uso paso a paso
# Definir variables necesarias
vars_id_diseno <- c("folioviv", "foliohog", "factor", "upm", "est_dis")
vars_gasto <- c("gasto_mon")
vars_ingreso <- c("ing_cor")  # Variable de ingreso para crear deciles

# Paso 1: Importar datos (ahora funcionará con .sav)
datos_enigh <- importar_datos("./data", "concentradohogar.sav")
print("Paso 1 completado: Datos importados")

# Paso 2: Preparar datos básicos
datos_preparados <- preparar_datos_basicos(
  datos_enigh, 
  c(vars_id_diseno, vars_gasto, vars_ingreso)
)
print("Paso 2 completado: Datos preparados")

# Paso 3: Crear deciles (con la función corregida)
datos_con_deciles <- crear_deciles(datos_preparados, "ing_cor")
print("Paso 3 completado: Deciles creados")
# Verificar la distribución de deciles
print(table(datos_con_deciles$decil))

# Paso 4: Crear diseño muestral
disenio_enigh <- crear_disenio(datos_con_deciles)
print("Paso 4 completado: Diseño muestral creado")

# Paso 5: Calcular promedios por decil
resultados_deciles <- calcular_promedios_por_decil(disenio_enigh, vars_gasto)
print("Paso 5 completado: Promedios por decil calculados")
print(resultados_deciles)

# Paso 6: Crear tabla de resultados
tabla_deciles <- crear_tabla_completa_deciles(resultados_deciles)

# Paso 7: Obtener etiquetas (opcional)
etiquetas_gasto <- obtener_etiquetas(datos_enigh, vars_gasto)
print("Paso 7 completado: Etiquetas obtenidas")
print(etiquetas_gasto)

# Paso 8: Crear tabla con etiquetas
colnames(tabla_deciles)[-1] <- unname(etiquetas_gasto)
print("Paso 8 completado: Tabla final con etiquetas")
print(tabla_deciles)

