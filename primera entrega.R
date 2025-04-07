# Instalar y cargar los paquetes necesarios
# install.packages("ganttrify")
# install.packages("dplyr")
# install.packages("lubridate")

pacman::p_load(dplyr,lubridate,tidyverse)
# install.packages("remotes")
#remotes::install_github("giocomai/ganttrify")
library(ganttrify)
# Crear un dataframe con las tareas del proyecto
tareas <- data.frame(
  Fase = c(rep("Fase 1 (20%)", 3), 
           rep("Fase 2 (30%)", 3),
           rep("Fase 3 (50%)", 9)),
  Tarea = c(
    "Revisión de literatura", 
    "Formulación de pregunta", 
    "Presentación diagrama Gantt",
    "Limpieza datos ENIGH",
    "Análisis descriptivo",
    "Entrega reporte descriptivo",
    "Preparación de datos",
    "Indicadores de inflación",
    "Modelo de regresión",
    "Análisis por subgrupos",
    "Redacción de introducción",
    "Redacción de metodología",
    "Redacción de resultados",
    "Revisión del documento",
    "Entrega reporte final"
  ),
  Inicio = as.Date(c(
    "2024-02-01", "2024-02-15", "2024-03-01",
    "2024-03-15", "2024-04-01", "2024-04-15",
    "2024-04-20", "2024-04-25", "2024-05-01",
    "2024-05-10", "2024-05-15", "2024-05-20",
    "2024-05-25", "2024-06-01", "2024-06-12"
  )),
  Fin = as.Date(c(
    "2024-02-14", "2024-02-29", "2024-03-10",
    "2024-03-31", "2024-04-14", "2024-04-20",
    "2024-04-24", "2024-04-30", "2024-05-09",
    "2024-05-14", "2024-05-19", "2024-05-24",
    "2024-05-31", "2024-06-11", "2024-06-13"
  ))
)

# Calcular la duración de cada tarea en días
tareas <- tareas %>%
  mutate(Duración = as.numeric(Fin - Inicio))

# Crear gráfico de Gantt con ggplot2
gantt_ggplot <- ggplot(tareas, aes(x = Inicio, y = Tarea, color = Fase, fill = Fase)) +
  geom_segment(aes(x = Inicio, xend = Fin, y = Tarea, yend = Tarea), size = 8, alpha = 0.6) +
  geom_text(aes(x = Inicio + (Fin - Inicio)/2, label = Tarea), color = "black", size = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_discrete(limits = rev(unique(tareas$Tarea))) +
  labs(title = "Diagrama de Gantt: Proyecto de Investigación ENIGH",
       subtitle = "Impacto de la inflación en patrones de consumo según situación laboral",
       x = "Fecha", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(face = "bold")
  )

# Para mostrar el gráfico básico en R:
print(gantt_ggplot)

# Marcar hitos importantes - Versión corregida
hitos <- data.frame(
  fecha = as.Date(c("2024-03-10", "2024-04-20", "2024-06-13")),
  hito = c("Presentación pregunta", "Entrega reporte descriptivo", "Entrega reporte final"),
  # Añadimos una posición Y para los hitos y la fase para que no dé error
  Tarea = "Entrega reporte final", # Posición de referencia donde aparecerán los hitos
  Fase = "Hito" # Añadimos esta columna para evitar el error
)

# Añadir hitos al diagrama de forma correcta
gantt_con_hitos <- gantt_ggplot +
  # Agregar líneas verticales para los hitos
  geom_vline(data = hitos, aes(xintercept = fecha), 
             linetype = "dashed", color = "darkred", alpha = 0.7) +
  # Añadir etiquetas en la parte superior
  annotate("text", 
           x = hitos$fecha, 
           y = 16, # Posición por encima de todas las tareas
           label = hitos$hito,
           color = "darkred", 
           angle = 90, 
           hjust = 0, 
           size = 3)

# Mostrar el gráfico con hitos
print(gantt_con_hitos)

# Para guardar el gráfico:
# ggsave("diagrama_gantt_proyecto.png", gantt_con_hitos, width = 10, height = 8)