
library(ggplot2)


# Supuesto: 1 semana = 5 días hábiles
dias_por_semana <- 5

gantt_df <- data.frame(
  bloque = c(
    # Semana 1
    "Modelo de datos y reglas de negocio",
    "Integración datos CRM",
    "Integración datos UX (NPS y preferencias)",
    "Consolidación y limpieza de preferencias",
    "Normalización de instrumentos (tickers)",
    "Validaciones de calidad de datos",
    
    # Semana 2
    "Integración precios de mercado",
    "Cálculo de retornos y métricas de riesgo",
    "Preparación base de carteras por cuenta",
    "Transformación a formato matricial",
    "Cálculo de pesos por acción",
    
    # Semana 3
    "Cálculo métricas cartera actual",
    "Funciones de optimización (Sharpe)",
    "Optimización de carteras sin preferencias",
    "Optimización de carteras con preferencias",
    "Cálculo de brechas y KPIs",
    
    # Semana 4
    "Base de salida a nivel cuenta",
    "Base consolidada por ejecutivo",
    "Validación funcional de resultados",
    "Documentación y ajustes finales",
    "Preparación para carga en CRM",
    
    # Operación
    "Actualización semanal de mercado y carteras",
    "Actualización mensual encuesta NPS"
  ),
  
  # Inicio en semanas (no fin)
  inicio = c(
    1.0, 1.1, 1.2, 1.4, 1.3, 1.6,
    2.0, 2.2, 2.3, 2.4, 2.6,
    3.0, 3.1, 3.3, 3.4, 3.6,
    4.0, 4.1, 4.3, 4.4, 4.5,
    5.0, 5.2
  ),
  
  # Duración REAL en días (fuente de verdad)
  dias = c(
    2, 3, 2, 2, 2, 1,
    2, 2, 2, 2, 1,
    2, 2, 2, 2, 1,
    2, 2, 2, 2, 2,
    5, 4
  ),
  
  fase = c(
    rep("Semana 1", 6),
    rep("Semana 2", 5),
    rep("Semana 3", 5),
    rep("Semana 4", 5),
    rep("Operación", 2)
  )
)


# Cálculo de fin

gantt_df$fin <- gantt_df$inicio + gantt_df$dias / dias_por_semana


# Orden lógico del eje Y

orden_bloques <- c(
  "Modelo de datos y reglas de negocio",
  "Integración datos CRM",
  "Integración datos UX (NPS y preferencias)",
  "Consolidación y limpieza de preferencias",
  "Normalización de instrumentos (tickers)",
  "Validaciones de calidad de datos",
  
  "Integración precios de mercado",
  "Cálculo de retornos y métricas de riesgo",
  "Preparación base de carteras por cuenta",
  "Transformación a formato matricial",
  "Cálculo de pesos por acción",
  
  "Cálculo métricas cartera actual",
  "Funciones de optimización (Sharpe)",
  "Optimización de carteras sin preferencias",
  "Optimización de carteras con preferencias",
  "Cálculo de brechas y KPIs",
  
  "Base de salida a nivel cuenta",
  "Base consolidada por ejecutivo",
  "Validación funcional de resultados",
  "Documentación y ajustes finales",
  "Preparación para carga en CRM",
  
  "Actualización semanal de mercado y carteras",
  "Actualización mensual encuesta NPS"
)

gantt_df$bloque <- factor(gantt_df$bloque, levels = rev(orden_bloques))


# Paleta sobria

colores_sobrios <- c(
  "Semana 1" = "#8C8C8C",
  "Semana 2" = "#5F8DB8",
  "Semana 3" = "#4F6F52",
  "Semana 4" = "#7A6C9D",
  "Operación" = "#B35C5C"
)


# Gráfico Gantt final

ggplot(gantt_df, aes(y = bloque, fill = fase)) +
  
  # Líneas separadoras de semanas
  geom_vline(
    xintercept = c(2, 3, 4, 5),
    color = "grey70",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  
  # Barras Gantt
  geom_rect(
    aes(
      xmin = inicio,
      xmax = fin,
      ymin = as.numeric(bloque) - 0.35,
      ymax = as.numeric(bloque) + 0.35
    ),
    color = "white",
    linewidth = 0.3
  ) +
  
  # Texto con duración (días)
  geom_text(
    aes(
      x = (inicio + fin) / 2,
      y = bloque,
      label = paste0(dias, "d")
    ),
    color = "white",
    size = 3,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = colores_sobrios) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6),
    labels = c("Semana 1", "Semana 2", "Semana 3", "Semana 4", "Semana 5+", "")
  ) +
  labs(
    title = "Carta Gantt – Planificación del Proyecto",
    subtitle = "Bloques de trabajo organizados por etapa",
    x = "Periodo",
    y = "",
    fill = "Etapa"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )


