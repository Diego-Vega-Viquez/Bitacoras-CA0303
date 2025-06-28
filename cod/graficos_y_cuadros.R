library(tidyverse)
library(haven)
library(ggridges)
library(viridis)
library(scales)
library(labelled)
library(here)
library(plyr)
#No olvide cargar base de datos en manejo_de_datos.R
enadis <- readRDS(here("data", "enadis.rds")) # Listo
enadis_completa <- readRDS(here("data/enadis_completa.rds")) # Listo
enadis_oc <- readRDS(here("data/enadis_oc.rds")) # Listo

##############
# GRAFICOS  #
##############

# Grafico distribución de la condición de actividad laboral según el grado de discapacidad. (JOSE)

grafico1 <- ggplot(enadis, aes(x = Cap_grado, fill = condic_activi)) +
  geom_bar(position = "fill", width = 0.75) +  # Ajuste de ancho
  scale_fill_viridis_d(option = "D",
                       begin = 0.2,
                       end = 0.8) +  # Ajuste de la paleta de colores
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title = "Gráfico 1. \nCondición de actividad según grado de discapacidad",
    # subtitle = "Distribución porcentual de la condición de actividad laboral en Costa Rica, 2023.",
    x = "Grado de Discapacidad",
    y = "",
    fill = "Condición de Actividad",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  annotate(
    "text",
    x = 1,
    y = 0.2505 / 2,
    label = "25%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 1,
    y =  1 - 0.2505 / 2,
    label = "71%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 2,
    y = 0.2505 / 2,
    label = "28%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 2,
    y =  1 - 0.2505 / 2,
    label = "68%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 3,
    y = 0.2505 / 2,
    label = "33%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 3,
    y =  1 - 0.2505 / 2,
    label = "62%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 4,
    y = 0.2505 / 2,
    label = "45%",
    color = "white",
    size = 10
  ) +
  annotate(
    "text",
    x = 4,
    y =  1 - 0.2505 / 2,
    label = "51%",
    color = "white",
    size = 10
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    # plot.title = element_text(size = 32, face = "bold", hjust = 0),
    # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 20),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 16)  
    
  ) +
  coord_flip()

ggsave("res/graficos/cond_act_seg_grad_disc.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 12, 
       height = 6, 
       dpi = 300)  


# Gráfico de distribución de la posición en el trabajo según el grado de discapacidad. (JOSE)

grafico2 <- ggplot(enadis %>% drop_na(), aes(x = B8a, fill = Cap_grado)) +
  geom_bar(position = "fill", width = 0.75) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    # title = "Gráfico 2. \nPosición en el trabajo según grado de discapacidad",
    # subtitle = "Distribución porcentual de personas ocupadas según tipo de empleo en Costa Rica, 2023",
    fill = "Grado de Discapacidad",
    y = NULL,
    x = "Posición en el Trabajo",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  annotate(
    "text",
    x = 1,
    y= 1 - 0.6/2,
    label = "60%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 2,
    y= 1 - 0.43/2,
    label = "43%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 3,
    y= 1 - 0.48/2,
    label = "48%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 4,
    y= 1 - 0.35/2,
    label = "35%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 1,
    y = 0.05/2,
    label = "4%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 2,
    y= 0.14/2,
    label = "14%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 3,
    y= 0.12/2,
    label = "12%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 4,
    y= 0.21/2,
    label = "21%",
    color = "white",
    size = 5
  ) +
  annotate(
    "text",
    x = 1,
    y = 0.04 + 0.16/2,
    label = "16%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 2,
    y= 0.14 + 0.21/2,
    label = "21%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 3,
    y= 0.12 + 0.19/2,
    label = "19%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 4,
    y= 0.21 + 0.24/2,
    label = "24%",
    color = "white",
    size = 5
  ) +
  annotate(
    "text",
    x = 1,
    y = 0.04 + 0.16 + 0.19/2,
    label = "19%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 2,
    y= 0.14 + 0.21 + 0.22/2,
    label = "22%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 3,
    y= 0.12 + 0.19 + 0.21/2,
    label = "21%",
    color = "white",
    size = 5
  ) + 
  annotate(
    "text",
    x = 4,
    y= 0.21 + 0.24 + 0.21/2,
    label = "21%",
    color = "white",
    size = 5
  ) +
  theme_minimal(base_size = 14) +
  theme(
   # plot.title = element_text(size = 32, face = "bold", hjust = 0),
   # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title = element_text(face = "bold", size = 15, hjust = 1),
    axis.text = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 15),
    plot.caption = element_text(size = 10),
    axis.text.x = element_blank()
  ) +
  coord_flip()

ggsave("res/graficos/pos_trabajo_vs_grad_disc.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 10, # Tamaño: 11.5 pulgadas de ancho
       height = 3, # Tamaño: 6 pulgadas de alto
       dpi = 300)  # Calidad: 900 pixeles por pulgada

# Gráfico de barras que muestra la cantidad de personas con discapacidad según región de planificación y zona de residencia (urbana o rural) (JOSE)
grafico3 <- enadis %>%
  filter(!is.na(Cap_grado)) %>%
  count(region, zona) %>%
  ggplot(aes(x = region, y = n, fill = zona)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    color = "white"
  ) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  labs(
    # title = "Gráfico 3. \nDistribución de personas con discapacidad por región y zona de residencia",
    # subtitle = "Comparación entre zonas urbanas y rurales en las distintas regiones de Costa Rica, 2023",
    x = "Región",
    y = "Personas con Discapacidad",
    fill = "Zona de Residencia",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
   # plot.title = element_text(size = 32, face = "bold", hjust = 0),
   # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title.x = element_text(face = "bold", size = 30),  # Solo título eje X
    axis.title.y = element_text(face = "bold", size = 30, hjust = 0.9),  # Solo título eje Y
    axis.text = element_text(size = 30),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 30),
    legend.text = element_text(size = 30),
    plot.caption = element_text(size = 20)
  )

ggsave("res/graficos/grad_disc_por_reg_zon.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 8.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada




# Gráfico de distribución del ingreso neto del hogar según grado de discapacidad (DIEGO)
grafico4 <- ggplot(enadis %>% drop_na(ING_PERCAPITA_HOGAR, Cap_grado),
                   aes(x = Cap_grado, y = ING_PERCAPITA_HOGAR / 1000, fill = Cap_grado)) +
  geom_boxplot(width = 0.6, linewidth = 1) +
  scale_y_log10(
    labels = scales::label_number(prefix = "₡", big.mark = ",")
  ) +
  scale_fill_viridis_d(option = "D", begin = 0.4, end = 1, guide = "none") +
  labs(
    # title = "Gráfico 4. \nIngreso per capita del hogar según grado de discapacidad",
    # subtitle = "Distribución del ingreso mensual neto del hogar en Costa Rica, 2023",
    x = "Grado de Discapacidad",
    y = "Ingreso per cápita del hogar (escala logarítmica, miles de colones)",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  annotate(
    "text",
    x = 1,
    y = 285065/1000,
    label = "x",
    color = "white",
    size = 10
  ) + 
  annotate(
    "text",
    x = 2,
    y = 256304/1000,
    label = "x",
    color = "white",
    size = 10
  ) + 
  annotate(
    "text",
    x = 3,
    y = 247662/1000,
    label = "x",
    color = "white",
    size = 10
  ) + 
  annotate(
    "text",
    x = 4,
    y = 213771/1000,
    label = "x",
    color = "darkred",
    size = 10
  ) + 
  theme_minimal(base_size = 14) +
  theme(
    # plot.title = element_text(size = 32, face = "bold", hjust = 0),
    # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title.x = element_text(face = "bold", size = 30),
    axis.title.y = element_text(face = "bold", size = 30),
    axis.text = element_text(size = 30),
    legend.position = "none",
    plot.caption = element_text(size = 20)
  ) + 
  coord_flip()

ggsave("res/graficos/ingreso_hogar_vs_grado_discapacidad.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18,
       height = 8.5,
       dpi = 900)

# Gráfico de dispersión del ingreso per cápita según puntaje de discapacidad (DIEGO)
grafico5 <- ggplot(enadis_completa %>% drop_na(ING_PERCAPITA_HOGAR, Dis_puntaje),
       aes(x = Dis_puntaje, y = ING_PERCAPITA_HOGAR)) +
  geom_point(alpha = 0.3, color = "#2c7fb8") +
  geom_smooth(method = "loess", se = FALSE, color = "#f03b20", linewidth = 1.5) +
  scale_y_continuous(
    labels = scales::comma_format(prefix = "₡"),
    limits = c(0, 2500000)
  ) +
  scale_x_continuous(name = "Puntaje de discapacidad") +
  labs(
    # title = "Gráfico 5. \nRelación entre puntaje de discapacidad e ingreso per cápita",
    # subtitle = "Tendencia del ingreso según nivel de discapacidad en Costa Rica, 2023",
    y = "Ingreso per cápita del hogar (colones)",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
   # plot.title = element_text(size = 32, face = "bold", hjust = 0),
   # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 15),
    legend.position = "none"
  )

ggsave("res/graficos/ingreso_vs_puntaje_discapacidad.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18,
       height = 8.5,
       dpi = 900)




# Gráfico de selación entre edad y desempeño funcional (DIEGO)

grafico6 <- ggplot(enadis_completa %>% drop_na(A5, Des_puntaje),
       aes(x = A5, y = Des_puntaje)) +
  geom_point(alpha = 0.2, color = "#3182bd") +
  geom_smooth(method = "loess", se = FALSE, color = "#e6550d", linewidth = 1.5) +
  scale_y_continuous(name = "Puntaje de desempeño") +
  scale_x_continuous(name = "Edad (años)", breaks = seq(20, 100, by = 10)) +
  labs(
    # title = "Gráfico 6. \nRelación entre edad y puntaje de desempeño funcional",
    # subtitle = "Tendencia del desempeño físico y cognitivo según edad en Costa Rica, 2023",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
   # plot.title = element_text(size = 32, face = "bold", hjust = 0),
   # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 15),
    legend.position = "none"
  )

ggsave("res/graficos/edad_vs_desempeno.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18,
       height = 8.5,
       dpi = 900)

#Gráfico de Horas Laboradas para detectar Outliers (Andrey)

grafico7 <- enadis_oc %>% mutate(B7 = mapvalues(
  B7,
  from = c(
    "Menos de 15 horas",
    "De 15 a menos de 40 horas",
    "De 40 a 48 horas",
    "Más de 48 horas"
  ),
  to = c("<15", "[15,40)", "[40,48]", ">48")
)) %>% mutate(B7 = fct_relevel(B7, c(">48", "[40,48]", "[15,40)", "<15"))) %>%  ggplot(aes(Cap_grado, fill = B7)) +
  geom_bar(position = "fill", width = 0.75) +
  scale_fill_viridis_d(option = "D",
                       begin = 0.2,
                       end = 0.8) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Grado de Discapacidad",
    y = NULL,
    fill = "Horas Laboradas",
    caption = "Fuente: INEC, ENADIS 2023"
  ) +
  annotate(
    "text",
    x = 1,
    y = 0.06/2,
    label = "6%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 2,
    y = 0.08/2,
    label = "8%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 3,
    y = 0.12/2,
    label = "12%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 4,
    y = 0.16/2,
    label = "16%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 1,
    y = 0.06 + 0.18/2,
    label = "18%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 2,
    y = 0.08 + 0.18/2,
    label = "18%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 3,
    y = 0.12 + 0.2/2,
    label = "20%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 4,
    y = 0.16 + 0.23/2,
    label = "23%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 1,
    y = 0.06 + 0.18 + 0.56/2,
    label = "56%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 2,
    y = 0.08 + 0.18 + 0.48/2,
    label = "48%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 3,
    y = 0.12 + 0.2 + 0.44/2,
    label = "44%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 4,
    y = 0.16 + 0.23 + 0.41/2,
    label = "41%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 1,
    y = 0.06 + 0.18 + 0.56 + 0.2/2,
    label = "20%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 2,
    y = 0.08 + 0.18 + 0.48 + 0.26/2,
    label = "26%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 3,
    y = 0.12 + 0.2 + 0.44 + 0.24/2,
    label = "24%",
    size = 12,
    color = "white"
  ) + 
  annotate(
    "text",
    x = 4,
    y = 0.16 + 0.23 + 0.41 + 0.2/2,
    label = "20%",
    size = 12,
    color = "white"
  ) + 
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", size = 30),
    axis.text = element_text(size = 30),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 30),
    legend.text = element_text(size = 30),
    plot.caption = element_text(size = 20),
    axis.text.x = element_blank()
  ) +
  coord_flip()
  
  
ggsave("res/graficos/grado_disc_vs_horas_lab.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18,
       height = 8.5,
       dpi = 900)

#Gráfico de Ingreso Per Cápita según grado de discapacidad  SIN OUTLIERS(ANDREY)
grafico8 <- enadis_oc %>% 
  ggplot(aes(x = "", y = ING_PERCAPITA_HOGAR, fill = Cap_grado)) +
  geom_boxplot(outlier.shape = NA, width = 0.6) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  labs(
    x = NULL,  # Elimina el título del eje X
    y = "Ingreso per cápita (₡)",
    fill = "Grado de Discapacidad",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text.x = element_blank(),  # Oculta etiquetas del eje X
    axis.text.y = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 18),
    panel.grid.major.x = element_blank()  # Elimina líneas de cuadrícula en X
  ) +
  coord_cartesian(ylim = c(0, 1e6)) 

print(grafico8)

#Gráfico de Ingreso Per Cápita según grado de discapacidad CON OUTLIERS (ANDREY)
grafico9 <- enadis_oc %>% 
  ggplot(aes(x = "", y = ING_PERCAPITA_HOGAR, fill = Cap_grado)) +
  geom_boxplot(outlier.colour = "gray", outlier.shape = 16, outlier.size = 2, width = 0.6)+
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  labs(
    x = NULL,  # Elimina el título del eje X
    y = "Ingreso per cápita (₡)",
    fill = "Grado de Discapacidad",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text.x = element_blank(),  # Oculta etiquetas del eje X
    axis.text.y = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 18),
    panel.grid.major.x = element_blank()  # Elimina líneas de cuadrícula en X
  )

# Grafico estructura ocupacional según el total de horas trabajadas

# Etiquetas para los paneles
df_plot <- enadis_completa %>%
  mutate(
    Enfermedad = fct_recode(Tiene_enfer,
                            "No tiene enfermedades crónicas" = "No tiene ninguna",
                            "Tiene al menos una enfermedad crónica" = "Si tiene alguna")
  ) %>%
  filter(!is.na(Enfermedad), !is.na(B7), !is.na(Cali_grup_ocup))

# Crear gráfico
grafico_doble <- ggplot(df_plot, aes(x = B7, fill = Cali_grup_ocup)) +
  geom_bar(position = "fill", color = "white") +
  facet_wrap(~ Enfermedad) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.85) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Horas trabajadas semanalmente",
    y = "Proporción",
    fill = "Grupo Ocupacional",
    caption = "Fuente: INEC, ENADIS 2023"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 15)
  )
# Guardar gráfico
ggsave("res/graficos/doble_b7_vs_grupoocup_por_enfermedad.png",
       plot = grafico_doble,
       device = "png",
       width = 18,
       height = 8,
       dpi = 900)





##############
#   TABLAS   #
##############

# Tabla de la distribucion del ingreso per capita del hogar segun grado de discapacidad

cuadro_ingreso_vs_grad_disc <- enadis %>%
  group_by(Cap_grado) %>%
  summarise(
    n = n(),
    Media = mean(ING_PERCAPITA_HOGAR, na.rm = TRUE),
    Desviación = sd(ING_PERCAPITA_HOGAR, na.rm = TRUE),
    Q1 = quantile(ING_PERCAPITA_HOGAR, 0.25, na.rm = TRUE),
    Mediana = median(ING_PERCAPITA_HOGAR, na.rm = TRUE),
    Q3 = quantile(ING_PERCAPITA_HOGAR, 0.75, na.rm = TRUE)
    )
# Tabla de horas trabajadas segun el grado de discapacidad

cuadro_horas_vs_grad_disc <- enadis %>%
  drop_na(Cap_grado, B7) %>%
  count(Cap_grado, B7, name = "Cantidad") %>%
  group_by(Cap_grado) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = B7, values_from = Proporcion, values_fill = "0,00%")

# Tabla de puestos de trabajo segun grado de discapacidad

cuadro_puestos_vs_grad_disc <- enadis %>%
  drop_na(Cap_grado, B8a) %>%
  count(Cap_grado, B8a, name = "Cantidad") %>%
  group_by(Cap_grado) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = B8a, values_from = Proporcion, values_fill = "0,00%")



# Tabla de distribución porcentual de la posición en el trabajo según el grado de dificultad en la capacidad. (JOSE)
cuadro_pos_trab_vs_grad_disc <- enadis %>%
  drop_na(Cap_grado, B8a) %>%
  count(Cap_grado, B8a, name = "Cantidad") %>%
  group_by(B8a) %>%  # Cambia la agrupación para pivotear al revés
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = Cap_grado, values_from = Proporcion, values_fill = "0,00%")

# Tabla de distribución porcentual que muestra la posición en el trabajo según el grado de dificultad en la capacidad (JOSE)
cuadro_condic_actividad_vs_grad_disc <- enadis %>%
  drop_na(Cap_grado, condic_activi) %>%
  count(Cap_grado, condic_activi, name = "Cantidad") %>%
  group_by(Cap_grado) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = condic_activi, values_from = Proporcion, values_fill = "0,00%")

# Tabla de distribución de personas según zona (urbana o rural) y región de planificación, con sus respectivas cantidades y proporciones. (JOSE)
cuadro_discapacidad_region_zona <- enadis %>%
  filter(!is.na(Cap_grado)) %>%  
  count(region, zona, name = "Cantidad") %>%
  group_by(region) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad),
         Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  ungroup()

label_col <- var_label(enadis_completa$Grado_discap)

# Tabla de distribución porcentual del ingreso per cápita por grado de discapacidad (DIEGO)
cuadro_ingreso_vs_grado_discap <- enadis_completa %>%
  drop_na(Grado_discap, QUINTIL_INGRESO) %>%
  count(Grado_discap, QUINTIL_INGRESO, name = "Cantidad") %>%
  group_by(Grado_discap) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = QUINTIL_INGRESO, values_from = Proporcion, values_fill = "0,00%") %>%
  rename(!!label_col  := Grado_discap)

# Tabla de distribución de enfermedades crónicas por grado de discapacidad (DIEGO)
cuadro_enfermedades_vs_grado_discap <- enadis_completa %>%
  drop_na(Grado_discap, Tiene_enfer) %>%
  count(Grado_discap, Tiene_enfer, name = "Cantidad") %>%
  group_by(Grado_discap) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = Tiene_enfer, values_from = Proporcion, values_fill = "0,00%") %>%
  rename(!!label_col  := Grado_discap)

# Tabla de necesidad de productos de apoyo según grado de discapacidad (DIEGO)
cuadro_productos_vs_grado_discap <- enadis_completa %>%
  drop_na(Grado_discap, PROD_ASIS) %>%
  count(Grado_discap, PROD_ASIS, name = "Cantidad") %>%
  group_by(Grado_discap) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = PROD_ASIS, values_from = Proporcion, values_fill = "0,00%") %>%
  rename(!!label_col  := Grado_discap)

#Tablas (ANDREY)

cuadro_frec_relativa_pos_grado <- enadis_oc %>% 
  count(Cap_grado, B8a) %>% group_by(Cap_grado) %>% 
  mutate(
    Total = sum(n),
    Frecuencia_Relativa = n/Total
  ) %>% ungroup() %>% select(-Total) %>% 
  pivot_wider(
    names_from = B8a,
    values_from = Frecuencia_Relativa,
    values_fill = 0
  ) %>% 
  mutate_if(is.numeric, ~ percent(.x, acurracy = 0.1))

print(cuadro_frec_relativa_pos_grado)

cuadro_promedio_ingreso_vs_pos_y_grado <- enadis_completa %>% 
  filter(!is.na(ING_PERCAPITA_HOGAR), !is.na(condic_activi), !is.na(Cap_grado)) %>% 
  group_by(condic_activi, Cap_grado) %>%  summarise(
    Promedio_Ingreso = mean(ING_PERCAPITA_HOGAR, na.rm = TRUE),
    .groups = "drop") %>% 
  pivot_wider(
    names_from = Cap_grado,
    values_from = Promedio_Ingreso
  ) %>% mutate_if(is.numeric, ~ round(.x, 0))

print(cuadro_promedio_ingreso_vs_pos_y_grado)

rm(grafico5)
rm(grafico6)
rm(grafico8)
rm(grafico9)