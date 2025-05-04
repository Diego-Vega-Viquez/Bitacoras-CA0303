library(tidyverse)
library(haven)
library(ggridges)
library(viridis)
library(scales)
library(labelled)
library(here)

#No olvide cargar base de datos en manejo_de_datos.R
enadis <- readRDS(here("data", "enadisJOSE.rds")) # Listo
enadis_completa <- readRDS(here("data/enadis_completa.rds")) # Listo

##############
# GRAFICOS  #
##############

# Grafico distribución de la condición de actividad laboral según el grado de discapacidad. (JOSE)

grafico1 <- ggplot(enadis, aes(x = Cap_grado, fill = condic_activi)) +
  geom_bar(position = "fill", color = "white", width = 0.75) +  # Ajuste de ancho
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +  # Ajuste de la paleta de colores
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title = "Gráfico 1. \nCondición de actividad según grado de discapacidad",
    # subtitle = "Distribución porcentual de la condición de actividad laboral en Costa Rica, 2023.",
    x = "Grado de Discapacidad",
    y = "",
    fill = "Condición de Actividad",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +  # Tamaño base ajustado
  theme(
    # plot.title = element_text(size = 32, face = "bold", hjust = 0),
    # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title = element_text(face = "bold", size = 20),  # Títulos de ejes más grandes
    axis.text = element_text(face = "bold", size = 15),  # Texto de ejes más grande
    legend.position = "bottom",  # Posición de la leyenda
    legend.title = element_text(face = "bold", size = 20),  # Título de la leyenda
    legend.text = element_text(size = 15)  # Texto de la leyenda más grande
  )+
  coord_flip()

ggsave("res/graficos/cond_act_seg_grad_disc.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada




# Gráfico de distribución de la posición en el trabajo según el grado de discapacidad. (JOSE)

grafico2 <- ggplot(enadis %>% drop_na(), aes(x = Cap_grado, fill = B8a)) +
  geom_bar(position = "fill", width = 0.75) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    # title = "Gráfico 2. \nPosición en el trabajo según grado de discapacidad",
    # subtitle = "Distribución porcentual de personas ocupadas según tipo de empleo en Costa Rica, 2023",
    x = "Grado de Discapacidad",
    y = "",
    fill = "Posición en el Trabajo",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
   # plot.title = element_text(size = 32, face = "bold", hjust = 0),
   # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 18)
  ) +
  coord_flip()

ggsave("res/graficos/pos_trabajo_vs_grad_disc.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 20, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada




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
    axis.title.x = element_text(face = "bold", size = 20),  # Solo título eje X
    axis.title.y = element_text(face = "bold", size = 20, hjust = 0.9),  # Solo título eje Y
    axis.text = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 20),
    legend.text = element_text(size = 15)
  )

ggsave("res/graficos/grad_disc_por_reg_zon.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 8.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada




# Gráfico de distribución del ingreso neto del hogar según grado de discapacidad (DIEGO)
grafico4 <- ggplot(enadis %>% drop_na(ING_PERCAPITA_HOGAR, Cap_grado),
                   aes(x = Cap_grado, y = ING_PERCAPITA_HOGAR, fill = Cap_grado)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    labels = scales::label_number(prefix = "₡", big.mark = ",")
  ) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8, guide = "none") +
  labs(
    # title = "Gráfico 4. \nIngreso per capita del hogar según grado de discapacidad",
    # subtitle = "Distribución del ingreso mensual neto del hogar en Costa Rica, 2023",
    x = "Grado de Discapacidad",
    y = "Ingreso per cápita del hogar (escala logarítmica, colones)",
    caption = "Fuente: INEC, ENADIS 2023."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # plot.title = element_text(size = 32, face = "bold", hjust = 0),
    # plot.subtitle = element_text(size = 28, hjust = 0),
    axis.title.x = element_text(face = "bold", size = 20),
    axis.title.y = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 15),
    legend.position = "none"
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
  group_by(Cap_grado) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = B8a, values_from = Proporcion, values_fill = "0,00%")

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


