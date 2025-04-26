library(tidyverse)
library(haven)
library(ggridges)
library(viridis)
library(scales)

#No olvide cargar base de datos en manejo_de_datos.R

ggplot(enadis, aes(x = Cap_grado, fill = condic_activi)) +
  geom_bar(position = "fill", color = "white", width = 0.75) +  # Ajuste de ancho
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +  # Ajuste de la paleta de colores
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Grado de Discapacidad",
    y = "",
    fill = "Condición de Actividad"
  ) +
  theme_minimal(base_size = 14) +  # Tamaño base ajustado
  theme(
    axis.title = element_text(face = "bold", size = 30),  # Títulos de ejes más grandes
    axis.text = element_text(face = "bold", size = 25),  # Texto de ejes más grande
    legend.position = "bottom",  # Posición de la leyenda
    legend.title = element_text(face = "bold", size = 30),  # Título de la leyenda
    legend.text = element_text(size = 25)  # Texto de la leyenda más grande
  )+
  coord_flip()

ggsave("res/graficos/cond_act_seg_grad_disc.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

ggplot(enadis %>% drop_na(), aes(x = Cap_grado, fill = B8a)) +
  geom_bar(position = "fill", width = 0.75) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Grado de Discapacidad",
    y = "",
    fill = "Posición en el Trabajo"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", size = 30),
    axis.text = element_text(size = 25),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 18)
  ) +
  coord_flip()

ggsave("res/graficos/pos_trabajo_vs_grad_disc.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada

cuadro_pos_trab_vs_grad_disc <- enadis %>%
  drop_na(Cap_grado, B8a) %>%
  count(Cap_grado, B8a, name = "Cantidad") %>%
  group_by(Cap_grado) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = B8a, values_from = Proporcion, values_fill = "0,00%")

cuadro_condic_actividad_vs_grad_disc <- enadis %>%
  drop_na(Cap_grado, condic_activi) %>%
  count(Cap_grado, condic_activi, name = "Cantidad") %>%
  group_by(Cap_grado) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad)) %>%
  ungroup() %>%
  mutate(Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  select(-Cantidad) %>%
  pivot_wider(names_from = condic_activi, values_from = Proporcion, values_fill = "0,00%")

cuadro_discapacidad_region_zona <- enadis %>%
  filter(!is.na(Cap_grado)) %>%  
  count(region, zona, name = "Cantidad") %>%
  group_by(region) %>%
  mutate(Proporcion = Cantidad / sum(Cantidad),
         Proporcion = percent(Proporcion, accuracy = 0.01, decimal.mark = ",")) %>%
  ungroup()

enadis %>%
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
    x = "Región",
    y = "Personas con Discapacidad",
    fill = "Zona de Residencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "bold", size = 30),  # Solo título eje X
    axis.title.y = element_text(face = "bold", size = 30, hjust = 0.9),  # Solo título eje Y
    axis.text = element_text(size = 25),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 30),
    legend.text = element_text(size = 25)
  )

ggsave("res/graficos/grad_disc_por_reg_zon.png", 
       plot = last_plot(), 
       device = "jpg", 
       width = 18, # Tamaño: 11.5 pulgadas de ancho
       height = 6.5, # Tamaño: 6 pulgadas de alto
       dpi = 900)  # Calidad: 900 pixeles por pulgada
