library(tidyverse)
library(broom)
library(car)
library(performance)
library(ggplot2)

# Partimos de la base ya limpia: 
enadis <- readRDS("data/enadisJOSE.rds") 

enadis <- enadis %>%
  mutate(log_ingreso = log(ING_PERCAPITA_HOGAR + 1)) %>% drop_na()

modelo_full <- lm(log_ingreso ~ A5 + A4 + region + zona + Nivel_Instru + 
                    Cap_grado + B8a + B7,
                  data = enadis)

modelo_step <- step(modelo_full, direction = "both", trace = TRUE)

tabla_modelo_step <- tidy(modelo_step)

summary(modelo_step)

#Diagnostico del modelo 

residuos_df <- data.frame(
  Ajustados = modelo_step$fitted.values,
  Residuos = modelo_step$residuals
)

ggplot(residuos_df, aes(x = Ajustados, y = Residuos)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()

#Se cumple el principio de varianza constante, con pocos valores atipicos

#Revisamos normalidad:
residuos_log_df <- data.frame(
  Residuos = modelo_step$residuals
)

ggplot(residuos_log_df, aes(sample = Residuos)) +
  stat_qq_point(size = 2, alpha = 0.5) +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot de Residuos (Modelo Logarítmico)",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Observados") +
  theme_minimal()

#Revisar valore influyentes
cooks_log_df <- data.frame(
  Observacion = 1:length(modelo_step$residuals),
  CooksDistance = cooks.distance(modelo_step)
)

# Gráfico
ggplot(cooks_log_df, aes(x = Observacion, y = CooksDistance)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  geom_hline(yintercept = 4/length(modelo_log$residuals), color = "red", linetype = "dashed") +
  labs(title = "Distancia de Cook (Modelo Logarítmico)",
       x = "Observación",
       y = "Distancia de Cook") +
  theme_minimal()

