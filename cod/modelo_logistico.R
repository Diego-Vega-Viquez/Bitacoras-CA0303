library(tidyverse)
library(haven)
library(ggridges)
library(viridis)
library(scales)
library(car)
library(broom)
library(qqplotr)
library(nnet)
library(pscl)
library(caret)


# Ajustar el modelo multinomial
modelo_mnlogit <- multinom(condic_activi ~ A5 + A4 + region + zona + Cap_grado + Nivel_Instru + ING_PERCAPITA_HOGAR,
                           data = enadis)

summary(modelo_mnlogit)

# Extraer resumen del modelo
tabla_modelo_mnlogit <- tidy(modelo_mnlogit)

# Agregar columna de Odds Ratios
tabla_modelo_mnlogit <- tabla_modelo_mnlogit %>%
  mutate(Odds_Ratio = exp(estimate))

# Ver la tabla
tabla_modelo_mnlogit

# Eliminar el intercepto para el gráfico
tabla_plot <- tabla_modelo_mnlogit %>%
  filter(term != "(Intercept)")

# Gráfico
ggplot(tabla_plot, aes(x = Odds_Ratio, y = term, color = y.level)) +
  geom_point() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Odds Ratios del modelo logístico multinomial",
       x = "Odds Ratio",
       y = "Variable") +
  theme_minimal()

#validacion del modelo

#calculo de pseudo R2

pR2(modelo_mnlogit)

# Predecir las clases más probables
predicciones_clase <- predict(modelo_mnlogit)

# Crear tabla simple de conteos
tabla_confusion <- table(Predicho = predicciones_clase, Real = enadis$condic_activi)

# Convertir a data frame
tabla_confusion_df <- as.data.frame(tabla_confusion)

# Visualizar con ggplot2
ggplot(tabla_confusion_df, aes(x = Real, y = Predicho)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de Confusión (Modelo Multinomial)",
       x = "Condición Real",
       y = "Condición Predicha") +
  theme_minimal()

# Prueba de Razón de Verosimilitud variable por variable
Anova(modelo_mnlogit, type = "III", test.statistic = "LR")


