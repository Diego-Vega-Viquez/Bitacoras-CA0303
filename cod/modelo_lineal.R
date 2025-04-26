library(tidyverse)
library(haven)
library(ggridges)
library(viridis)
library(scales)
library(car)
library(broom)
library(qqplotr)


#No olvide cargar base de datos en manejo_de_datos.R

#1-Pruebas para aplicar modelo de regresion lineal multiple

#Revisar indice de correlacion entre edad e ingreso (unicas variables numericas):

cor(enadis$A5, enadis$ING_PERCAPITA_HOGAR, use = "complete.obs")

#Correlacion de 0.0647 es muy baja. No hay colinealidad

#Realizar tablas cruzadas entre variables categoricas para ver si hay relaciones muy fuertes

disc_sexo <- prop.table(table(enadis$Cap_grado, enadis$A4), margin = 1) 
disc_sexo

disc_region <- prop.table(table(enadis$Cap_grado, enadis$region), margin = 1)
disc_region

disc_zona <- prop.table(table(enadis$Cap_grado, enadis$zona), margin = 1) 
disc_zona

disc_condic <- prop.table(table(enadis$Cap_grado, enadis$condic_activi), margin = 1) 
disc_condic

disc_puesto <- prop.table(table(enadis$Cap_grado, enadis$B8a), margin = 1) 
disc_puesto

disc_horas <- prop.table(table(enadis$Cap_grado, enadis$B7), margin = 1) 
disc_horas

disc_nivinst <- prop.table(table(enadis$Cap_grado, enadis$Nivel_Instru), margin = 1) 
disc_nivinst

#la discapacidad no se relaciona fuertemente con otras variables categoricas de control

# Relación de SEXO con otras variables categóricas
sexo_region <- prop.table(table(enadis$A4, enadis$region), margin = 1)
sexo_region

sexo_zona <- prop.table(table(enadis$A4, enadis$zona), margin = 1)
sexo_zona

sexo_condic <- prop.table(table(enadis$A4, enadis$condic_activi), margin = 1)
sexo_condic #los hombres ocupados podrian estar sobrerepresentados

sexo_puesto <- prop.table(table(enadis$A4, enadis$B8a), margin = 1)
sexo_puesto

sexo_horas <- prop.table(table(enadis$A4, enadis$B7), margin = 1)
sexo_horas

sexo_nivinst <- prop.table(table(enadis$A4, enadis$Nivel_Instru), margin = 1)
sexo_nivinst

# -----------------------------------------------

# Relación de REGION con otras variables categóricas
region_zona <- prop.table(table(enadis$region, enadis$zona), margin = 1)
region_zona

region_condic <- prop.table(table(enadis$region, enadis$condic_activi), margin = 1)
region_condic

region_puesto <- prop.table(table(enadis$region, enadis$B8a), margin = 1)
region_puesto

region_horas <- prop.table(table(enadis$region, enadis$B7), margin = 1)
region_horas

region_nivinst <- prop.table(table(enadis$region, enadis$Nivel_Instru), margin = 1)
region_nivinst

# -----------------------------------------------

# Relación de ZONA con otras variables categóricas
zona_condic <- prop.table(table(enadis$zona, enadis$condic_activi), margin = 1)
zona_condic

zona_puesto <- prop.table(table(enadis$zona, enadis$B8a), margin = 1)
zona_puesto

zona_horas <- prop.table(table(enadis$zona, enadis$B7), margin = 1)
zona_horas

zona_nivinst <- prop.table(table(enadis$zona, enadis$Nivel_Instru), margin = 1)
zona_nivinst

# -----------------------------------------------

# Relación de CONDICIÓN DE ACTIVIDAD con otras variables categóricas
condic_puesto <- prop.table(table(enadis$condic_activi, enadis$B8a), margin = 1)
condic_puesto #la condicion de actividad y el puesto no son independientes: se omitira el puesto en el modelo lineal

condic_horas <- prop.table(table(enadis$condic_activi, enadis$B7), margin = 1)
condic_horas #la condicion de actividad y las horas laboradas no son independientes

condic_nivinst <- prop.table(table(enadis$condic_activi, enadis$Nivel_Instru), margin = 1)
condic_nivinst

# Puesto de trabajo vs Horas laboradas
puesto_horas <- prop.table(table(enadis$B8a, enadis$B7), margin = 1)
puesto_horas

# Puesto de trabajo vs Nivel de instrucción
puesto_nivinst <- prop.table(table(enadis$B8a, enadis$Nivel_Instru), margin = 1)
puesto_nivinst

# Horas laboradas vs Nivel de instrucción
horas_nivinst <- prop.table(table(enadis$B7, enadis$Nivel_Instru), margin = 1)
horas_nivinst

#No se hayo colinealidad ni relaciones muy fuertes entre variables
#Procedemos a poner a prueba al modelo con VIF

modelo_vif <- lm(ING_PERCAPITA_HOGAR ~ A5 + A4 + region + zona + Cap_grado + Nivel_Instru + condic_activi, data = enadis)

vif(modelo_vif)

#"Se ajustó un modelo preliminar para calcular los factores de inflación de la varianza (VIF) y verificar la existencia de colinealidad entre las variables independientes. Los valores de GVIF^(1/(2*Df)) fueron cercanos a 1 en todas las variables, indicando que no existe multicolinealidad significativa. Por tanto, se consideró apropiado continuar con la estimación del modelo de regresión lineal múltiple."

#Aplicacion de modelo lineal

modelo_final <- lm(ING_PERCAPITA_HOGAR ~ A5 + A4 + region + zona + Cap_grado + Nivel_Instru + condic_activi, data = enadis)

tabla_modelo <- tidy(modelo_final)

print(tabla_modelo)

#Diagnostico del modelo:
##

residuos_df <- data.frame(
  Ajustados = modelo_final$fitted.values,
  Residuos = modelo_final$residuals
)

ggplot(residuos_df, aes(x = Ajustados, y = Residuos)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuos vs Valores Ajustados",
       x = "Valores Ajustados",
       y = "Residuos") +
  theme_minimal()

#parece que la varianza no es constante (se supone que los valores deben ser cercanos a 0 uniformemente)
#aplicamos una transformacion logaritmica al ingreso a ver que

enadis$log_ingreso <- log(enadis$ING_PERCAPITA_HOGAR + 1)

modelo_log <- lm(log_ingreso ~ A5 + A4 + region + zona + Cap_grado + Nivel_Instru + condic_activi, data = enadis)

tabla_modelo_log <- tidy(modelo_log)

print(tabla_modelo_log)

#Diagnostico del modelo logaritmico

residuos_df <- data.frame(
  Ajustados = modelo_log$fitted.values,
  Residuos = modelo_log$residuals
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
  Residuos = modelo_log$residuals
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
  Observacion = 1:length(modelo_log$residuals),
  CooksDistance = cooks.distance(modelo_log)
)

# Gráfico
ggplot(cooks_log_df, aes(x = Observacion, y = CooksDistance)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  geom_hline(yintercept = 4/length(modelo_log$residuals), color = "red", linetype = "dashed") +
  labs(title = "Distancia de Cook (Modelo Logarítmico)",
       x = "Observación",
       y = "Distancia de Cook") +
  theme_minimal()

