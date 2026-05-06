# =============================================================================
# Predicción del incumplimiento del umbral sanitario de Lepeophtheirus salmonis
# en centros de cultivo mediante regresión logística binaria
#
# Autor: Manolo Marcos Meza Rodríguez
# Universidad Nacional Agraria La Molina (UNALM)
# =============================================================================

# ---- 1. CARGA DE LIBRERÍAS ---------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)


# ---- 2. CARGA Y PREPARACIÓN DE DATOS -----------------------------------------
# Los datos provienen del DFO (Fisheries and Oceans Canada)
# Fuente pública: https://open.canada.ca/
dat <- read_csv(
  "datos/DFO_farm_abundance.csv",
  show_col_types = FALSE
)

# Variable respuesta: 1 si excede el umbral (>3 hembras adultas por pez)
dat$y <- as.integer(!is.na(dat$lep_af_ab) & dat$lep_af_ab > 3)

# Selección de predictores relevantes
dat_small <- dat[, c(
  "y", "year", "month", "num_pens_sampled",
  "chalimus_ab", "lep_motile_ab", "cal_motile_ab"
)]

# Imputación simple de valores faltantes
dat_small[is.na(dat_small)] <- 0

# Mes como factor
dat_small$month <- factor(dat_small$month)

str(dat_small)
summary(dat_small$y)


# ---- 3. MODELO LOGÍSTICO PRINCIPAL -------------------------------------------
m <- glm(
  y ~ factor(month) + num_pens_sampled +
    lep_motile_ab + cal_motile_ab,
  data   = dat_small,
  family = binomial()
)

summary(m)

# Pseudo-R² basado en desviancias
R2 <- (1 - m$deviance / m$null.deviance) * 100
R2
# [1] 77.74

# Razones de odds (OR) e intervalos de confianza al 95%
OR <- exp(coef(m))
IC <- exp(confint.default(m, level = 0.95))


# ---- 4. VALIDACIÓN 70/30 -----------------------------------------------------
set.seed(100)

indice <- sample(
  2,
  nrow(dat_small),
  replace = TRUE,
  prob    = c(0.7, 0.3)
)

Datos_E <- dat_small[indice == 1, ]   # Entrenamiento (70%)
Datos_P <- dat_small[indice == 2, ]   # Prueba (30%)

# Ajuste del modelo en el conjunto de entrenamiento
Modelo <- glm(
  y ~ factor(month) + num_pens_sampled +
    lep_motile_ab + cal_motile_ab,
  data   = Datos_E,
  family = binomial()
)

summary(Modelo)


# ---- 5. EVALUACIÓN DEL MODELO ------------------------------------------------
# Predicción sobre conjunto de prueba
Prob_Pred <- predict(Modelo, Datos_P, type = "response")
Pred      <- ifelse(Prob_Pred >= 0.5, "Si_Excede", "No_Excede")
Obs       <- ifelse(Datos_P$y == 1, "Si_Excede", "No_Excede")

# Matriz de confusión
Tabla_Conf <- table(
  "Observado:" = Obs,
  "Predecido:" = Pred
)
Tabla_Conf

# Componentes
VP <- Tabla_Conf["Si_Excede", "Si_Excede"]   # Verdaderos positivos
FN <- Tabla_Conf["Si_Excede", "No_Excede"]   # Falsos negativos
FP <- Tabla_Conf["No_Excede", "Si_Excede"]   # Falsos positivos
VN <- Tabla_Conf["No_Excede", "No_Excede"]   # Verdaderos negativos
N  <- VP + FN + FP + VN

# Métricas de desempeño
TA  <- ((VP + VN) / N) * 100   # Tasa de acierto
TE  <- ((FN + FP) / N) * 100   # Tasa de error
TVP <- (VP / (VP + FN)) * 100  # Sensibilidad
TVN <- (VN / (FP + VN)) * 100  # Especificidad
TFP <- (FP / (VN + FP)) * 100  # Tasa de falso positivo
TFN <- (FN / (VP + FN)) * 100  # Tasa de falso negativo
PP  <- (VP / (VP + FP)) * 100  # Valor predictivo positivo
PN  <- (VN / (VN + FN)) * 100  # Valor predictivo negativo

metricas <- round(c(
  TA = TA, TE = TE,
  TVP = TVP, TVN = TVN,
  TFP = TFP, TFN = TFN,
  PP  = PP,  PN  = PN
), 2)

metricas
# TA: 98.10%  -  Tasa de acierto
# TVP: 74.21% -  Sensibilidad
# TVN: 99.57% -  Especificidad
# PP:  91.47% -  Valor predictivo positivo
# PN:  98.43% -  Valor predictivo negativo


# ---- 6. TABLAS DE RESULTADOS -------------------------------------------------
# Tabla 1. Coeficientes, OR e IC
coefs <- summary(m)$coefficients

tabla1 <- data.frame(
  Parametro       = rownames(coefs),
  Beta            = round(coefs[, "Estimate"], 3),
  Error_estandar  = round(coefs[, "Std. Error"], 3),
  Valor_z         = round(coefs[, "z value"], 3),
  p_valor         = signif(coefs[, "Pr(>|z|)"], 3),
  OR              = round(OR, 3),
  IC_inf_95       = round(IC[, 1], 3),
  IC_sup_95       = round(IC[, 2], 3)
)

kable(
  tabla1,
  caption = "Tabla 1. Coeficientes del modelo logístico, razones de odds e intervalos de confianza al 95%."
)

# Tabla 2. Métricas de desempeño
tabla2 <- data.frame(
  Metrica = c(
    "Tasa de acierto (%)",
    "Tasa de error (%)",
    "Sensibilidad (TVP) (%)",
    "Especificidad (TVN) (%)",
    "Tasa de falso positivo",
    "Tasa de falso negativo",
    "Valor predictivo positivo (%)",
    "Valor predictivo negativo (%)"
  ),
  Valor = as.numeric(metricas)
)

kable(
  tabla2,
  caption = "Tabla 2. Desempeño del modelo de regresión logística en el conjunto de prueba."
)


# ---- 7. PREDICCIÓN EN ESCENARIOS NUEVOS --------------------------------------
nuevo <- data.frame(
  month            = levels(dat_small$month),
  num_pens_sampled = 2,
  lep_motile_ab    = 7,
  cal_motile_ab    = 5
)

prob_nueva  <- predict(m, newdata = nuevo, type = "response")
clase_nueva <- ifelse(prob_nueva >= 0.5, "Si_Excede", "No_Excede")

tabla_pred <- data.frame(
  Mes              = levels(dat_small$month),
  Probabilidad     = round(prob_nueva, 4),
  Clasificacion    = clase_nueva
)
print(tabla_pred)


# ---- 8. VISUALIZACIONES ------------------------------------------------------

# Figura 1. Prevalencia de incumplimiento por mes
prev_mes <- dat_small %>%
  group_by(month) %>%
  summarize(
    n     = n(),
    casos = sum(y == 1),
    prev  = casos / n,
    .groups = "drop"
  )

ggplot(prev_mes, aes(x = month, y = prev)) +
  geom_col(fill = "steelblue") +
  labs(
    x     = "Mes",
    y     = "Proporción de observaciones que exceden el umbral",
    title = "Figura 1. Prevalencia de incumplimiento del umbral sanitario por mes"
  ) +
  theme_minimal()

# Figura 2. Relación entre L. salmonis móviles y probabilidad predicha
dat_plot       <- dat_small
dat_plot$phat  <- predict(m, type = "response")

ggplot(dat_plot, aes(x = lep_motile_ab, y = phat)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  coord_cartesian(xlim = c(0, quantile(dat_plot$lep_motile_ab, 0.99))) +
  labs(
    x     = "Abundancia de estadios móviles de L. salmonis por pez",
    y     = "Probabilidad predicha de exceder el umbral",
    title = "Figura 2. Relación entre L. salmonis móviles y riesgo de incumplimiento"
  ) +
  theme_minimal()

# =============================================================================
# FIN DEL ANÁLISIS
# =============================================================================
