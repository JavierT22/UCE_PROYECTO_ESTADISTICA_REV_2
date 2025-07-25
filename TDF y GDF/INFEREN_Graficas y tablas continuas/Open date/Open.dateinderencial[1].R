#-------------------------------------------------------
# CARGAR LIBRER?AS
#-------------------------------------------------------
library(ggplot2)
library(fitdistrplus)
library(dplyr)
library(tibble)
library(DT)

#-------------------------------------------------------
# CARGAR DATOS Y CONVERTIR FECHAS
#-------------------------------------------------------
datos <- read.csv("C:/Users/User/OneDrive/Documentos/estadistica/Electric and Alternative Fuel Charging Stations.csv",
                  header = TRUE, sep = ",", dec = ",")

datos$Open.Date <- as.Date(datos$Open.Date, tryFormats = c("%m/%d/%Y", "%d/%m/%Y"))
open_numeric <- as.numeric(datos$Open.Date - as.Date("1970-01-01"))
open_numeric <- na.omit(open_numeric)
open_numeric <- open_numeric[open_numeric > 0]

#-------------------------------------------------------
# FILTRAR OUTLIERS USANDO REGLA DE TUKEY (IQR)
#-------------------------------------------------------
Q1 <- quantile(open_numeric, 0.25)
Q3 <- quantile(open_numeric, 0.75)
IQR_val <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR_val
lim_sup <- Q3 + 1.5 * IQR_val
open_numeric <- open_numeric[open_numeric >= lim_inf & open_numeric <= lim_sup]

#-------------------------------------------------------
# TRANSFORMACI?N LOGAR?TMICA
#-------------------------------------------------------
open_numeric <- log(open_numeric)

#-------------------------------------------------------
# PAR?METROS B?SICOS
#-------------------------------------------------------
n <- length(open_numeric)
media <- mean(open_numeric)
desv <- sd(open_numeric)
error <- desv / sqrt(n)
k <- 7
#k <- 5  # n?mero de clases
ggplot(data.frame(open_numeric), aes(x = open_numeric)) +
  geom_histogram(aes(y = after_stat(density / max(density)),
                     fill = after_stat(density / max(density))),
                 bins = k, color = "black", alpha = 0.8) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Grafica Nº8.1 Histograma de Densidad Normalizada – Log(Días desde 1970)",
       x = "Log(Dias desde 1970)", y = "Densidad normalizada") +
  scale_x_continuous(expand = c(0, 0)) +  # <- elimina espacios en eje X
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 0)  # Sin rotar texto del eje X
  ) +
  scale_y_continuous(expand = c(0, 0))

#-------------------------------------------------------
# AJUSTES A MODELOS
#-------------------------------------------------------
ajuste_gamma <- fitdist(open_numeric, "gamma")
ajuste_lognorm <- fitdist(open_numeric, "lnorm")

#-------------------------------------------------------
# PAR?METROS B?SICOS
#-------------------------------------------------------
k <- 7  # n?mero de barras
breaks_manual <- seq(min(open_numeric), max(open_numeric), length.out = k + 1)

#-------------------------------------------------------
# AJUSTES A MODELOS
#-------------------------------------------------------
ajuste_gamma <- fitdist(open_numeric, "gamma")
ajuste_lognorm <- fitdist(open_numeric, "lnorm")

#-------------------------------------------------------
# HISTOGRAMA CON CURVA GAMMA (normalizado y sin espacios)
#-------------------------------------------------------
# Generar histograma manual
hist_data <- hist(open_numeric, breaks = breaks_manual, plot = FALSE)

# Data frame del histograma normalizado
df_hist_gamma <- data.frame(
  x = hist_data$mids,
  density_norm = hist_data$density / max(hist_data$density),
  width = diff(breaks_manual)[1]
)

# Curva gamma normalizada
x_vals <- seq(min(open_numeric), max(open_numeric), length.out = 500)
y_gamma <- dgamma(x_vals, shape = ajuste_gamma$estimate["shape"],
                  rate = ajuste_gamma$estimate["rate"])
y_gamma_norm <- y_gamma / max(hist_data$density)

df_gamma <- data.frame(x = x_vals, y = y_gamma_norm)

# Gr?fica Gamma
ggplot() +
  geom_col(data = df_hist_gamma, aes(x = x, y = density_norm, fill = density_norm),
           color = "black", alpha = 0.8, width = df_hist_gamma$width) +
  geom_line(data = df_gamma, aes(x = x, y = y), color = "red", size = 1.2) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +  # Gradiente de color
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(limits = range(breaks_manual), expand = c(0, 0)) +
  labs(title = "Grafica Nº8.2 Histograma Normalizado con Curva Ajustada – Distribución Gamma",
       x = "Log(Dias desde 1970)", y = "Densidad Normalizada") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0)) +
  guides(fill = "none")  # Oculta leyenda de color (opcional)
#-------------------------------------------------------
# HISTOGRAMA CON CURVA LOG-NORMAL (misma densidad y cortes)
#-------------------------------------------------------
# Calcular histograma manualmente con los mismos cortes
hist_data_ln <- hist(open_numeric, breaks = breaks_manual, plot = FALSE)

# Crear data frame para ggplot con densidad normalizada
df_hist_ln <- data.frame(
  x = hist_data_ln$mids,
  density_norm = hist_data_ln$density / max(hist_data_ln$density),
  width = diff(breaks_manual)[1]
)

# Crear curva log-normal normalizada
x_vals_ln <- seq(min(open_numeric), max(open_numeric), length.out = 500)
y_lognorm <- dlnorm(x_vals_ln, 
                    meanlog = ajuste_lognorm$estimate["meanlog"], 
                    sdlog = ajuste_lognorm$estimate["sdlog"])
y_lognorm_norm <- y_lognorm / max(hist_data_ln$density)

df_lognorm <- data.frame(x = x_vals_ln, y = y_lognorm_norm)

# Gr?fico
ggplot() +
  geom_col(data = df_hist_ln, aes(x = x, y = density_norm, fill = density_norm),
           color = "black", alpha = 0.9, width = df_hist_ln$width) +
  geom_line(data = df_lognorm, aes(x = x, y = y),
            color = "red", size = 1.2) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(limits = range(breaks_manual), expand = c(0, 0)) +
  labs(title = "Grafica Nº8.3 	Histograma Normalizado con Curva Ajustada – Distribución Log-Normal",
       x = "Log(Dias desde 1970)", y = "Densidad Normalizada") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "none")


#-------------------------------------------------------
# CORRELACI?N FO vs FE ? MODELO GAMMA (Base R)
#-------------------------------------------------------
ajuste_gamma$aic
ajuste_lognorm$aic
histograma <- hist(open_numeric, breaks = k, plot = FALSE)
FO <- histograma$counts
breaks <- histograma$breaks

FE_gamma <- diff(pgamma(breaks, shape = ajuste_gamma$estimate["shape"],
                        rate = ajuste_gamma$estimate["rate"])) * n

plot(FO, FE_gamma,
     main = "Grafica Nº8.4 Comparación de Frecuencia Observada y Esperada – Distribución Gamma",
     xlab = "Frecuencia Observada", ylab = "Frecuencia Esperada",
     col = "darkblue", pch = 19)
abline(lm(FE_gamma ~ FO), col = "red", lwd = 2)
#-------------------------------------------------------
# CORRELACI?N FO vs FE ? MODELO LOG-NORMAL (Base R)
#-------------------------------------------------------
FE_lognorm <- diff(plnorm(breaks, meanlog = ajuste_lognorm$estimate["meanlog"],
                          sdlog = ajuste_lognorm$estimate["sdlog"])) * n

plot(FO, FE_lognorm,
     main = "Grafica Nº8.5 	Comparación de Frecuencia Observada y Esperada – Distribución Log-Normal",
     xlab = "Frecuencia Observada", ylab = "Frecuencia Esperada",
     col = "darkblue", pch = 19)
abline(lm(FE_lognorm ~ FO), col = "red", lwd = 2)
#-------------------------------------------------------
# PRUEBAS DE BONDAD DE AJUSTE (CHI-CUADRADO Y PEARSON)
#-------------------------------------------------------
evaluar_ajuste <- function(ajuste, nombre_modelo) {
  histograma <- hist(open_numeric, breaks = k, plot = FALSE)
  obs <- histograma$counts
  breaks <- histograma$breaks
  
  if (nombre_modelo == "Gamma") {
    esperados <- diff(pgamma(breaks, shape = ajuste$estimate["shape"],
                             rate = ajuste$estimate["rate"])) * n
  } else if (nombre_modelo == "Log-Normal") {
    esperados <- diff(plnorm(breaks, meanlog = ajuste$estimate["meanlog"],
                             sdlog = ajuste$estimate["sdlog"])) * n
  }
  
  esperados[esperados < 1] <- 1  # evita divisi?n por cero
  chi_sq <- sum((obs - esperados)^2 / esperados)
  pearson_corr <- cor(obs, esperados)
  
  chi_result <- ifelse(pchisq(chi_sq, df = k - 2, lower.tail = FALSE) >= 0.05, "Aprueba", "No aprueba")
  pearson_result <- ifelse(pearson_corr >= 0.80, "Aprueba", "No aprueba")
  
  tibble(
    Modelo = nombre_modelo,
    Chi_Cuadrado = round(chi_sq, 3),
    `Chi (= 95%)` = chi_result,
    Pearson = round(pearson_corr, 3),
    `Pearson (= 0.80)` = pearson_result
  )
}

# Evaluaci?n de modelos
resultados <- bind_rows(
  evaluar_ajuste(ajuste_gamma, "Gamma"),
  evaluar_ajuste(ajuste_lognorm, "Log-Normal")
)
#-------------------------------------------------------
# PAR?METROS PARA UMBRAL CR?TICO
#-------------------------------------------------------
vc <- qchisq(0.95, df = k - 2)        # Valor cr?tico Chi?
r_crit <- 0.80                        # Umbral Pearson

# Extraer valores desde resultados existentes
X2_gamma <- resultados$Chi_Cuadrado[resultados$Modelo == "Gamma"]
X2_ln    <- resultados$Chi_Cuadrado[resultados$Modelo == "Log-Normal"]
pearson_gamma <- resultados$Pearson[resultados$Modelo == "Gamma"]
pearson_ln    <- resultados$Pearson[resultados$Modelo == "Log-Normal"]

#-------------------------------------------------------
# TABLA COMPARATIVA ? FORMATO PERSONALIZADO
#-------------------------------------------------------
tabla_test_comparativa <- data.frame(
  Modelo = c("Log-Normal", "Gamma"),
  `Chi? calculado` = round(c(X2_ln, X2_gamma), 4),
  `Chi? cr?tico (95%)` = round(vc, 4),
  `?Acepta H0? (Chi?)` = ifelse(c(X2_ln, X2_gamma) < vc, "Si", "No"),
  `Coef. Pearson` = round(c(pearson_ln, pearson_gamma), 4),
  `Umbral Pearson (a=0.05)` = round(r_crit, 4),
  `?Acepta H0? (Pearson)` = ifelse(c(pearson_ln, pearson_gamma) >= r_crit, "Si", "No")
)

# Mostrar tabla din?mica
DT::datatable(
  tabla_test_comparativa,
  options = list(
    pageLength = 5,
    autoWidth = TRUE,
    scrollX = TRUE,
    dom = 't',
    columnDefs = list(
      list(width = '120px', className = 'dt-center', targets = 0:6)
    ),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({",
      "'background-color': 'darkblue',",
      "'color': 'white',",
      "'font-weight': 'bold',",
      "'font-family': 'Segoe UI'",
      "});",
      "}"
    ),
    rowCallback = JS(
      "function(row, data, index) {",
      "$(row).css('font-family', 'Segoe UI');",
      "$(row).css({'font-weight': 'bold', 'background-color': '#f2f2f2'});",
      "$('td', row).css({'text-align': 'center', 'padding': '8px'});",
      "}"
    )
  ),
  class = 'stripe hover compact nowrap',
  rownames = FALSE,
  colnames = c(
    "Modelo",
    "Chi? calculado",
    "Chi? critico (95%)",
    "Acepta H0 (Chi?)",
    "Coef. Pearson",
    "Umbral Pearson (a=0.05)",
    "Acepta H0 (Pearson)"
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size:18px; font-weight: bold; font-family: Segoe UI; padding: 10px;',
    "Tabla Nº8.6 Comparación de Bondad de Ajuste entre Distribuciones Gamma y Log-Normal para la Variable Fecha de Apertura"
  )
)



#-------------------------------------------------------
# C?LCULO DE PROBABILIDADES (Ejemplo: P(X < Media))
#-------------------------------------------------------
prob_gamma <- pgamma(media, shape = ajuste_gamma$estimate["shape"],
                     rate = ajuste_gamma$estimate["rate"])
prob_lognorm <- plnorm(media, meanlog = ajuste_lognorm$estimate["meanlog"],
                       sdlog = ajuste_lognorm$estimate["sdlog"])

cat("\n-------------------------------\n")
cat("Probabilidad P(X < Media):\n")
cat("-------------------------------\n")
cat("Gamma:      ", round(prob_gamma, 4), "\n")
cat("Log-Normal: ", round(prob_lognorm, 4), "\n")
library(ggplot2)

# Secuencia de valores x para graficar
x_vals <- seq(min(open_numeric), max(open_numeric), length.out = 1000)

# Curva Gamma
df_gamma <- data.frame(
  x = x_vals,
  y = dgamma(x_vals, shape = ajuste_gamma$estimate["shape"], rate = ajuste_gamma$estimate["rate"])
)
df_gamma_fill <- subset(df_gamma, x <= media)

ggplot(df_gamma, aes(x = x, y = y)) +
  geom_area(data = df_gamma_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
  geom_line(color = "red", size = 1.2) +
  geom_vline(xintercept = media, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Grafica Nº8.7 Probabilidad Acumulada Bajo la Distribución Gamma hasta la Media ",
       x = "Log(Dias desde 1970)", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
# Curva Log-Normal
df_lognorm <- data.frame(
  x = x_vals,
  y = dlnorm(x_vals, meanlog = ajuste_lognorm$estimate["meanlog"], sdlog = ajuste_lognorm$estimate["sdlog"])
)
df_lognorm_fill <- subset(df_lognorm, x <= media)

ggplot(df_lognorm, aes(x = x, y = y)) +
  geom_area(data = df_lognorm_fill, aes(x = x, y = y), fill = "red", alpha = 0.5) +
  geom_line(color = "skyblue", size = 1.2) +
  geom_vline(xintercept = media, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Grafica Nº8.8 Probabilidad Acumulada Bajo la Distribución Log-Normal hasta la Media ",
       x = "Log(Dias desde 1970)", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



#-------------------------------------------------------
# TEOREMA DEL L?MITE CENTRAL
#-------------------------------------------------------
limite_superior <- media + 1.96 * error
limite_inferior <- media - 1.96 * error

cat("\n------------------------------------------\n")
cat("Teorema del L?mite Central (95% confianza):\n")
cat("------------------------------------------\n")
cat("Media poblacional estimada:  ", round(media, 2), "\n")
cat("Desviaci?n est?ndar (s):     ", round(desv, 2), "\n")
cat("Error est?ndar (s/vn):       ", round(error, 2), "\n")
cat("L?mite inferior (95%):       ", round(limite_inferior, 2), "\n")
cat("L?mite superior (95%):       ", round(limite_superior, 2), "\n")
#-------------------------------------------------------
# TABLA ? PAR?METROS DEL TEOREMA DEL L?MITE CENTRAL
#-------------------------------------------------------
tabla_tlc <- data.frame(
  `Media Poblacional` = round(media_poblacional, 4),
  `Desviaci?n Est?ndar (s)` = round(desv_std_poblacional, 4),
  `Error Est?ndar (s/vn)` = round(error_estandar, 4),
  `L?mite Inferior (95%)` = round(limite_inferior, 4),
  `L?mite Superior (95%)` = round(limite_superior, 4)
)

DT::datatable(
  tabla_tlc,
  options = list(
    pageLength = 5,
    autoWidth = TRUE,
    scrollX = TRUE,
    dom = 't',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({",
      "'background-color': 'darkblue',",
      "'color': 'white',",
      "'font-weight': 'bold',",
      "'font-family': 'Segoe UI'",
      "});",
      "}"
    ),
    rowCallback = JS(
      "function(row, data, index) {",
      "$(row).css('font-family', 'Segoe UI');",
      "$(row).css({'font-weight': 'bold', 'background-color': '#eaf2ff'});",
      "$('td', row).css({'text-align': 'center', 'padding': '8px'});",
      "}"
    )
  ),
  class = 'stripe hover compact nowrap',
  rownames = FALSE,
  colnames = c(
    "Media Poblacional",
    "Desviación Estanndar (s)",
    "Error Estandar (s/vn)",
    "Limite Inferior (95%)",
    "Limite Superior (95%)"
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size:18px; font-weight: bold; font-family: Segoe UI; padding: 10px;',
    "Tabla Nº 8.9 Parámetros Inferenciales del Teorema del Límite Central para la Variable Fecha de Apertura"
  )
)

