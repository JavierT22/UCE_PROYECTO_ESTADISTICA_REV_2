
#-------------------------------------------------------
# CARGAR LIBRER?AS
#-------------------------------------------------------
library(ggplot2)
library(fitdistrplus)
library(DT)
library(htmltools)

#-------------------------------------------------------
# CARGAR DATOS
#-------------------------------------------------------
datos <- read.csv("C:/Users/User/OneDrive/Documentos/estadistica/Electric and Alternative Fuel Charging Stations.csv",
                  header = TRUE, sep = ",", dec = ",")

#-------------------------------------------------------
# PROCESAR LONGITUD
#-------------------------------------------------------
datos$Longitude <- suppressWarnings(as.numeric(as.character(datos$Longitude)))
latitud_valida <- na.omit(datos$Longitude)
if (length(latitud_valida) == 0) stop("No hay datos v?lidos en Longitude")

n <- length(latitud_valida)
k <- ceiling(1 + 3.3 * log10(n))  # N? clases
#-------------------------------------------------------
# HISTOGRAMA + DENSIDAD NORMAL
#-------------------------------------------------------
ggplot(data.frame(latitud_valida), aes(x = latitud_valida)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(density)),
                 bins = k, color = "black", alpha = 0.9) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Grafica Nº4.1 Distribución Geográfica (Longitud) de las Estaciones de Carga",
       x = "Latitude", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 0)  # Sin rotar texto del eje X
  ) +
  scale_y_continuous(expand = c(0, 0))

#-------------------------------------------------------
# TRASLADAR PARA AJUSTES (DATOS > 0)
#-------------------------------------------------------
latitud_trasladada <- latitud_valida - min(latitud_valida) + 1e-6

#-------------------------------------------------------
# AJUSTES DE DISTRIBUCI?N
#-------------------------------------------------------
ajuste_lognormal <- fitdist(latitud_trasladada, "lnorm")
media_ln <- ajuste_lognormal$estimate["meanlog"]
sd_ln <- ajuste_lognormal$estimate["sdlog"]

ajuste_gamma <- fitdist(latitud_trasladada, "gamma")
shape_gamma <- ajuste_gamma$estimate["shape"]
rate_gamma <- ajuste_gamma$estimate["rate"]

#-------------------------------------------------------
# FRECUENCIAS
#-------------------------------------------------------
breaks_lat <- pretty(latitud_valida, n = k)
hist_data <- cut(latitud_valida, breaks = breaks_lat, right = FALSE)
tabla_frec <- as.data.frame(table(hist_data))
colnames(tabla_frec) <- c("Intervalo", "FO")
tabla_frec$Relativa <- tabla_frec$FO / sum(tabla_frec$FO)

lim_inf <- breaks_lat[-length(breaks_lat)]
lim_sup <- breaks_lat[-1]

# FE Log-Normal
lim_inf_ln <- lim_inf - min(latitud_valida) + 1e-6
lim_sup_ln <- lim_sup - min(latitud_valida) + 1e-6
P_lognorm <- plnorm(lim_sup_ln, media_ln, sd_ln) - plnorm(lim_inf_ln, media_ln, sd_ln)
FE_lognorm <- P_lognorm * n

# FE Gamma
P_gamma <- pgamma(lim_sup_ln, shape = shape_gamma, rate = rate_gamma) - 
  pgamma(lim_inf_ln, shape = shape_gamma, rate = rate_gamma)
FE_gamma <- P_gamma * n

#-------------------------------------------------------
# CHI? Y PEARSON
#-------------------------------------------------------
X2_ln <- sum((tabla_frec$FO - FE_lognorm)^2 / FE_lognorm)
pearson_ln <- cor(tabla_frec$FO, FE_lognorm)

X2_gamma <- sum((tabla_frec$FO - FE_gamma)^2 / FE_gamma)
pearson_gamma <- cor(tabla_frec$FO, FE_gamma)

df <- length(tabla_frec$FO) - 3
vc <- qchisq(0.95, df)
t_crit <- qt(1 - 0.05/2, n - 2)
r_crit <- sqrt(t_crit^2 / (t_crit^2 + (n - 2)))

#-------------------------------------------------------
# TABLA COMPARATIVA
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
    "Chi calculado",
    "Chi critico (95%)",
    "Acepta H0 (Chi?)",
    "Coef. Pearson",
    "Umbral Pearson (a=0.05)",
    "Acepta H0 (Pearson)"
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size:18px; font-weight: bold; font-family: Segoe UI; padding: 10px;',
    "Tabla Nº4.2 Comparación de Bondad de Ajuste: Log-Normal vs Gamma para Longitud"
  )
)

#-------------------------------------------------------
# GR?FICAS DE AJUSTE
#-------------------------------------------------------
# Histograma + Log-Normal
ggplot(data.frame(latitud_valida), aes(x = latitud_valida)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(density)),
                 bins = k, color = "black", alpha = 0.95, boundary = min(latitud_valida)) +
  scale_fill_gradient(low = "skyblue", high = "blue") +
  stat_function(fun = function(x) dlnorm(x - min(latitud_valida) + 1e-6,
                                         meanlog = media_ln, sdlog = sd_ln),
                color = "red", size = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Grafica Nº4.3 Ajuste Log-Normal a la Distribución de Longitud",
       x = "Longitud", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Histograma + Gamma
ggplot(data.frame(latitud_valida), aes(x = latitud_valida)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(density)),
                 bins = k, color = "black", alpha = 0.95, boundary = min(latitud_valida)) +
  scale_fill_gradient(low = "skyblue", high = "blue") +
  stat_function(fun = function(x) dgamma(x - min(latitud_valida) + 1e-6,
                                         shape = shape_gamma, rate = rate_gamma),
                color = "red", size = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Grafica Nº4.4 Ajuste Gamma a la Distribución de Longitud",
       x = "Longitud", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#-------------------------------------------------------
# CORRELACI?N FO vs FE (Log-Normal)
#-------------------------------------------------------
plot(tabla_frec$FO, FE_lognorm,
     main = "Grafica Nº4.5 Comparación de Frecuencia Observada vs Esperada – Modelo Log-Normal",
     xlab = "Frecuencia Observada", ylab = "Frecuencia Esperada",
     col = "skyblue", pch = 19)
abline(lm(FE_lognorm ~ tabla_frec$FO), col = "red", lwd = 2)

#-------------------------------------------------------
# CORRELACI?N FO vs FE (Gamma)
#-------------------------------------------------------
plot(tabla_frec$FO, FE_gamma,
     main = "Grafica Nº4.6 Comparación de Frecuencia Observada vs Esperada – Modelo Gamma",
     xlab = "Frecuencia Observada", ylab = "Frecuencia Esperada",
     col = "blue", pch = 19)
abline(lm(FE_gamma ~ tabla_frec$FO), col = "red", lwd = 2)
#-------------------------------------------------------
# C?LCULO DE PROBABILIDADES (P(X < Media))
#-------------------------------------------------------
media <- mean(latitud_trasladada)

prob_gamma <- pgamma(media, shape = shape_gamma, rate = rate_gamma)
prob_lognorm <- plnorm(media, meanlog = media_ln, sdlog = sd_ln)

cat("\n-------------------------------\n")
cat("Probabilidad P(X < Media):\n")
cat("-------------------------------\n")
cat("Gamma:      ", round(prob_gamma, 4), "\n")
cat("Log-Normal: ", round(prob_lognorm, 4), "\n")

#-------------------------------------------------------
# GR?FICA GAMMA ? SOMBRA HASTA LA MEDIA
#-------------------------------------------------------
x_vals <- seq(min(latitud_trasladada), max(latitud_trasladada), length.out = 1000)

df_gamma <- data.frame(
  x = x_vals,
  y = dgamma(x_vals, shape = shape_gamma, rate = rate_gamma)
)
df_gamma_fill <- subset(df_gamma, x <= media)

ggplot(df_gamma, aes(x = x, y = y)) +
  geom_area(data = df_gamma_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
  geom_line(color = "red", size = 1.2) +
  geom_vline(xintercept = media, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Grafica Nº4.7 Probabilidad Acumulada hasta la Media – Distribución GammaP(X < media)",
       x = "Longitud (ajustada)", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0)  # Sin rotar texto del eje X
  ) +
  scale_y_continuous(expand = c(0, 0))

#-------------------------------------------------------
# GR?FICA LOG-NORMAL ? SOMBRA HASTA LA MEDIA
#-------------------------------------------------------
df_lognorm <- data.frame(
  x = x_vals,
  y = dlnorm(x_vals, meanlog = media_ln, sdlog = sd_ln)
)
df_lognorm_fill <- subset(df_lognorm, x <= media)

ggplot(df_lognorm, aes(x = x, y = y)) +
  geom_area(data = df_lognorm_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
  geom_line(color = "red", size = 1.2) +
  geom_vline(xintercept = media, linetype = "dashed", color = "blue", size = 1) +
  labs(title = "Grafica Nº4.8 Probabilidad Acumulada hasta la Media – Distribución Log-NormalP(X < media)",
       x = "Longitud (ajustada)", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),   
    axis.text.x = element_text(angle = 0)  # Sin rotar texto del eje X
  ) +
  scale_y_continuous(expand = c(0, 0))

#-------------------------------------------------------
# C?LCULO ESTAD?STICO PREVIO AL T.L.C.
#-------------------------------------------------------
media_poblacional <- mean(latitud_trasladada)
desv_std_poblacional <- sd(latitud_trasladada)
error_estandar <- desv_std_poblacional / sqrt(n)

#-------------------------------------------------------
# TEOREMA DEL L?MITE CENTRAL ? INTERVALO 95%
#-------------------------------------------------------
limite_superior <- media_poblacional + 1.96 * error_estandar
limite_inferior <- media_poblacional - 1.96 * error_estandar

cat("\n------------------------------------------\n")
cat("Teorema del L?mite Central (95% confianza):\n")
cat("------------------------------------------\n")
cat("Media poblacional estimada:  ", round(media_poblacional, 4), "\n")
cat("Desviaci?n est?ndar (s):     ", round(desv_std_poblacional, 4), "\n")
cat("Error est?ndar (s/vn):       ", round(error_estandar, 4), "\n")
cat("L?mite inferior (95%):       ", round(limite_inferior, 4), "\n")
cat("L?mite superior (95%):       ", round(limite_superior, 4), "\n")

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
    "Desviaci?n Est?ndar (s)",
    "Error Est?ndar (s/vn)",
    "L?mite Inferior (95%)",
    "L?mite Superior (95%)"
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size:18px; font-weight: bold; font-family: Segoe UI; padding: 10px;',
    "Tabla Nº4.9 Parámetros Inferenciales del Teorema del Límite Central para la Variable Longitud (IC 95%)"
  )
)

