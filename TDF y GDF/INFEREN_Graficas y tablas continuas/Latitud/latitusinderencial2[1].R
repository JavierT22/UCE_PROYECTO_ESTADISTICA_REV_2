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
# PROCESAR LATITUD
#-------------------------------------------------------
datos$Latitude <- suppressWarnings(as.numeric(as.character(datos$Latitude)))
latitud_valida <- na.omit(datos$Latitude)
if (length(latitud_valida) == 0) stop("No hay datos v?lidos en Latitude")
n <- length(latitud_valida)
media <- mean(latitud_valida)
desv <- sd(latitud_valida)
error <- desv / sqrt(n)
limite_superior <- media + 1.96 * error
limite_inferior <- media - 1.96 * error
k <- ceiling(1 + 3.3 * log10(n))  # N? clases

#-------------------------------------------------------
# HISTOGRAMA + DENSIDAD NORMAL
#-------------------------------------------------------
ggplot(data.frame(latitud_valida), aes(x = latitud_valida)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(density)),
                 bins = k, color = "black", alpha = 0.9) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Grafica Nº3.1: Distribución Geográfica de las Estaciones de Carga según Latitud",
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
# AJUSTES DE DISTRIBUCI?N
#-------------------------------------------------------
ajuste_normal <- fitdist(latitud_valida, "norm")

latitud_trasladada <- latitud_valida - min(latitud_valida) + 1e-6
ajuste_lognormal <- fitdist(latitud_trasladada, "lnorm")
media_ln <- ajuste_lognormal$estimate["meanlog"]
sd_ln <- ajuste_lognormal$estimate["sdlog"]

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

# FE Normal
P_norm <- pnorm(lim_sup, media, desv) - pnorm(lim_inf, media, desv)
FE_norm <- P_norm * n

# FE Log-Normal
lim_inf_ln <- lim_inf - min(latitud_valida) + 1e-6
lim_sup_ln <- lim_sup - min(latitud_valida) + 1e-6
P_lognorm <- plnorm(lim_sup_ln, media_ln, sd_ln) - plnorm(lim_inf_ln, media_ln, sd_ln)
FE_lognorm <- P_lognorm * n

#-------------------------------------------------------
# CHI? Y PEARSON
#-------------------------------------------------------
X2_norm <- sum((tabla_frec$FO - FE_norm)^2 / FE_norm)
pearson_norm <- cor(tabla_frec$FO, FE_norm)

X2_ln <- sum((tabla_frec$FO - FE_lognorm)^2 / FE_lognorm)
pearson_ln <- cor(tabla_frec$FO, FE_lognorm)

df <- length(tabla_frec$FO) - 3
vc <- qchisq(0.95, df)
t_crit <- qt(1 - 0.05/2, n - 2)
r_crit <- sqrt(t_crit^2 / (t_crit^2 + (n - 2)))

#-------------------------------------------------------
# TABLA COMPARATIVA DE MODELOS
#-------------------------------------------------------
tabla_test_comparativa <- data.frame(
  Modelo = c("Normal", "Log-Normal"),
  `Chi? calculado` = round(c(X2_norm, X2_ln), 4),
  `Chi? cr?tico (95%)` = round(vc, 4),
  `?Acepta H0? (Chi?)` = ifelse(c(X2_norm, X2_ln) < vc, "S?", "No"),
  `Coef. Pearson` = round(c(pearson_norm, pearson_ln), 4),
  `Umbral Pearson (a=0.05)` = round(r_crit, 4),
  `?Acepta H0? (Pearson)` = ifelse(c(pearson_norm, pearson_ln) >= r_crit, "S?", "No")
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
      "'background-color': 'blue',",
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
    "Chi? cr?tico (95%)",
    "?Acepta H0? (Chi?)",
    "Coef. Pearson",
    "Umbral Pearson (a=0.05)",
    "?Acepta H0? (Pearson)"
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: center; font-size:18px; font-weight: bold; font-family: Segoe UI; padding: 10px;',
    "Tabla Nº3.2 Comparación de la Adecuación de los Modelos Normal y Log-Normal para la Variable Latitud"
  )
)

#-------------------------------------------------------
# GR?FICA DE AJUSTE NORMAL Y LOG-NORMAL
#-------------------------------------------------------
# Ajuste Normal
ggplot(data.frame(latitud_valida), aes(x = latitud_valida)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(density)),
                 bins = k, color = "black", alpha = 0.95, boundary = min(latitud_valida)) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  stat_function(fun = dnorm, args = list(mean = media, sd = desv),
                color = "red", size = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +   # Quitar espacio en eje X
  scale_y_continuous(expand = c(0, 0)) +   # Quitar espacio en eje Y
  labs(title = "Grafica Nº3.3 Ajuste de la Distribución Normal sobre la Latitud de las Estaciones de Carga",
       x = "Latitud", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Ajuste Log-Normal
ggplot(data.frame(latitud_valida), aes(x = latitud_valida)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(density)),
                 bins = k, color = "black", alpha = 0.95, boundary = min(latitud_valida)) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  stat_function(fun = function(x) dlnorm(x - min(latitud_valida) + 1e-6,
                                         meanlog = media_ln, sdlog = sd_ln),
                color = "red", size = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +   # Quitar espacio en eje X
  scale_y_continuous(expand = c(0, 0)) +   # Quitar espacio en eje Y
  labs(title = "Grafica Nº3.4 Ajuste de la Distribución Log-Normal sobre la Latitud de las Estaciones de Carga",
       x = "Latitud", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#-------------------------------------------------------
# CORRELACI?N FO vs FE (NORMAL)
#-------------------------------------------------------
plot(tabla_frec$FO, FE_norm,
     main = "Grafica Nº3.5 Relación entre Frecuencia Observada y Esperada según Modelo Normal",
     xlab = "Frecuencia Observada", ylab = "Frecuencia Esperada",
     col = "darkblue", pch = 19)
abline(lm(FE_norm ~ tabla_frec$FO), col = "red", lwd = 2)

#-------------------------------------------------------
# CORRELACI?N FO vs FE (LOG-NORMAL)
#-------------------------------------------------------
plot(tabla_frec$FO, FE_lognorm,
     main = "Grafica Nº3.6 Relación entre Frecuencia Observada y Esperada según Modelo Log-Normal",
     xlab = "Frecuencia Observada", ylab = "Frecuencia Esperada",
     col = "skyblue", pch = 19)
abline(lm(FE_lognorm ~ tabla_frec$FO), col = "red", lwd = 2)
#-------------------------------------------------------
# C?LCULO DE PROBABILIDADES (P(X < Media))
#-------------------------------------------------------
prob_norm <- pnorm(media, mean = media, sd = desv)
prob_lognorm <- plnorm(media, meanlog = media_ln, sdlog = sd_ln)

cat("\n-------------------------------\n")
cat("Probabilidad P(X < Media):\n")
cat("-------------------------------\n")
cat("Normal:     ", round(prob_norm, 4), "\n")
cat("Log-Normal: ", round(prob_lognorm, 4), "\n")

#-------------------------------------------------------
# GR?FICA ? DISTRIBUCI?N NORMAL CON SOMBRA
#-------------------------------------------------------
x_vals <- seq(min(latitud_valida), max(latitud_valida), length.out = 1000)

df_norm <- data.frame(
  x = x_vals,
  y = dnorm(x_vals, mean = media, sd = desv)
)
df_norm_fill <- subset(df_norm, x <= media)

ggplot(df_norm, aes(x = x, y = y)) +
  geom_area(data = df_norm_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_vline(xintercept = media, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Grafica Nº3.7 Distribución Normal de la Latitud con Sombreado para P(X < media)",
       x = "Latitud", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#-------------------------------------------------------
# GR?FICA ? DISTRIBUCI?N LOG-NORMAL CON SOMBRA
#-------------------------------------------------------
df_lognorm <- data.frame(
  x = x_vals,
  y = dlnorm(x_vals - min(latitud_valida) + 1e-6, meanlog = media_ln, sdlog = sd_ln)
)
df_lognorm_fill <- subset(df_lognorm, x <= media)

ggplot(df_lognorm, aes(x = x, y = y)) +
  geom_area(data = df_lognorm_fill, aes(x = x, y = y), fill = "skyblue", alpha = 0.5) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_vline(xintercept = media, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Grafica Nº3.8 Distribución Log-Normal de la Latitud con Sombreado para P(X < media)",
       x = "Latitud", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

#-------------------------------------------------------
# TEOREMA DEL L?MITE CENTRAL (IC al 95%)
#-------------------------------------------------------
media_poblacional <- media
desv_std_poblacional <- desv
error_estandar <- error
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
# TABLA DIN?MICA ? TEOREMA DEL L?MITE CENTRAL
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
      "$(row).css({'font-weight': 'bold', 'background-color': '#f2fff2'});",
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
    "Tabla Nº3.9 Teorema del Límite Central aplicado a la Latitud (IC 95%)"
  )
)

#-------------------------------------------------------
# TEOREMA DEL L?MITE CENTRAL ? SIMULACI?N GR?FICA
#-------------------------------------------------------
set.seed(123)
num_muestras <- 1000
tam_muestra <- 30
medias_muestrales <- replicate(num_muestras, mean(sample(latitud_valida, tam_muestra, replace = TRUE)))

ggplot(data.frame(media = medias_muestrales), aes(x = media)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgreen", color = "black", alpha = 0.9) +
  stat_function(fun = dnorm, args = list(mean = media_poblacional, sd = desv_std_poblacional / sqrt(tam_muestra)),
                color = "darkgreen", linewidth = 1.2) +
  labs(title = "Gráfica Nº3.10  Distribución de Medias Muestrales: Aplicación del Teorema del Límite Central a la Latitud

",
       x = "Media Muestral", y = "Densidad") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )




