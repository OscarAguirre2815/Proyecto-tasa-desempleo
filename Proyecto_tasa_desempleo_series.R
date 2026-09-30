# =========================================================
#   MODELO ESTRUCTURAL (Estado-Espacio) - Tasa de desempleo
# =========================================================

getwd()
list.files()


library(readxl)
library(dlm)
library(forecast)

# -------- 1) Cargar y armar ts --------
Desempleo_nacional <- read_excel(
  "C:/Users/MSI/Downloads/Tasa_desempleo.xlsx",
  sheet = "Series de datos", range = "A1:B295",
  col_types = c("date", "numeric")
)

Desempleo_nacional$Fecha <- as.Date(Desempleo_nacional$Fecha)
Desempleo_nacional <- Desempleo_nacional[order(Desempleo_nacional$Fecha), ]

start_year  <- as.numeric(format(min(Desempleo_nacional$Fecha), "%Y"))
start_month <- as.numeric(format(min(Desempleo_nacional$Fecha), "%m"))

y <- ts(
  Desempleo_nacional$Tasa_desempleo,
  start = c(start_year, start_month),
  frequency = 12
)

par(mfrow = c(1,1))
plot(y, main="Tasa de desempleo (original)", ylab="Tasa", xlab="Tiempo")

# Log (si hay ceros, evita log(0))
if(any(y <= 0, na.rm = TRUE)) stop("Hay valores <= 0 en la serie: no se puede usar log().")
ly <- log(y)
plot(ly, main="log(Tasa de desempleo)", ylab="log(tasa)", xlab="Tiempo")


# -------- 2) Definir MODELO ESTRUCTURAL --------
build_structural <- function(par){
  V      <- exp(par[1])  # var observación
  W_mu   <- exp(par[2])  # var nivel
  W_beta <- exp(par[3])  # var pendiente
  W_seas <- exp(par[4])  # var estacionalidad
  
  s <- as.integer(frequency(ly))  # 12
  
  # Tendencia local lineal (mu, beta) + estacionalidad
  mod <- dlmModPoly(order = 2, dV = V, dW = c(W_mu, W_beta)) +
    dlmModSeas(s, dV = 0, dW = rep(W_seas, s - 1))
  
  mod
}

# -------- 3) Estimar hiperparámetros por MLE --------
init_par <- log(c(
  var(ly, na.rm = TRUE),  # V
  1e-4,                   # W_mu
  1e-6,                   # W_beta
  1e-4                    # W_seas
))

fit <- dlmMLE(ly, parm = init_par, build = build_structural,
              control = list(maxit = 500))

fit$convergence  # 0 = OK
mod_hat <- build_structural(fit$par)


# -------- 4) Filtro y suavizado --------
filt   <- dlmFilter(ly, mod_hat)
smooth <- dlmSmooth(filt)

# Estados suavizados
st <- dropFirst(smooth$s)  # matriz (n x m)

mu_hat   <- st[, 1]  # nivel
beta_hat <- st[, 2]  # pendiente

# Estacionalidad (primer estado estacional)
# orden de estados: [mu, beta, seas1, seas2, ..., seas(11)]
seas_hat <- st[, 3]
seas_ts  <- ts(seas_hat, start = start(ly), frequency = frequency(ly))
plot(seas_hat)

# Ajuste one-step-ahead
fitted_ly <- dropFirst(filt$f)

# ---- Gráficas básicas ----
par(mfrow = c(3,1))
ts.plot(ly, main="log(y) y ajuste (one-step-ahead)", ylab="log(y)", xlab="Tiempo", col="darkgrey")
lines(fitted_ly, lty="longdash", col=2)

ts.plot(ts(mu_hat, start=start(ly), frequency=frequency(ly)),
        main="Nivel suavizado mu_t", ylab="mu_t", xlab="Tiempo")

ts.plot(seas_ts, main="Estacionalidad suavizada s_t", ylab="s_t", xlab="Tiempo")
abline(h=0, lty=2)
par(mfrow = c(1,1))

# (Opcional) Reconstrucción de señal estimada: mu + seas
signal_hat <- ts(mu_hat, start=start(ly), frequency=frequency(ly)) + seas_ts
plot(ly, main="log(y) vs (mu_t + s_t)", ylab="log(y)", xlab="Tiempo", col="darkgrey")
lines(signal_hat, lty="longdash", col=4)


# -------- 5) Diagnóstico de residuos (innovaciones) --------
e <- residuals(filt, sd = FALSE)
e <- na.omit(as.numeric(e))

par(mfrow=c(1,2))
acf(e, main="ACF innovaciones")
pacf(e, main="PACF innovaciones")
par(mfrow=c(1,1))
hist(e, breaks=20, main="Histograma innovaciones", xlab="e_t")


# -------- 6) Pronóstico h pasos adelante --------
h <- 18
fc <- dlmForecast(filt, nAhead = h)

mean_ly <- as.numeric(fc$f)
sd_ly   <- sqrt(as.numeric(unlist(fc$Q)))

lo_ly <- mean_ly - 1.96 * sd_ly
hi_ly <- mean_ly + 1.96 * sd_ly

# ts para graficar pronóstico (en log)
end_time <- tsp(ly)[2]
freq <- frequency(ly)

fc_ts <- ts(mean_ly, start = end_time + 1/freq, frequency = freq)
lo_ts <- ts(lo_ly,   start = end_time + 1/freq, frequency = freq)
hi_ts <- ts(hi_ly,   start = end_time + 1/freq, frequency = freq)

plot(ly, main=paste("Pronóstico estructural (log), h =", h), ylab="log(tasa)", xlab="Tiempo")
lines(fc_ts, lty="longdash", col=2)
lines(lo_ts, lty="dotted", col=3)
lines(hi_ts, lty="dotted", col=3)

# Tabla final en escala original
pronostico_structural <- data.frame(
  mean = exp(mean_ly),
  lo95 = exp(lo_ly),
  hi95 = exp(hi_ly)
)
pronostico_structural




# =========================
# GRÁFICAS EXTRA
# =========================

#Serie vs filtro
par(mfrow = c(1,1))

ts.plot(ly, col="darkgrey", main="log(y) y nivel filtrado (mu_t | y_1..y_t)",
        ylab="log(y)", xlab="Tiempo")
lines(ts(dropFirst(filt$m)[,1], start=start(ly), frequency=frequency(ly)),
      lty="longdash", col=2)


#Serie vs suavizado
par(mfrow = c(1,1))

ts.plot(ly, col="darkgrey", main="log(y) y nivel suavizado (mu_t | y_1..y_n)",
        ylab="log(y)", xlab="Tiempo")
lines(ts(dropFirst(smooth$s)[,1], start=start(ly), frequency=frequency(ly)),
      lty="longdash", col=3)


### serie vs señal estimada
mu_ts <- ts(dropFirst(smooth$s)[,1], start=start(ly), frequency=frequency(ly))
signal_hat <- mu_ts + seas_ts

par(mfrow=c(1,1))
ts.plot(ly, col="darkgrey", main="log(y) vs señal estimada (mu_t + s_t)",
        ylab="log(y)", xlab="Tiempo")
lines(signal_hat, lty="longdash", col=4)

legend("topleft",
       legend=c("log(y)", "mu_t + s_t"),
       lty=c(1,2), col=c("darkgrey",4), bty="n")


cbind(seas_hat, dropFirst(filt$m)[,1], dropFirst(smooth$s)[,1])

# Asegurar objetos ts con el mismo tiempo
mu_filt_ts   <- ts(dropFirst(filt$m)[,1],   start = start(ly), frequency = frequency(ly))
mu_smooth_ts <- ts(dropFirst(smooth$s)[,1], start = start(ly), frequency = frequency(ly))
seas_ts      <- ts(dropFirst(smooth$s)[,3], start = start(ly), frequency = frequency(ly))  # estacionalidad suave (col 3)

# Señal estimada (mu + s)
signal_hat <- mu_smooth_ts + seas_ts

# -------- 1) Serie vs nivel filtrado --------
par(mfrow = c(1,1))
ts.plot(ly, col="darkgrey",
        main="log(y) vs nivel filtrado (mu_t | y_1..y_t)",
        ylab="log(y)", xlab="Tiempo")
lines(mu_filt_ts, lty="longdash", col=2)
legend("topleft", legend=c("log(y)", "mu filtrado"),
       col=c("darkgrey",2), lty=c(1,2), bty="n")

# -------- 2) Serie vs nivel suavizado --------
par(mfrow = c(1,1))
ts.plot(ly, col="darkgrey",
        main="log(y) vs nivel suavizado (mu_t | y_1..y_n)",
        ylab="log(y)", xlab="Tiempo")
lines(mu_smooth_ts, lty="longdash", col=3)
legend("topleft", legend=c("log(y)", "mu suavizado"),
       col=c("darkgrey",3), lty=c(1,2), bty="n")

# -------- 3) Serie vs señal estimada (mu + s) --------
par(mfrow=c(1,1))
ts.plot(ly, col="darkgrey",
        main="log(y) vs señal estimada (mu_t + s_t)",
        ylab="log(y)", xlab="Tiempo")
lines(signal_hat, lty="longdash", col=4)
legend("topleft", legend=c("log(y)", "mu + s"),
       col=c("darkgrey",4), lty=c(1,2), bty="n")

# -------- 4) Estacionalidad sola (como tu gráfica S_t del ejemplo) --------
par(mfrow=c(1,1))
ts.plot(seas_ts,
        main="Componente estacional s_t (suavizado)",
        ylab="s_t", xlab="Tiempo")
abline(h=0, lty=2)

# -------- 5) Tabla tipo cbind (como el ejemplo) --------
# (Todo ya viene alineado en tiempo y misma longitud)
comparacion <- cbind(
  seas_ts,
  mu_filt_ts,
  mu_smooth_ts,
  signal_hat
)
colnames(comparacion) <- c("s_t (suave)", "mu filtrado", "mu suavizado", "mu+s (señal)")
comparacion

# -------- 6) (OPCIONAL) Igualito al ejemplo: y vs filtro/suave y abajo s_t --------
par(mfrow=c(2,1))
ts.plot(ly, col="darkgrey",
        main="log(y) y mu filtrado", ylab="log(y)", xlab="Tiempo")
lines(mu_filt_ts, lty="longdash", col=2)
ts.plot(seas_ts, main="s_t (suave)", ylab="s_t", xlab="Tiempo")
abline(h=0, lty=2)

par(mfrow=c(2,1))
ts.plot(ly, col="darkgrey",
        main="log(y) y mu suavizado", ylab="log(y)", xlab="Tiempo")
lines(mu_smooth_ts, lty="longdash", col=3)
ts.plot(seas_ts, main="s_t (suave)", ylab="s_t", xlab="Tiempo")
abline(h=0, lty=2)

par(mfrow=c(1,1))


comparacion_ok <- cbind(
  y_real = ly,
  fitted_one_step = ts(dropFirst(filt$f), start=start(ly), frequency=frequency(ly)),
  mu_smooth = ts(dropFirst(smooth$s)[,1], start=start(ly), frequency=frequency(ly)),
  s_smooth  = seas_ts,
  mu_plus_s = signal_hat
)
head(comparacion_ok, 12)
