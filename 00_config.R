# =====================================================================
# 00_config.R
# Proyecto Integrador 3 - Scoring de riesgo de credito con variables macro
# Extension del modelo de Bermudez-Vera, Mosquera & Manotas (Risks 2025, 13(2):20)
# ---------------------------------------------------------------------
# Configuracion central: rutas, paquetes, semilla, mapeo de variables,
# niveles de factores y funciones auxiliares comunes a todos los scripts.
# Cargar SIEMPRE al inicio con:  source("codigo/00_config.R")
# =====================================================================

# ---- 1. Ruta base -----------------------------------------------------
# Ajustar UNA sola vez a la raiz del proyecto (carpeta que contiene
# datos/, codigo/, resultados/). El resto de rutas son relativas.
ruta.base <- normalizePath(".", winslash = "/", mustWork = FALSE)
# Si se ejecuta desde RStudio con el .Rproj en la raiz, "." ya es correcto.
# En su defecto, descomentar y fijar a mano, por ejemplo:
# ruta.base <- "C:/ruta/a/la/carpeta/del/proyecto"

dir.datos      <- file.path(ruta.base, "datos")
dir.codigo     <- file.path(ruta.base, "codigo")
dir.resultados <- file.path(ruta.base, "resultados")
if (!dir.exists(dir.resultados)) dir.create(dir.resultados, showWarnings = FALSE)

archivo.bd    <- file.path(dir.datos, "BD_CONSUMO_2020.xlsx")
archivo.macro <- file.path(dir.datos, "Variables macro 2005 a 2023.xlsx")

# ---- 2. Paquetes ------------------------------------------------------
paquetes <- c("readxl", "dplyr", "tidyr", "stringr", "lubridate",
              "pROC", "ggplot2", "purrr", "tibble")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)
invisible(lapply(paquetes, require, character.only = TRUE))

# ---- 3. Reproducibilidad ---------------------------------------------
SEMILLA <- 2026
set.seed(SEMILLA)

# ---- 4. Parametros de diseno (decisiones del proyecto) ---------------
N_REPS        <- 200        # repeticiones para AUC pareado
PROP_TRAIN    <- 0.80       # particion 80/20
UMBRAL_METODO <- "youden"   # umbral estimado en TRAIN por Youden
USAR_REMUESTREO <- FALSE     # SIN SMOTE/remuestreo (decision del proyecto)
LAGS_MESES    <- c(3, 6)     # rezagos macro: 3m (L1) y 6m (L2)

# ---- 5. Mapeo de variables -------------------------------------------
# Nombres tal como aparecen en la hoja "Data" (fila de encabezado 2).
# Identificacion y respuesta
col.id      <- "ID"
col.fecha   <- "fecha.referencia"   # formato anio_mes (aaaamm), p.ej. 202006
col.calpos  <- "cal.pos"            # peor calificacion en los 12 meses SIGUIENTES (A-E)
col.default <- "default"            # respuesta binaria (Si/No)

# Las 25 variables internas del autor, agrupadas (segun la propuesta).
# OJO: "dias.de.mora" y una "calificacion crediticia" externa contemporanea
# figuran en la lista del autor pero NO estan en BD_CONSUMO_2020.xlsx.
# Ver 01_carga_validacion.R (se detectan y se reporta la ausencia).
vars.sociodemo <- c("sexo", "edad", "estrato", "nivel.estudio",
                    "estado.civil", "salario.1")                      # 6
vars.internas  <- c("antiguedad", "ahorros.1", "deuda.con.int.1",
                    "deuda.int.1", "cuotas.restantes", "valor.cuota.1",
                    "cred.viv.int", "niv.deuda.int.salario",
                    "niv.deuda.int.ahorro", "niv.cuota")              # 10 (falta dias.de.mora -> 11 del autor)
vars.externas  <- c("deuda.con.ext.1", "deuda.tc.1", "deuda.ext.1",
                    "cred.viv.ext", "niv.deuda.ext.salario",
                    "niv.deuda.ext.ahorro", "niv.tc")                 # 7 (falta calificacion externa -> 8 del autor)
# Modelo extendido disponible en datos (internas + externas presentes):
vars.extendido <- c(vars.sociodemo, vars.internas, vars.externas)

# --- Modelo base de 9 variables del autor (CV, CCE, EC, EDAD, SAL, AHO, DE, NTC, NDAHO)
# Mapeo CONFIRMADO por el usuario (2026-09):
#   CV    = antiguedad            (la propuesta dice literal "antiguedad (CV)")
#   AHO   = ahorros.1             (la propuesta dice "el ahorro (AHO)")
#   SAL   = salario.1             (la propuesta dice "el salario (SAL)")
#   EDAD  = edad
#   EC    = estado.civil
#   CCE   = cred.con.ext          (Tiene credito de consumo externo)
#   DE    = deuda.externa         (indicador Si/No de deuda externa)
#   NTC   = niv.tc                (nivel de deuda por tarjeta de credito)
#   NDAHO = niv.deuda.ext.ahorro  (nivel de deuda externa por ahorro)
vars.base <- c(CV = "antiguedad", CCE = "cred.con.ext", EC = "estado.civil",
               EDAD = "edad", SAL = "salario.1", AHO = "ahorros.1",
               DE = "deuda.externa", NTC = "niv.tc", NDAHO = "niv.deuda.ext.ahorro")

# ---- 6. Tipos de variable --------------------------------------------
vars.categoricas <- c("sexo", "estrato", "nivel.estudio", "estado.civil",
                      "tipo.vivienda", "estamento", "cred.viv.int",
                      "cred.con.ext", "cred.viv.ext", "tc.ext",
                      "deuda.externa", "credito.viv")

# ---- 7. Variables macro a conservar de la fuente ----------------------
# Frecuencia de la fuente: TODAS trimestrales (cierres de trimestre).
# TIR se EXCLUYE por identidad exacta: Var.IPC = Tasa Banrep - TIR(Banrep).
# Se conserva un conjunto no colineal (niveles/variaciones independientes).
macro.vars.fuente <- c(
  "Variación anual del IPC (e.a.)",
  "Tasa Banrep",
  "DTF (90 días e.a.)",
  "Tasa de desempleo 7 áreas",
  "Variación anual del PIB de los últimos doce meses a precios constantes",
  "Variación trimestral TRM (COP)",
  "Precio Petróleo Brent (USD)"
)
# Variables a EXCLUIR explicitamente por colinealidad/derivacion:
macro.vars.excluir <- c(
  "Tasa de interés real e.a. (Banrep-Inflación)",   # = Banrep - Var.IPC  (identidad)
  "Tasa de interés real e.a. (DTF-Inflación)"       # = DTF    - Var.IPC  (identidad)
)

# Bloques para el indice macro por PCA (PC1 por bloque, sobre periodos unicos).
# Decision del proyecto: en vez de 1 solo PC1 o de las 7 crudas (colineales),
# 3 indices interpretables de baja dimension.
macro.bloques <- list(
  precios_tasas = c("Variación anual del IPC (e.a.)", "Tasa Banrep", "DTF (90 días e.a.)"),
  actividad     = c("Variación anual del PIB de los últimos doce meses a precios constantes",
                    "Precio Petróleo Brent (USD)", "Tasa de desempleo 7 áreas"),
  cambiario     = c("Variación trimestral TRM (COP)")
)

# Forzar codificacion UTF-8 en los nombres macro para que el emparejamiento
# con los nombres leidos del Excel (UTF-8) no dependa del locale del sistema.
Encoding(macro.vars.fuente)  <- "UTF-8"
Encoding(macro.vars.excluir) <- "UTF-8"
macro.bloques <- lapply(macro.bloques, function(v) { Encoding(v) <- "UTF-8"; v })

# ---- 8. Funciones auxiliares -----------------------------------------
# aaaamm (numerico) -> fecha (primer dia del mes)
aaaamm_a_fecha <- function(x) {
  x <- as.integer(x)
  lubridate::make_date(x %/% 100, x %% 100, 1)
}
# fecha -> etiqueta de trimestre "aaaaQn"
fecha_a_trim <- function(f) paste0(lubridate::year(f), "Q", lubridate::quarter(f))
# cierre de trimestre de una fecha (para cruzar con la fuente macro trimestral)
fecha_a_cierre_trim <- function(f) lubridate::ceiling_date(f, "quarter") - lubridate::days(1)

# Umbral de Youden a partir de un objeto pROC::roc
umbral_youden <- function(roc_obj) {
  co <- pROC::coords(roc_obj, "best", best.method = "youden",
                     ret = "threshold", transpose = FALSE)
  thr <- suppressWarnings(as.numeric(co[["threshold"]]))
  thr <- thr[is.finite(thr)]
  if (length(thr) == 0) return(0.5)
  thr[1]
}
# Exactitud balanceada dado observado (0/1) y prediccion (0/1)
exactitud_balanceada <- function(y, yhat) {
  sens <- sum(yhat == 1 & y == 1) / max(1, sum(y == 1))
  spec <- sum(yhat == 0 & y == 0) / max(1, sum(y == 0))
  (sens + spec) / 2
}


# ---- Funciones de modelado (usadas por 04_modelos.R y 05_validacion.R) ----
# Formula "y ~ v1 + v2 + ..." con nombres entre comillas invertidas.
formula_modelo <- function(vars)
  as.formula(paste("y ~", paste(sprintf("`%s`", vars), collapse = " + ")))

# SMOTE simple sobre una matriz numerica de predictores (sin intercepto).
# Genera sinteticos de la clase minoritaria interpolando con k vecinos.
# Se aplica SOLO sobre el conjunto de entrenamiento.
smote_train <- function(X, y, k = 5) {
  Xmin <- X[y == 1, , drop = FALSE]; nmin <- nrow(Xmin); nmaj <- sum(y == 0)
  need <- nmaj - nmin
  if (need <= 0 || nmin < 2) return(list(X = X, y = y))
  kk <- max(1, min(k, nmin - 1))
  D <- as.matrix(stats::dist(Xmin)); diag(D) <- Inf
  nb <- t(apply(D, 1, function(d) order(d)[seq_len(kk)]))
  syn <- matrix(NA_real_, need, ncol(X)); colnames(syn) <- colnames(X)
  for (t in seq_len(need)) {
    i <- sample.int(nmin, 1); j <- nb[i, sample.int(kk, 1)]; g <- runif(1)
    syn[t, ] <- Xmin[i, ] + g * (Xmin[j, ] - Xmin[i, ])
  }
  list(X = rbind(X, syn), y = c(y, rep(1L, need)))
}

# Regresion logistica por IRLS con cresta L2 minima (estabilidad numerica
# ante separacion). X incluye la columna de intercepto; no se penaliza el
# intercepto. Equivale al ajuste usado en el documento (lambda = 1e-3).
.fit_logit_ridge <- function(X, y, lam = 1e-3, iters = 100) {
  p <- ncol(X); b <- rep(0, p)
  P <- diag(lam, p); P[1, 1] <- 0
  for (it in seq_len(iters)) {
    eta <- pmin(pmax(as.vector(X %*% b), -30), 30)
    mu  <- 1 / (1 + exp(-eta)); W <- pmax(mu * (1 - mu), 1e-9)
    z   <- eta + (y - mu) / W
    XtW <- t(X * W)
    bn  <- solve(XtW %*% X + P, XtW %*% z)
    if (max(abs(bn - b)) < 1e-8) { b <- bn; break }
    b <- bn
  }
  as.vector(b)
}

# Ajusta regresion logistica y evalua en test.
# Diseno construido sobre train+test juntos para garantizar columnas
# consistentes (evita desajustes por niveles de factor ausentes).
# Umbral de Youden en train (o 0.5 con SMOTE). Metricas: AUC, sensibilidad
# (recall), especificidad, BA y PR = Retencion de Desempeno (BA_test / BA_train).
ajustar_evaluar <- function(train, test, vars, usar_smote = FALSE) {
  vars <- vars[vars %in% names(train)]
  comb <- rbind(train[, c("y", vars), drop = FALSE], test[, c("y", vars), drop = FALSE])
  mm  <- model.matrix(formula_modelo(vars), data = comb)
  ntr <- nrow(train)
  cn  <- setdiff(colnames(mm), "(Intercept)")
  Xtr <- mm[seq_len(ntr), cn, drop = FALSE]
  Xte <- mm[(ntr + 1):nrow(mm), cn, drop = FALSE]
  ytr <- train$y; yte <- test$y
  mu  <- colMeans(Xtr); sdv <- apply(Xtr, 2, sd); sdv[!is.finite(sdv) | sdv == 0] <- 1
  Xtr <- scale(Xtr, center = mu, scale = sdv)
  Xte <- scale(Xte, center = mu, scale = sdv)
  if (usar_smote) { sm <- smote_train(Xtr, ytr); Xfit <- sm$X; yfit <- sm$y; thr <- 0.5 }
  else            { Xfit <- Xtr; yfit <- ytr; thr <- NA_real_ }
  b <- .fit_logit_ridge(cbind(1, Xfit), yfit)
  predp <- function(Z) as.numeric(stats::plogis(cbind(1, Z) %*% b))
  p_tr <- predp(Xtr); p_te <- predp(Xte)
  if (!usar_smote) thr <- umbral_youden(pROC::roc(ytr, p_tr, quiet = TRUE, direction = "<"))
  auc_te <- as.numeric(pROC::auc(pROC::roc(yte, p_te, quiet = TRUE, direction = "<")))
  ba_te  <- exactitud_balanceada(yte, as.integer(p_te >= thr))
  ba_tr  <- exactitud_balanceada(ytr, as.integer(p_tr >= thr))
  rec    <- if (sum(yte == 1) > 0) sum(p_te >= thr & yte == 1) / sum(yte == 1) else NA_real_
  esp    <- if (sum(yte == 0) > 0) sum(p_te <  thr & yte == 0) / sum(yte == 0) else NA_real_
  list(prob_test = p_te, umbral = thr, AUC = auc_te, BA = ba_te,
       PR = if (ba_tr > 0) ba_te / ba_tr else NA_real_, recall = rec, especificidad = esp)
}

message("00_config.R cargado. ruta.base = ", ruta.base)
