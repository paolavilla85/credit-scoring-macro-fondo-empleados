# =====================================================================
# 08_sintonizacion.R
# Sintonizacion de los dos ensambles (Random Forest y XGBoost) por
# busqueda en malla con validacion cruzada estratificada de 5 pliegues
# (criterio: AUC). Reporta, para cada especificacion, el AUC de validacion
# cruzada (con los mejores hiperparametros) y el AUC en prueba (80/20
# repetido). Produce la Tabla 8, la Figura 7 y el Anexo B del documento.
# Produce: resultados/08_sintonizacion.csv, resultados/08_hiperparametros.csv,
#          resultados/08_sintonizacion.txt
# NOTA: XGBoost requiere el paquete 'xgboost' (API xgb.train / xgb.DMatrix).
# =====================================================================
source(file.path("codigo", "00_config.R"))
for (p in c("randomForest", "xgboost")) {
  if (!p %in% rownames(installed.packages())) install.packages(p)
}
tiene_xgb <- requireNamespace("xgboost", quietly = TRUE)
suppressMessages(require(randomForest))

dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores
con <- file(file.path(dir.resultados, "08_sintonizacion.txt"), open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }
reportar("=== 08. SINTONIZACION POR VALIDACION CRUZADA (5 pliegues) ===")

specs <- list(MB = predictores$base, `MB+macroL3` = predictores$macroL3_base,
              ME = predictores$extendido, `ME+macroL3` = predictores$macroL3_ext)
fml <- function(v) as.formula(paste("y ~", paste(sprintf("`%s`", v), collapse = " + ")))

auc_roc <- function(y, s) as.numeric(pROC::auc(pROC::roc(y, s, quiet = TRUE, direction = "<")))
# diseno numerico (dummies) consistente para una especificacion
design_full <- function(vars) {
  vars <- vars[vars %in% names(datos)]
  mm <- model.matrix(fml(vars), data = datos)[, -1, drop = FALSE]
  list(X = mm, y = datos$y)
}
# pliegues estratificados
make_folds <- function(y, k = 5, seed = SEMILLA) {
  set.seed(seed); fold <- integer(length(y))
  for (cl in unique(y)) { idx <- which(y == cl); fold[idx] <- sample(rep(seq_len(k), length.out = length(idx))) }
  fold
}

cv_auc_rf <- function(X, y, folds, mtry, nodesize, ntree = 300) {
  a <- c()
  for (f in sort(unique(folds))) {
    tr <- folds != f; te <- !tr
    m <- randomForest(x = X[tr, , drop = FALSE], y = factor(y[tr], levels = c(0, 1)),
                      mtry = mtry, nodesize = nodesize, ntree = ntree)
    p <- predict(m, X[te, , drop = FALSE], type = "prob")[, "1"]
    a <- c(a, auc_roc(y[te], p))
  }
  mean(a)
}
cv_auc_xgb <- function(X, y, folds, max_depth, eta, nrounds) {
  a <- c()
  for (f in sort(unique(folds))) {
    tr <- folds != f; te <- !tr
    dtr <- xgboost::xgb.DMatrix(data = X[tr, , drop = FALSE], label = y[tr])
    m <- xgboost::xgb.train(params = list(objective = "binary:logistic",
          max_depth = max_depth, eta = eta, subsample = 0.9, colsample_bytree = 0.9),
          data = dtr, nrounds = nrounds, verbose = 0)
    p <- predict(m, xgboost::xgb.DMatrix(data = X[te, , drop = FALSE]))
    a <- c(a, auc_roc(y[te], p))
  }
  mean(a)
}
# AUC en prueba (80/20 repetido) con hiperparametros fijos
test_auc <- function(vars, algo, par, NR = 50) {
  d <- design_full(vars); X <- d$X; y <- d$y
  i1 <- which(y == 1); i0 <- which(y == 0); A <- numeric(NR)
  for (r in seq_len(NR)) {
    set.seed(1000 + r)
    tr <- c(sample(i0, floor(0.8 * length(i0))), sample(i1, floor(0.8 * length(i1))))
    te <- setdiff(seq_along(y), tr)
    if (algo == "RF") {
      m <- randomForest(x = X[tr, , drop = FALSE], y = factor(y[tr], levels = c(0, 1)),
                        mtry = par$mtry, nodesize = par$nodesize, ntree = 300)
      p <- predict(m, X[te, , drop = FALSE], type = "prob")[, "1"]
    } else {
      dtr <- xgboost::xgb.DMatrix(data = X[tr, , drop = FALSE], label = y[tr])
      m <- xgboost::xgb.train(params = list(objective = "binary:logistic",
            max_depth = par$max_depth, eta = par$eta, subsample = 0.9, colsample_bytree = 0.9),
            data = dtr, nrounds = par$nrounds, verbose = 0)
      p <- predict(m, xgboost::xgb.DMatrix(data = X[te, , drop = FALSE]))
    }
    A[r] <- auc_roc(y[te], p)
  }
  mean(A)
}

res <- data.frame(); hip <- data.frame()
for (nm in names(specs)) {
  d <- design_full(specs[[nm]]); X <- d$X; y <- d$y
  folds <- make_folds(y); p <- ncol(X)
  # --- RF ---
  grid_rf <- expand.grid(mtry = unique(c(max(1, floor(sqrt(p))), max(1, floor(p/3)))),
                         nodesize = c(1, 5))
  best <- NULL
  for (i in seq_len(nrow(grid_rf))) {
    cv <- cv_auc_rf(X, y, folds, grid_rf$mtry[i], grid_rf$nodesize[i])
    if (is.null(best) || cv > best$cv) best <- list(cv = cv, mtry = grid_rf$mtry[i], nodesize = grid_rf$nodesize[i])
  }
  ta <- test_auc(specs[[nm]], "RF", best)
  res <- rbind(res, data.frame(algo = "RF", spec = nm, CV_AUC = best$cv, test_AUC = ta))
  reportar(sprintf("  RF  %-11s CV_AUC=%.3f test_AUC=%.3f (mtry=%d nodesize=%d)",
                   nm, best$cv, ta, best$mtry, best$nodesize))
  if (nm == "ME+macroL3") hip <- rbind(hip, data.frame(algo = "Random Forest (RF)",
     hiperparametros = sprintf("mtry=%d; nodesize=%d; ntree=300", best$mtry, best$nodesize)))
  # --- XGB ---
  if (tiene_xgb) {
    grid_xgb <- expand.grid(max_depth = c(2, 3, 4), eta = c(0.05, 0.1), nrounds = c(200, 400))
    bestx <- NULL
    for (i in seq_len(nrow(grid_xgb))) {
      cv <- cv_auc_xgb(X, y, folds, grid_xgb$max_depth[i], grid_xgb$eta[i], grid_xgb$nrounds[i])
      if (is.null(bestx) || cv > bestx$cv) bestx <- list(cv = cv, max_depth = grid_xgb$max_depth[i],
                                                         eta = grid_xgb$eta[i], nrounds = grid_xgb$nrounds[i])
    }
    tax <- test_auc(specs[[nm]], "XGB", bestx)
    res <- rbind(res, data.frame(algo = "XGB", spec = nm, CV_AUC = bestx$cv, test_AUC = tax))
    reportar(sprintf("  XGB %-11s CV_AUC=%.3f test_AUC=%.3f (max_depth=%d eta=%.2f nrounds=%d)",
                     nm, bestx$cv, tax, bestx$max_depth, bestx$eta, bestx$nrounds))
    if (nm == "ME+macroL3") hip <- rbind(hip, data.frame(algo = "XGBoost",
       hiperparametros = sprintf("max_depth=%d; eta=%.2f; nrounds=%d; subsample/colsample=0.9",
                                 bestx$max_depth, bestx$eta, bestx$nrounds)))
  } else {
    reportar(sprintf("  XGB %-11s (omitido: paquete 'xgboost' no disponible)", nm))
  }
}
write.csv(res, file.path(dir.resultados, "08_sintonizacion.csv"), row.names = FALSE)
write.csv(hip, file.path(dir.resultados, "08_hiperparametros.csv"), row.names = FALSE)
reportar("\nGuardado: resultados/08_sintonizacion.csv y 08_hiperparametros.csv")
if (!tiene_xgb) reportar(">> XGBoost no se ejecuto aqui; correr en un entorno con el paquete 'xgboost'.")
close(con)
message("08_sintonizacion.R OK -> resultados/08_sintonizacion.txt")
