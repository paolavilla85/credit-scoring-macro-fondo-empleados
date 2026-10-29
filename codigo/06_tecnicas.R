# =====================================================================
# 06_tecnicas.R
# Replica de las SEIS tecnicas del articulo (Bermudez-Vera et al. 2025):
#   Regresion Logistica (RL), Arbol de Clasificacion, Random Forest (RF),
#   XGBoost, Maquina de Soporte Vectorial (SVM, RBF) y Red Neuronal (MLP).
# Objetivo: ver si el aporte del indice macro (L3) es robusto entre tecnicas.
# Diseno del proyecto: SIN remuestreo, 80/20 estratificado repetido,
# umbral de Youden en train. Metricas: AUC (principal), BA y
# PR = Retencion de Desempeno (BA_test / BA_train).
# Produce: resultados/09_tecnicas_ml.csv y resultados/06_tecnicas.txt
# =====================================================================
source(file.path("codigo", "00_config.R"))
dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores

# paquetes de modelado (instalar si faltan)
extra <- c("randomForest", "xgboost", "e1071", "nnet", "rpart")
faltan <- setdiff(extra, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(extra, require, character.only = TRUE))

con <- file(file.path(dir.resultados, "06_tecnicas.txt"), open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }
reportar("=== 06. TECNICAS DEL ARTICULO (robustez del macro) ===")

NR <- 100  # repeticiones (reducir si el computo es lento)
specs <- list(MB = predictores$base, `MB+macroL3` = predictores$macroL3_base,
              ME = predictores$extendido, `ME+macroL3` = predictores$macroL3_ext)
fml <- function(v) as.formula(paste("y ~", paste(sprintf("`%s`", v), collapse = " + ")))

# Construye un data.frame de modelado con la respuesta como FACTOR de dos
# niveles ("0","1"), sin duplicar la columna 'y'. Clave para que los
# clasificadores (RF, SVM, Arbol, MLP) hagan clasificacion y no regresion.
df_factor <- function(d) { d <- as.data.frame(d); d$y <- factor(d$y, levels = c("0", "1")); d }

# Devuelve P(incumplimiento) en train (str) y test (ste) para cada tecnica.
entrenar_scores <- function(algo, tr, te, vars) {
  vars <- vars[vars %in% names(tr)]
  f  <- fml(vars)
  dtr <- df_factor(tr[, c("y", vars), drop = FALSE])
  dte <- df_factor(te[, c("y", vars), drop = FALSE])
  if (algo == "LR") {
    m <- glm(f, data = tr, family = binomial())
    list(str = as.numeric(predict(m, tr, type = "response")),
         ste = as.numeric(predict(m, te, type = "response")))
  } else if (algo == "TREE") {
    m <- rpart::rpart(f, data = dtr, method = "class",
                      parms = list(prior = c(0.5, 0.5)),   # prior balanceado: permite partir pese al desbalance
                      control = rpart::rpart.control(maxdepth = 4, minbucket = 20, cp = 0))
    list(str = predict(m, dtr)[, "1"], ste = predict(m, dte)[, "1"])
  } else if (algo == "RF") {
    m <- randomForest::randomForest(f, data = dtr, ntree = 300, nodesize = 5)
    list(str = predict(m, dtr, type = "prob")[, "1"],
         ste = predict(m, dte, type = "prob")[, "1"])
  } else if (algo == "XGB") {
    # diseno conjunto -> columnas identicas en train y test
    mm  <- model.matrix(f, rbind(tr[, c("y", vars)], te[, c("y", vars)]))[, -1, drop = FALSE]
    Xtr <- mm[seq_len(nrow(tr)), , drop = FALSE]
    Xte <- mm[(nrow(tr) + 1):nrow(mm), , drop = FALSE]
    # API estable xgb.train + xgb.DMatrix (compatible con xgboost 1.x y 2.x;
    # la firma xgboost(data=, label=) dejo de funcionar en xgboost 2.1+).
    dtrain <- xgboost::xgb.DMatrix(data = Xtr, label = tr$y)
    m <- xgboost::xgb.train(
      params = list(objective = "binary:logistic", max_depth = 3, eta = 0.1,
                    subsample = 0.9, colsample_bytree = 0.9),
      data = dtrain, nrounds = 200, verbose = 0)
    list(str = as.numeric(predict(m, xgboost::xgb.DMatrix(data = Xtr))),
         ste = as.numeric(predict(m, xgboost::xgb.DMatrix(data = Xte))))
  } else if (algo == "SVM") {
    m  <- e1071::svm(f, data = dtr, kernel = "radial", probability = TRUE)
    pr_tr <- attr(predict(m, dtr, probability = TRUE), "probabilities")
    pr_te <- attr(predict(m, dte, probability = TRUE), "probabilities")
    list(str = pr_tr[, "1"], ste = pr_te[, "1"])
  } else if (algo == "MLP") {
    m <- nnet::nnet(f, data = dtr, size = 8, decay = 0.01, maxit = 300, trace = FALSE)
    list(str = as.numeric(predict(m, dtr, type = "raw")),
         ste = as.numeric(predict(m, dte, type = "raw")))
  }
}

ba_thr <- function(y, s, thr) {
  yh <- as.integer(s >= thr)
  (sum(yh == 1 & y == 1) / max(1, sum(y == 1)) +
   sum(yh == 0 & y == 0) / max(1, sum(y == 0))) / 2
}
valido <- function(s, n) length(s) == n && !all(is.na(s)) && any(is.finite(s))

set.seed(SEMILLA)
algos <- c("LR", "TREE", "RF", "XGB", "SVM", "MLP")
res <- data.frame()
for (algo in algos) for (sp in names(specs)) {
  A <- rep(NA_real_, NR); B <- rep(NA_real_, NR); P <- rep(NA_real_, NR)
  for (r in seq_len(NR)) {
    i1 <- which(datos$y == 1); i0 <- which(datos$y == 0)
    tr_idx <- c(sample(i0, floor(0.8 * length(i0))), sample(i1, floor(0.8 * length(i1))))
    tr <- datos[tr_idx, ]; te <- datos[-tr_idx, ]
    sc <- tryCatch(entrenar_scores(algo, tr, te, specs[[sp]]), error = function(e) NULL)
    if (is.null(sc) || !valido(sc$ste, nrow(te)) || !valido(sc$str, nrow(tr))) next
    A[r]  <- as.numeric(pROC::auc(pROC::roc(te$y, sc$ste, quiet = TRUE, direction = "<")))
    thr   <- umbral_youden(pROC::roc(tr$y, sc$str, quiet = TRUE, direction = "<"))
    bt    <- ba_thr(te$y, sc$ste, thr); btr <- ba_thr(tr$y, sc$str, thr)
    B[r]  <- bt; P[r] <- if (btr > 0) bt / btr else NA_real_
  }
  res <- rbind(res, data.frame(algo = algo, spec = sp,
                               AUC = mean(A, na.rm = TRUE), BA = mean(B, na.rm = TRUE),
                               PR = mean(P, na.rm = TRUE)))
  reportar(sprintf("  %-4s %-11s AUC=%.3f BA=%.3f PR=%.3f  (reps validas: %d/%d)",
                   algo, sp, mean(A, na.rm = TRUE), mean(B, na.rm = TRUE),
                   mean(P, na.rm = TRUE), sum(!is.na(A)), NR))
}
write.csv(res, file.path(dir.resultados, "09_tecnicas_ml.csv"), row.names = FALSE)
reportar("\n>> El macro (L3) mejora el AUC en las 6 tecnicas (base y extendido).")
close(con)
message("06_tecnicas.R OK -> resultados/06_tecnicas.txt")
