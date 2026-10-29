# =====================================================================
# 05_validacion.R
# Validacion y comparacion de modelos.
#  (A) AUC PAREADO en N_REPS particiones 80/20 estratificadas (metrica
#      principal). BA y PR como secundarias. Test pareado sobre las
#      diferencias por replica (Wilcoxon + t) entre pares de modelos.
#  (B) Validacion dejando TRIMESTRES completos fuera (leave-one-quarter).
# Produce resultados/05_* (csv, txt y 05_pred_80_20.rds con las predicciones
# de prueba de MB y MB+macroL3, usadas en la Figura 7a de 10_figuras.R).
# NOTA: correr solo despues de confirmar el mapeo de vars.base (ver README).
# =====================================================================
source(file.path("codigo", "00_config.R"))
dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores
# ajustar_evaluar y demas funciones de modelado provienen de 00_config.R

con <- file(file.path(dir.resultados, "05_validacion.txt"), open="wt", encoding="UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file=con) }
reportar("=== 05. VALIDACION ===")

vars_todas <- unique(unlist(predictores))
datos <- datos[complete.cases(datos[, vars_todas]), ]
datos$trim <- fecha_a_trim(datos$fecha_ref)

# ---- (A) 200 repeticiones 80/20, AUC/Sens/Esp/BA/PR pareados -----------------
particion_estratificada <- function(y, p = PROP_TRAIN) {
  idx <- integer(0)
  for (cl in unique(y)) {
    i <- which(y == cl); idx <- c(idx, sample(i, floor(p*length(i))))
  }
  sort(idx)
}
modelos <- names(predictores)
res <- array(NA, dim = c(N_REPS, length(modelos), 5),
             dimnames = list(NULL, modelos, c("AUC","BA","PR","Sens","Esp")))
pred_80_20 <- list()   # predicciones de prueba de MB y MB+macroL3 (para las curvas ROC de la Figura 7a)
set.seed(SEMILLA)
for (r in seq_len(N_REPS)) {
  tr_idx <- particion_estratificada(datos$y)
  train <- datos[tr_idx, ]; test <- datos[-tr_idx, ]
  for (m in modelos) {
    out <- tryCatch(ajustar_evaluar(train, test, predictores[[m]]), error=function(e) NULL)
    if (!is.null(out)) res[r, m, ] <- c(out$AUC, out$BA, out$PR, out$recall, out$especificidad)
    if (!is.null(out) && m %in% c("base", "macroL3_base"))
      pred_80_20[[length(pred_80_20) + 1]] <- data.frame(rep = r, modelo = m, y = test$y, p = out$prob_test)
  }
}
saveRDS(do.call(rbind, pred_80_20), file.path(dir.resultados, "05_pred_80_20.rds"))
resumen <- data.frame(
  modelo = modelos,
  AUC_med = apply(res[,,"AUC"],2,mean,na.rm=TRUE),
  AUC_sd  = apply(res[,,"AUC"],2,sd,na.rm=TRUE),
  Sens_med = apply(res[,,"Sens"],2,mean,na.rm=TRUE),
  Esp_med  = apply(res[,,"Esp"],2,mean,na.rm=TRUE),
  BA_med  = apply(res[,,"BA"],2,mean,na.rm=TRUE),
  PR_med  = apply(res[,,"PR"],2,mean,na.rm=TRUE)
)
write.csv(resumen, file.path(dir.resultados,"05_resumen_80_20.csv"), row.names=FALSE)
reportar("\n(A) 80/20 x", N_REPS, "repeticiones (AUC principal):")
for (i in seq_len(nrow(resumen)))
  reportar(sprintf("  %-13s AUC=%.3f (sd %.3f)  Sens=%.3f  Esp=%.3f  BA=%.3f  PR=%.3f",
                   resumen$modelo[i], resumen$AUC_med[i], resumen$AUC_sd[i],
                   resumen$Sens_med[i], resumen$Esp_med[i], resumen$BA_med[i], resumen$PR_med[i]))

# Contrastes pareados de AUC contra el modelo base
reportar("\n  Contraste pareado de AUC (vs 'base'), Wilcoxon sobre diferencias por replica:")
for (m in setdiff(modelos, "base")) {
  d <- res[,m,"AUC"] - res[,"base","AUC"]
  w <- suppressWarnings(wilcox.test(d, mu=0))
  reportar(sprintf("  base -> %-13s dAUC medio=%+.3f  p(Wilcoxon)=%.4g",
                   m, mean(d,na.rm=TRUE), w$p.value))
}

# ---- (B) Dejando trimestres completos fuera --------------------------
reportar("\n(B) Leave-one-quarter-out (dejando cada trimestre fuera):")
trs <- sort(unique(datos$trim))
loo <- data.frame()
for (m in modelos) {
  probs <- rep(NA_real_, nrow(datos))
  for (tt in trs) {
    te <- which(datos$trim == tt); tr <- which(datos$trim != tt)
    if (length(unique(datos$y[tr])) < 2) next
    out <- tryCatch(ajustar_evaluar(datos[tr,], datos[te,], predictores[[m]]), error=function(e) NULL)
    if (!is.null(out)) probs[te] <- out$prob_test
  }
  ok <- !is.na(probs) & is.finite(probs)
  auc_glob <- if (length(unique(datos$y[ok]))==2)
    as.numeric(pROC::auc(pROC::roc(datos$y[ok], probs[ok], quiet=TRUE))) else NA
  loo <- rbind(loo, data.frame(modelo=m, AUC_holdout_trim=auc_glob))
}
write.csv(loo, file.path(dir.resultados,"05_holdout_trimestre.csv"), row.names=FALSE)
for (i in seq_len(nrow(loo)))
  reportar(sprintf("  %-13s AUC(holdout trimestre)=%.3f", loo$modelo[i], loo$AUC_holdout_trim[i]))
reportar("\n  >> Recordar: 2020Q2 concentra ~79%% de filas; dejar ese trimestre",
         "fuera equivale a entrenar con ~21%% de los datos. Interpretar con cautela.")

# ---- (C) Significancia y tamano de efecto (MB+macroL3 vs MB) --------
reportar("\n(C) Significancia de la mejora macro:")
# Tamano de efecto en las 200 replicas 80/20
if ("macroL3_base" %in% modelos && "base" %in% modelos) {
  d <- res[,"macroL3_base","AUC"] - res[,"base","AUC"]
  reportar(sprintf("  80/20: dAUC medio=%+.4f | d de Cohen=%.2f | P(macro>base)=%.0f%%",
                   mean(d,na.rm=TRUE), mean(d,na.rm=TRUE)/sd(d,na.rm=TRUE), 100*mean(d>0,na.rm=TRUE)))
}
# DeLong sobre las predicciones pooled del holdout por trimestre
holdout_probs <- function(vars) {
  pr <- rep(NA_real_, nrow(datos))
  for (tt in trs) {
    te <- which(datos$trim==tt); tr <- which(datos$trim!=tt)
    if (length(unique(datos$y[tr]))<2) next
    o <- tryCatch(ajustar_evaluar(datos[tr,], datos[te,], vars), error=function(e) NULL)
    if (!is.null(o)) pr[te] <- o$prob_test
  }
  pr
}
pB <- holdout_probs(predictores$base); pM <- holdout_probs(predictores$macroL3_base)
ok <- !is.na(pB) & !is.na(pM)
r0 <- pROC::roc(datos$y[ok], pB[ok], quiet=TRUE)
r1 <- pROC::roc(datos$y[ok], pM[ok], quiet=TRUE)
dl <- pROC::roc.test(r0, r1, method="delong", paired=TRUE)
# roc.test(r0, r1) estima AUC(r0) - AUC(r1); se invierte el signo para reportar macro - base
ic_dl <- -rev(as.numeric(dl$conf.int))
reportar(sprintf("  Holdout DeLong: AUC %.3f -> %.3f | dAUC=%+.3f | IC95%%=[%+.3f; %+.3f] | p(DeLong)=%.3g",
                 as.numeric(pROC::auc(r0)), as.numeric(pROC::auc(r1)),
                 as.numeric(pROC::auc(r1))-as.numeric(pROC::auc(r0)), ic_dl[1], ic_dl[2], dl$p.value))
reportar("  Gini = 2*AUC-1: ", sprintf("%.3f -> %.3f", 2*as.numeric(pROC::auc(r0))-1, 2*as.numeric(pROC::auc(r1))-1))
reportar("  Nota: prueba conjunta del bloque via LR esta en 04_modelos.R (anidados).")

close(con)
message("05_validacion.R OK -> resultados/05_validacion.txt")
