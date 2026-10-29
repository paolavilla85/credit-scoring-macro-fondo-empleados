# =====================================================================
# 04_modelos.R
# Modelos de regresion logistica SIN remuestreo (umbral de Youden en
# train) como diseno principal, mas:
#  (a) prueba de RAZON DE VEROSIMILITUD (modelos anidados) del bloque macro,
#      con AIC/BIC, prueba de Wald de cada indice y comparacion L3 + L6 juntos,
#  (b) variante de REFERENCIA con SMOTE (replica del enfoque del autor).
# Las funciones de modelado (formula_modelo, ajustar_evaluar, smote_train)
# se definen en 00_config.R y quedan disponibles al hacer source().
# Produce resultados/04_modelos.txt.
# =====================================================================
source(file.path("codigo", "00_config.R"))
dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores

con <- file(file.path(dir.resultados, "04_modelos.txt"), open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }
reportar("=== 04. MODELOS ===")

# ---- (a) Prueba de razon de verosimilitud del bloque macro -----------
reportar("\n--- Prueba LR: aporte del bloque macro (datos completos) ---")
lr_bloque <- function(vars_base, vars_full, etiqueta) {
  vars_base <- vars_base[vars_base %in% names(datos)]
  vars_full <- vars_full[vars_full %in% names(datos)]
  d  <- datos[complete.cases(datos[, vars_full]), ]
  m0 <- glm(formula_modelo(vars_base), data = d, family = binomial())
  m1 <- glm(formula_modelo(vars_full), data = d, family = binomial())
  an <- anova(m0, m1, test = "LRT")
  reportar(sprintf("  %s: LR=%.2f (df=%d) p=%.4g | dAIC=%.1f dBIC=%.1f",
                   etiqueta, an$Deviance[2], an$Df[2], an$`Pr(>Chi)`[2],
                   AIC(m1) - AIC(m0), BIC(m1) - BIC(m0)))
  invisible(m1)
}
# Prueba de Wald de cada indice macro dentro del modelo completo (coeficiente a coeficiente)
wald_indices <- function(m1, etiqueta) {
  cf <- summary(m1)$coefficients
  cf <- cf[grepl("^idx_", rownames(cf)), , drop = FALSE]
  reportar(sprintf("  Wald individual en %s:", etiqueta))
  for (i in seq_len(nrow(cf)))
    reportar(sprintf("    %-24s coef=%+.3f  ee=%.3f  z=%+.2f  p=%.3f",
                     rownames(cf)[i], cf[i, 1], cf[i, 2], cf[i, 3], cf[i, 4]))
}
m_l3 <- lr_bloque(predictores$base, predictores$macroL3_base, "MB vs MB+macroL3")
m_l6 <- lr_bloque(predictores$base, predictores$macroL6_base, "MB vs MB+macroL6")
# Ambos rezagos a la vez (L3 + L6): justifica usar un solo rezago
vars_l3l6 <- unique(c(predictores$macroL3_base, predictores$macroL6_base))
lr_bloque(predictores$base, vars_l3l6, "MB vs MB+macro(L3+L6)")
lr_bloque(predictores$macroL3_base, vars_l3l6, "MB+macroL3 vs MB+macro(L3+L6)")
reportar("  (LR = prueba conjunta del bloque; abajo, el Wald de cada indice por separado)")
wald_indices(m_l3, "MB+macroL3")

# ---- (b) Referencia con y sin SMOTE (200 x 70/30, estilo autor) ------
reportar("\n--- Referencia MB con y sin SMOTE (200 x 70/30) ---")
comparar_smote <- function(vars, NR = 200, p = 0.70) {
  i1 <- which(datos$y == 1); i0 <- which(datos$y == 0)
  out <- list()
  for (sm in c(FALSE, TRUE)) {
    A <- numeric(NR)
    for (r in seq_len(NR)) {
      tr <- c(sample(i0, floor(p * length(i0))), sample(i1, floor(p * length(i1))))
      o  <- tryCatch(ajustar_evaluar(datos[tr, ], datos[-tr, ], vars, usar_smote = sm),
                     error = function(e) NULL)
      A[r] <- if (is.null(o)) NA_real_ else o$AUC
    }
    out[[if (sm) "SMOTE" else "sinSMOTE"]] <- mean(A, na.rm = TRUE)
  }
  out
}
set.seed(SEMILLA)
r_mb <- comparar_smote(predictores$base)
reportar(sprintf("  MB  AUC sin SMOTE=%.3f | con SMOTE=%.3f", r_mb$sinSMOTE, r_mb$SMOTE))
reportar("  (El AUC es invariante al umbral; SMOTE no mejora la discriminacion",
         "en regresion logistica. Comparacion ampliada en 07_benchmark_autor.md)")

close(con)
message("04_modelos.R OK -> resultados/04_modelos.txt")
