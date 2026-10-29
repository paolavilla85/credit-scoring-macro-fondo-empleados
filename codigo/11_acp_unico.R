# =====================================================================
# 11_acp_unico.R
# Comparacion: indices por bloque (03_ingenieria.R) frente a un ACP UNICO
# sobre los siete indicadores macroeconomicos.
#  - El ACP unico se estima, igual que los de bloque, sobre los 19
#    trimestres del cruce (rezagos L3 y L6), una fila por trimestre.
#  - Se reportan la varianza explicada y las cargas de sus componentes,
#    y su correlacion con los tres indices por bloque.
#  - Se comparan, con rezago de 3 meses, MB + 3 indices por bloque frente a
#    MB + 1 y MB + 3 componentes del ACP unico: razon de verosimilitud
#    contra MB, AIC/BIC y AUC en las mismas 200 particiones 80/20 de 05.
# Produce: resultados/11_acp_unico.txt
# =====================================================================
source(file.path("codigo", "00_config.R"))
dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores; pcas <- dm$pcas
macro_wide <- as.data.frame(readRDS(file.path(dir.resultados, "macro_trimestral.rds")))
names(macro_wide) <- make.names(names(macro_wide))

con <- file(file.path(dir.resultados, "11_acp_unico.txt"), open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }
reportar("=== 11. ACP UNICO FRENTE A INDICES POR BLOQUE ===")

# ---- 1. ACP unico sobre los 19 trimestres del cruce ------------------
cols <- make.names(unlist(macro.bloques, use.names = FALSE))
q_used <- sort(unique(c(datos$ct_L3, datos$ct_L6)))
mw <- macro_wide[macro_wide$cierre_trim %in% q_used, ]
X  <- as.matrix(mw[, cols])
mu <- colMeans(X); sdv <- apply(X, 2, sd)
pc <- prcomp(scale(X, center = mu, scale = sdv), center = FALSE, scale. = FALSE)
ve <- pc$sdev^2 / sum(pc$sdev^2)
etq <- c("IPC", "Tasa Banrep", "DTF", "PIB", "Brent", "Desempleo", "TRM")
reportar(sprintf("Trimestres usados: %d", nrow(X)))
reportar("Varianza explicada por componente (%):",
         paste(sprintf("PC%d=%.1f", seq_along(ve), 100 * ve), collapse = "  "))
reportar("Cargas de las tres primeras componentes:")
for (j in seq_along(cols))
  reportar(sprintf("  %-12s PC1=%+.2f  PC2=%+.2f  PC3=%+.2f", etq[j],
                   pc$rotation[j, 1], pc$rotation[j, 2], pc$rotation[j, 3]))

# Correlacion (en los trimestres) entre las componentes unicas y los indices por bloque
idx_blq <- sapply(pcas, function(p) {
  Z <- scale(as.matrix(mw[, make.names(p$cols), drop = FALSE]), center = p$mu, scale = p$sd)
  as.numeric(Z %*% p$load) })
scr <- pc$x[, 1:3]
reportar("\nCorrelacion entre componentes del ACP unico e indices por bloque (19 trimestres):")
R <- cor(scr, idx_blq)
for (i in 1:3) reportar(sprintf("  PC%d:  precios/tasas=%+.2f  actividad=%+.2f  cambiario=%+.2f",
                                i, R[i, 1], R[i, 2], R[i, 3]))

# ---- 2. Proyeccion a cada credito (rezago 3 meses) --------------------
Xc <- as.matrix(macro_wide[match(datos$ct_L3, macro_wide$cierre_trim), cols])
Sc <- scale(Xc, center = mu, scale = sdv) %*% pc$rotation[, 1:3]
datos$acpU1_L3 <- Sc[, 1]; datos$acpU2_L3 <- Sc[, 2]; datos$acpU3_L3 <- Sc[, 3]
specs <- list(
  "MB"                      = predictores$base,
  "MB + 3 indices bloque"   = predictores$macroL3_base,
  "MB + PC1 ACP unico"      = c(predictores$base, "acpU1_L3"),
  "MB + PC1-PC3 ACP unico"  = c(predictores$base, "acpU1_L3", "acpU2_L3", "acpU3_L3"))

# ---- 3. Razon de verosimilitud contra MB, AIC y BIC -------------------
reportar("\nRazon de verosimilitud contra MB (datos completos):")
m0 <- glm(formula_modelo(specs[[1]]), data = datos, family = binomial())
for (nm in names(specs)[-1]) {
  m1 <- glm(formula_modelo(specs[[nm]]), data = datos, family = binomial())
  an <- anova(m0, m1, test = "LRT")
  reportar(sprintf("  %-24s LR=%.2f (gl=%d) p=%.4g | dAIC=%+.1f dBIC=%+.1f", nm,
                   an$Deviance[2], an$Df[2], an$`Pr(>Chi)`[2], AIC(m1) - AIC(m0), BIC(m1) - BIC(m0)))
}

# ---- 4. AUC en las mismas 200 particiones 80/20 de 05_validacion.R ----
vars_todas <- unique(unlist(predictores))
datos <- datos[complete.cases(datos[, vars_todas]), ]
particion_estratificada <- function(y, p = PROP_TRAIN) {
  idx <- integer(0)
  for (cl in unique(y)) { i <- which(y == cl); idx <- c(idx, sample(i, floor(p * length(i)))) }
  sort(idx)
}
A <- matrix(NA, N_REPS, length(specs), dimnames = list(NULL, names(specs)))
set.seed(SEMILLA)
for (r in seq_len(N_REPS)) {
  tr <- particion_estratificada(datos$y)
  for (nm in names(specs)) {
    o <- tryCatch(ajustar_evaluar(datos[tr, ], datos[-tr, ], specs[[nm]]), error = function(e) NULL)
    if (!is.null(o)) A[r, nm] <- o$AUC
  }
}
reportar("\nAUC medio en 200 particiones 80/20 (mismas particiones que 05):")
for (nm in names(specs))
  reportar(sprintf("  %-24s AUC=%.3f (sd %.3f)", nm, mean(A[, nm], na.rm = TRUE), sd(A[, nm], na.rm = TRUE)))
d <- A[, "MB + PC1-PC3 ACP unico"] - A[, "MB + 3 indices bloque"]
reportar(sprintf("  Diferencia ACP unico (3 comp.) - bloques: media=%+.4f; gana el ACP unico en %.0f%% de las particiones",
                 mean(d, na.rm = TRUE), 100 * mean(d > 0, na.rm = TRUE)))
close(con)
message("11_acp_unico.R OK -> resultados/11_acp_unico.txt")
