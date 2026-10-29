# =====================================================================
# 03_ingenieria.R
# Ingenieria de caracteristicas + INDICES MACRO POR BLOQUE (PCA).
# Cada bloque (precios/tasas, actividad, cambiario) se resume en su PC1,
# estimado sobre PERIODOS UNICOS (trimestres), no sobre las 1038 filas,
# para no sobreponderar 2020Q2. Se construyen a rezago L3 (3m) y L6 (6m).
# Produce: resultados/datos_modelo.rds y resultados/03_ingenieria.txt
# =====================================================================
source(file.path("codigo", "00_config.R"))
base_macro <- readRDS(file.path(dir.resultados, "base_macro.rds"))
macro_wide <- readRDS(file.path(dir.resultados, "macro_trimestral.rds"))
names(macro_wide) <- make.names(names(macro_wide))  # consistencia con make.names de los bloques

con <- file(file.path(dir.resultados, "03_ingenieria.txt"), open="wt", encoding="UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file=con) }
reportar("=== 03. INGENIERIA + INDICES MACRO POR BLOQUE ===")

# --- 1. Ajuste de PCA por bloque sobre periodos unicos -----------------
# Se usan los cierres de trimestre efectivamente cruzados (L3 union L6),
# una fila por trimestre, estandarizados. PC1 orientado a signo estable.
q_used <- sort(unique(c(base_macro$ct_L3, base_macro$ct_L6)))
mw <- macro_wide[macro_wide$cierre_trim %in% q_used, ]

# === CORRECCIÓN AQUÍ ===
# 1. Convertimos los nombres a formato estándar con puntos
colnames(mw) <- make.names(colnames(mw))
# 2. Forzamos a que sea un data.frame tradicional (evita errores de subsetting en tibbles)
mw <- as.data.frame(mw)
# =======================

ajustar_pca_bloque <- function(cols) {
  cols <- make.names(cols)
  X <- as.matrix(mw[, cols, drop = FALSE]); X <- X[complete.cases(X), , drop = FALSE]
  mu <- colMeans(X); sdv <- apply(X, 2, sd); sdv[sdv == 0] <- 1
  Xs <- scale(X, center = mu, scale = sdv)
  pc <- prcomp(Xs, center = FALSE, scale. = FALSE)
  load <- pc$rotation[, 1]
  if (cor(as.numeric(Xs %*% load), Xs[, 1]) < 0) load <- -load  # signo estable
  list(cols = cols, mu = mu, sd = sdv, load = load,
       var = (pc$sdev^2 / sum(pc$sdev^2))[1])
}
pcas <- lapply(macro.bloques, ajustar_pca_bloque)


# === CORRECCIÓN AQUÍ ===
# Estandarizamos los nombres de macro_wide y la convertimos a data.frame tradicional
colnames(macro_wide) <- make.names(colnames(macro_wide))
macro_wide <- as.data.frame(macro_wide)
# =======================

# proyeccion de una serie de cierres de trimestre sobre el PC1 del bloque
proyectar <- function(info, ct) {
  X <- as.matrix(macro_wide[match(ct, macro_wide$cierre_trim), info$cols, drop = FALSE])
  Xs <- scale(X, center = info$mu, scale = info$sd)
  as.numeric(Xs %*% info$load)
}

for (b in names(pcas)) {
  base_macro[[paste0("idx_", b, "_L3")]] <- proyectar(pcas[[b]], base_macro$ct_L3)
  base_macro[[paste0("idx_", b, "_L6")]] <- proyectar(pcas[[b]], base_macro$ct_L6)
}

reportar("Indices por bloque (PC1, % varianza y loadings):")
for (b in names(pcas)) {
  reportar(sprintf("  [%s] PC1 var=%.1f%%", b, 100*pcas[[b]]$var))
  for (nm in names(pcas[[b]]$load))
    reportar(sprintf("      %-45s %+.2f", nm, pcas[[b]]$load[[nm]]))
}

# --- 2. Conjuntos de predictores --------------------------------------
existe <- function(v) v[v %in% names(base_macro)]
idxL3 <- paste0("idx_", names(macro.bloques), "_L3")
idxL6 <- paste0("idx_", names(macro.bloques), "_L6")
predictores <- list(
  base         = existe(unname(vars.base)),                 # MB: 9 vars del autor
  extendido    = existe(vars.extendido),                    # ME
  macroL3_base = c(existe(unname(vars.base)), idxL3),
  macroL6_base = c(existe(unname(vars.base)), idxL6),
  macroL3_ext  = c(existe(vars.extendido),  idxL3),
  macroL6_ext  = c(existe(vars.extendido),  idxL6)
)
reportar("\nConjuntos de predictores:")
for (nm in names(predictores))
  reportar(sprintf("  %-13s (%d)", nm, length(predictores[[nm]])))

saveRDS(list(datos = base_macro, predictores = predictores, pcas = pcas),
        file.path(dir.resultados, "datos_modelo.rds"))
reportar("\nGuardado: resultados/datos_modelo.rds")
close(con)
message("03_ingenieria.R OK -> resultados/03_ingenieria.txt")
