# =====================================================================
# 09_segmentacion.R
# Perfiles de riesgo mediante segmentacion de asociados (objetivo 3).
#  (A) Agrupamiento no supervisado (k-medias) sobre 8 variables del perfil
#      sociodemografico y financiero; k elegido por coeficiente de silueta.
#      Caracterizacion por perfil y prueba chi-cuadrado de la tasa de default.
#  (B) Bandas de riesgo por quintiles de la probabilidad de incumplimiento
#      (PD) del modelo base + macro L3, con predicciones fuera de muestra
#      (validacion cruzada de 5 pliegues).
# Produce: resultados/09_seg_perfiles.csv, 09_seg_zmeans.csv,
#          resultados/09_seg_k.csv (silueta y suma de cuadrados por k),
#          resultados/09_seg_asignacion.csv (perfil de cada asociado),
#          resultados/09_seg_bandas.csv, resultados/09_segmentacion.txt
# =====================================================================
source(file.path("codigo", "00_config.R"))
suppressMessages(require(cluster))
dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores

con <- file(file.path(dir.resultados, "09_segmentacion.txt"), open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }
reportar("=== 09. SEGMENTACION Y PERFILES DE RIESGO ===")

# ---- (A) k-medias ----------------------------------------------------
feat <- c("edad", "antiguedad", "salario.1", "ahorros.1",
          "niv.deuda.int.salario", "niv.deuda.ext.salario", "niv.cuota", "niv.tc")
X <- scale(as.matrix(datos[, feat]))
reportar("Seleccion de k por coeficiente de silueta:")
set.seed(SEMILLA)
sel_k <- data.frame()
for (k in 2:6) {
  km <- kmeans(X, centers = k, nstart = 25)
  sil <- mean(cluster::silhouette(km$cluster, dist(X))[, 3])
  sel_k <- rbind(sel_k, data.frame(k = k, silueta = sil, wss = km$tot.withinss,
                                   var_entre = km$betweenss / km$totss))
  reportar(sprintf("  k=%d: silueta=%.3f  varianza entre grupos=%.1f%%", k, sil, 100 * km$betweenss / km$totss))
}
write.csv(sel_k, file.path(dir.resultados, "09_seg_k.csv"), row.names = FALSE)
K <- 4
set.seed(SEMILLA)
km <- kmeans(X, centers = K, nstart = 25)
datos$seg <- km$cluster
# caracterizacion por segmento (ordenado por tasa de default desc)
agg <- aggregate(cbind(y, edad, antiguedad, salario.1, ahorros.1,
                       niv.deuda.int.salario, niv.deuda.ext.salario, niv.cuota, niv.tc) ~ seg,
                 data = datos, FUN = mean)
cnt <- as.numeric(table(datos$seg))
prof <- data.frame(seg = agg$seg, n = cnt[agg$seg], tasa = agg$y * 100,
                   edad = agg$edad, antig = agg$antiguedad, salario = agg$salario.1,
                   ahorros = agg$ahorros.1, ndint = agg$niv.deuda.int.salario,
                   ndext = agg$niv.deuda.ext.salario, ncuota = agg$niv.cuota, ntc = agg$niv.tc)
prof <- prof[order(-prof$tasa), ]
reportar("\nPerfiles (ordenados por tasa de default):")
for (i in seq_len(nrow(prof)))
  reportar(sprintf("  seg%d: n=%d tasa=%.1f%% edad=%.0f antig=%.0f sal=%.1f aho=%.1f ndint=%.1f ndext=%.1f ntc=%.1f",
                   prof$seg[i], prof$n[i], prof$tasa[i], prof$edad[i], prof$antig[i],
                   prof$salario[i], prof$ahorros[i], prof$ndint[i], prof$ndext[i], prof$ntc[i]))
# medias estandarizadas por segmento (para el mapa de calor, Figura 9)
zm <- aggregate(X ~ seg, data = data.frame(seg = datos$seg, X), FUN = mean)
zm <- zm[match(prof$seg, zm$seg), ]
# prueba chi-cuadrado
 chi <- suppressWarnings(chisq.test(table(datos$seg, datos$y)))
reportar(sprintf("\nChi-cuadrado tasa default entre segmentos: X2=%.1f (gl=%d) p=%.2g",
                 chi$statistic, chi$parameter, chi$p.value))
write.csv(cbind(orden = seq_len(nrow(prof)), prof), file.path(dir.resultados, "09_seg_perfiles.csv"), row.names = FALSE)
write.csv(cbind(orden = seq_len(nrow(zm)), zm), file.path(dir.resultados, "09_seg_zmeans.csv"), row.names = FALSE)
# perfil de cada asociado (P1 = mayor tasa de default ... P4 = menor), para la Figura de justificacion
write.csv(data.frame(fila = seq_len(nrow(datos)), seg = datos$seg,
                     perfil = match(datos$seg, prof$seg), y = datos$y),
          file.path(dir.resultados, "09_seg_asignacion.csv"), row.names = FALSE)

# ---- (B) Bandas de riesgo por PD (fuera de muestra, 5 pliegues) ------
vars <- predictores$macroL3_base
y <- datos$y
fold <- integer(length(y)); set.seed(SEMILLA)
for (cl in unique(y)) { idx <- which(y == cl); fold[idx] <- sample(rep(seq_len(5), length.out = length(idx))) }
pd <- rep(NA_real_, length(y))
for (f in 1:5) {
  tr <- which(fold != f); te <- which(fold == f)
  o <- ajustar_evaluar(datos[tr, ], datos[te, ], vars)   # prob_test = PD fuera de muestra
  pd[te] <- o$prob_test
}
datos$pd <- pd
cortes <- quantile(pd, probs = seq(0, 1, 0.2))
datos$banda <- cut(pd, breaks = cortes, include.lowest = TRUE,
                   labels = c("B1 (menor)", "B2", "B3", "B4", "B5 (mayor)"))
band <- aggregate(y ~ banda, data = datos, FUN = function(v) c(n = length(v), def = sum(v)))
band <- data.frame(banda = band$banda, n = band$y[, "n"], incumpl = band$y[, "def"])
band$tasa <- band$incumpl / band$n * 100
band$pct_def <- band$incumpl / sum(band$incumpl) * 100
band$pd_prom <- as.numeric(tapply(datos$pd, datos$banda, mean)) * 100
write.csv(band, file.path(dir.resultados, "09_seg_bandas.csv"), row.names = FALSE)
reportar("\nBandas de riesgo (quintiles de PD):")
for (i in seq_len(nrow(band)))
  reportar(sprintf("  %-11s n=%d  incumpl=%d  tasa=%.1f%%  %%deft=%.1f%%  PD=%.1f%%",
                   band$banda[i], band$n[i], band$incumpl[i], band$tasa[i], band$pct_def[i], band$pd_prom[i]))
reportar(sprintf("Cortes de PD (quintiles) %%: %s",
                 paste(sprintf("%.1f", as.numeric(cortes) * 100), collapse = " | ")))
close(con)
message("09_segmentacion.R OK -> resultados/09_segmentacion.txt")
