# =====================================================================
# 10_figuras.R
# Genera TODAS las figuras del documento en R (graficos base, sin
# dependencias externas), a partir de los resultados del pipeline.
# Produce: resultados/figuras/image1.png ... image10.png (resultados),
#          fig_macro_correlaciones.png, fig_macro_pca.png (metodologia, 6.3.2) y
#          fig_kmedias_justificacion.png (resultados, perfiles por k-medias)
# Requiere haber corrido antes: 01, 02, 03, 05, 06, 07, 08, 09.
# =====================================================================
source(file.path("codigo", "00_config.R"))
dir.fig <- file.path(dir.resultados, "figuras")
if (!dir.exists(dir.fig)) dir.create(dir.fig, recursive = TRUE, showWarnings = FALSE)

AZUL <- "#2a78d6"; NARA <- "#eb6834"; VERDE <- "#1baf7a"; ROJO <- "#e34948"
VIOL <- "#4a3aa7"; GRIS <- "#9a9a97"
abrir <- function(f, w, h) png(file.path(dir.fig, f), width = w, height = h, units = "in", res = 150)
par0 <- function() par(mar = c(4.2, 4.2, 2.4, 1), mgp = c(2.4, 0.7, 0), bty = "n",
                       col.axis = "#333333", col.lab = "#333333", cex.axis = 0.85)

base <- readRDS(file.path(dir.resultados, "base_limpia.rds"))
dm   <- readRDS(file.path(dir.resultados, "datos_modelo.rds")); datos <- dm$datos; predictores <- dm$predictores

# ---------- Figura 1: desbalance ----------
abrir("image1.png", 5, 3.5); par0()
tb <- c(sum(base$y == 0), sum(base$y == 1)); pct <- round(tb / sum(tb) * 100, 1)
b <- barplot(tb, names.arg = c("Cumple (0)", "Incumple (1)"), col = c(AZUL, NARA),
             border = NA, ylim = c(0, 1080), ylab = "Número de créditos",
             main = "Distribución de la variable objetivo (default)")
text(b, tb + 45, sprintf("%d\n(%.1f %%)", tb, pct), cex = 0.9)
dev.off()

# ---------- Figura 2: distribucion temporal ----------
fr <- base[[col.fecha]]; yr <- fr %/% 100; mo <- fr %% 100; q <- (mo - 1) %/% 3 + 1
trim <- paste0(yr, "Q", q); ord <- order(yr * 10 + q); utr <- unique(trim[ord])
cnt <- as.numeric(table(factor(trim, levels = utr)))
tasa <- tapply(base$y, factor(trim, levels = utr), mean) * 100
colb <- ifelse(utr == "2020Q2", NARA, AZUL)
abrir("image2.png", 8, 5.2)
par(mfrow = c(2, 1), mar = c(1.5, 4.2, 2.2, 1), mgp = c(2.4, 0.7, 0), bty = "n", cex.axis = 0.8)
barplot(cnt, col = colb, border = NA, ylab = "Créditos",
        main = "Distribución temporal de la muestra y tasa de incumplimiento", names.arg = rep("", length(utr)))
par(mar = c(4.5, 4.2, 0.6, 1))
bb <- barplot(tasa, col = colb, border = NA, ylab = "Tasa default (%)", names.arg = utr, las = 2, cex.names = 0.7)
abline(h = 9.06, lty = 2, col = GRIS); text(bb[2], 11, "media 9,06 %", cex = 0.75, col = "#555555", pos = 4)
dev.off()

# ---------- Figura 3: indices macro en el tiempo ----------
xt <- as.Date(sprintf("%d-%02d-01", datos[[col.fecha]] %/% 100, datos[[col.fecha]] %% 100))
o <- order(xt)
abrir("image3.png", 8, 3.6); par0()
plot(xt[o], datos$idx_precios_tasas_L3[o], type = "l", col = AZUL, lwd = 2,
     ylim = range(c(datos$idx_precios_tasas_L3, datos$idx_actividad_L3, datos$idx_cambiario_L3)),
     xlab = "", ylab = "Índice (rezago 3m)",
     main = "Índices macroeconómicos por bloque (PCA), rezago de 3 meses")
lines(xt[o], datos$idx_actividad_L3[o], col = NARA, lwd = 2)
lines(xt[o], datos$idx_cambiario_L3[o], col = VERDE, lwd = 2)
legend("topright", c("Precios y tasas", "Actividad", "Cambiario"), col = c(AZUL, NARA, VERDE),
       lwd = 2, bty = "n", cex = 0.8, horiz = TRUE)
dev.off()

# ---------- Figura 4: AUC 80/20 y holdout ----------
r80 <- read.csv(file.path(dir.resultados, "05_resumen_80_20.csv"))
hld <- read.csv(file.path(dir.resultados, "05_holdout_trimestre.csv"))
ordm <- c("base", "macroL3_base", "macroL6_base", "extendido", "macroL3_ext", "macroL6_ext")
lab6 <- c("MB", "MB+macroL3", "MB+macroL6", "ME", "ME+macroL3", "ME+macroL6")
r80 <- r80[match(ordm, r80$modelo), ]; hld <- hld[match(ordm, hld$modelo), ]
colm <- ifelse(grepl("macro", ordm), NARA, AZUL)
abrir("image4.png", 10, 4)
par(mfrow = c(1, 2), mar = c(6.5, 4.2, 2.4, 1), mgp = c(2.5, 0.7, 0), bty = "n", cex.axis = 0.8)
b1 <- barplot(r80$AUC_med, col = colm, border = NA, ylim = c(0.5, 0.75), xpd = FALSE,
              names.arg = lab6, las = 2, ylab = "AUC medio", main = "AUC en 200 particiones 80/20")
arrows(b1, r80$AUC_med - r80$AUC_sd, b1, r80$AUC_med + r80$AUC_sd, angle = 90, code = 3, length = 0.03, col = "#7a7a77")
text(b1, r80$AUC_med + r80$AUC_sd + 0.008, sprintf("%.3f", r80$AUC_med), cex = 0.72)
legend("topright", c("Sin macro", "Con índice macro"), fill = c(AZUL, NARA), border = NA, bty = "n", cex = 0.8)
b2 <- barplot(hld$AUC_holdout_trim, col = colm, border = NA, ylim = c(0.5, 0.75), xpd = FALSE,
              names.arg = lab6, las = 2, ylab = "AUC", main = "AUC en validación temporal por trimestres")
text(b2, hld$AUC_holdout_trim + 0.008, sprintf("%.3f", hld$AUC_holdout_trim), cex = 0.72)
dev.off()

# ---------- Figura 5 (Figura 7 del documento): curvas ROC, MB con y sin indice macro L3 ----------
# a) Validacion 80/20: curva ROC promedio de las 200 particiones (promedio vertical de la
#    sensibilidad sobre una malla de 1 - especificidad). La variabilidad entre particiones
#    se reporta en la Tabla 7 (desviacion del AUC).
#    Usa las predicciones de prueba guardadas por 05_validacion.R (05_pred_80_20.rds).
# b) Validacion temporal por trimestres: predicciones fuera de muestra dejando cada trimestre fuera.
roc_xy <- function(y, s) { o <- order(-s); ys <- y[o]; P <- sum(ys); N <- length(ys) - P
  list(fpr = c(0, cumsum(1 - ys) / N), tpr = c(0, cumsum(ys) / P)) }
aucf <- function(y, s) as.numeric(pROC::auc(pROC::roc(y, s, quiet = TRUE, direction = "<")))
f3c  <- function(x) formatC(x, format = "f", digits = 3, decimal.mark = ",")
malla <- seq(0, 1, by = 0.01)
tpr_en_malla <- function(y, s) { r <- roc_xy(y, s)
  sapply(malla, function(f) max(r$tpr[r$fpr <= f + 1e-12])) }
pr8 <- readRDS(file.path(dir.resultados, "05_pred_80_20.rds"))
curvas_8020 <- function(m) {
  sub <- pr8[pr8$modelo == m, ]; reps <- sort(unique(sub$rep))
  T <- sapply(reps, function(r) { z <- sub[sub$rep == r, ]; tpr_en_malla(z$y, z$p) })
  A <- sapply(reps, function(r) { z <- sub[sub$rep == r, ]; aucf(z$y, z$p) })
  list(media = rowMeans(T), auc = mean(A))
}
cB <- curvas_8020("base"); cM <- curvas_8020("macroL3_base")

vt <- unique(unlist(predictores)); dd <- datos[complete.cases(datos[, vt]), ]
dd$trim <- fecha_a_trim(dd$fecha_ref); trs <- sort(unique(dd$trim))
hold_pred <- function(vars) { pr <- rep(NA_real_, nrow(dd))
  for (tt in trs) { te <- which(dd$trim == tt); tr <- which(dd$trim != tt)
    if (length(unique(dd$y[tr])) < 2) next
    o <- tryCatch(ajustar_evaluar(dd[tr, ], dd[te, ], vars), error = function(e) NULL)
    if (!is.null(o)) pr[te] <- o$prob_test }; pr }
pB <- hold_pred(predictores$base); pM <- hold_pred(predictores$macroL3_base)
ok <- !is.na(pB) & !is.na(pM)
r0 <- roc_xy(dd$y[ok], pB[ok]); r1 <- roc_xy(dd$y[ok], pM[ok])

abrir("image5.png", 10.5, 5)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.6, 1), mgp = c(2.4, 0.7, 0), bty = "n",
    col.axis = "#333333", col.lab = "#333333", cex.axis = 0.85, cex.main = 0.95)
# a) 80/20
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "1 - Especificidad (FPR)", ylab = "Sensibilidad (TPR)",
     main = "a) Validación 80/20 (curva promedio de 200 particiones)")
lines(malla, cB$media, col = AZUL, lwd = 2); lines(malla, cM$media, col = NARA, lwd = 2)
abline(0, 1, lty = 2, col = GRIS)
legend("bottomright", c(sprintf("MB (AUC medio = %s)", f3c(cB$auc)),
                        sprintf("MB + macro L3 (AUC medio = %s)", f3c(cM$auc))),
       col = c(AZUL, NARA), lwd = 2, bty = "n", cex = 0.8)
# b) temporal
plot(r0$fpr, r0$tpr, type = "l", col = AZUL, lwd = 2, xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1 - Especificidad (FPR)", ylab = "Sensibilidad (TPR)",
     main = "b) Validación temporal por trimestres")
lines(r1$fpr, r1$tpr, col = NARA, lwd = 2); abline(0, 1, lty = 2, col = GRIS)
legend("bottomright", c(sprintf("MB (AUC = %s)", f3c(aucf(dd$y[ok], pB[ok]))),
                        sprintf("MB + macro L3 (AUC = %s)", f3c(aucf(dd$y[ok], pM[ok])))),
       col = c(AZUL, NARA), lwd = 2, bty = "n", cex = 0.8)
dev.off()

# ---------- Figura 6: seis tecnicas ----------
tec <- read.csv(file.path(dir.resultados, "09_tecnicas_ml.csv"))
alg <- c("LR", "TREE", "RF", "XGB", "SVM", "MLP"); etq <- c("Logística", "Árbol", "R. Forest", "XGBoost", "SVM", "MLP")
mb <- sapply(alg, function(a) tec$AUC[tec$algo == a & tec$spec == "MB"])
mm <- sapply(alg, function(a) tec$AUC[tec$algo == a & tec$spec == "MB+macroL3"])
abrir("image6.png", 8.5, 4); par0()
m <- rbind(mb, mm)
b <- barplot(m, beside = TRUE, col = c(AZUL, NARA), border = NA, names.arg = etq,
             ylim = c(0.45, 0.72), xpd = FALSE, ylab = "AUC",
             main = "Aporte del índice macro en las seis técnicas")
legend("topright", c("MB (sin macro)", "MB + índice macro L3"), fill = c(AZUL, NARA), border = NA, bty = "n", cex = 0.8)
text(b[1, ], mb + 0.006, sprintf("%.2f", mb), cex = 0.62); text(b[2, ], mm + 0.006, sprintf("%.2f", mm), cex = 0.62)
dev.off()

# ---------- Figura 7: ensambles sintonizados ----------
if (file.exists(file.path(dir.resultados, "08_sintonizacion.csv"))) {
  tun <- read.csv(file.path(dir.resultados, "08_sintonizacion.csv"))
  sp4 <- c("MB", "MB+macroL3", "ME", "ME+macroL3")
  rf <- sapply(sp4, function(s) { v <- tun$test_AUC[tun$algo == "RF" & tun$spec == s]; if (length(v)) v else NA })
  xg <- sapply(sp4, function(s) { v <- tun$test_AUC[tun$algo == "XGB" & tun$spec == s]; if (length(v)) v else NA })
  abrir("image7.png", 8.5, 4); par0()
  b <- barplot(rbind(rf, xg), beside = TRUE, col = c(VERDE, VIOL), border = NA, names.arg = sp4,
               ylim = c(0.55, 0.72), xpd = FALSE, ylab = "AUC (prueba)",
               main = "Ensambles ajustados por validación cruzada")
  legend("topleft", c("Random Forest", "XGBoost"), fill = c(VERDE, VIOL), border = NA, bty = "n", cex = 0.8)
  text(b[1, ], rf + 0.006, sprintf("%.3f", rf), cex = 0.62)
  text(b[2, ], xg + 0.006, ifelse(is.na(xg), "", sprintf("%.3f", xg)), cex = 0.62)
  dev.off()
}

# ---------- Figura 8: tasa por perfil ----------
prof <- read.csv(file.path(dir.resultados, "09_seg_perfiles.csv"))
labp <- c("P1 Alto apalancamiento", "P2 Adultos, ahorro bajo", "P3 Sénior consolidado", "P4 Sénior, uso de tarjeta")
colp <- ifelse(prof$tasa >= 12, ROJO, ifelse(prof$tasa >= 9, NARA, AZUL))
abrir("image8.png", 7, 3.6); par(mar = c(5.5, 4.2, 2.4, 1), mgp = c(2.5, 0.7, 0), bty = "n", cex.axis = 0.8)
b <- barplot(prof$tasa, col = colp, border = NA, ylim = c(0, 15), names.arg = labp, las = 1, cex.names = 0.72,
             ylab = "Tasa de incumplimiento (%)", main = "Tasa de incumplimiento por perfil de asociado (k-medias)")
abline(h = 9.06, lty = 2, col = GRIS)
text(b, prof$tasa + 0.5, sprintf("%.1f %%\n(n=%d)", prof$tasa, prof$n), cex = 0.78)
dev.off()

# ---------- Figura 9: huella de perfiles (mapa de calor) ----------
zm <- read.csv(file.path(dir.resultados, "09_seg_zmeans.csv"))
fcols <- c("edad", "antiguedad", "salario.1", "ahorros.1", "niv.deuda.int.salario", "niv.deuda.ext.salario", "niv.cuota", "niv.tc")
flab <- c("Edad", "Antigüedad", "Salario", "Ahorros", "Deuda int./sal.", "Deuda ext./sal.", "Nivel cuota", "Nivel tarjeta")
Z <- as.matrix(zm[, fcols]); rownames(Z) <- labp
pal <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(41)
# Escala simetrica que cubre el mayor valor absoluto observado (z = +2,3 en P1), para que
# ninguna celda quede fuera del rango de color (image() deja en blanco los valores fuera de zlim).
lim <- ceiling(max(abs(Z)) * 2) / 2
Zc  <- pmax(pmin(Z, lim), -lim)
etq <- function(v) { v <- round(v, 1); ifelse(v == 0, "0,0", formatC(v, format = "f", digits = 1, flag = "+", decimal.mark = ",")) }
abrir("image9.png", 8.2, 3.6); par(mar = c(6.5, 9, 2.4, 2), mgp = c(2.4, 0.7, 0))
image(x = 1:ncol(Z), y = 1:nrow(Z), z = t(Zc[nrow(Zc):1, ]), col = pal, zlim = c(-lim, lim),
      axes = FALSE, xlab = "", ylab = "", main = "Huella de cada perfil (z por encima/por debajo del promedio)")
axis(1, 1:ncol(Z), flab, las = 2, cex.axis = 0.75, tick = FALSE)
axis(2, 1:nrow(Z), rev(labp), las = 1, cex.axis = 0.72, tick = FALSE)
for (i in 1:nrow(Z)) for (j in 1:ncol(Z)) text(j, nrow(Z) - i + 1, etq(Z[i, j]), cex = 0.68,
       col = ifelse(abs(Z[i, j]) > 0.45 * lim, "white", "#222222"))
mtext(sprintf("Escala de color de %s a %s desviaciones estándar; 0 = promedio de los asociados",
              formatC(-lim, format = "f", digits = 1, decimal.mark = ","), formatC(lim, format = "f", digits = 1, flag = "+", decimal.mark = ",")),
      side = 1, line = 5.3, cex = 0.62, col = "#555555")
dev.off()

# ---------- Figura 10: bandas de riesgo ----------
band <- read.csv(file.path(dir.resultados, "09_seg_bandas.csv"))
colbd <- c(AZUL, AZUL, AZUL, NARA, ROJO)
abrir("image10.png", 7, 3.6); par(mar = c(4.2, 4.2, 2.4, 1), mgp = c(2.5, 0.7, 0), bty = "n", cex.axis = 0.8)
b <- barplot(band$tasa, col = colbd, border = NA, ylim = c(0, 22), names.arg = band$banda, las = 1, cex.names = 0.85,
             ylab = "Tasa de incumplimiento (%)", main = "Bandas de riesgo según el puntaje del modelo (quintiles de PD)")
abline(h = 9.06, lty = 2, col = GRIS); text(b[1], 9.8, "media 9,06 %", cex = 0.75, col = "#555555", pos = 4)
text(b, band$tasa + 0.6, sprintf("%.1f %%", band$tasa), cex = 0.8)
dev.off()


# ---------- Metodologia 6.3.2: correlaciones y PCA de los indicadores macro ----------
# Se usan los mismos 19 trimestres unicos sobre los que 03_ingenieria.R estima el PCA
# (cierres de trimestre cruzados con rezago de 3 o 6 meses), una fila por trimestre.
macro_wide <- readRDS(file.path(dir.resultados, "macro_trimestral.rds"))
macro_wide <- as.data.frame(macro_wide); names(macro_wide) <- make.names(names(macro_wide))
q_used <- sort(unique(c(datos$ct_L3, datos$ct_L6)))
mw <- macro_wide[macro_wide$cierre_trim %in% q_used, ]
pcas <- dm$pcas
cols_m <- make.names(unlist(macro.bloques, use.names = FALSE))  # mismo orden que en 03_ingenieria.R
lab_m  <- c("Inflación (IPC)", "Tasa Banrep", "DTF", "Crecimiento PIB", "Petróleo Brent",
            "Desempleo", "Variación TRM")       # mismo orden que macro.bloques
blq_m  <- rep(c("Precios y tasas", "Actividad", "Cambiario"), sapply(pcas, function(p) length(p$cols)))
col_blq <- c("Precios y tasas" = AZUL, "Actividad" = NARA, "Cambiario" = VERDE)

# Figura: matriz de correlaciones (19 trimestres)
R <- cor(as.matrix(mw[, cols_m]))
k <- length(cols_m)
pal_r <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(41)
abrir("fig_macro_correlaciones.png", 7, 6)
par(mar = c(8.5, 8.5, 3, 1.5))
image(1:k, 1:k, t(R[k:1, ]), col = pal_r, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "",
      main = "Correlación entre indicadores macroeconómicos")
mtext(sprintf("%d trimestres del cruce (una fila por trimestre)", nrow(mw)), side = 3, line = 0.3, cex = 0.8, col = "#555555")
axis(1, 1:k, lab_m, las = 2, cex.axis = 0.8, tick = FALSE)
axis(2, 1:k, rev(lab_m), las = 1, cex.axis = 0.8, tick = FALSE)
for (i in 1:k) for (j in 1:k) text(j, k - i + 1, formatC(R[i, j], format = "f", digits = 2, decimal.mark = ","),
                                   cex = 0.8, col = ifelse(abs(R[i, j]) > 0.6, "white", "#222222"))
corte <- cumsum(sapply(pcas, function(p) length(p$cols)))[-length(pcas)] + 0.5
abline(v = corte, h = k + 1 - corte, col = "#333333", lwd = 1.5)
dev.off()

# Figura: PCA por bloque (cargas de la PC1 y varianza explicada)
ld  <- unlist(lapply(pcas, function(p) unname(p$load)))
vex <- sapply(pcas, `[[`, "var") * 100
abrir("fig_macro_pca.png", 9, 4.2)
layout(matrix(1:2, 1), widths = c(1.6, 1))
par(mar = c(4.2, 8.5, 3, 1), mgp = c(2.4, 0.7, 0), bty = "n", cex.axis = 0.8, cex.main = 0.95)
yy <- barplot(rev(ld), horiz = TRUE, names.arg = rev(lab_m), las = 1, col = rev(col_blq[blq_m]),
              border = NA, xlim = c(-1, 1.3), xlab = "Carga en la primera componente (PC1)",
              main = "a) Cargas de la PC1 por indicador")
abline(v = 0, col = GRIS)
text(rev(ld), yy, formatC(rev(ld), format = "f", digits = 2, decimal.mark = ","),
     pos = ifelse(rev(ld) >= 0, 4, 2), cex = 0.75)
par(mar = c(4.2, 4.2, 3, 1))
bb <- barplot(vex, names.arg = c("Precios y\ntasas", "Actividad", "Cambiario"), col = col_blq, border = NA,
              ylim = c(0, 110), ylab = "Varianza explicada por PC1 (%)", main = "b) Varianza explicada por PC1")
text(bb, vex + 5, paste0(formatC(vex, format = "f", digits = 1, decimal.mark = ","), " %"), cex = 0.8)
dev.off()

# ---------- Resultados: justificacion de los perfiles por k-medias ----------
# a) Coeficiente de silueta y varianza entre grupos para k = 2..6 (de 09_segmentacion.R).
# b) Proyeccion de los asociados en las dos primeras componentes principales de las
#    8 variables estandarizadas usadas en k-medias, coloreados por perfil, con los
#    centroides y la direccion de cada variable.
sel_k <- read.csv(file.path(dir.resultados, "09_seg_k.csv"))
asig  <- read.csv(file.path(dir.resultados, "09_seg_asignacion.csv"))
feat  <- c("edad", "antiguedad", "salario.1", "ahorros.1",
           "niv.deuda.int.salario", "niv.deuda.ext.salario", "niv.cuota", "niv.tc")
flab2 <- c("Edad", "Antigüedad", "Salario", "Ahorros", "Deuda int./sal.", "Deuda ext./sal.", "Cuota", "Tarjeta")
Xk  <- scale(as.matrix(datos[, feat]))
pck <- prcomp(Xk)
sc  <- pck$x[, 1:2]; vexk <- 100 * pck$sdev^2 / sum(pck$sdev^2)
col_p <- c(ROJO, NARA, AZUL, VERDE)
lab_p <- c("P1 Alto apalancamiento", "P2 Adultos, ahorro bajo", "P3 Sénior consolidado", "P4 Sénior, uso de tarjeta")
alfa  <- function(cl, a) grDevices::adjustcolor(cl, alpha.f = a)
fmt   <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
abrir("fig_kmedias_justificacion.png", 10.5, 5)
layout(matrix(1:2, 1), widths = c(1, 1.5))
par(mar = c(4.2, 4.2, 3, 4.2), mgp = c(2.4, 0.7, 0), bty = "n", cex.axis = 0.8, cex.main = 0.95)
plot(sel_k$k, sel_k$silueta, type = "b", pch = 19, col = AZUL, lwd = 2, ylim = c(0, 0.5), xaxt = "n",
     xlim = c(1.7, 6.3), xlab = "Número de grupos (k)", ylab = "Coeficiente de silueta",
     main = "a) Elección del número de grupos")
axis(1, sel_k$k)
text(sel_k$k, sel_k$silueta - 0.035, fmt(sel_k$silueta, 3), cex = 0.7, col = AZUL)
par(new = TRUE)
plot(sel_k$k, 100 * sel_k$var_entre, type = "b", pch = 17, col = NARA, lwd = 2, ylim = c(0, 60),
     xlim = c(1.7, 6.3), axes = FALSE, xlab = "", ylab = "")
text(sel_k$k, 100 * sel_k$var_entre, paste0(fmt(100 * sel_k$var_entre, 0), " %"), cex = 0.7, col = NARA,
     pos = ifelse(sel_k$k == 2, 4, 3))
axis(4, col.axis = NARA); mtext("Varianza entre grupos (%)", side = 4, line = 2.4, cex = 0.8, col = NARA)
abline(v = 4, lty = 2, col = GRIS); text(4, 58, "k = 4", cex = 0.8, pos = 4)
legend("bottomright", c("Silueta", "Varianza entre grupos"), col = c(AZUL, NARA), pch = c(19, 17),
       lwd = 2, bty = "n", cex = 0.75)
par(mar = c(4.2, 4.2, 3, 1))
plot(sc, type = "n", xlab = sprintf("Componente 1 (%s %% de la varianza)", fmt(vexk[1], 1)),
     ylab = sprintf("Componente 2 (%s %% de la varianza)", fmt(vexk[2], 1)),
     main = "b) Asociados por perfil (primeras dos componentes principales)")
abline(h = 0, v = 0, col = "#dddddd")
points(sc, pch = 16, cex = 0.55, col = alfa(col_p[asig$perfil], 0.45))
# direccion de las variables (flechas); las de direccion casi igual se rotulan juntas
esc <- 6
rot <- pck$rotation[, 1:2] * esc
arrows(0, 0, rot[, 1], rot[, 2], length = 0.06, col = "#555555")
grp <- list("Edad, antigüedad\ny ahorros" = c(1, 2, 4), "Salario" = 3,
            "Deuda interna/salario\ny cuota" = c(5, 7), "Deuda externa/salario" = 6, "Uso de tarjeta" = 8)
for (g in names(grp)) {
  xy <- colMeans(rot[grp[[g]], , drop = FALSE]) * 1.12
  text(xy[1], xy[2], g, cex = 0.62, col = "#333333", offset = 0.2,
       pos = if (g == "Uso de tarjeta") 3 else if (xy[1] >= 0) 4 else 2)
}
ctr <- aggregate(sc, by = list(perfil = asig$perfil), FUN = mean)
points(ctr[, 2], ctr[, 3], pch = 21, bg = col_p[ctr$perfil], col = "black", cex = 1.8, lwd = 1.2)
text(ctr[, 2], ctr[, 3], paste0("P", ctr$perfil), pos = c(2, 3, 1, 4)[ctr$perfil], offset = 0.8, font = 2, cex = 0.8)
legend("bottomright", lab_p, col = col_p, pch = 16, bty = "n", cex = 0.72, title = "Perfil", title.adj = 0)
dev.off()

message("10_figuras.R OK -> ", dir.fig)
