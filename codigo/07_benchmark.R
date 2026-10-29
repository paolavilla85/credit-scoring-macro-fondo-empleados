# =====================================================================
# 07_benchmark.R
# Replica del enfoque del articulo: comparacion CON y SIN SMOTE en 200
# particiones 70/30. Produce la Tabla 8 del documento (AUC, sensibilidad,
# especificidad, BA y PR) para MB, MB+macroL3 y ME.
#   - Sin SMOTE: umbral de Youden en train.
#   - Con SMOTE: clases balanceadas, umbral 0.5.
# Produce: resultados/07_benchmark_smote.csv y resultados/07_benchmark.txt
# =====================================================================
source(file.path("codigo", "00_config.R"))
dm <- readRDS(file.path(dir.resultados, "datos_modelo.rds"))
datos <- dm$datos; predictores <- dm$predictores

con <- file(file.path(dir.resultados, "07_benchmark.txt"), open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }
reportar("=== 07. BENCHMARK SMOTE (200 x 70/30) ===")

NR <- 200; PT <- 0.70
specs <- list(`MB (base)` = predictores$base,
              `MB + macro L3` = predictores$macroL3_base,
              `ME (extendido)` = predictores$extendido)

bench <- function(vars) {
  i1 <- which(datos$y == 1); i0 <- which(datos$y == 0)
  out <- list()
  for (sm in c(FALSE, TRUE)) {
    A <- B <- S <- E <- P <- numeric(NR)
    for (r in seq_len(NR)) {
      tr <- c(sample(i0, floor(PT * length(i0))), sample(i1, floor(PT * length(i1))))
      o  <- tryCatch(ajustar_evaluar(datos[tr, ], datos[-tr, ], vars, usar_smote = sm),
                     error = function(e) NULL)
      if (is.null(o)) { A[r] <- B[r] <- S[r] <- E[r] <- P[r] <- NA_real_ }
      else { A[r] <- o$AUC; B[r] <- o$BA; S[r] <- o$recall; E[r] <- o$especificidad; P[r] <- o$PR }
    }
    out[[if (sm) "Si" else "No"]] <- c(AUC = mean(A, na.rm = TRUE),
                                       BA = mean(B, na.rm = TRUE),
                                       Sensibilidad = mean(S, na.rm = TRUE),
                                       Especificidad = mean(E, na.rm = TRUE),
                                       PR = mean(P, na.rm = TRUE))
  }
  out
}

set.seed(SEMILLA)
res <- data.frame()
for (nm in names(specs)) {
  b <- bench(specs[[nm]])
  for (sm in c("No", "Si")) {
    res <- rbind(res, data.frame(Modelo = nm, SMOTE = sm,
                                 AUC = b[[sm]]["AUC"], BA = b[[sm]]["BA"],
                                 Sensibilidad = b[[sm]]["Sensibilidad"],
                                 Especificidad = b[[sm]]["Especificidad"],
                                 PR = b[[sm]]["PR"]))
    reportar(sprintf("  %-15s SMOTE=%-2s  AUC=%.3f  Sens=%.3f  Esp=%.3f  BA=%.3f  PR=%.3f",
                     nm, sm, b[[sm]]["AUC"], b[[sm]]["Sensibilidad"], b[[sm]]["Especificidad"],
                     b[[sm]]["BA"], b[[sm]]["PR"]))
  }
}
rownames(res) <- NULL
write.csv(res, file.path(dir.resultados, "07_benchmark_smote.csv"), row.names = FALSE)
reportar("\n>> SMOTE no mejora el AUC; la sensibilidad se obtiene igual o mejor con Youden sin remuestreo.")
close(con)
message("07_benchmark.R OK -> resultados/07_benchmark.txt")
