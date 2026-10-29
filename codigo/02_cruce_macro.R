# =====================================================================
# 02_cruce_macro.R
# Reconstruye el cruce de la base con las variables macroeconomicas.
# Fuente macro: formato ANCHO (variables en filas, trimestres en columnas),
# frecuencia TRIMESTRAL (cierres de trimestre 2005Q1..2023Q1).
# Estrategia: por cada fecha.referencia (mensual) se calcula la fecha
# rezagada 3m y 6m, se mapea al CIERRE DE TRIMESTRE correspondiente y se
# une con la serie macro de ese trimestre.
# Produce:
#   - resultados/macro_trimestral.rds   (macro larga: trimestre x variable)
#   - resultados/base_macro.rds         (base + macro L3 y L6)
#   - resultados/02_cruce_macro.txt
# =====================================================================
source(file.path("codigo", "00_config.R"))
base <- readRDS(file.path(dir.resultados, "base_limpia.rds"))

con <- file(file.path(dir.resultados, "02_cruce_macro.txt"), open="wt", encoding="UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file=con) }
reportar("=== 02. CRUCE MACRO ===")

# ---- 1. Leer macro (hoja Hoja1) y pasar a formato largo --------------
macro_raw <- readxl::read_excel(archivo.macro, sheet = "Hoja1", col_names = FALSE)
fechas_macro <- as.Date(as.numeric(unlist(macro_raw[1, -1])), origin = "1899-12-30")
nombres_var  <- as.character(unlist(macro_raw[-1, 1]))
valores <- macro_raw[-1, -1]
macro <- as.data.frame(t(apply(valores, 1, as.numeric)))
names(macro) <- format(fechas_macro, "%Y-%m-%d")
macro$variable <- trimws(nombres_var)
macro_long <- tidyr::pivot_longer(macro, cols = -variable,
                                  names_to = "cierre_trim", values_to = "valor")
macro_long$cierre_trim <- as.Date(macro_long$cierre_trim)
macro_long$trim <- fecha_a_trim(macro_long$cierre_trim)

reportar("Frecuencia de la fuente macro: TRIMESTRAL (cierres de trimestre).")
reportar("Rango:", format(min(fechas_macro)), "-", format(max(fechas_macro)),
         "| trimestres:", length(fechas_macro), "| variables:", length(nombres_var))

# ---- 2. Verificacion de identidad TIR y exclusion --------------------
w <- tidyr::pivot_wider(macro_long[,c("variable","cierre_trim","valor")],
                        names_from = variable, values_from = valor)
if (all(c("Variación anual del IPC (e.a.)","Tasa Banrep",
          "Tasa de interés real e.a. (Banrep-Inflación)") %in% names(w))) {
  err <- max(abs(w[["Variación anual del IPC (e.a.)"]] -
                 (w[["Tasa Banrep"]] - w[["Tasa de interés real e.a. (Banrep-Inflación)"]])),
             na.rm = TRUE)
  reportar(sprintf("Identidad Var.IPC = Banrep - TIR(Banrep): error max = %.2e (=> TIR derivada).", err))
}
reportar("Se EXCLUYEN por colinealidad/derivacion:",
         paste(macro.vars.excluir, collapse = " ; "))

# Conservar solo las variables macro seleccionadas (no colineales)
macro_sel <- macro_long[macro_long$variable %in% macro.vars.fuente, ]
reportar("Variables macro conservadas:", length(unique(macro_sel$variable)))

# tabla ancha por cierre de trimestre (una fila = un trimestre unico)
macro_wide <- tidyr::pivot_wider(macro_sel[,c("cierre_trim","variable","valor")],
                                 names_from = variable, values_from = valor)
macro_wide <- macro_wide[order(macro_wide$cierre_trim), ]
saveRDS(macro_wide, file.path(dir.resultados, "macro_trimestral.rds"))

# ---- 3. Fechas de referencia y rezagos -------------------------------
base$fecha_ref <- aaaamm_a_fecha(base[[col.fecha]])
base$fecha_L3  <- base$fecha_ref %m-% months(3)   # rezago 3 meses (L1)
base$fecha_L6  <- base$fecha_ref %m-% months(6)   # rezago 6 meses (L2)
# mapear cada fecha rezagada a su CIERRE DE TRIMESTRE (clave de union)
base$ct_L3 <- fecha_a_cierre_trim(base$fecha_L3)
base$ct_L6 <- fecha_a_cierre_trim(base$fecha_L6)

# ---- 4. Chequeo: L3 y L6 deben caer en trimestres DISTINTOS ----------
# (En datos.xlsx previo, las columnas de L6 eran una COPIA de L3 -> bug.)
distintos <- mean(fecha_a_trim(base$ct_L3) != fecha_a_trim(base$ct_L6))
reportar(sprintf("\n%% de registros con trimestre(L3) != trimestre(L6): %.1f%%", 100*distintos))
reportar("  >> Si esto es ~100%%, L3 y L6 aportan info distinta.",
         "El resultado previo 'lag 6m no aporta' debe re-evaluarse: en datos.xlsx",
         "las columnas L6 eran identicas a las L3 (copia), no un verdadero rezago 6m.")

# ---- 5. Union macro por rezago ---------------------------------------
sufijar <- function(df, suf) {
  vv <- setdiff(names(df), "cierre_trim")
  names(df)[match(vv, names(df))] <- paste0(make.names(vv), suf)
  df
}
mL3 <- sufijar(macro_wide, "_L3"); names(mL3)[1] <- "ct_L3"
mL6 <- sufijar(macro_wide, "_L6"); names(mL6)[1] <- "ct_L6"
base_macro <- merge(base, mL3, by = "ct_L3", all.x = TRUE)
base_macro <- merge(base_macro, mL6, by = "ct_L6", all.x = TRUE)

# ---- 6. Validacion del cruce -----------------------------------------
faltan_L3 <- sum(is.na(base_macro[[paste0(make.names(macro.vars.fuente[1]), "_L3")]]))
faltan_L6 <- sum(is.na(base_macro[[paste0(make.names(macro.vars.fuente[1]), "_L6")]]))
reportar("\nRegistros sin match macro L3:", faltan_L3, "| L6:", faltan_L6)
reportar("Trimestres macro efectivamente usados (L3):",
         length(unique(base_macro$ct_L3)), "| (L6):", length(unique(base_macro$ct_L6)))
reportar("  >> Para el PCA (03) se estima sobre estos periodos UNICOS, no sobre filas.")

saveRDS(base_macro, file.path(dir.resultados, "base_macro.rds"))
reportar("\nGuardado: resultados/base_macro.rds y macro_trimestral.rds")
close(con)
message("02_cruce_macro.R OK -> resultados/02_cruce_macro.txt")
