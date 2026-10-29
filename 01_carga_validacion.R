# =====================================================================
# 01_carga_validacion.R
# Carga de BD_CONSUMO_2020.xlsx (hoja "Data") y validacion de la base.
# Produce:
#   - resultados/base_limpia.rds        (data.frame analitico, 1038 x 35)
#   - resultados/01_validacion.txt      (reporte de validacion)
# =====================================================================
# setwd("C:/ruta/a/la/carpeta/del/proyecto")  # o abrir el proyecto con la carpeta raiz como directorio de trabajo
source(file.path("codigo", "00_config.R"))
#source(file.path(ruta.base, "00_config.R"))

sink_txt <- file.path(dir.resultados, "01_validacion.txt")
con <- file(sink_txt, open = "wt", encoding = "UTF-8")
reportar <- function(...) { cat(..., "\n"); cat(..., "\n", file = con) }

# ---- 1. Lectura -------------------------------------------------------
# La hoja "Data" trae encabezados de grupo en la fila 1 y los nombres
# reales de variable en la fila 2. Los datos empiezan en la fila 3.
# Ademas, tras la col 35 hay un bloque "sin deflactar" (ID, salario, ...)
# que NO se usa aca. Nos quedamos con las 35 columnas analiticas.
crudo <- readxl::read_excel(archivo.bd, sheet = "Data", skip = 1)
base  <- crudo[, 1:35]
names(base) <- make.names(names(base))  # normaliza nombres
# La hoja trae dos columnas "ID" (deflactada y sin deflactar); read_excel las
# renombra ID...1 / ID...37. La primera columna es el identificador: la fijamos
# a col.id ("ID") para que los conteos por ID funcionen.
names(base)[1] <- col.id

reportar("=== 01. VALIDACION DE LA BASE ===")
reportar("Dimensiones (analitico):", nrow(base), "filas x", ncol(base), "columnas")

# ---- 2. Tipado -------------------------------------------------------
# Numericas: todo lo que no es categorica/identificacion/respuesta.
cat.presentes <- intersect(vars.categoricas, names(base))
for (v in cat.presentes) base[[v]] <- factor(trimws(as.character(base[[v]])))
base[[col.default]] <- factor(trimws(as.character(base[[col.default]])),
                              levels = c("No", "Si"))
base[[col.calpos]]  <- factor(trimws(as.character(base[[col.calpos]])),
                              levels = c("A", "B", "C", "D", "E"))
num.candidatas <- setdiff(names(base),
                          c(col.id, col.fecha, col.default, col.calpos, cat.presentes))
for (v in num.candidatas) base[[v]] <- suppressWarnings(as.numeric(base[[v]]))
base[[col.fecha]] <- as.integer(base[[col.fecha]])

# respuesta 0/1 auxiliar
base$y <- ifelse(base[[col.default]] == "Si", 1L, 0L)

# ---- 3. Cobertura de variables del autor -----------------------------
reportar("\n--- Cobertura vs las 25 variables internas del autor ---")
esperadas_autor <- list(
  sociodemo = c("sexo","edad","estrato","nivel.estudio","estado.civil","salario"),
  interna   = c("dias.de.mora","antiguedad","ahorros","deuda.con.int","deuda.int",
                "cuotas.restantes","valor.cuota","cred.viv.int",
                "niv.deuda.int.salario","niv.deuda.int.ahorro","niv.cuota"),
  externa   = c("calificacion.ext","deuda.con.ext","deuda.tc","deuda.ext",
                "cred.viv.ext","niv.deuda.ext.salario","niv.deuda.ext.ahorro","niv.tc")
)
norm <- function(x) tolower(gsub("\\.1$", "", make.names(x)))
present_norm <- norm(names(base))
for (g in names(esperadas_autor)) {
  faltan_g <- esperadas_autor[[g]][!esperadas_autor[[g]] %in% present_norm]
  reportar(sprintf("  [%s] faltan en datos: %s", g,
                   if (length(faltan_g)) paste(faltan_g, collapse=", ") else "ninguna"))
}
reportar("  >> Nota: algunas variables de la lista del autor no figuran en la fuente;",
         "el modelo base se ajusta con las variables efectivamente disponibles.")

# ---- 4. Faltantes y duplicados ---------------------------------------
reportar("\n--- Faltantes por columna (solo >0) ---")
na_col <- sort(colSums(is.na(base)), decreasing = TRUE)
na_col <- na_col[na_col > 0]
reportar(if (length(na_col)) paste(names(na_col), na_col, sep="=", collapse=", ") else "  ninguno")
reportar("--- Duplicados ---")
reportar("  IDs unicos:", length(unique(base[[col.id]])), "de", nrow(base))
reportar("  Filas duplicadas (todas las columnas):", sum(duplicated(base)))
reportar("  Un solo registro por ID => base transversal (no panel).")

# ---- 5. Respuesta y COLINEALIDAD cal.pos ~ default -------------------
reportar("\n--- Respuesta default ---")
tb <- table(base[[col.default]]); reportar(paste(names(tb), tb, sep="=", collapse=", "))
reportar("  Tasa de default:", round(mean(base$y), 4))
reportar("\n--- Tabla cal.pos x default ---")
ct <- table(base[[col.calpos]], base[[col.default]])
for (i in seq_len(nrow(ct)))
  reportar(sprintf("  cal.pos=%s : No=%d  Si=%d", rownames(ct)[i], ct[i,"No"], ct[i,"Si"]))
colineal <- all(ct[rownames(ct)!="A","No"] == 0) && ct["A","Si"] == 0
reportar("  >> cal.pos == 'A'  <=>  default == 'No' :", colineal)
reportar("  >> ALERTA: cal.pos es la definicion prospectiva del default",
         "(peor calificacion en los 12 meses SIGUIENTES). NO usar como predictor: es fuga.")

# ---- 6. Distribucion temporal (concentracion) ------------------------
reportar("\n--- fecha.referencia ---")
fr <- base[[col.fecha]]
reportar("  Rango:", min(fr), "-", max(fr), "| periodos unicos:", length(unique(fr)))
trim <- fecha_a_trim(aaaamm_a_fecha(fr))
reportar("  Trimestres unicos:", length(unique(trim)))
reportar("  % registros en 202006:", round(mean(fr == 202006), 3))
reportar("  >> ALERTA: fuerte concentracion en 2020Q2 (~79% de filas, ~66% de defaults).",
         "Afecta el holdout por trimestre y las 200 particiones 80/20.")

# ---- 7. Rangos de continuas ------------------------------------------
reportar("\n--- edad ---")
reportar("  min:", round(min(base$edad),1), "max:", round(max(base$edad),1),
         "| con decimales:", sum(base$edad %% 1 != 0), "de", nrow(base),
         "(edad aparentemente en anios exactos con fraccion; poblacion con jubilados)")

# ---- 8. Guardar ------------------------------------------------------
saveRDS(base, file.path(dir.resultados, "base_limpia.rds"))
reportar("\nGuardado: resultados/base_limpia.rds")
close(con)
message("01_carga_validacion.R OK -> resultados/01_validacion.txt")
