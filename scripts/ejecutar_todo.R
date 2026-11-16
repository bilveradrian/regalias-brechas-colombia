# Flujo completo, desde la carpeta raíz del proyecto:  Rscript scripts/ejecutar_todo.R
# Paso 0 (Python 3 + poppler-utils): extracción y validación de los PDF de leyes y decretos.
#   Las lecturas visuales están en datos/pdf/revision_manual.csv (trazables por página).
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
for (py in c("extraer_pdf.py", "conciliar_pdf.py", "validar_final.py")) {
  message("== ", py); stopifnot(system2("python3", c("-I", file.path("scripts/pdf", py))) == 0)
}
for (s in c("00_base_maestra.R", "01_analisis.R", "01b_robustez_adicional.R", "02_figuras.R", "02b_mapas.R", "03_tablas.R")) {
  if (file.exists(file.path("scripts", s))) { message("== ", s); source(file.path("scripts", s), encoding = "UTF-8", local = new.env()) }
}
writeLines(capture.output(sessionInfo()), "resultados/sessionInfo.txt")
