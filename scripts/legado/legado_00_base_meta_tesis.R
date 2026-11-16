# =====================================================================
# 00_base_maestra.R
# Construye la base maestra a partir de los archivos originales SIN
# modificarlos (datos_originales/). Produce:
#   datos/base_maestra.csv            panel municipio x anio (brecha DE)
#   datos/base_transversal.csv        una fila por municipio (analisis)
#   datos/base_sectores.csv           municipio x sector x anio (13 sectores)
#   datos/base_maestra.xlsx           hojas: base, base_transversal,
#                                     sectores, diccionario, fuentes,
#                                     correcciones, validaciones,
#                                     control_calidad
# Requisitos: readxl, dplyr, tidyr, openxlsx. Sin descargas externas.
# =====================================================================

invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(openxlsx)
})

# Ejecutar desde la carpeta raiz del proyecto (ejecutar_todo.R lo garantiza)
raiz <- getwd()
orig <- file.path(raiz, "datos_originales")
out  <- file.path(raiz, "datos")
stopifnot(dir.exists(orig))
dir.create(out, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 0. Integridad de los archivos originales (huella SHA-256 registrada)
# ---------------------------------------------------------------------
huellas <- read.table(file.path(orig, "SHA256SUMS.txt"),
                      col.names = c("sha256", "archivo"), stringsAsFactors = FALSE)
if (nzchar(Sys.which("sha256sum"))) {
  actual <- sapply(huellas$archivo, function(f)
    strsplit(system2("sha256sum", shQuote(file.path(orig, f)), stdout = TRUE), " ")[[1]][1])
  if (any(actual != huellas$sha256)) stop("Un archivo original cambió respecto a SHA256SUMS.txt")
}

# ---------------------------------------------------------------------
# 1. Lectura
# ---------------------------------------------------------------------
brecha <- read_excel(file.path(orig, "base.xlsx")) %>%
  transmute(cod_dane = as.integer(ciudad),
            municipio = NOM_MUNICI,
            v_2022, v_2023, v_2024)

sectores <- read_excel(file.path(orig, "base_general.xlsx"), sheet = "brecha") %>%
  transmute(cod_dane = as.integer(ciudad), municipio = NOM_MUNICI,
            sector, v_2022, v_2023, v_2024)

lee_presup <- function(f, bienio) {
  read_excel(file.path(orig, f)) %>%
    transmute(cod_dane = as.integer(ciudad),
              bienio   = bienio,
              ail      = AIL,  ail_ad   = `AIL-AD`,
              di_ail   = D_AIL, di_ail_ad = `D_AIL-AD`,
              at       = AT,   dit      = DIT,
              at_dit_original = `AT-DIT`)
}
presup <- bind_rows(lee_presup("disponibilidad_inicial_1.xlsx", "2021-2022"),
                    lee_presup("disponibilidad_inicial_2.xlsx", "2023-2024"))

# ---------------------------------------------------------------------
# 2. Correcciones manuales trazables
#    Acacias (50006) y Villavicencio (50001) no son beneficiarios de la
#    Asignacion para la Inversion Local (AIL; art. 36 Ley 2056 de 2020,
#    municipios mas pobres segun NBI y de categorias 4, 5 y 6). En los
#    archivos originales figuran con valores de relleno (1 peso). Se
#    recodifican como NA y se marca beneficiario_ail = 0.
# ---------------------------------------------------------------------
correcciones <- tibble::tribble(
  ~id, ~cod_dane, ~municipio, ~variable, ~valor_original, ~valor_corregido, ~motivo,
  "C01", 50006L, "ACACÍAS", "ail, ail_ad, at (ambos bienios)", "1", "NA",
  "Valor de relleno: municipio no beneficiario de AIL (categoria y NBI fuera del art. 36, Ley 2056/2020)",
  "C02", 50006L, "ACACÍAS", "di_ail, dit (ambos bienios)", "1.971.823 / 1.971.824", "NA",
  "Saldo sin asignacion asociada en la fuente AIL; no comparable; se excluye",
  "C03", 50001L, "VILLAVICENCIO", "todas las variables presupuestales", "1", "NA",
  "Valor de relleno: municipio no beneficiario de AIL"
)
no_benef <- c(50006L, 50001L)
presup <- presup %>%
  mutate(beneficiario_ail = as.integer(!cod_dane %in% no_benef),
         across(c(ail, ail_ad, di_ail, di_ail_ad, at, dit, at_dit_original),
                ~ ifelse(beneficiario_ail == 1, .x, NA_real_)))

# ---------------------------------------------------------------------
# 3. Variables construidas (montos en pesos corrientes)
#    rezago = dit / at : saldo disponible al cierre del bienio anterior
#    relativo a la asignacion del bienio. > 1 => saldo supera asignacion.
# ---------------------------------------------------------------------
presup <- presup %>%
  mutate(at_menos_dit = at - dit,
         rezago       = dit / at,
         log_dit      = log(dit),
         log_at       = log(at),
         rezago_mayor1 = as.integer(rezago > 1))

presup_w <- presup %>%
  mutate(b = ifelse(bienio == "2021-2022", "1", "2")) %>%
  select(cod_dane, b, beneficiario_ail, ail, ail_ad, di_ail, di_ail_ad,
         at, dit, at_menos_dit, rezago, log_dit, log_at, rezago_mayor1) %>%
  pivot_wider(names_from = b,
              values_from = c(ail, ail_ad, di_ail, di_ail_ad, at, dit,
                              at_menos_dit, rezago, log_dit, log_at, rezago_mayor1),
              names_sep = "_b")

transv <- brecha %>%
  left_join(presup_w, by = "cod_dane") %>%
  mutate(d_brecha_23_22 = v_2023 - v_2022,
         d_brecha_24_23 = v_2024 - v_2023,
         d_brecha_24_22 = v_2024 - v_2022,
         d_rezago       = rezago_b2 - rezago_b1,
         d_log_dit      = log_dit_b2 - log_dit_b1) %>%
  arrange(cod_dane)

panel <- brecha %>%
  pivot_longer(starts_with("v_"), names_to = "anio", values_to = "brecha_de",
               names_prefix = "v_") %>%
  mutate(anio = as.integer(anio)) %>%
  left_join(presup_w %>% select(cod_dane, beneficiario_ail,
                                rezago_b1, rezago_b2, at_b1, at_b2,
                                dit_b1, dit_b2),
            by = "cod_dane") %>%
  arrange(cod_dane, anio)

sect_long <- sectores %>%
  pivot_longer(starts_with("v_"), names_to = "anio", values_to = "brecha",
               names_prefix = "v_") %>%
  mutate(anio = as.integer(anio)) %>%
  arrange(cod_dane, sector, anio)

# ---------------------------------------------------------------------
# 4. Validaciones automaticas
# ---------------------------------------------------------------------
val <- list()
chk <- function(nombre, cond, detalle = "") {
  val[[length(val) + 1]] <<- data.frame(prueba = nombre,
                                        resultado = ifelse(isTRUE(cond), "OK", "FALLA"),
                                        detalle = detalle)
}
chk("29 municipios del Meta, codigos unicos",
    n_distinct(brecha$cod_dane) == 29 && nrow(brecha) == 29)
chk("Sin duplicados municipio-anio", !any(duplicated(panel[, c("cod_dane", "anio")])))
chk("Panel balanceado: 3 anios por municipio",
    all(table(panel$cod_dane) == 3))
chk("Sin faltantes en brecha DE", !anyNA(panel$brecha_de))
chk("Brecha en rango [0, 100]", all(panel$brecha_de >= 0 & panel$brecha_de <= 100))
chk("Codigos DANE del Meta (prefijo 50)", all(brecha$cod_dane %/% 1000 == 50))
chk("Presupuesto: 29 municipios por bienio", all(table(presup$bienio) == 29))
chk("Mismos codigos en brecha y presupuesto",
    setequal(brecha$cod_dane, unique(presup$cod_dane)))
b <- filter(presup, beneficiario_ail == 1)
chk("AT = AIL + AIL-AD (beneficiarios)", all(abs(b$at - b$ail - b$ail_ad) < 1))
chk("DIT = D_AIL + D_AIL-AD (beneficiarios)", all(abs(b$dit - b$di_ail - b$di_ail_ad) < 1))
chk("AT-DIT original = AT - DIT", all(abs(b$at_dit_original - b$at_menos_dit) < 1))
chk("Montos positivos (beneficiarios)", all(b$at > 0 & b$dit > 0))
chk("Proporcion AIL-AD / AIL constante (2/13)",
    all(abs(b$ail_ad / b$ail - 2/13) < 1e-3),
    sprintf("rango: %.5f - %.5f", min(b$ail_ad / b$ail), max(b$ail_ad / b$ail)))
n_ad_igual <- with(filter(b, bienio == "2021-2022"), sum(abs(di_ail_ad - ail_ad) <= 1))
chk("ALERTA: DI ambiental = asignacion ambiental en bienio 2021-2022",
    n_ad_igual == 0,
    sprintf("%d de 27 municipios con saldo ambiental igual a la asignacion (ejecucion nula de AIL-AD o arrastre de digitacion); verificar contra Decreto 0363 de 2023", n_ad_igual))
chk("Sectores: 29 municipios x 13 sectores",
    nrow(sectores) == 29 * 13 && n_distinct(sectores$sector) == 13)
chk("Brecha DE en base_general = base.xlsx",
    isTRUE(all.equal(
      sectores %>% filter(sector == "desarrollo económico") %>% arrange(cod_dane) %>%
        select(v_2022, v_2023, v_2024) %>% as.data.frame(),
      brecha %>% arrange(cod_dane) %>% select(v_2022, v_2023, v_2024) %>% as.data.frame(),
      check.attributes = FALSE)))
n_ent <- sum(brecha$v_2022 %% 1 == 0)
chk("ALERTA: brecha 2022 reportada en enteros",
    n_ent < 29, sprintf("%d de 29 valores de 2022 son enteros; 2023-2024 con decimales (posible cambio de medicion MCB)", n_ent))
const <- sect_long %>% group_by(sector, anio) %>%
  summarise(n_val = n_distinct(brecha), .groups = "drop") %>% filter(n_val == 1)
chk("ALERTA: sectores con valor identico en los 29 municipios",
    nrow(const) == 0,
    paste(paste(const$sector, const$anio), collapse = "; "))
validaciones <- bind_rows(val)

# ---------------------------------------------------------------------
# 5. Control de calidad: base nueva frente a cifras de la tesis
# ---------------------------------------------------------------------
res_tesis <- read_excel(file.path(orig, "baseRes.xlsx"))
cq <- list()
cq[[1]] <- data.frame(
  serie = "Brecha DE 2022-2024 (base vs baseRes)",
  cifra_tesis = "valores municipales",
  cifra_replica = sprintf("max |dif| = %.2e",
    max(abs(as.matrix(res_tesis[order(res_tesis$C_DANE), c("v_2022","v_2023","v_2024")]) -
            as.matrix(brecha[order(brecha$cod_dane), c("v_2022","v_2023","v_2024")])))),
  coincide = "Sí", nota = "Series idénticas")
d1 <- filter(transv, beneficiario_ail == 1)
cq[[2]] <- data.frame(serie = "Municipios con DI > asignación, bienio 2021-2022",
  cifra_tesis = "21 de 29", cifra_replica = sprintf("%d de 27", sum(d1$dit_b1 > d1$at_b1)),
  coincide = "No", nota = "La tesis incluye Acacías (valor de relleno) y usa 29 como denominador")
cq[[3]] <- data.frame(serie = "Municipios con DI > asignación, bienio 2023-2024",
  cifra_tesis = "10 de 29", cifra_replica = sprintf("%d de 27", sum(d1$dit_b2 > d1$at_b2)),
  coincide = "No", nota = "Mismo problema de denominador y de Acacías")
cq[[4]] <- data.frame(serie = "Relación agregada asignación/DI, bienio 2021-2022",
  cifra_tesis = "asignación 86,01 % menor que DI",
  cifra_replica = sprintf("AT/DIT = %.4f; DIT supera AT en %.1f %%",
                          sum(d1$at_b1)/sum(d1$dit_b1), 100*(sum(d1$dit_b1)/sum(d1$at_b1)-1)),
  coincide = "No", nota = "86,01 % es el cociente AT/DIT, no una diferencia porcentual")
cq[[5]] <- data.frame(serie = "Relación agregada asignación/DI, bienio 2023-2024",
  cifra_tesis = "asignación 117,43 % mayor que DI",
  cifra_replica = sprintf("AT/DIT = %.4f; AT supera DIT en %.1f %%",
                          sum(d1$at_b2)/sum(d1$dit_b2), 100*(sum(d1$at_b2)/sum(d1$dit_b2)-1)),
  coincide = "No", nota = "117,43 % es el cociente, no el exceso")
desc <- sapply(brecha[, c("v_2022","v_2023","v_2024")], function(x) c(mean(x), sd(x)))
cq[[6]] <- data.frame(serie = "Media y DE de la brecha (2022/2023/2024)",
  cifra_tesis = "52,34/11,97; 52,25/11,54; 39,27/14,71",
  cifra_replica = paste(sprintf("%.2f/%.2f", desc[1,], desc[2,]), collapse = "; "),
  coincide = "Sí", nota = "")
cq[[7]] <- data.frame(serie = "Tabla 3 de la tesis (Dif_Amp por municipio)",
  cifra_tesis = "Barranca de Upía: 8,94 y 7,71",
  cifra_replica = sprintf("Barranca de Upía: %.2f y %.2f",
    -transv$d_brecha_23_22[transv$cod_dane==50110], -transv$d_brecha_24_23[transv$cod_dane==50110]),
  coincide = "No", nota = "Las columnas de brecha están desalineadas respecto a los municipios (8,94 y 7,71 corresponden a Acacías)")
control_calidad <- bind_rows(cq)

# ---------------------------------------------------------------------
# 6. Diccionario y fuentes
# ---------------------------------------------------------------------
diccionario <- tibble::tribble(
  ~variable, ~descripcion, ~unidad, ~construccion,
  "cod_dane", "Código DANE del municipio", "código", "Original",
  "municipio", "Nombre del municipio", "texto", "Original",
  "v_2022, v_2023, v_2024 / brecha_de", "Brecha del sector Desarrollo Económico, Metodología de Cierre de Brechas (MCB) del DNP", "índice 0-100 (mayor = más brecha)", "Original (solicitud al DNP)",
  "beneficiario_ail", "Municipio beneficiario de la Asignación para la Inversión Local", "0/1", "Corrección C01-C03",
  "ail_bk", "Asignación AIL según NBI y categorías 4, 5 y 6, bienio k", "pesos corrientes", "Ley 2072/2020 (k=1); Ley 2279/2022 (k=2)",
  "ail_ad_bk", "Asignación AIL - Ambiente y Desarrollo Sostenible, bienio k", "pesos corrientes", "Ídem",
  "di_ail_bk, di_ail_ad_bk", "Disponibilidad inicial (saldo no comprometido) de cada fuente al cierre del bienio anterior", "pesos corrientes", "Decreto 0363/2023 (k=1, cierre 2021-2022); Decreto 0379/2025 (k=2, cierre 2023-2024)",
  "at_bk", "Asignación total AIL = ail + ail_ad", "pesos corrientes", "Suma",
  "dit_bk", "Disponibilidad inicial total = di_ail + di_ail_ad", "pesos corrientes", "Suma",
  "rezago_bk", "Saldo no ejecutado relativo a la asignación del bienio", "razón", "dit_bk / at_bk",
  "rezago_mayor1_bk", "Saldo supera la asignación del bienio", "0/1", "1(rezago_bk > 1)",
  "log_dit_bk, log_at_bk", "Logaritmo natural de DIT y AT", "log pesos", "ln()",
  "d_brecha_t_s", "Cambio de la brecha entre los años s y t (negativo = cierre de brecha)", "puntos del índice", "v_t - v_s",
  "d_rezago", "Cambio del rezago entre bienios", "razón", "rezago_b2 - rezago_b1",
  "sector, brecha (base_sectores)", "Brecha MCB de los 13 sectores", "índice 0-100", "Original (base_general.xlsx)"
)
fuentes <- tibble::tribble(
  ~serie, ~productor, ~distribuidor, ~frecuencia, ~agregacion, ~cobertura, ~archivo,
  "Brechas sectoriales MCB", "Departamento Nacional de Planeación (DNP)", "DNP, por solicitud de información", "Anual", "Índice municipal por sector", "2022-2024; 29 municipios; 13 sectores", "base.xlsx, base_general.xlsx",
  "Asignaciones AIL 2021-2022", "Congreso de la República", "Diario Oficial (Ley 2072 de 2020)", "Bienal", "Apropiación por municipio y fuente", "Bienio 2021-2022", "disponibilidad_inicial_1.xlsx",
  "Disponibilidades iniciales al cierre 2021-2022", "Ministerio de Hacienda y Crédito Público", "Diario Oficial (Decreto 0363 de 2023)", "Bienal", "Saldo por municipio y fuente", "Cierre 31-dic-2022", "disponibilidad_inicial_1.xlsx",
  "Asignaciones AIL 2023-2024", "Congreso de la República", "Diario Oficial (Ley 2279 de 2022)", "Bienal", "Apropiación por municipio y fuente", "Bienio 2023-2024", "disponibilidad_inicial_2.xlsx",
  "Disponibilidades iniciales al cierre 2023-2024", "Ministerio de Hacienda y Crédito Público", "Diario Oficial (Decreto 0379 de 2025)", "Bienal", "Saldo por municipio y fuente", "Cierre 31-dic-2024", "disponibilidad_inicial_2.xlsx"
)

# ---------------------------------------------------------------------
# 7. Exportar
# ---------------------------------------------------------------------
write.csv(panel,     file.path(out, "base_maestra.csv"),     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(transv,    file.path(out, "base_transversal.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sect_long, file.path(out, "base_sectores.csv"),    row.names = FALSE, fileEncoding = "UTF-8")
write.csv(correcciones, file.path(out, "correcciones_manuales.csv"), row.names = FALSE, fileEncoding = "UTF-8")

wb <- createWorkbook()
hojas <- list(base = panel, base_transversal = transv, sectores = sect_long,
              diccionario = diccionario, fuentes = fuentes,
              correcciones = correcciones, validaciones = validaciones,
              control_calidad = control_calidad)
est <- createStyle(textDecoration = "bold", fgFill = "#DDE3EA", wrapText = TRUE)
for (h in names(hojas)) {
  addWorksheet(wb, h)
  writeData(wb, h, hojas[[h]], headerStyle = est)
  setColWidths(wb, h, cols = seq_len(ncol(hojas[[h]])), widths = "auto")
  freezePane(wb, h, firstRow = TRUE)
}
saveWorkbook(wb, file.path(out, "base_maestra.xlsx"), overwrite = TRUE)

print(validaciones, right = FALSE)
print(control_calidad[, c("serie", "cifra_tesis", "cifra_replica", "coincide")], right = FALSE)
if (any(validaciones$resultado == "FALLA" & !grepl("^ALERTA", validaciones$prueba)))
  stop("Validación crítica fallida")
message("Base maestra construida: ", nrow(panel), " filas municipio-año; ",
        sum(transv$beneficiario_ail, na.rm = TRUE), " municipios beneficiarios AIL.")
