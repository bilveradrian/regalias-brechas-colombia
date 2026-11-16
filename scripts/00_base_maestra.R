# =====================================================================
# 00_base_maestra.R — Base maestra nacional
# Rezago en el giro de la Asignación para la Inversión Local (AIL) del SGR
# y brechas sectoriales municipales (Metodología de Cierre de Brechas, DNP)
# Periodo: brechas 2022-2024; bienios presupuestales 2021-2022 y 2023-2024.
#
# Entradas (sin modificar, con huella SHA-256 en datos_originales/SHA256SUMS.txt):
#   datos/pdf/ail_municipal_final.csv   (extraído de los PDF por scripts/pdf/*.py)
#   datos_originales/nacional/Brechas/{2022,2023,2024}.xlsx
#   datos_originales/nacional/Controles/*.xlsx
#   datos_originales/anh/*.xlsx         (producción fiscalizada de crudo, ANH)
# Salidas: datos/base_panel.csv (municipio × año), datos/base_bienio.csv
#   (municipio × bienio), datos/base_sectores.csv (municipio × sector × año),
#   datos/base_maestra.xlsx (con diccionario, fuentes, correcciones,
#   validaciones y control de calidad).
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(readxl); library(dplyr); library(tidyr); library(openxlsx) })
orig <- "datos_originales"; out <- "datos"
num <- function(x) as.numeric(gsub(",", ".", gsub("\\.", "", x)))       # "25.788,00" -> 25788
nrm <- function(x) toupper(gsub("[^A-Za-z]", "", iconv(x, "UTF-8", "ASCII//TRANSLIT")))

# ---------------------------------------------------------------------
# 1. Universo municipal (DANE) y controles
# ---------------------------------------------------------------------
ig <- read_excel(file.path(orig, "nacional/Controles/Indicadores Generales.xlsx"), col_types = "text")
mun <- ig %>% filter(nchar(`Código Entidad`) == 5, substr(`Código Entidad`, 3, 5) != "000",
                     `Código Entidad` != "01001") %>%
  distinct(cod_dane = `Código Entidad`, municipio = Entidad, departamento = Departamento)
ctrl_ig <- ig %>% filter(`Código Entidad` %in% mun$cod_dane) %>%
  transmute(cod_dane = `Código Entidad`, anio = as.integer(Año), Indicador,
            val = num(`Dato Numérico`), cual = `Dato Cualitativo`)
pob <- ctrl_ig %>% filter(Indicador == "Población total") %>% select(cod_dane, anio, poblacion = val)
dens <- ctrl_ig %>% filter(Indicador == "Densidad poblacional") %>% select(cod_dane, anio, densidad = val)
cat617 <- ctrl_ig %>% filter(Indicador == "Categoría ley 617 de 2000") %>%
  transmute(cod_dane, anio, categoria = recode(cual, "Especial" = "0"))
tipol <- ctrl_ig %>% filter(grepl("^Tipologías de los municipios", Indicador)) %>%
  transmute(cod_dane, tipologia_2024 = cual)

mdm_raw <- read_excel(file.path(orig, "nacional/Controles/Medicion Desempe#U00f1o Municipal.xlsx"), col_types = "text")
mdm <- mdm_raw %>% filter(Indicador %in% c("MDM", "Componente de gestión", "Componente de resultados", "Movilización de recursos", "Ejecución de recursos")) %>%
  transmute(cod_dane = `Código Entidad`, anio = as.integer(Año),
            var = recode(Indicador, "MDM" = "mdm", "Componente de gestión" = "mdm_gestion",
                         "Componente de resultados" = "mdm_resultados", "Movilización de recursos" = "mdm_movilizacion",
                         "Ejecución de recursos" = "mdm_ejecucion"), val = num(`Dato Numérico`)) %>%
  pivot_wider(names_from = var, values_from = val)
grupo_cap <- mdm_raw %>% filter(Indicador == "Grupo de capacidades iniciales", Año == "2022") %>%
  transmute(cod_dane = `Código Entidad`, grupo_capacidades_2022 = `Dato Cualitativo`)

# ---------------------------------------------------------------------
# 2. Brechas sectoriales (DNP): armonización de nombres de sector
# ---------------------------------------------------------------------
sec_std <- function(x) {
  u <- toupper(iconv(x, "UTF-8", "ASCII//TRANSLIT"))
  case_when(grepl("AGRI|AGROP", u) ~ "agricultura", grepl("AMBIENTE", u) ~ "ambiente",
            grepl("CULTURA", u) ~ "cultura", grepl("DEPORTE", u) ~ "deporte",
            grepl("DESARROLLO ECON", u) ~ "desarrollo_economico", grepl("EDUCACI", u) ~ "educacion",
            grepl("GOBIERNO", u) ~ "gobierno_territorial", grepl("INCLUSI", u) ~ "inclusion_social",
            grepl("JUSTICIA", u) ~ "justicia", grepl("MINAS|SERVICIOS Y ENERG", u) ~ "minas_energia",
            grepl("SALUD", u) ~ "salud", grepl("TECNOLOG", u) ~ "tic", grepl("TRANSP", u) ~ "transporte",
            grepl("VIVIENDA", u) ~ "vivienda", TRUE ~ NA_character_)
}
b22 <- read_excel(file.path(orig, "nacional/Brechas/2022.xlsx"), col_types = "text") %>%
  filter(grepl("^Brecha", Nombre_Indicador)) %>%
  transmute(cod_dane = sprintf("%05d", as.integer(cod_mpio)), anio = 2022L, sector = sec_std(Nombre_Indicador),
            brecha = as.numeric(Brecha_sector), grupo_referencia = Grupo)
b23 <- read_excel(file.path(orig, "nacional/Brechas/2023.xlsx"), sheet = "Brechas", col_types = "text") %>%
  transmute(cod_dane = sprintf("%05d", as.integer(cod_dane)), anio = 2023L, sector = sec_std(Sector),
            brecha = as.numeric(Brecha), pdet = PDET, grupo_referencia = NA_character_)
b24 <- read_excel(file.path(orig, "nacional/Brechas/2024.xlsx"), col_types = "text") %>%
  filter(!is.na(Brecha_sector)) %>%
  transmute(cod_dane = sprintf("%05d", as.integer(DANE)), anio = 2024L, sector = sec_std(Nombre_Sector),
            brecha = as.numeric(Brecha_sector), grupo_referencia = Tipología_Brechas)
pdet <- b23 %>% distinct(cod_dane, pdet) %>% mutate(pdet = as.integer(pdet == "PDET"))
sectores <- bind_rows(b22, b23 %>% select(-pdet), b24) %>% filter(!is.na(sector)) %>% arrange(cod_dane, sector, anio)
grupo_ref <- sectores %>% filter(sector == "desarrollo_economico") %>% select(cod_dane, anio, grupo_referencia)
brecha_de <- sectores %>% filter(sector == "desarrollo_economico") %>% select(cod_dane, anio, brecha_de = brecha)

# ---------------------------------------------------------------------
# 3. AIL por municipio y bienio (PDF validados)
# ---------------------------------------------------------------------
ail <- read.csv(file.path(out, "pdf/ail_municipal_final.csv"), colClasses = c(cod_dane = "character"))
ail <- ail %>% mutate(
  asignacion_ley = ifelse(beneficiario_ley == 1, ley_ail + ley_ad, NA),
  apropiacion = apropiacion_ail + apropiacion_ad, giros = giros_ail + giros_ad, di = di_ail + di_ad,
  # Rezago en el giro: proporción de la apropiación definitiva (ajustada al recaudo) que no se giró al cierre del bienio
  rezago = ifelse(apropiacion > 0, di / apropiacion, NA),
  rezago_ail = ifelse(apropiacion_ail > 0, di_ail / apropiacion_ail, NA),
  # Definición de la tesis de origen (disponibilidad inicial / asignación de ley)
  rezago_ley = ifelse(asignacion_ley > 0, di / asignacion_ley, NA),
  ajuste_recaudo = ifelse(asignacion_ley > 0, apropiacion / asignacion_ley, NA))

# ---------------------------------------------------------------------
# 4. Producción de crudo (ANH): municipio productor
# ---------------------------------------------------------------------
meses <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
anh <- bind_rows(lapply(list.files(file.path(orig, "anh"), pattern = "xlsx$", full.names = TRUE), function(f) {
  a <- as.integer(regmatches(f, regexpr("20[0-9]{2}", f)))
  d <- read_excel(f, sheet = "Campos bpdc", skip = 10, col_types = "text"); names(d) <- tolower(trimws(names(d)))
  d <- d %>% filter(!is.na(departamento), !is.na(municipio), !grepl("TOTAL|FUENTE", toupper(departamento)))
  dias <- as.numeric(format(as.Date(sprintf("%d-%02d-01", a, 1:12)) + 31 - as.numeric(format(as.Date(sprintf("%d-%02d-01", a, 1:12)) + 31, "%d")), "%d"))
  M <- sapply(meses, function(m) suppressWarnings(as.numeric(d[[m]]))); M[is.na(M)] <- 0
  tibble(anio = a, dep = nrm(d$departamento), mun = nrm(d$municipio), bpdc = as.vector(M %*% dias) / sum(dias))
})) %>% group_by(anio, dep, mun) %>% summarise(bpdc = sum(bpdc), .groups = "drop")
alias_anh <- tibble::tribble(~dep, ~mun, ~cod_dane,
  "META", "CASTILLANUEVA", "50150",        # Castilla La Nueva (nombre abreviado en ANH)
  "CESAR", "LAJAGUAIBIRICO", "20400",      # La Jagua de Ibirico
  "CESAR", "SANALBETO", "20710",           # San Alberto (error tipográfico en ANH)
  "GUAJIRA", "RIOHACHA", "44001")          # Riohacha (departamento "Guajira" en ANH)
mun_key <- mun %>% mutate(dep = nrm(departamento), munk = nrm(municipio))
anh_m <- anh %>% left_join(mun_key %>% select(dep, mun = munk, cod_dane), by = c("dep", "mun")) %>%
  left_join(alias_anh %>% rename(cod_alias = cod_dane), by = c("dep", "mun")) %>%
  mutate(cod_dane = coalesce(cod_dane, cod_alias)) %>% select(-cod_alias)
anh_sin_cod <- anh_m %>% filter(is.na(cod_dane)) %>% distinct(dep, mun, anio, bpdc)
# Campos sin municipio asignado por la ANH ("Municipio NN"): se documentan y no se asignan
produccion <- anh_m %>% filter(!is.na(cod_dane)) %>% group_by(cod_dane, anio) %>% summarise(bpdc = sum(bpdc), .groups = "drop")
# Participación del Meta en la producción fiscalizada nacional (incluye campos "NN": total nacional completo)
part_meta <- anh %>% group_by(anio) %>% summarise(nacional = sum(bpdc), meta = sum(bpdc[dep == "META"]), .groups = "drop") %>%
  mutate(participacion_meta = meta / nacional)
write.csv(part_meta, file.path(out, "anh_participacion_meta.csv"), row.names = FALSE)
productor <- produccion %>% group_by(cod_dane) %>% summarise(productor_crudo = as.integer(any(bpdc > 0)), bpdc_media = mean(bpdc))

# ---------------------------------------------------------------------
# 5. Ensamble
# ---------------------------------------------------------------------
panel <- expand_grid(cod_dane = mun$cod_dane, anio = 2022:2024) %>%
  left_join(mun, by = "cod_dane") %>% left_join(brecha_de, by = c("cod_dane", "anio")) %>%
  left_join(grupo_ref, by = c("cod_dane", "anio")) %>%
  left_join(pob, by = c("cod_dane", "anio")) %>% left_join(dens, by = c("cod_dane", "anio")) %>%
  left_join(cat617, by = c("cod_dane", "anio")) %>% left_join(mdm, by = c("cod_dane", "anio")) %>%
  left_join(produccion, by = c("cod_dane", "anio")) %>% mutate(bpdc = coalesce(bpdc, 0)) %>%
  left_join(pdet, by = "cod_dane") %>% left_join(grupo_cap, by = "cod_dane") %>% left_join(tipol, by = "cod_dane") %>%
  left_join(productor %>% select(cod_dane, productor_crudo), by = "cod_dane") %>%
  mutate(productor_crudo = coalesce(productor_crudo, 0L), meta = as.integer(substr(cod_dane, 1, 2) == "50"))

bienio <- ail %>% select(cod_dane, bienio, beneficiario_ley, ley_ail, ley_ad, asignacion_ley, apropiacion_ail, giros_ail, di_ail,
                         apropiacion_ad, giros_ad, di_ad, apropiacion, giros, di, rezago, rezago_ail, rezago_ley, ajuste_recaudo,
                         calidad_ley, calidad_dec_ail, calidad_dec_ad) %>%
  pivot_wider(names_from = bienio, values_from = -c(cod_dane, bienio), names_glue = "{.value}_b{bienio}")

transv <- mun %>% left_join(bienio, by = "cod_dane") %>%
  left_join(brecha_de %>% pivot_wider(names_from = anio, values_from = brecha_de, names_prefix = "de_"), by = "cod_dane") %>%
  left_join(pob %>% filter(anio == 2022) %>% select(cod_dane, poblacion_2022 = poblacion), by = "cod_dane") %>%
  left_join(cat617 %>% filter(anio == 2022) %>% select(cod_dane, categoria_2022 = categoria), by = "cod_dane") %>%
  left_join(mdm %>% filter(anio == 2022) %>% select(cod_dane, mdm_2022 = mdm, mdm_gestion_2022 = mdm_gestion), by = "cod_dane") %>%
  left_join(pdet, by = "cod_dane") %>% left_join(grupo_cap, by = "cod_dane") %>% left_join(tipol, by = "cod_dane") %>%
  left_join(productor, by = "cod_dane") %>%
  mutate(productor_crudo = coalesce(productor_crudo, 0L), bpdc_media = coalesce(bpdc_media, 0),
         meta = as.integer(substr(cod_dane, 1, 2) == "50"),
         muestra = as.integer(coalesce(beneficiario_ley_b1, 0L) == 1 & coalesce(beneficiario_ley_b2, 0L) == 1 &
                                !is.na(rezago_b1) & !is.na(rezago_b2) & !is.na(de_2022) & !is.na(de_2024)),
         d_de_23_22 = de_2023 - de_2022, d_de_24_23 = de_2024 - de_2023, d_de_24_22 = de_2024 - de_2022,
         d_rezago = rezago_b2 - rezago_b1,
         aprop_pc_b1 = apropiacion_b1 / poblacion_2022, log_aprop_pc_b1 = log(aprop_pc_b1),
         log_pob_2022 = log(poblacion_2022))

# ---------------------------------------------------------------------
# 6. Validaciones automáticas
# ---------------------------------------------------------------------
val <- list(); chk <- function(n, c, d = "") val[[length(val) + 1]] <<- data.frame(prueba = n, resultado = ifelse(isTRUE(c), "OK", "FALLA"), detalle = d)
chk("Universo DANE: 1.102 municipios + áreas no municipalizadas", nrow(mun) >= 1102, sprintf("%d entidades", nrow(mun)))
chk("Sin duplicados municipio-año en el panel", !any(duplicated(panel[, c("cod_dane", "anio")])))
chk("Brecha DE disponible para 1.102 municipios en cada año",
    all(table(brecha_de$anio[!is.na(brecha_de$brecha_de)]) == 1102), paste(table(brecha_de$anio), collapse = "/"))
chk("Brecha en [0, 100]", all(sectores$brecha >= 0 & sectores$brecha <= 100, na.rm = TRUE),
    sprintf("rango %.2f-%.2f", min(sectores$brecha, na.rm = TRUE), max(sectores$brecha, na.rm = TRUE)))
chk("Códigos AIL contenidos en el universo DANE", all(ail$cod_dane %in% c(mun$cod_dane, "88000")))
chk("Identidad apropiación − giros = DI (±2 pesos) en toda la AIL",
    all(abs(ail$apropiacion_ail - ail$giros_ail - ail$di_ail) <= 2, na.rm = TRUE) &
      all(abs(ail$apropiacion_ad - ail$giros_ad - ail$di_ad) <= 2, na.rm = TRUE))
chk("Rezago en [0, 1]", all(ail$rezago >= -1e-9 & ail$rezago <= 1 + 1e-9, na.rm = TRUE),
    sprintf("rango %.3f-%.3f", min(ail$rezago, na.rm = TRUE), max(ail$rezago, na.rm = TRUE)))
chk("Identidad 2/13 en la asignación de ley (beneficiarios, ±1 peso salvo imputados)",
    all(abs(round(ail$ley_ail * 2 / 13) - ail$ley_ad) <= 1 | ail$calidad_ley != "exacta", na.rm = TRUE))
sub <- read.csv(file.path(out, "pdf/validacion_subtotales.csv"))
chk("Subtotales departamentales impresos vs. suma municipal", mean(sub$resultado == "coincide") > 0.95,
    sprintf("%d coinciden; %d con error de OCR en el subtotal impreso; %d con lectura parcial", sum(sub$resultado == "coincide"),
            sum(grepl("1_digito", sub$resultado)), sum(sub$resultado %in% c("sin_lectura", "difiere"))))
chk("Producción ANH asignada a códigos DANE (salvo campos 'NN' sin municipio)", all(grepl("NN", anh_sin_cod$mun)),
    paste(unique(paste(anh_sin_cod$dep, anh_sin_cod$mun)), collapse = "; "))
chk("Muestra analítica (beneficiarios en ambos bienios con datos completos)", sum(transv$muestra) > 900, sprintf("n = %d", sum(transv$muestra)))
validaciones <- bind_rows(val)

# ---------------------------------------------------------------------
# 7. Control de calidad frente a la base del Meta de la tesis de origen
# ---------------------------------------------------------------------
tes <- read_excel(file.path(orig, "base.xlsx")) %>% transmute(cod_dane = sprintf("%05d", as.integer(ciudad)), v_2022, v_2023, v_2024)
qc <- tes %>% left_join(transv %>% select(cod_dane, de_2022, de_2023, de_2024), by = "cod_dane") %>%
  summarise(max_dif_2022 = max(abs(v_2022 - de_2022)), max_dif_2023 = max(abs(v_2023 - de_2023)), max_dif_2024 = max(abs(v_2024 - de_2024)),
            n_2022_entero = sum(v_2022 %% 1 == 0))
mtes <- read.csv(file.path(out, "pdf/control_meta_tesis.csv"))
control_calidad <- bind_rows(
  data.frame(serie = "Brecha DE 2022 (tesis vs DNP)", resultado = sprintf("dif. máx. %.3f; la tesis redondeó a enteros (%d de 29)", qc$max_dif_2022, qc$n_2022_entero)),
  data.frame(serie = "Brecha DE 2023 (tesis vs DNP)", resultado = sprintf("dif. máx. %.2e", qc$max_dif_2023)),
  data.frame(serie = "Brecha DE 2024 (tesis vs DNP)", resultado = sprintf("dif. máx. %.2e", qc$max_dif_2024)),
  data.frame(serie = paste("AIL Meta:", mtes$variable_tesis, "bienio", mtes$bienio),
             resultado = sprintf("%d de %d coinciden %s", mtes$coinciden, mtes$n, ifelse(is.na(mtes$no_coinciden) | mtes$no_coinciden == "", "", paste("| difiere:", mtes$no_coinciden)))))

# ---------------------------------------------------------------------
# 8. Diccionario, fuentes y correcciones
# ---------------------------------------------------------------------
diccionario <- tibble::tribble(
  ~variable, ~descripcion, ~unidad, ~construccion,
  "cod_dane", "Código DANE del municipio", "código", "DANE",
  "de_YYYY / brecha_de", "Brecha del sector Desarrollo Económico (MCB, DNP)", "índice 0-100", "Promedio de brechas estandarizadas de densidad empresarial, ingresos tributarios y no tributarios per cápita (log) y valor agregado per cápita (log) frente al mejor municipio del grupo de referencia",
  "grupo_referencia", "Grupo de comparación de la MCB", "categoría", "2022: grupos de capacidades G1-G5 y ciudades (C); 2024: tipologías T1-T5 y C",
  "ley_ail_bk, ley_ad_bk", "Asignación inicial AIL según NBI y categorías 4-6 / AIL Ambiente y Desarrollo Sostenible", "pesos corrientes", "Anexos 3 y 4 de las Leyes 2072/2020 (k=1) y 2279/2022 (k=2)",
  "apropiacion_*_bk", "Apropiación definitiva del bienio ajustada al recaudo", "pesos corrientes", "Decreto 0363/2023 Anexo 2 (k=1); Decreto 0379/2025 Anexos 3-4 (k=2)",
  "giros_*_bk", "Giros o pagos realizados en el bienio", "pesos corrientes", "Ídem",
  "di_*_bk", "Disponibilidad inicial: apropiación − giros al cierre del bienio", "pesos corrientes", "Ídem; contrastada con Decreto 0363 Anexos 5-6 y Decreto 0379 Anexos 8-9",
  "rezago_bk", "Rezago en el giro: proporción de la apropiación no girada al cierre del bienio", "proporción 0-1", "di / apropiacion (AIL + AIL-AD)",
  "rezago_ail_bk", "Rezago en el giro solo de la AIL según NBI", "proporción 0-1", "di_ail / apropiacion_ail",
  "rezago_ley_bk", "Saldo no girado frente a la asignación de ley (definición de la tesis)", "razón", "di / asignacion_ley",
  "ajuste_recaudo_bk", "Apropiación definitiva frente a la asignación de ley", "razón", "apropiacion / asignacion_ley",
  "beneficiario_ley_bk", "Municipio con asignación AIL en la ley de presupuesto del bienio", "0/1", "Presencia en Anexo 3 de la ley",
  "muestra", "Pertenece a la muestra analítica", "0/1", "Beneficiario en ambos bienios con rezago y brecha DE 2022 y 2024",
  "productor_crudo", "Municipio con producción fiscalizada de crudo en 2022-2024", "0/1", "ANH, producción por campo",
  "bpdc", "Producción fiscalizada de crudo", "barriles por día calendario", "ANH; promedio anual ponderado por días",
  "meta", "Municipio del departamento del Meta", "0/1", "Código DANE 50xxx",
  "pdet", "Municipio PDET", "0/1", "Archivo de brechas 2023 (DNP)",
  "poblacion, densidad", "Población total y densidad", "personas; hab/km2", "TerriData (DANE)",
  "categoria", "Categoría Ley 617 de 2000", "0 (especial)-6", "TerriData",
  "mdm, mdm_gestion, mdm_resultados, mdm_movilizacion, mdm_ejecucion", "Medición de Desempeño Municipal y componentes", "puntos 0-100", "DNP",
  "grupo_capacidades_2022", "Grupo de capacidades iniciales MDM", "categoría", "DNP")
fuentes <- tibble::tribble(
  ~serie, ~productor, ~distribuidor, ~frecuencia, ~agregacion, ~cobertura, ~fecha,
  "Brechas sectoriales MCB", "DNP", "DNP (solicitud de información)", "Anual", "Municipio × sector", "2022-2024; 1.102 municipios", "Recibido en 2026",
  "Asignación AIL de ley", "Congreso de la República", "Diario Oficial: Leyes 2072/2020 y 2279/2022 (PDF escaneado)", "Bienal", "Municipio", "2021-2022; 2023-2024", "Leyes de dic. 2020 y dic. 2022",
  "Cierre presupuestal AIL", "MHCP", "Diario Oficial: Decretos 0363/2023 y 0379/2025 (PDF escaneado)", "Bienal", "Municipio", "Cierres a 31-dic-2022 y 31-dic-2024", "16-mar-2023; 31-mar-2025",
  "Producción de crudo", "ANH", "anh.gov.co, Estadísticas de producción", "Mensual", "Campo, municipio", "2022-2024", "Archivos anuales",
  "Población, densidad, categoría, tipología", "DANE / MinHacienda / DNP", "TerriData (DNP)", "Anual", "Municipio", "2022-2024", "Descarga 2026",
  "MDM", "DNP", "TerriData (DNP)", "Anual", "Municipio", "2022-2024", "Descarga 2026")
correcciones <- bind_rows(
  read.csv(file.path(out, "pdf/revision_manual.csv"), colClasses = "character") %>% mutate(tipo_correccion = "lectura visual del PDF"),
  read.csv(file.path(out, "pdf/correccion_codigos.csv"), colClasses = "character") %>% mutate(tipo_correccion = "código DANE leído por OCR"),
  alias_anh %>% mutate(tipo_correccion = "nombre ANH sin coincidencia exacta"))

# ---------------------------------------------------------------------
# 9. Exportar
# ---------------------------------------------------------------------
write.csv(panel, file.path(out, "base_panel.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(transv, file.path(out, "base_transversal.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sectores, file.path(out, "base_sectores.csv"), row.names = FALSE, fileEncoding = "UTF-8")
wb <- createWorkbook(); est <- createStyle(textDecoration = "bold", fgFill = "#DDE3EA", wrapText = TRUE)
hojas <- list(base_transversal = transv, base_panel = panel, sectores = sectores, diccionario = diccionario, fuentes = fuentes,
              correcciones = correcciones, validaciones = validaciones, control_calidad = control_calidad,
              subtotales_pdf = sub)
for (h in names(hojas)) { addWorksheet(wb, h); writeData(wb, h, hojas[[h]], headerStyle = est); freezePane(wb, h, firstRow = TRUE)
  setColWidths(wb, h, cols = seq_len(ncol(hojas[[h]])), widths = pmin(40, pmax(10, nchar(names(hojas[[h]])) + 2))) }
saveWorkbook(wb, file.path(out, "base_maestra.xlsx"), overwrite = TRUE)
print(validaciones, right = FALSE); print(control_calidad, right = FALSE)
if (any(validaciones$resultado == "FALLA")) warning("Hay validaciones con FALLA: revisar")
