# =====================================================================
# 03_tablas.R — Tablas en formato Cuadernos de Economía (APA 7): "Tabla n." en negrita,
# título en cursiva, fuente y notas debajo. Salidas: tablas/tablas.docx (archivo editable aparte)
# y tablas/json/tabla_*.json (insumo de texto/armar_docx.py, que las inserta en el manuscrito).
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(flextable); library(officer); library(jsonlite) })
dir.create("tablas", showWarnings = FALSE)
res <- readRDS("resultados/resultados.rds")

f <- function(x, k = 2) ifelse(is.na(x), "", gsub("-", "−", formatC(x, format = "f", digits = k, big.mark = ".", decimal.mark = ",")))
fp <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", f(p, 3)))
ic <- function(a, b, k = 2) paste0("[", f(a, k), "; ", f(b, k), "]")
FUENTE <- "Fuente: elaboración propia con datos del DNP (Metodología de Cierre de Brechas 2022-2024; TerriData), Leyes 2072 de 2020 y 2279 de 2022, Decretos 0363 de 2023 y 0379 de 2025 y ANH."
ANCHO <- 16.5 / 2.54

estilo <- function(tab, num, titulo, notas, fuente = FUENTE, anchos = NULL) {
  ft <- flextable(tab)
  if (!is.null(anchos)) ft <- width(ft, j = seq_along(anchos), width = anchos / 2.54)
  ft <- ft %>% font(fontname = "Times New Roman", part = "all") %>% fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>% align(align = "center", part = "header") %>% align(j = -1, align = "center", part = "body") %>%
    border_remove() %>% hline_top(border = fp_border(width = 1), part = "header") %>%
    hline_bottom(border = fp_border(width = .75), part = "header") %>% hline_bottom(border = fp_border(width = 1), part = "body") %>%
    padding(padding = 1.5, part = "all")
  if (is.null(anchos)) ft <- fit_to_width(ft, ANCHO)
  list(num = num, titulo = titulo, ft = ft, notas = notas, fuente = fuente, datos = tab, anchos = anchos)
}
tabs <- list()
NOTA_INF <- "EE CR2 = errores agrupados por departamento con corrección de sesgo de Bell y McCaffrey y grados de libertad de Satterthwaite; p wild = wild cluster bootstrap-t con nula impuesta (pesos de Webb, 9.999 réplicas)."

# Tabla 1. Variables
tabs[[1]] <- estilo(data.frame(
  Variable = c("Brecha de Desarrollo Económico (DE)", "Rezago en el giro (R)", "Asignación de ley", "Apropiación definitiva",
               "Giros", "Disponibilidad inicial (DI)", "Controles", "Efectos fijos"),
  Construcción = c("Promedio de las brechas estandarizadas de densidad empresarial, ingresos tributarios y no tributarios per cápita y valor agregado per cápita frente al mejor municipio del grupo de referencia; mayor valor = mayor brecha",
                   "DI / apropiación definitiva de la AIL (NBI + Ambiente) al cierre del bienio",
                   "Anexos 3 y 4 de la ley de presupuesto del SGR", "Asignación ajustada al recaudo efectivo",
                   "Pagos efectuados en el bienio", "Apropiación − giros; incorporada al bienio siguiente",
                   "Log población 2022; MDM-gestión 2022; log apropiación per cápita 2021-2022; productor de crudo; PDET",
                   "Departamento; categoría Ley 617 de 2022; grupo de referencia 2022 y tipología 2024 de la MCB"),
  Unidad = c("Índice 0-100", "Proporción 0-1", "Pesos corrientes", "Pesos corrientes", "Pesos corrientes", "Pesos corrientes", "Varias", "—"),
  Fuente = c("DNP", "Cálculo propio", "Leyes 2072/2020 y 2279/2022", "Decretos 0363/2023 y 0379/2025", "Ídem", "Ídem", "DANE, DNP, ANH", "DNP, MinHacienda"),
  check.names = FALSE), 1, "Variables del estudio",
  "Notas: AIL = Asignación para la Inversión Local del Sistema General de Regalías (art. 22 y 36, Ley 2056 de 2020). Los valores de leyes y decretos se extrajeron de los PDF escaneados y se validaron con identidades contables, doble lectura y subtotales departamentales (anexo metodológico). MCB = Metodología de Cierre de Brechas; MDM = Medición de Desempeño Municipal; PDET = Programas de Desarrollo con Enfoque Territorial.",
  anchos = c(3, 7.5, 2.2, 3.8))

# Tabla 2. Descriptivos
nom <- c(de_2022 = "Brecha DE 2022", de_2023 = "Brecha DE 2023", de_2024 = "Brecha DE 2024", d_de_24_22 = "Δ brecha DE 2022-2024",
         rezago_b1 = "Rezago al cierre de 2022", rezago_b2 = "Rezago al cierre de 2024", d_rezago = "Δ rezago",
         rezago_ley_b1 = "DI / asignación de ley 2021-2022", ajuste_recaudo_b1 = "Apropiación / asignación de ley 2021-2022",
         aprop_pc_b1 = "Apropiación per cápita 2021-2022 (miles $)", poblacion_2022 = "Población 2022 (miles)",
         mdm_gestion_2022 = "MDM gestión 2022", productor_crudo = "Productor de crudo", pdet = "PDET")
esc <- c(aprop_pc_b1 = 1e3, poblacion_2022 = 1e3)
ds <- res$descr %>% mutate(e = ifelse(variable %in% names(esc), esc[variable], 1))
t2 <- ds %>% filter(grupo == "todos") %>% transmute(Variable = nom[variable], `Media nacional` = f(media / e), DE = f(de / e),
                                                   Mediana = f(mediana / e)) %>%
  bind_cols(ds %>% filter(grupo == "meta") %>% transmute(`Media Meta` = f(media / e))) %>%
  bind_cols(ds %>% filter(grupo == "resto") %>% transmute(`Media resto` = f(media / e)))
# Agregados del rezago (billones de pesos): proporción no girada de la apropiación total
d <- res$d
agr <- function(x, b) { a <- sum(x[[paste0("apropiacion_", b)]]); i <- sum(x[[paste0("di_", b)]]); c(a / 1e12, i / 1e12, i / a) }
grp <- list(todos = d, meta = d[d$meta == 1, ], resto = d[d$meta == 0, ])
filas <- list(c("Apropiación total 2021-2022 (billones $)", 1, "b1", 2), c("DI total 2021-2022 (billones $)", 2, "b1", 2),
              c("Proporción no girada, agregado 2021-2022", 3, "b1", 3), c("Apropiación total 2023-2024 (billones $)", 1, "b2", 2),
              c("DI total 2023-2024 (billones $)", 2, "b2", 2), c("Proporción no girada, agregado 2023-2024", 3, "b2", 3))
t2b <- bind_rows(lapply(filas, function(r) { k <- as.integer(r[2]); dg <- as.integer(r[4])
  v <- sapply(grp, function(x) agr(x, r[3])[k])
  data.frame(Variable = r[1], `Media nacional` = f(v[1], dg), DE = "", Mediana = "", `Media Meta` = f(v[2], dg), `Media resto` = f(v[3], dg), check.names = FALSE) }))
t2 <- bind_rows(t2, t2b)
anh <- read.csv("datos/anh_participacion_meta.csv")
tabs[[2]] <- estilo(t2, 2, "Estadísticas descriptivas, municipios beneficiarios de la AIL",
  sprintf("Notas: n = %s municipios (27 del Meta): beneficiarios de la AIL en ambos bienios con datos completos, de un universo de 1.102 municipios con brecha DE medida en 2022 y 2024; se excluye Providencia, único municipio de su departamento en la muestra. DE = desviación estándar. Las seis últimas filas son totales (no medias): suma de la disponibilidad inicial (DI) sobre la suma de la apropiación definitiva. Diferencia Meta − resto (prueba t de Welch): rezago 2022 %s (p = %s); rezago 2024 %s (p = %s); Δ brecha DE %s puntos (p %s). Participación del Meta en la producción fiscalizada nacional de crudo (ANH): %s %% en 2022 y %s %% en 2024. Disponibilidad inicial de todo el SGR al cierre de 2021-2022: 19,60 billones de pesos (Decreto 0363 de 2023, art. 4).",
          f(nrow(res$d), 0),
          f(res$dif_meta$dif[1]), fp(res$dif_meta$p[1]), f(res$dif_meta$dif[2]), fp(res$dif_meta$p[2]),
          f(res$dif_meta$dif[5]), ifelse(res$dif_meta$p[5] < .001, "< 0,001", paste("=", fp(res$dif_meta$p[5]))),
          f(100 * anh$participacion_meta[anh$anio == 2022], 1), f(100 * anh$participacion_meta[anh$anio == 2024], 1)),
  anchos = c(5.6, 2.2, 1.8, 1.9, 2.2, 2.2))

# Tabla 3. Hipótesis
tabs[[3]] <- estilo(data.frame(
  Hipótesis = c("H1", "H2", "H3", "H1 sectorial"),
  Enunciado = c("Un mayor rezago al cierre de 2022 se asocia con un menor cierre de la brecha DE entre 2022 y 2024",
                "Una reducción del rezago entre bienios se asocia con un mayor cierre de la brecha DE",
                "Los municipios con mayor brecha DE inicial acumulan más rezago (capacidad de absorción)",
                "El efecto de H1 se concentra en los sectores que la MCB obliga a priorizar"),
  Estimando = c("β₁ > 0", "β₂ > 0", "π_{b} > 0", "β₄ > 0"), Ecuación = c("[2]", "[3]", "[4]", "[5]"),
  Prueba = c("t CR2; wild cluster bootstrap; TOST", "t CR2; wild cluster bootstrap; TOST", "t CR2; wild cluster bootstrap; TOST; aleatorización para el Meta",
             "t CR2 (departamento y municipio); contraste con sectores no prioritarios"), check.names = FALSE), 3,
  "Correspondencia entre hipótesis, estimando, ecuación y prueba",
  "Notas: las hipótesis y las especificaciones se fijaron antes de estimar (especificacion_econometrica.md en el paquete de replicación). H3 se añadió tras documentar que los insumos de la brecha DE usan años previos al giro.",
  fuente = "Fuente: elaboración propia.", anchos = c(1.8, 6.4, 2.1, 2.0, 3.2))

# Tabla 4. Resultados principales (H1, H2, H3)
fila4 <- function(x, i, etq, esp) data.frame(Especificación = etq, Coeficiente = f(x$coef[i], 4), `EE CR2` = f(x$ee_cr2[i], 4),
  `p CR2` = fp(x$p_cr2[i]), `p wild` = fp(x$p_wild[i]), `R²` = f(x$r2[i], 3), Controles = esp, check.names = FALSE)
t4 <- bind_rows(
  fila4(res$H1, 1, "H1 (1): rezago 2022 → Δ brecha", "Ninguno"),
  fila4(res$H1, 2, "H1 (2)", "Brecha inicial y EF"),
  fila4(res$H1, 3, "H1 (3) preferida", "Completos"),
  fila4(res$H2, 1, "H2 (1): Δ rezago → Δ brecha", "Ninguno"),
  fila4(res$H2, 2, "H2 (2)", "Brecha inicial y EF"),
  fila4(res$H2, 3, "H2 (3) preferida", "Completos"),
  fila4(res$H3, 1, "H3 2022 (1): brecha 2022 → rezago", "Ninguno"),
  fila4(res$H3, 2, "H3 2022 (2) preferida", "Completos"),
  fila4(res$H3, 3, "H3 2024 (1): brecha 2022 → rezago", "Ninguno"),
  fila4(res$H3, 4, "H3 2024 (2) preferida", "Completos"))
tabs[[4]] <- estilo(t4, 4, "Rezago en el giro de la AIL y brecha de Desarrollo Económico", anchos = c(5.4, 1.9, 1.7, 1.5, 1.5, 1.3, 2.6), notas =
  paste("Notas: n = 1.037 municipios; 31 departamentos. H1 y H2: variable dependiente = cambio de la brecha DE 2022-2024 (negativo = cierre); regresor = rezago al cierre de 2022 (H1) o cambio del rezago entre bienios (H2). H3: variable dependiente = rezago al cierre de cada bienio; regresor = brecha DE 2022 (puntos).",
        NOTA_INF, sprintf("Completos = brecha inicial (salvo en H3), log población, MDM-gestión, log apropiación per cápita, productor de crudo, PDET y efectos fijos (EF) de departamento, categoría y grupos de referencia de la MCB. En H2 la estimación sin controles es casi igual a la de un panel de dos periodos con efectos fijos de municipio y de departamento × año (coeficiente %s; EE %s).", f(res$H2_panel$coef, 3), f(res$H2_panel$ee, 3))))

# Tabla 5. Heterogeneidad e inferencia para el Meta
het <- res$het; rip <- res$ri_p; ms <- res$meta_sola
t5 <- data.frame(
  Especificación = c("H1: rezago × Meta", "H3 2022: brecha × Meta", "H3 2024: brecha × Meta", "Nivel: Δ brecha DE del Meta",
                     "H1: rezago × productor", "H3 2022: brecha × productor"),
  Coef. = f(c(het$coef[1], het$coef[3], het$coef[4], res$meta_nivel$coef, het$coef[2], het$coef[5]), 4),
  `p CR2` = fp(c(het$p_cr2[1], het$p_cr2[3], het$p_cr2[4], res$meta_nivel$p_cr2, het$p_cr2[2], het$p_cr2[5])),
  `p HC3` = fp(c(het$p_hc3[1], het$p_hc3[3], het$p_hc3[4], res$meta_nivel$p_hc3, het$p_hc3[2], het$p_hc3[5])),
  `p aleatorización (dos colas)` = fp(c(rip$p_ri[3], rip$p_ri[1], rip$p_ri[2], rip$p_ri[4], NA, NA)),
  `p aleatorización (una cola)` = fp(c(rip$p_ri_una_cola[3], rip$p_ri_una_cola[1], rip$p_ri_una_cola[2], rip$p_ri_una_cola[4], NA, NA)),
  `p dentro del Meta` = fp(c(ms$p_perm[3], ms$p_perm[1], ms$p_perm[2], NA, NA, NA)), check.names = FALSE)
tabs[[5]] <- estilo(t5, 5, "Heterogeneidad: el Meta y los municipios productores de crudo", anchos = c(4.2, 1.9, 1.6, 1.6, 2.4, 2.4, 2.4), notas =
  paste0("Notas: el Meta es un solo conglomerado departamental, por lo que los errores agrupados sobrestiman la precisión de sus interacciones. Aleatorización: la misma interacción se estima para cada uno de los 22 departamentos con al menos 15 municipios y se ubica al Meta en esa distribución (Conley y Taber, 2011). Dentro del Meta: regresión con los 27 municipios (controles: brecha inicial, log población y productor) y permutación de Freedman-Lane (9.999 réplicas)."))

# Tabla 6. H1 sectorial y placebos
sec <- res$sectorial; pl <- res$placebo
t6 <- data.frame(Especificación = c("H1 sectorial: rezago × 3 sectores de mayor brecha", "Sectores no prioritarios: rezago × 3 sectores de menor brecha",
                                    "Anticipación: rezago 2024 → Δ brecha 2022-2023", "Horizonte corto: rezago 2022 → Δ brecha 2022-2023"),
                 n = f(c(sec$n, pl$n), 0), Coef. = f(c(sec$coef, pl$coef), 3), `EE CR2` = f(c(sec$ee_cr2, pl$ee_cr2), 3),
                 `IC 95 %` = ic(c(sec$ic_inf, pl$ic_inf), c(sec$ic_sup, pl$ic_sup), 3), `p CR2` = fp(c(sec$p_cr2, pl$p_cr2)),
                 `p alt.` = fp(c(sec$p_mun, pl$p_wild)), check.names = FALSE)
tabs[[6]] <- estilo(t6, 6, "Triple diferencia sectorial y pruebas de falsación", anchos = c(5.2, 1.5, 1.6, 1.6, 3.2, 1.4, 1.4), notas =
  "Notas: triple diferencia con 10 sectores medidos en 2022 y 2024; efectos fijos de municipio y de sector, pendientes sectoriales de la brecha inicial e interacciones del indicador sectorial con los controles. p alt.: errores agrupados por municipio en las filas sectoriales; wild cluster bootstrap en el contraste de anticipación.")

# Tabla 7. Equivalencia y efecto mínimo detectable
eq <- res$equiv
tabs[[7]] <- estilo(data.frame(Especificación = c("H1 (3)", "H2 (3)", "H3 2022", "H3 2024", "H1 sectorial"),
  `Umbral ±Δ` = f(eq$delta, 4), `IC 90 %` = ic(eq$ic90_inf, eq$ic90_sup, 4), `p TOST` = fp(eq$p_tost),
  `¿Equivalente a cero?` = ifelse(eq$equivalente, "Sí", "No"), `EMD (DE)` = f(eq$mde_std, 3), check.names = FALSE), 7,
  "Pruebas de equivalencia (TOST) y efecto mínimo detectable", anchos = c(2.8, 2.2, 4.4, 1.8, 2.6, 2.2), notas =
  "Notas: umbral = efecto de 0,10 desviaciones estándar del resultado por desviación estándar del regresor. Se declara equivalencia a cero si el IC 90 % cae dentro de ±Δ. EMD = efecto mínimo detectable (α = 0,05; potencia 0,80) en desviaciones estándar.")

# Tabla 8 (anexo). Malla de robustez
rb <- res$rob %>% mutate(modelo = sub("log_pob_2022", "log población", sub("mdm_gestion_2022", "MDM gestión", sub("log_aprop_pc_b1", "log apropiación per cápita", sub("productor_crudo", "productor de crudo", sub("pdet", "PDET", modelo))))))
tabs[[8]] <- estilo(data.frame(Variante = rb$modelo, n = f(rb$n, 0), Coeficiente = f(rb$coef, 4), EE = f(rb$ee_cr2, 4),
                               `IC 95 %` = ic(rb$ic_inf, rb$ic_sup, 4), p = fp(rb$p_cr2), check.names = FALSE), "A2",
  "Malla de robustez para H1 y H3 (cierre 2022)",
  sprintf("Notas: EE CR2 salvo en la regresión de mediana (bootstrap, 999 réplicas). Las filas \"EF + brecha inicial + control\" añaden un control a la vez: el coeficiente positivo de H1 (2) desaparece al condicionar en el tamaño poblacional. Diagnóstico de H1 (3): Breusch-Pagan p %s, RESET p %s (por ello se reporta la versión con spline); VIF máximo %s.",
          ifelse(res$diag$breusch_pagan_p[1] < .001, "< 0,001", fp(res$diag$breusch_pagan_p[1])), ifelse(res$diag$reset_p[1] < .001, "< 0,001", fp(res$diag$reset_p[1])), f(res$diag$vif_max[1])),
  anchos = c(6.5, 1.2, 2, 1.8, 3.4, 1.6))

# Tabla A2 (anexo). Correlaciones entre rezago, brecha y escala municipal
vc <- c(rezago_b1 = "Rezago 2022", d_rezago = "Δ rezago", d_de_24_22 = "Δ brecha DE", de_2022 = "Brecha DE 2022",
        log_pob_2022 = "Log población", log_aprop_tot = "Log apropiación total 2021-2022")
dc <- res$d %>% mutate(log_aprop_tot = log(apropiacion_b1))
cm <- cor(dc[, names(vc)]); cm[upper.tri(cm, diag = TRUE)] <- NA
t9 <- data.frame(Variable = unname(vc), apply(cm, 2, function(x) f(x, 3)), check.names = FALSE)
names(t9)[-1] <- paste0("(", seq_along(vc), ")"); t9$Variable <- paste0("(", seq_along(vc), ") ", t9$Variable)
tabs[[9]] <- estilo(t9[, -ncol(t9)], "A3", "Correlaciones entre el rezago, la brecha y la escala municipal",
  "Notas: coeficientes de correlación de Pearson, n = 1.037 municipios. La escala municipal (población y apropiación total) se asocia con un menor rezago, una brecha inicial mayor y un mayor cierre de la brecha.",
  anchos = c(5, 2, 2, 2, 2, 2))
# Tabla A1 (anexo). Validación de la base extraída de los PDF
fin <- read.csv("datos/pdf/ail_municipal_final.csv"); vs <- read.csv("datos/pdf/validacion_subtotales.csv")
cmt <- read.csv("datos/pdf/control_meta_tesis.csv"); rm_ <- read.csv("datos/pdf/revision_manual.csv"); cc <- read.csv("datos/pdf/correccion_codigos.csv")
nn <- function(x, v) sum(x %in% v)
t10 <- data.frame(Validación = c(
  "Registros municipio-bienio extraídos",
  "Asignación de ley: lectura exacta que cumple la proporción entre AIL y componente ambiental",
  "Asignación de ley: componente imputado por la proporción (diferencia ≤ 6 pesos)",
  "Asignación de ley: municipio sin asignación en el anexo",
  "Decretos, AIL: identidad apropiación − giros = DI en la lectura automática",
  "Decretos, AIL: identidad cumplida con la segunda lectura de la DI",
  "Decretos, AIL: identidad cumplida tras lectura visual de la página",
  "Decretos, ambiente: identidad en la lectura automática",
  "Decretos, ambiente: cumplida con segunda lectura de la DI, ceros o guiones",
  "Decretos, ambiente: cumplida tras lectura visual de la página",
  "Decretos, ambiente: identidad no verificable (se conservan los valores del decreto)",
  "Subtotales departamentales: suma municipal = subtotal impreso",
  "Subtotales: error de un dígito en el subtotal impreso (OCR)",
  "Subtotales: sin lectura o con diferencia",
  "Códigos DANE corregidos por coincidencia de nombre",
  "Lecturas visuales documentadas con página de origen",
  "Control externo, Meta: valores que coinciden con la tesis de origen"),
  Registros = f(c(nrow(fin), nn(fin$estado_ley, "ok_R1"), nn(fin$estado_ley, c("imputado_AIL", "imputado_AD")), nn(fin$estado_ley, "pendiente"),
    nn(fin$estado_dec_ail, "ok_R2"), nn(fin$estado_dec_ail, c("ok_R2_reparado_R3", "ok_DI_doble_G_implicito", "reparado_DI")), nn(fin$estado_dec_ail, "revisado_visual"),
    nn(fin$estado_dec_ad, "ok_R2"), nn(fin$estado_dec_ad, c("cero_guiones", "reparado_DI", "ok_DI_cero", "ok_DI_doble_G_implicito")), nn(fin$estado_dec_ad, "revisado_visual"),
    nn(fin$estado_dec_ad, "falla_R2"), nn(vs$resultado, "coincide"), nn(vs$resultado, "difiere_1_digito_en_subtotal_impreso"),
    nn(vs$resultado, c("sin_lectura", "difiere")), nrow(cc), nrow(rm_), sum(cmt$coinciden)), 0),
  check.names = FALSE)
t10$Registros[17] <- paste0(f(sum(cmt$coinciden), 0), " de ", f(sum(cmt$n), 0))
tabs[[10]] <- estilo(t10, "A1", "Validación de la información presupuestal extraída de los documentos escaneados",
  "Notas: registros municipio-bienio de las Leyes 2072 de 2020 y 2279 de 2022 y de los Decretos 0363 de 2023 y 0379 de 2025. La DI se publica en dos anexos de cada decreto, lo cual permite una segunda lectura independiente. La única discrepancia con la tesis de origen corresponde a un error de digitación en la tesis (La Macarena, 2.000 pesos). La diferencia en un subtotal departamental de Antioquia se concilia con el anexo alterno del mismo decreto.",
  anchos = c(12.5, 3))

# Tabla A4 (anexo). Forma funcional y variables omitidas
ra <- read.csv("resultados/exploratorio_forma_omitidas.csv"); ra <- ra[ra$modelo != "asinh(DI per cápita)", ]; os <- read.csv("resultados/oster.csv")
t11 <- data.frame(Variante = ra$modelo, Coef. = f(ra$coef, 4), EE = f(ra$ee_cr2, 4), `IC 95 %` = ic(ra$ic_inf, ra$ic_sup, 4),
                  p = fp(ra$p_cr2), `Coef. estandarizado` = f(ra$coef_std, 4), check.names = FALSE)
tabs[[11]] <- estilo(t11, "A4", "Primera hipótesis: forma funcional del regresor y controles adicionales",
  sprintf("Notas: n = 1.037; especificación preferida de la ecuación [2] con el regresor indicado en cada fila; errores CR2 agrupados por departamento. Montos per cápita en miles de pesos por habitante (población 2022); en la versión logarítmica, el log de la DI total y el de la DI per cápita producen el mismo coeficiente porque el log de la población es un control. Controles adicionales del MDM 2022 (movilización de recursos, ejecución de recursos, componente de resultados, gobierno abierto) y log densidad poblacional. Cota de Oster (2019) con R² máximo = %s y selección en no observables igual a la de observables (δ = 1): β* = %s, frente a %s sin controles adicionales y %s en la especificación preferida.",
          f(os$rmax, 2), f(os$beta_oster, 3), f(os$b_corto, 3), f(os$b_largo, 3)),
  anchos = c(5.6, 1.6, 1.6, 3, 1.3, 2.3))
doc <- read_docx(); dir.create("tablas/json", showWarnings = FALSE)
tx <- function(...) fp_text(font.family = "Times New Roman", font.size = 10, ...)
for (t in tabs) {
  fu <- sub("^Fuente: ", "", t$fuente)
  doc <- doc %>%
    body_add_fpar(fpar(ftext(paste0("Tabla ", t$num, "."), tx(bold = TRUE)), fp_p = fp_par(text.align = "center"))) %>%
    body_add_fpar(fpar(ftext(t$titulo, tx(italic = TRUE)), fp_p = fp_par(text.align = "center", padding.bottom = 4))) %>%
    body_add_flextable(t$ft, align = "center") %>%
    body_add_fpar(fpar(ftext("Fuente: ", tx(bold = TRUE)), ftext(fu, tx()), fp_p = fp_par(text.align = "justify", padding.top = 3))) %>%
    body_add_fpar(fpar(ftext(t$notas, tx()), fp_p = fp_par(text.align = "justify", padding.bottom = 12)))
  write_json(list(num = as.character(t$num), titulo = t$titulo, fuente = fu, notas = t$notas,
                  encabezado = names(t$datos), cuerpo = unname(as.matrix(t$datos)), anchos = t$anchos),
             file.path("tablas/json", paste0("tabla_", t$num, ".json")), auto_unbox = TRUE, pretty = TRUE)
}
print(doc, target = "tablas/tablas.docx"); saveRDS(tabs, "tablas/tablas.rds")
message("Tablas generadas: ", length(tabs))
