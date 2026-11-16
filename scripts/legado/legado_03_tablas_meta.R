# =====================================================================
# 03_tablas.R — Tablas en Word (flextable) con formato Apuntes del CENES:
# título "Tabla n." TNR 10 negrita centrado; cuerpo, notas y fuente TNR 10.
# Salida: tablas/tablas.docx (archivo de tablas aparte exigido por la revista)
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(flextable); library(officer) })
dir.create("tablas", showWarnings = FALSE)
res <- readRDS("resultados/resultados.rds")

f <- function(x, d = 2) ifelse(is.na(x), "", gsub("-", "−",
       formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")))
fp <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", f(p, 3)))
FUENTE <- "Fuente: elaboración propia con datos del DNP (Metodología de Cierre de Brechas), Leyes 2072 de 2020 y 2279 de 2022, y Decretos 0363 de 2023 y 0379 de 2025."
ANCHO <- 16.5  # cm útiles en página carta con márgenes de 2,5 cm

estilo <- function(ft, num, titulo, notas, fuente = FUENTE) {
  ft <- ft %>% font(fontname = "Times New Roman", part = "all") %>% fontsize(size = 10, part = "all") %>%
    bold(part = "header") %>% align(align = "center", part = "header") %>%
    border_remove() %>% hline_top(border = fp_border(width = 1), part = "header") %>%
    hline_bottom(border = fp_border(width = 0.75), part = "header") %>%
    hline_bottom(border = fp_border(width = 1), part = "body") %>%
    padding(padding = 2, part = "all") %>% set_caption(NULL)
  ft <- fit_to_width(ft, max_width = ANCHO / 2.54)
  list(num = num, titulo = titulo, ft = ft, notas = notas, fuente = fuente)
}
tabs <- list()

# Tabla 1. Variables
t1 <- data.frame(
  Variable = c("Brecha de desarrollo económico (B)", "Cambio de la brecha (ΔB)", "Asignación AIL (AT)",
               "Disponibilidad inicial (DIT)", "Rezago (R)", "Cambio del rezago (ΔR)", "Brecha en otros sectores"),
  Construcción = c("Índice sectorial de la Metodología de Cierre de Brechas; mayor valor = mayor brecha",
                   "B del año final menos B del año inicial; negativo = cierre de brecha",
                   "Suma de la AIL según NBI y categorías 4-6 y de la AIL Ambiente y Desarrollo Sostenible",
                   "Apropiación ajustada al recaudo menos giros, al cierre del bienio previo",
                   "DIT / AT del mismo bienio", "R(2023-2024) − R(2021-2022)",
                   "Índices de los 12 sectores restantes de la metodología"),
  Unidad = c("Índice 0-100", "Puntos", "Pesos corrientes", "Pesos corrientes", "Razón", "Razón", "Índice 0-100"),
  Fuente = c("DNP (solicitud de información)", "DNP", "Leyes 2072/2020 y 2279/2022", "Decretos 0363/2023 y 0379/2025",
             "Cálculo propio", "Cálculo propio", "DNP"),
  Obs. = c("27 × 3", "27", "27 × 2", "27 × 2", "27 × 2", "27", "27 × 3 × 12"), check.names = FALSE)
tabs[[1]] <- estilo(flextable(t1) %>% width(j = 1:5, width = c(3.2, 6.5, 2.2, 3.2, 1.4) / 2.54),
  1, "Variables del estudio",
  "Notas: AIL = Asignación para la Inversión Local del Sistema General de Regalías (art. 36, Ley 2056 de 2020). Se excluyen Acacías y Villavicencio, que no reciben la AIL. Los montos se expresan en pesos corrientes; el rezago es una razón sin unidades y no requiere deflactar.")

# Tabla 2. Estadísticas descriptivas
nom <- c(v_2022 = "Brecha 2022", v_2023 = "Brecha 2023", v_2024 = "Brecha 2024",
         d_brecha_24_22 = "Δ Brecha 2022-2024", rezago_b1 = "Rezago 2021-2022",
         rezago_b2 = "Rezago 2023-2024", d_rezago = "Δ Rezago", at_b1 = "AT 2021-2022 (mil millones)",
         dit_b1 = "DIT cierre 2022 (mil millones)", at_b2 = "AT 2023-2024 (mil millones)",
         dit_b2 = "DIT cierre 2024 (mil millones)")
d <- res$descr; esc <- ifelse(grepl("^at_|^dit_", d$variable), 1e9, 1)
t2 <- data.frame(Variable = nom[d$variable], Media = f(d$media / esc), DE = f(d$de / esc),
                 Mín. = f(d$min / esc), Mediana = f(d$mediana / esc), Máx. = f(d$max / esc), check.names = FALSE)
tabs[[2]] <- estilo(flextable(t2) %>% width(j = 1, width = 5.5 / 2.54), 2,
  "Estadísticas descriptivas, 27 municipios del Meta beneficiarios de la AIL",
  sprintf("Notas: DE = desviación estándar. AT = asignación; DIT = disponibilidad inicial total. La prueba t pareada rechaza un cambio medio nulo de la brecha entre 2022 y 2024 (p %s; Wilcoxon p %s); entre 2022 y 2023 no (p = %s). Correlación de Spearman entre el rezago de ambos bienios: %s (p = %s).",
          ifelse(res$camb$p_t[3] < .001, "< 0,001", paste("=", fp(res$camb$p_t[3]))), ifelse(res$camb$p_wilcoxon[3] < .001, "< 0,001", paste("=", fp(res$camb$p_wilcoxon[3]))), fp(res$camb$p_t[1]),
          f(res$corr_rez["rho"]), fp(res$corr_rez["p"])))

# Tabla 3. Hipótesis - estimando - ecuación - prueba
t3 <- data.frame(
  Hipótesis = c("H1", "H2", "Placebo"),
  Enunciado = c("Un mayor rezago al cierre de 2022 se asocia con un menor cierre de la brecha entre 2022 y 2024",
                "Una reducción del rezago entre bienios se asocia con un mayor cierre de la brecha",
                "El rezago al cierre de 2024 no se asocia con el cambio de la brecha 2022-2023"),
  Estimando = c("β₁ > 0", "θ₁ > 0", "π₁ = 0"),
  Ecuación = c("[1]", "[2]-[3]", "[4]"),
  Prueba = c("t con EE HC3; permutación Freedman-Lane; wild bootstrap; TOST",
             "t con EE HC3 y agrupados por municipio; permutación; wild bootstrap; TOST",
             "t con EE HC3; permutación; wild bootstrap"), check.names = FALSE)
tabs[[3]] <- estilo(flextable(t3) %>% width(j = 1:5, width = c(1.6, 6.4, 2, 1.8, 4.7) / 2.54), 3,
  "Correspondencia entre hipótesis, estimando, ecuación y prueba",
  "Notas: el signo esperado de H1 y H2 proviene de la hipótesis de la tesis de origen: el saldo no girado retrasa la inversión y, con ella, el cierre de la brecha. Las hipótesis se fijaron antes de estimar y no se reformularon tras ver los datos.", fuente = "Fuente: elaboración propia.")

# Tabla 4. Resultados principales
mk <- function(df, lab) data.frame(Especificación = lab, Coef. = f(df$coef), `EE HC3` = f(df$ee),
  `IC 95 %` = paste0("[", f(df$ic95_inf), "; ", f(df$ic95_sup), "]"), `p HC3` = fp(df$p_hc),
  `p perm.` = fp(df$p_perm), `p wild` = fp(df$p_wild), R2 = f(df$r2), check.names = FALSE)
t4 <- bind_rows(mk(res$H1, c("H1 (1): rezago 2022, sin controles", "H1 (2): + brecha inicial", "H1 (3): + brecha inicial y log AT")),
                mk(res$H2, c("H2 (1): Δ rezago", "H2 (2): Δ rezago + brecha inicial")),
                mk(res$placebo, c("Placebo: rezago 2024 → ΔB 2022-2023", "Horizonte de un año: rezago 2022 → ΔB 2022-2023")))
names(t4)[8] <- "R²"
tabs[[4]] <- estilo(flextable(t4) %>% width(j = 1, width = 5.3 / 2.54), 4,
  "Rezago en el giro de la AIL y cambio de la brecha de desarrollo económico",
  sprintf("Notas: n = 27 en todas las columnas. Variable dependiente: cambio de la brecha (negativo = cierre). Coef. = puntos de brecha por unidad de rezago. EE HC3 = errores robustos a heterocedasticidad de MacKinnon y White. p perm. = permutación de Freedman-Lane (10.000 réplicas); p wild = wild bootstrap Rademacher con nula impuesta (9.999 réplicas). En H2 (1), el estimador coincide con el de efectos fijos de municipio y de periodo; su EE agrupado por municipio es %s.", f(res$H2_fe$ee)))

# Tabla 5. Equivalencia y potencia
eq <- res$eq
t5 <- data.frame(Hipótesis = eq$hipotesis, `Umbral (puntos por DE)` = f(eq$puntos_por_DE, 0),
  `±Δ por unidad` = f(eq$delta), `IC 90 %` = paste0("[", f(eq$ic90_inf), "; ", f(eq$ic90_sup), "]"),
  `p TOST` = fp(eq$p_tost), Equivalente = ifelse(eq$equivalente, "Sí", "No"), check.names = FALSE)
pot <- res$pot
tabs[[5]] <- estilo(flextable(t5) %>% width(j = 1:6, width = c(2, 3, 2.5, 4, 2, 2.5) / 2.54), 5, "Pruebas de equivalencia (TOST) y efecto mínimo detectable",
  sprintf("Notas: el umbral expresa el efecto mínimo de interés como puntos de brecha por una desviación estándar del regresor (DE del rezago 2022 = %s; DE del cambio del rezago = %s). Se declara equivalencia si el IC 90 %% cae dentro de ±Δ. Con n = 27, el efecto mínimo detectable (α = 0,05; potencia 0,80) es %s puntos por DE en H1 y %s en H2.",
          f(res$sd_r1), f(res$sd_dr), f(pot$mde_puntos_por_DE[1], 1), f(pot$mde_puntos_por_DE[2], 1)))

# Tabla 6. Robustez
rb <- res$rob
t6 <- data.frame(Variante = rb$modelo, Coef. = f(rb$coef), EE = f(rb$ee),
  `IC 95 %` = paste0("[", f(rb$ic95_inf), "; ", f(rb$ic95_sup), "]"), `p` = fp(rb$p_hc),
  `p perm.` = fp(rb$p_perm), n = rb$n, check.names = FALSE)
tabs[[6]] <- estilo(flextable(t6) %>% width(j = 1, width = 6 / 2.54), 6, "Malla de robustez para H1",
  sprintf("Notas: todas las variantes controlan por la brecha inicial. EE HC3 salvo indicación. El indicador rezago > 1 y log DI (con log AT como control) cambian la escala del coeficiente. Correlación de Spearman entre rezago 2022 y ΔB 2022-2024: %s (p = %s). Al excluir un municipio a la vez, el coeficiente de H1 (2) varía entre %s y %s y su valor p mínimo es %s. Diagnóstico de H1 (2): Breusch-Pagan p = %s; Shapiro-Wilk de residuos p = %s; RESET p = %s.",
          f(res$spear["rho"]), fp(res$spear["p"]), f(min(res$jk$coef)), f(max(res$jk$coef)), fp(min(res$jk$p)),
          fp(res$diag$p[1]), fp(res$diag$p[2]), fp(res$diag$p[3])))

# Tabla 7. Sectores
sr <- res$sect_res
t7 <- data.frame(Sector = paste0(toupper(substr(sr$sector, 1, 1)), substring(sr$sector, 2)),
  Coef. = f(sr$coef), EE = f(sr$ee), `p HC3` = fp(sr$p_hc), `p Holm` = fp(sr$p_holm),
  `Controla brecha inicial` = ifelse(sr$control_inicial, "Sí", "No"), check.names = FALSE)
tabs[[7]] <- estilo(flextable(t7) %>% width(j = 1, width = 6.5 / 2.54), 7,
  "Rezago al cierre de 2022 y cambio de la brecha 2022-2024 en los 13 sectores",
  "Notas: n = 27 por sector. Ajuste de Holm por 13 comparaciones. En cultura, deporte y recreación, y transporte, la brecha de 2022 es idéntica en todos los municipios, por lo que no se incluye como control.")

doc <- read_docx()
for (t in tabs) {
  doc <- doc %>%
    body_add_fpar(fpar(ftext(paste0("Tabla ", t$num, ". ", t$titulo),
      fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)),
      fp_p = fp_par(text.align = "center", padding.bottom = 4))) %>%
    body_add_flextable(t$ft, align = "center") %>%
    body_add_fpar(fpar(ftext(t$notas, fp_text(font.family = "Times New Roman", font.size = 10)),
      fp_p = fp_par(text.align = "justify", padding.top = 3))) %>%
    body_add_fpar(fpar(ftext(t$fuente, fp_text(font.family = "Times New Roman", font.size = 10)),
      fp_p = fp_par(text.align = "justify", padding.bottom = 12)))
}
print(doc, target = "tablas/tablas.docx")
saveRDS(tabs, "tablas/tablas.rds")
message("Tablas generadas: ", length(tabs))
