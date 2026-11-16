# =====================================================================
# 01_analisis.R — Estimaciones nacionales (ver especificacion_econometrica.md)
# Entradas: datos/base_transversal.csv, datos/base_panel.csv, datos/base_sectores.csv
# Salida:   resultados/resultados.rds y resultados/*.csv
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(estimatr) })
source("scripts/funciones.R")
set.seed(20261110)
dir.create("resultados", showWarnings = FALSE)
B_WILD <- 9999

# ---------------------------------------------------------------------
# 1. Datos
# ---------------------------------------------------------------------
leer <- function(f) read.csv(f, colClasses = c(cod_dane = "character"), fileEncoding = "UTF-8")
panel <- leer("datos/base_panel.csv")
ref <- panel %>% filter(anio %in% c(2022, 2024)) %>% select(cod_dane, anio, grupo_referencia) %>%
  pivot_wider(names_from = anio, values_from = grupo_referencia, names_prefix = "ref_")
d <- leer("datos/base_transversal.csv") %>% filter(muestra == 1) %>% left_join(ref, by = "cod_dane") %>%
  # Efectos fijos sin celdas unitarias: categorías 1-3 agrupadas (9 municipios), grupo "C" (1 ciudad) con G1,
  # y se excluye el único municipio de su departamento (San Andrés), que el EF ajustaría de forma perfecta.
  group_by(departamento = substr(cod_dane, 1, 2)) %>% filter(n() > 1) %>% ungroup() %>%
  mutate(departamento = factor(departamento),
         categoria = factor(ifelse(categoria_2022 %in% c("1", "2", "3"), "1-3", categoria_2022)),
         ref_2022 = factor(ifelse(ref_2022 == "C", "G1", ref_2022)), ref_2024 = factor(ref_2024),
         mdm_gestion_2022 = ifelse(is.na(mdm_gestion_2022), mean(mdm_gestion_2022, na.rm = TRUE), mdm_gestion_2022),
         rezago_ley_w_b1 = pmin(rezago_ley_b1, quantile(rezago_ley_b1, .99, na.rm = TRUE)),
         sin_giro_b1 = as.integer(rezago_b1 >= 0.999))
stopifnot(nrow(d) == 1037, !anyNA(d[, c("rezago_b1", "rezago_b2", "de_2022", "de_2024", "log_pob_2022", "log_aprop_pc_b1")]))

CTRL <- "log_pob_2022 + mdm_gestion_2022 + log_aprop_pc_b1 + productor_crudo + pdet"
FE   <- "factor(departamento) + categoria + ref_2022 + ref_2024"
fm <- function(y, x, ctrl = TRUE, fe = TRUE, extra = NULL)
  as.formula(paste(y, "~", paste(c(x, extra, if (ctrl) CTRL, if (fe) FE), collapse = " + ")))

# ---------------------------------------------------------------------
# 2. Descriptivos
# ---------------------------------------------------------------------
vars_desc <- c("de_2022", "de_2023", "de_2024", "d_de_24_22", "rezago_b1", "rezago_b2", "d_rezago", "rezago_ley_b1",
               "ajuste_recaudo_b1", "aprop_pc_b1", "poblacion_2022", "mdm_gestion_2022", "productor_crudo", "pdet")
resumen <- function(x) c(n = sum(!is.na(x)), media = mean(x, na.rm = TRUE), de = sd(x, na.rm = TRUE),
                         p10 = unname(quantile(x, .1, na.rm = TRUE)), mediana = median(x, na.rm = TRUE),
                         p90 = unname(quantile(x, .9, na.rm = TRUE)))
descr <- bind_rows(lapply(c(Nacional = "todos", Meta = "meta", Resto = "resto"), function(g) {
  s <- switch(g, todos = d, meta = filter(d, meta == 1), resto = filter(d, meta == 0))
  bind_rows(lapply(vars_desc, function(v) data.frame(grupo = g, variable = v, t(resumen(s[[v]])))))
}))
dif_meta <- bind_rows(lapply(c("rezago_b1", "rezago_b2", "de_2022", "de_2024", "d_de_24_22"), function(v) {
  t <- t.test(d[[v]][d$meta == 1], d[[v]][d$meta == 0])
  data.frame(variable = v, meta = t$estimate[1], resto = t$estimate[2], dif = diff(rev(t$estimate)), p = t$p.value)
}))

# ---------------------------------------------------------------------
# 3. H1, H2 y H3
# ---------------------------------------------------------------------
H1 <- bind_rows(
  estimar(fm("d_de_24_22", "rezago_b1", FALSE, FALSE), d, "rezago_b1", etiqueta = "H1 (1) sin controles"),
  estimar(fm("d_de_24_22", "rezago_b1", FALSE, TRUE, "de_2022"), d, "rezago_b1", etiqueta = "H1 (2) brecha inicial + EF"),
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, "de_2022"), d, "rezago_b1", etiqueta = "H1 (3) especificación preferida", wild = TRUE, B = B_WILD))
H2 <- bind_rows(
  estimar(fm("d_de_24_22", "d_rezago", FALSE, FALSE), d, "d_rezago", etiqueta = "H2 (1) sin controles"),
  estimar(fm("d_de_24_22", "d_rezago", FALSE, TRUE, "de_2022"), d, "d_rezago", etiqueta = "H2 (2) brecha inicial + EF"),
  estimar(fm("d_de_24_22", "d_rezago", TRUE, TRUE, "de_2022"), d, "d_rezago", etiqueta = "H2 (3) especificación preferida", wild = TRUE, B = B_WILD))
# H2 como panel de dos periodos con EF de municipio y de año × departamento (verificación de equivalencia)
pan2 <- bind_rows(d %>% transmute(cod_dane, departamento, t = 0, de = de_2022, rez = rezago_b1),
                  d %>% transmute(cod_dane, departamento, t = 1, de = de_2024, rez = rezago_b2)) %>%
  mutate(dep_t = paste(departamento, t))
H2_panel <- lm_robust(de ~ rez, data = pan2, fixed_effects = ~ cod_dane + dep_t, clusters = departamento, se_type = "CR2")
H2_fd    <- lm(d_de_24_22 ~ d_rezago + factor(departamento), data = d)        # debe coincidir con H2_panel
stopifnot(abs(coef(H2_panel)["rez"] - coef(H2_fd)["d_rezago"]) < 1e-8)
H3 <- bind_rows(
  estimar(fm("rezago_b1", "de_2022", FALSE, FALSE), d, "de_2022", etiqueta = "H3 bienio 2021-2022 (1) sin controles"),
  estimar(fm("rezago_b1", "de_2022", TRUE, TRUE), d, "de_2022", etiqueta = "H3 bienio 2021-2022 (2) preferida", wild = TRUE, B = B_WILD),
  estimar(fm("rezago_b2", "de_2022", FALSE, FALSE), d, "de_2022", etiqueta = "H3 bienio 2023-2024 (1) sin controles"),
  estimar(fm("rezago_b2", "de_2022", TRUE, TRUE), d, "de_2022", etiqueta = "H3 bienio 2023-2024 (2) preferida", wild = TRUE, B = B_WILD))
# Coeficientes completos de las especificaciones preferidas (para el apéndice)
pref <- list(H1 = lm(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, "de_2022"), d),
             H2 = lm(fm("d_de_24_22", "d_rezago", TRUE, TRUE, "de_2022"), d),
             H3_b1 = lm(fm("rezago_b1", "de_2022", TRUE, TRUE), d),
             H3_b2 = lm(fm("rezago_b2", "de_2022", TRUE, TRUE), d))
coefs_pref <- bind_rows(lapply(pref, function(m) {
  ct <- clubSandwich::coef_test(m, vcov = "CR2", cluster = d$departamento, test = "Satterthwaite")
  data.frame(termino = ct$Coef, coef = ct$beta, ee = ct$SE, p = ct$p_Satt)[!grepl("factor|categoria|ref_|Intercept", ct$Coef), ]
}), .id = "modelo")

# ---------------------------------------------------------------------
# 4. Heterogeneidad: Meta y productores
# ---------------------------------------------------------------------
het <- bind_rows(
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, c("de_2022", "rezago_b1:meta")), d, "rezago_b1:meta", etiqueta = "H1 × Meta"),
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, c("de_2022", "rezago_b1:productor_crudo")), d, "rezago_b1:productor_crudo", etiqueta = "H1 × productor"),
  estimar(fm("rezago_b1", "de_2022", TRUE, TRUE, "de_2022:meta"), d, "de_2022:meta", etiqueta = "H3 b1 × Meta"),
  estimar(fm("rezago_b2", "de_2022", TRUE, TRUE, "de_2022:meta"), d, "de_2022:meta", etiqueta = "H3 b2 × Meta"),
  estimar(fm("rezago_b1", "de_2022", TRUE, TRUE, "de_2022:productor_crudo"), d, "de_2022:productor_crudo", etiqueta = "H3 b1 × productor"))
# Diferencia de nivel del Meta en el cambio de la brecha (sin EF de departamento, que la absorbería)
meta_nivel <- estimar(update(fm("d_de_24_22", "meta", TRUE, FALSE, c("de_2022", "rezago_b1")), . ~ . + categoria + ref_2022 + ref_2024),
                      d, "meta", etiqueta = "Meta: cambio adicional de la brecha DE")

# Inferencia para el Meta (un solo conglomerado): (a) aleatorización entre departamentos (Conley y Taber, 2011):
# se estima la misma interacción para cada departamento con al menos 15 municipios y se ubica al Meta en esa
# distribución; (b) regresiones dentro del Meta (n = 27) con HC3 y permutación de Freedman-Lane.
deps <- names(which(table(d$departamento) >= 15))
ri_dep <- function(y, x, var_int) bind_rows(lapply(deps, function(k) {
  dd <- mutate(d, dk = as.integer(departamento == k))
  m <- lm(fm(y, x, TRUE, TRUE, paste0(x, ":dk")), dd)
  data.frame(departamento = k, coef = coef(m)[paste0(x, ":dk")])
})) %>% mutate(meta = departamento == "50")
ri <- list(H3_b1 = ri_dep("rezago_b1", "de_2022"), H3_b2 = ri_dep("rezago_b2", "de_2022"),
           H1 = ri_dep("d_de_24_22", "rezago_b1"))
ri_p <- bind_rows(lapply(ri, function(r) data.frame(n_dep = nrow(r), coef_meta = r$coef[r$meta],
                  rango_meta = rank(-abs(r$coef))[r$meta], p_ri = mean(abs(r$coef) >= abs(r$coef[r$meta])),
                  p_ri_una_cola = mean(sign(r$coef[r$meta]) * r$coef >= abs(r$coef[r$meta])))), .id = "modelo")
dm <- filter(d, meta == 1)
perm_fl <- function(f, data, var, B = 9999) {
  m <- lm(f, data); b0 <- coef(m)[var]; y <- all.vars(f)[1]
  m0 <- lm(update(f, paste(". ~ . -", var)), data); yh <- fitted(m0); e <- resid(m0)
  bs <- replicate(B, { dd <- data; dd[[y]] <- yh + sample(e); coef(lm(f, dd))[var] })
  ct <- coeftest(m, vcov = vcovHC(m, "HC3"))[var, ]
  data.frame(coef = b0, ee_hc3 = ct[2], p_hc3 = ct[4], p_perm = (1 + sum(abs(bs) >= abs(b0))) / (B + 1), n = nobs(m))
}
meta_sola <- bind_rows(
  "H3 b1 dentro del Meta" = perm_fl(rezago_b1 ~ de_2022 + log_pob_2022 + productor_crudo, dm, "de_2022"),
  "H3 b2 dentro del Meta" = perm_fl(rezago_b2 ~ de_2022 + log_pob_2022 + productor_crudo, dm, "de_2022"),
  "H1 dentro del Meta"    = perm_fl(d_de_24_22 ~ rezago_b1 + de_2022 + log_pob_2022 + productor_crudo, dm, "rezago_b1"),
  "H2 dentro del Meta"    = perm_fl(d_de_24_22 ~ d_rezago + de_2022 + log_pob_2022 + productor_crudo, dm, "d_rezago"), .id = "modelo")
ri_meta_nivel <- bind_rows(lapply(deps, function(k) {
  dd <- mutate(d, dk = as.integer(departamento == k))
  m <- lm(update(fm("d_de_24_22", "dk", TRUE, FALSE, c("de_2022", "rezago_b1")), . ~ . + categoria + ref_2022 + ref_2024), dd)
  data.frame(departamento = k, coef = coef(m)["dk"]) }))
ri_p <- bind_rows(ri_p, data.frame(modelo = "Nivel: cambio de brecha DE", n_dep = nrow(ri_meta_nivel),
  coef_meta = ri_meta_nivel$coef[ri_meta_nivel$departamento == "50"],
  rango_meta = rank(ri_meta_nivel$coef)[ri_meta_nivel$departamento == "50"],
  p_ri = mean(abs(ri_meta_nivel$coef) >= abs(ri_meta_nivel$coef[ri_meta_nivel$departamento == "50"])),
  p_ri_una_cola = mean(ri_meta_nivel$coef <= ri_meta_nivel$coef[ri_meta_nivel$departamento == "50"])))

# ---------------------------------------------------------------------
# 5. Placebo temporal
# ---------------------------------------------------------------------
placebo <- bind_rows(
  estimar(fm("d_de_23_22", "rezago_b2", TRUE, TRUE, "de_2022"), d, "rezago_b2", etiqueta = "Placebo: rezago 2024 → ΔDE 2022-2023", wild = TRUE, B = B_WILD),
  estimar(fm("d_de_23_22", "rezago_b1", TRUE, TRUE, "de_2022"), d, "rezago_b1", etiqueta = "Horizonte corto: rezago 2022 → ΔDE 2022-2023"))

# ---------------------------------------------------------------------
# 6. H1 sectorial: triple diferencia dentro del municipio
# ---------------------------------------------------------------------
sec <- leer("datos/base_sectores.csv") %>% filter(cod_dane %in% d$cod_dane, anio %in% c(2022, 2024)) %>%
  select(cod_dane, sector, anio, brecha) %>% pivot_wider(names_from = anio, values_from = brecha, names_prefix = "b_") %>%
  filter(!is.na(b_2022), !is.na(b_2024)) %>%
  group_by(cod_dane) %>% mutate(rango = rank(-b_2022, ties.method = "first"), n_sec = n(),
                                prior = as.integer(rango <= 3), bajo = as.integer(rango > n_sec - 3)) %>% ungroup() %>%
  left_join(d %>% select(cod_dane, departamento, rezago_b1, log_pob_2022, mdm_gestion_2022, log_aprop_pc_b1, productor_crudo, pdet), by = "cod_dane") %>%
  mutate(db = b_2024 - b_2022)
ddd <- function(p, etiqueta) {
  sec$P <- sec[[p]]
  f <- db ~ rezago_b1:P + P + factor(sector):b_2022 + factor(sector) +
    P:(log_pob_2022 + mdm_gestion_2022 + log_aprop_pc_b1 + productor_crudo + pdet)
  m_dep <- lm_robust(f, data = sec, fixed_effects = ~ cod_dane, clusters = departamento, se_type = "CR2")
  m_mun <- lm_robust(f, data = sec, fixed_effects = ~ cod_dane, clusters = cod_dane, se_type = "stata")
  k <- "rezago_b1:P"
  data.frame(modelo = etiqueta, termino = k, n = nobs(m_dep), coef = coef(m_dep)[k],
             ee_cr2 = m_dep$std.error[k], df = m_dep$df[k], p_cr2 = m_dep$p.value[k],
             ic_inf = m_dep$conf.low[k], ic_sup = m_dep$conf.high[k], ee_mun = m_mun$std.error[k], p_mun = m_mun$p.value[k],
             sd_y = sd(sec$db), sd_x = sd(d$rezago_b1), row.names = NULL)
}
sectorial <- bind_rows(ddd("prior", "H1 sectorial: rezago × sector prioritario (3 mayores brechas)"),
                       ddd("bajo", "Placebo sectorial: rezago × 3 menores brechas"))

# ---------------------------------------------------------------------
# 7. Equivalencia, efecto mínimo detectable y coeficientes estandarizados
# ---------------------------------------------------------------------
principales <- bind_rows(H1[3, ], H2[3, ], H3[c(2, 4), ])
equiv <- bind_rows(lapply(seq_len(nrow(principales)), function(i) tost(principales[i, ])),
                   tost(sectorial[1, ]))
principales_std <- estandarizar(principales)

# ---------------------------------------------------------------------
# 8. Malla de robustez (H1 y H3 bienio 1)
# ---------------------------------------------------------------------
sinMeta <- filter(d, meta == 0); sinProd <- filter(d, productor_crudo == 0); conGiro <- filter(d, sin_giro_b1 == 0)
rob <- bind_rows(
  H1[3, ],
  estimar(fm("d_de_24_22", "rezago_ail_b1", TRUE, TRUE, "de_2022"), d, "rezago_ail_b1", etiqueta = "H1: rezago solo AIL-NBI"),
  estimar(fm("d_de_24_22", "rezago_ley_w_b1", TRUE, TRUE, "de_2022"), d, "rezago_ley_w_b1", etiqueta = "H1: DI / asignación de ley (winsorizado p99)"),
  estimar(fm("d_de_24_22", "sin_giro_b1", TRUE, TRUE, "de_2022"), d, "sin_giro_b1", etiqueta = "H1: indicador de giro nulo"),
  estimar(fm("de_2024", "rezago_b1", TRUE, TRUE, "de_2022"), d, "rezago_b1", etiqueta = "H1: resultado en nivel 2024"),
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, "de_2022"), sinMeta, "rezago_b1", etiqueta = "H1: sin Meta"),
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, "de_2022"), sinProd, "rezago_b1", etiqueta = "H1: sin productores de crudo"),
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, "de_2022"), conGiro, "rezago_b1", etiqueta = "H1: excluye giro nulo"),
  mediana(fm("d_de_24_22", "rezago_b1", TRUE, FALSE, c("de_2022", "factor(departamento)")), d, "rezago_b1", "H1: regresión de mediana"),
  estimar(fm("d_de_24_22", "rezago_b1", TRUE, TRUE, "splines::ns(de_2022, df = 4)"), d, "rezago_b1", etiqueta = "H1: spline de la brecha inicial (gl = 4)"),
  bind_rows(lapply(strsplit(CTRL, " \\+ ")[[1]], function(cv)
    estimar(fm("d_de_24_22", "rezago_b1", FALSE, TRUE, c("de_2022", cv)), d, "rezago_b1", etiqueta = paste("H1: EF + brecha inicial +", cv)))),
  H3[2, ],
  estimar(fm("rezago_ail_b1", "de_2022", TRUE, TRUE), d, "de_2022", etiqueta = "H3: rezago solo AIL-NBI"),
  estimar(fm("sin_giro_b1", "de_2022", TRUE, TRUE), d, "de_2022", etiqueta = "H3: indicador de giro nulo (MLP)"),
  estimar(fm("rezago_b1", "de_2022", TRUE, TRUE), sinMeta, "de_2022", etiqueta = "H3: sin Meta"),
  estimar(fm("rezago_b1", "de_2022", TRUE, TRUE), sinProd, "de_2022", etiqueta = "H3: sin productores de crudo"),
  estimar(fm("rezago_b1", "de_2022", TRUE, TRUE), conGiro, "de_2022", etiqueta = "H3: excluye giro nulo"),
  mediana(fm("rezago_b1", "de_2022", TRUE, FALSE, "factor(departamento)"), d, "de_2022", "H3: regresión de mediana"))

# Diagnóstico de residuos de las especificaciones preferidas
diag <- bind_rows(lapply(pref[c("H1", "H3_b1")], function(m)
  data.frame(breusch_pagan_p = bptest(m)$p.value, reset_p = resettest(m, power = 2:3, type = "fitted")$p.value,
             vif_max = max(car::vif(lm(update(formula(m), . ~ . - factor(departamento) - categoria - ref_2022 - ref_2024), data = d))))),
  .id = "modelo")

# ---------------------------------------------------------------------
# 9. Guardar
# ---------------------------------------------------------------------
quitar <- function(x) { attr(x, "modelo") <- NULL; x }
res <- list(descr = descr, dif_meta = dif_meta, H1 = quitar(H1), H2 = quitar(H2), H3 = quitar(H3),
            H2_panel = data.frame(coef = coef(H2_panel)["rez"], ee = H2_panel$std.error["rez"], p = H2_panel$p.value["rez"]),
            coefs_pref = coefs_pref, het = quitar(het), meta_nivel = quitar(meta_nivel), placebo = quitar(placebo),
            sectorial = sectorial, equiv = equiv, ri = ri, ri_p = ri_p, meta_sola = meta_sola, principales_std = quitar(principales_std), rob = quitar(rob), diag = diag,
            n_sec = nrow(sec), d = d)
saveRDS(res, "resultados/resultados.rds")
for (n in setdiff(names(res), c("d", "n_sec", "ri"))) write.csv(res[[n]], file.path("resultados", paste0(n, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
op <- options(width = 200, digits = 3)
for (n in c("dif_meta", "H1", "H2", "H2_panel", "H3", "het", "meta_nivel", "ri_p", "meta_sola", "placebo", "sectorial", "equiv", "diag"))
  { cat("\n====", n, "\n"); x <- res[[n]]; print(x[, intersect(names(x), c("modelo","termino","n","coef","ee_cr2","p_cr2","p_wild","p_hc3","ic_inf","ic_sup","r2","p_mun","delta","p_tost","equivalente","mde","mde_std","variable","meta","resto","dif","p","ee","breusch_pagan_p","reset_p","vif_max","n_dep","coef_meta","rango_meta","p_ri","p_ri_una_cola","p_perm"))]) }
cat("\n==== robustez\n"); print(res$rob[, c("modelo", "coef", "ee_cr2", "p_cr2", "n")])
