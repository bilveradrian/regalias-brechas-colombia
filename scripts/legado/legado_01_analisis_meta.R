# =====================================================================
# 01_analisis.R  — Estimaciones del artículo
# Entrada: datos/base_transversal.csv, datos/base_sectores.csv
# Salida:  resultados/*.csv y resultados/resultados.rds
# Diseño: corte transversal de 27 municipios beneficiarios de la AIL.
#   H1: rezago al cierre de 2022 -> cambio de brecha DE 2022-2024
#   H2: cambio del rezago entre bienios -> cambio de brecha 2022-2024
#       (equivale a efectos fijos de municipio y de periodo con T = 2)
#   Placebo: rezago al cierre de 2024 -> cambio de brecha 2022-2023
# Inferencia: HC3, permutación (10.000), wild bootstrap Rademacher
# (9.999), equivalencia TOST, potencia, jackknife, malla de robustez.
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sandwich); library(lmtest); library(quantreg)
})
set.seed(20261110)
dir.create("resultados", showWarnings = FALSE)

tv <- read.csv("datos/base_transversal.csv", fileEncoding = "UTF-8") %>%
  filter(beneficiario_ail == 1)
stopifnot(nrow(tv) == 27)
tv <- tv %>% mutate(rezago_ail_b1 = di_ail_b1 / ail_b1,   # sin componente ambiental
                    rezago_ail_b2 = di_ail_b2 / ail_b2,
                    log_at_b1c = log_at_b1 - mean(log_at_b1),
                    brecha22c = v_2022 - mean(v_2022))
sec <- read.csv("datos/base_sectores.csv", fileEncoding = "UTF-8")

B_PERM <- 10000; B_WILD <- 9999

# ---------- utilidades ----------
hc <- function(m, type = "HC3") coeftest(m, vcov = vcovHC(m, type = type))
perm_p <- function(f, data, var, B = B_PERM) {
  # Permutación de Freedman y Lane (1983): se permutan los residuos del
  # modelo restringido (sin `var`), lo que preserva la correlación entre
  # el regresor de interés y los controles. Sin controles equivale a
  # permutar la variable dependiente.
  b0 <- coef(lm(f, data))[var]
  y  <- all.vars(f)[1]
  m0 <- lm(update(f, paste(". ~ . -", var)), data); yh <- fitted(m0); e <- resid(m0)
  bs <- replicate(B, { d <- data; d[[y]] <- yh + sample(e); coef(lm(f, d))[var] })
  list(p = (1 + sum(abs(bs) >= abs(b0))) / (B + 1), dist = bs)
}
wild_p <- function(f, data, var, B = B_WILD) {
  # wild bootstrap con hipótesis nula impuesta (Rademacher), estadístico t HC3
  m  <- lm(f, data); t0 <- coef(m)[var] / sqrt(vcovHC(m, "HC3")[var, var])
  f0 <- update(f, paste(". ~ . -", var)); m0 <- lm(f0, data)
  yh <- fitted(m0); e <- resid(m0); y <- all.vars(f)[1]
  ts <- replicate(B, { d <- data; d[[y]] <- yh + e * sample(c(-1, 1), nrow(d), TRUE)
                       mb <- lm(f, d); coef(mb)[var] / sqrt(vcovHC(mb, "HC3")[var, var]) })
  (1 + sum(abs(ts) >= abs(t0))) / (B + 1)
}
tost <- function(est, se, df, delta, alpha = 0.05) {
  p1 <- pt((est + delta) / se, df, lower.tail = FALSE)   # H0: b <= -delta
  p2 <- pt((est - delta) / se, df, lower.tail = TRUE)    # H0: b >=  delta
  ci90 <- est + c(-1, 1) * qt(1 - alpha, df) * se
  data.frame(delta = delta, p_tost = max(p1, p2), ic90_inf = ci90[1], ic90_sup = ci90[2],
             equivalente = max(p1, p2) < alpha)
}
mde <- function(se, df, alpha = 0.05, power = 0.8) (qt(1 - alpha/2, df) + qt(power, df)) * se
fila <- function(m, var, etiqueta, data, f, perm = TRUE, wild = TRUE, type = "HC3") {
  ct <- hc(m, type); se <- ct[var, 2]; df <- m$df.residual
  data.frame(modelo = etiqueta, n = nobs(m), coef = ct[var, 1], ee = se,
             t = ct[var, 3], p_hc = ct[var, 4],
             ic95_inf = ct[var, 1] - qt(.975, df) * se, ic95_sup = ct[var, 1] + qt(.975, df) * se,
             p_perm = if (perm) perm_p(f, data, var)$p else NA,
             p_wild = if (wild) wild_p(f, data, var) else NA,
             r2 = summary(m)$r.squared, df = df)
}

# ---------- 1. Descriptivos ----------
vars_desc <- c("v_2022", "v_2023", "v_2024", "d_brecha_24_22", "rezago_b1", "rezago_b2",
               "d_rezago", "at_b1", "dit_b1", "at_b2", "dit_b2")
descr <- bind_rows(lapply(vars_desc, function(v) { x <- tv[[v]]
  data.frame(variable = v, n = sum(!is.na(x)), media = mean(x), de = sd(x), min = min(x),
             p25 = quantile(x, .25), mediana = median(x), p75 = quantile(x, .75), max = max(x)) }))
# Cambio medio de la brecha: t pareado y Wilcoxon
camb <- data.frame(
  tramo = c("2022-2023", "2023-2024", "2022-2024"),
  media = c(mean(tv$d_brecha_23_22), mean(tv$d_brecha_24_23), mean(tv$d_brecha_24_22)),
  p_t   = c(t.test(tv$d_brecha_23_22)$p.value, t.test(tv$d_brecha_24_23)$p.value, t.test(tv$d_brecha_24_22)$p.value),
  p_wilcoxon = c(wilcox.test(tv$d_brecha_23_22, exact = FALSE)$p.value, wilcox.test(tv$d_brecha_24_23, exact = FALSE)$p.value,
                 wilcox.test(tv$d_brecha_24_22, exact = FALSE)$p.value),
  n_reducen = c(sum(tv$d_brecha_23_22 < 0), sum(tv$d_brecha_24_23 < 0), sum(tv$d_brecha_24_22 < 0)))
# Comparación con los demás sectores (mismos 27 municipios)
sec_camb <- sec %>% filter(cod_dane %in% tv$cod_dane) %>%
  pivot_wider(names_from = anio, values_from = brecha, names_prefix = "v_") %>%
  group_by(sector) %>%
  summarise(media_2022 = mean(v_2022), media_2024 = mean(v_2024),
            cambio_medio = mean(v_2024 - v_2022), de_2022 = sd(v_2022), de_2024 = sd(v_2024),
            .groups = "drop") %>% arrange(cambio_medio)
pruebas_dist <- data.frame(
  serie = c("v_2022", "v_2023", "v_2024", "d_brecha_24_22", "rezago_b1", "rezago_b2"),
  shapiro_W = sapply(c("v_2022","v_2023","v_2024","d_brecha_24_22","rezago_b1","rezago_b2"), function(v) shapiro.test(tv[[v]])$statistic),
  shapiro_p = sapply(c("v_2022","v_2023","v_2024","d_brecha_24_22","rezago_b1","rezago_b2"), function(v) shapiro.test(tv[[v]])$p.value))
corr_rez <- cor.test(tv$rezago_b1, tv$rezago_b2, method = "spearman", exact = FALSE)

# ---------- 2. H1: rezago 2022 -> cambio brecha 2022-2024 ----------
f1a <- d_brecha_24_22 ~ rezago_b1
f1b <- d_brecha_24_22 ~ rezago_b1 + brecha22c
f1c <- d_brecha_24_22 ~ rezago_b1 + brecha22c + log_at_b1c
m1a <- lm(f1a, tv); m1b <- lm(f1b, tv); m1c <- lm(f1c, tv)
H1 <- bind_rows(fila(m1a, "rezago_b1", "H1 (1) sin controles", tv, f1a),
                fila(m1b, "rezago_b1", "H1 (2) + brecha inicial", tv, f1b),
                fila(m1c, "rezago_b1", "H1 (3) + brecha inicial + log asignación", tv, f1c))
coef_ctrl <- bind_rows(lapply(list(`(1)` = m1a, `(2)` = m1b, `(3)` = m1c), function(m) {
  ct <- hc(m); data.frame(termino = rownames(ct), coef = ct[, 1], ee = ct[, 2], p = ct[, 4]) }), .id = "columna")
perm_dist_H1 <- perm_p(f1b, tv, "rezago_b1")$dist

# ---------- 3. H2: cambio del rezago -> cambio de brecha (EF con T = 2) ----------
f2a <- d_brecha_24_22 ~ d_rezago
f2b <- d_brecha_24_22 ~ d_rezago + brecha22c
m2a <- lm(f2a, tv); m2b <- lm(f2b, tv)
# Verificación: panel de dos periodos con efectos fijos de municipio y año
pan <- bind_rows(tv %>% transmute(cod_dane, t = 1, brecha = v_2022, rezago = rezago_b1),
                 tv %>% transmute(cod_dane, t = 2, brecha = v_2024, rezago = rezago_b2))
m2_fe <- lm(brecha ~ rezago + factor(cod_dane) + factor(t), pan)
stopifnot(abs(coef(m2_fe)["rezago"] - coef(m2a)["d_rezago"]) < 1e-8)
vc_cl <- vcovCL(m2_fe, cluster = ~cod_dane, type = "HC1")
H2 <- bind_rows(fila(m2a, "d_rezago", "H2 (1) diferencias", tv, f2a),
                fila(m2b, "d_rezago", "H2 (2) diferencias + brecha inicial", tv, f2b))
H2_fe <- data.frame(modelo = "H2 EF municipio y año, EE agrupados", coef = coef(m2_fe)["rezago"],
                    ee = sqrt(vc_cl["rezago", "rezago"]))

# ---------- 4. Equivalencia (TOST) y potencia ----------
# Efecto mínimo de interés (SESOI): 3 puntos del índice por una desviación
# estándar del rezago (≈ 1/4 de la DE de la brecha en 2022). Se expresa por
# unidad de rezago dividiendo por su DE. Se reporta también ±2 y ±5 puntos.
sd_r1 <- sd(tv$rezago_b1); sd_dr <- sd(tv$d_rezago)
eq <- bind_rows(lapply(c(2, 3, 5), function(pts) bind_rows(
  cbind(hipotesis = "H1 (2)", puntos_por_DE = pts,
        tost(coef(m1b)["rezago_b1"], hc(m1b)["rezago_b1", 2], m1b$df.residual, pts / sd_r1)),
  cbind(hipotesis = "H2 (2)", puntos_por_DE = pts,
        tost(coef(m2b)["d_rezago"], hc(m2b)["d_rezago", 2], m2b$df.residual, pts / sd_dr)))))
pot <- data.frame(hipotesis = c("H1 (2)", "H2 (2)"),
                  ee = c(hc(m1b)["rezago_b1", 2], hc(m2b)["d_rezago", 2]),
                  de_regresor = c(sd_r1, sd_dr)) %>%
  mutate(mde_por_unidad = mde(ee, 24), mde_puntos_por_DE = mde_por_unidad * de_regresor)

# ---------- 5. Placebo temporal ----------
fp1 <- d_brecha_23_22 ~ rezago_b2 + brecha22c      # el futuro no explica el pasado
fp2 <- d_brecha_23_22 ~ rezago_b1 + brecha22c      # horizonte de un año
mp1 <- lm(fp1, tv); mp2 <- lm(fp2, tv)
placebo <- bind_rows(fila(mp1, "rezago_b2", "Placebo: rezago 2024 -> cambio 2022-2023", tv, fp1),
                     fila(mp2, "rezago_b1", "Horizonte 1 año: rezago 2022 -> cambio 2022-2023", tv, fp2))

# ---------- 6. Malla de robustez ----------
grid <- list(
  list("Base: rezago total, Δ2022-2024", d_brecha_24_22 ~ rezago_b1 + brecha22c, "rezago_b1", "HC3"),
  list("EE HC1", d_brecha_24_22 ~ rezago_b1 + brecha22c, "rezago_b1", "HC1"),
  list("Rezago sin componente ambiental", d_brecha_24_22 ~ rezago_ail_b1 + brecha22c, "rezago_ail_b1", "HC3"),
  list("Indicador rezago > 1", d_brecha_24_22 ~ rezago_mayor1_b1 + brecha22c, "rezago_mayor1_b1", "HC3"),
  list("log DI con log asignación", d_brecha_24_22 ~ log_dit_b1 + log_at_b1 + brecha22c, "log_dit_b1", "HC3"),
  list("Dependiente: Δ2023-2024", d_brecha_24_23 ~ rezago_b1 + I(v_2023 - mean(v_2023)), "rezago_b1", "HC3"),
  list("Dependiente: nivel 2024 | nivel 2022", v_2024 ~ rezago_b1 + v_2022, "rezago_b1", "HC3"),
  list("Sin Uribe (atípico en 2022)", d_brecha_24_22 ~ rezago_b1 + brecha22c, "rezago_b1", "HC3", tv$cod_dane != 50370),
  list("Sin Puerto Gaitán (mayor asignación)", d_brecha_24_22 ~ rezago_b1 + brecha22c, "rezago_b1", "HC3", tv$cod_dane != 50568)
)
rob <- bind_rows(lapply(grid, function(g) {
  d <- if (length(g) >= 5) tv[g[[5]], ] else tv
  m <- lm(g[[2]], d)
  fila(m, g[[3]], g[[1]], d, g[[2]], perm = TRUE, wild = FALSE, type = g[[4]])
}))
qr <- summary(rq(d_brecha_24_22 ~ rezago_b1 + brecha22c, tau = .5, data = tv), se = "boot", R = 2000)
rob <- bind_rows(rob, data.frame(modelo = "Regresión mediana (EE bootstrap)", n = 27,
  coef = qr$coefficients["rezago_b1", 1], ee = qr$coefficients["rezago_b1", 2],
  t = qr$coefficients["rezago_b1", 3], p_hc = qr$coefficients["rezago_b1", 4],
  ic95_inf = qr$coefficients["rezago_b1", 1] - qt(.975, 24) * qr$coefficients["rezago_b1", 2],
  ic95_sup = qr$coefficients["rezago_b1", 1] + qt(.975, 24) * qr$coefficients["rezago_b1", 2],
  p_perm = NA, p_wild = NA, r2 = NA, df = 24))
spear <- cor.test(tv$rezago_b1, tv$d_brecha_24_22, method = "spearman", exact = FALSE)

# Jackknife (excluir un municipio a la vez)
jk <- bind_rows(lapply(seq_len(nrow(tv)), function(i) {
  m <- lm(f1b, tv[-i, ]); ct <- hc(m)
  data.frame(excluido = tv$municipio[i], coef = ct["rezago_b1", 1], ee = ct["rezago_b1", 2], p = ct["rezago_b1", 4]) }))
cook <- data.frame(municipio = tv$municipio, cook = cooks.distance(m1b), hat = hatvalues(m1b))

# ---------- 7. Heterogeneidad sectorial (Holm) ----------
sw <- sec %>% filter(cod_dane %in% tv$cod_dane) %>%
  pivot_wider(names_from = anio, values_from = brecha, names_prefix = "v_") %>%
  left_join(tv %>% select(cod_dane, rezago_b1), by = "cod_dane") %>%
  group_by(sector) %>% mutate(d = v_2024 - v_2022, b0 = v_2022 - mean(v_2022)) %>% ungroup()
sect_res <- bind_rows(lapply(split(sw, sw$sector), function(d) {
  m <- if (sd(d$v_2022) > 0) lm(d ~ rezago_b1 + b0, d) else lm(d ~ rezago_b1, d)
  ct <- hc(m); data.frame(sector = d$sector[1], coef = ct["rezago_b1", 1], ee = ct["rezago_b1", 2],
                          p_hc = ct["rezago_b1", 4], control_inicial = sd(d$v_2022) > 0) })) %>%
  mutate(p_holm = p.adjust(p_hc, "holm")) %>% arrange(p_hc)

# ---------- 8. Convergencia (regresión a la media) ----------
mconv <- lm(d_brecha_24_22 ~ v_2022, tv)
conv <- data.frame(coef = coef(mconv)[2], ee = hc(mconv)[2, 2], p = hc(mconv)[2, 4],
                   r2 = summary(mconv)$r.squared)

# ---------- 9. Diagnóstico de residuos (modelo base) ----------
diag <- data.frame(
  prueba = c("Breusch-Pagan", "Shapiro-Wilk residuos", "RESET (potencias 2-3)"),
  estadistico = c(bptest(m1b)$statistic, shapiro.test(resid(m1b))$statistic, resettest(m1b)$statistic),
  p = c(bptest(m1b)$p.value, shapiro.test(resid(m1b))$p.value, resettest(m1b)$p.value))

# ---------- Guardar ----------
res <- list(descr = descr, camb = camb, sec_camb = sec_camb, pruebas_dist = pruebas_dist,
            corr_rez = c(rho = unname(corr_rez$estimate), p = corr_rez$p.value),
            H1 = H1, coef_ctrl = coef_ctrl, H2 = H2, H2_fe = H2_fe, eq = eq, pot = pot,
            placebo = placebo, rob = rob, spear = c(rho = unname(spear$estimate), p = spear$p.value),
            jk = jk, cook = cook, sect_res = sect_res, conv = conv, diag = diag,
            perm_dist_H1 = perm_dist_H1, sd_r1 = sd_r1, sd_dr = sd_dr, tv = tv)
saveRDS(res, "resultados/resultados.rds")
for (n in c("descr","camb","sec_camb","pruebas_dist","H1","coef_ctrl","H2","H2_fe","eq","pot","placebo","rob","jk","cook","sect_res","conv","diag"))
  write.csv(res[[n]], file.path("resultados", paste0(n, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")

op <- options(digits = 3, width = 160)
print(camb); print(H1[, 1:12]); print(H2[, 1:12]); print(H2_fe); print(eq); print(pot)
print(placebo[, 1:10]); print(rob[, c(1, 3, 4, 6, 9)]); print(res$spear); print(range(jk$coef))
print(sect_res); print(conv); print(diag); print(sec_camb); print(res$corr_rez)
