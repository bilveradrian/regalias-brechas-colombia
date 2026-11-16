# =====================================================================
# funciones.R — utilidades de estimación e inferencia
# =====================================================================
suppressPackageStartupMessages({ library(dplyr); library(sandwich); library(lmtest); library(clubSandwich) })

#' Ajusta un modelo lineal y devuelve el coeficiente de `var` con varias inferencias.
#' @param f fórmula (efectos fijos como factor())
#' @param d datos; @param var nombre del regresor de interés; @param cl variable de conglomerado
estimar <- function(f, d, var, cl = "departamento", etiqueta = var, wild = FALSE, B = 9999, semilla = 20261110) {
  d <- d[complete.cases(d[, all.vars(f)]), , drop = FALSE]
  m <- lm(f, data = d)
  if (!var %in% names(coef(m)) || is.na(coef(m)[var])) stop("Coeficiente no estimable: ", var)
  cr2 <- coef_test(m, vcov = "CR2", cluster = d[[cl]], test = "Satterthwaite", coefs = var)
  hc3 <- coeftest(m, vcov = vcovHC(m, type = "HC3"))[var, ]
  df_s <- cr2$df_Satt
  out <- data.frame(modelo = etiqueta, termino = var, n = nobs(m), n_cl = length(unique(d[[cl]])),
                    coef = unname(coef(m)[var]), ee_cr2 = cr2$SE, df = df_s, p_cr2 = cr2$p_Satt,
                    ic_inf = coef(m)[var] - qt(.975, df_s) * cr2$SE, ic_sup = coef(m)[var] + qt(.975, df_s) * cr2$SE,
                    ee_hc3 = hc3[2], p_hc3 = hc3[4], r2 = summary(m)$r.squared,
                    sd_y = sd(model.response(model.frame(m))), sd_x = sd(d[[var]]), row.names = NULL)
  out$p_wild <- if (wild) wild_cluster(f, d, var, cl, B, semilla) else NA_real_
  attr(out, "modelo") <- m
  out
}

#' Wild cluster bootstrap-t con nula impuesta (pesos de Webb de seis puntos).
wild_cluster <- function(f, d, var, cl, B = 9999, semilla = 20261110) {
  set.seed(semilla)
  m <- lm(f, data = d)
  X <- model.matrix(m); X <- X[, !is.na(coef(m)), drop = FALSE]      # sin columnas colineales
  y <- model.response(model.frame(m))
  j <- which(colnames(X) == var); X0 <- X[, -j, drop = FALSE]
  g <- as.integer(factor(d[[cl]])); G <- max(g)
  XtXi <- solve(crossprod(X)); H <- XtXi %*% t(X)                     # se calcula una sola vez
  ajuste <- (G / (G - 1)) * ((nrow(X) - 1) / (nrow(X) - ncol(X)))
  tstat <- function(yy) {
    b <- H %*% yy; u <- as.vector(yy - X %*% b)
    a <- rowsum(t(H[j, , drop = FALSE]) * u, g)                         # contribución por conglomerado
    b[j] / sqrt(sum(a^2) * ajuste)
  }
  t0 <- tstat(y)
  yh <- as.vector(X0 %*% solve(crossprod(X0), crossprod(X0, y))); e <- y - yh
  w6 <- c(-sqrt(1.5), -1, -sqrt(.5), sqrt(.5), 1, sqrt(1.5))
  ts <- replicate(B, tstat(yh + e * sample(w6, G, replace = TRUE)[g]))
  (1 + sum(abs(ts) >= abs(t0))) / (B + 1)
}

#' Prueba de equivalencia TOST con umbral expresado en DE estandarizadas.
tost <- function(fila, umbral_std = 0.10, alpha = 0.05) {
  delta <- umbral_std * fila$sd_y / fila$sd_x
  t1 <- (fila$coef + delta) / fila$ee_cr2; t2 <- (fila$coef - delta) / fila$ee_cr2
  p <- max(pt(t1, fila$df, lower.tail = FALSE), pt(t2, fila$df))
  ic90 <- fila$coef + c(-1, 1) * qt(1 - alpha, fila$df) * fila$ee_cr2
  mde <- (qt(.975, fila$df) + qt(.80, fila$df)) * fila$ee_cr2
  data.frame(modelo = fila$modelo, termino = fila$termino, delta = delta, ic90_inf = ic90[1], ic90_sup = ic90[2],
             p_tost = p, equivalente = p < alpha, mde = mde, mde_std = mde * fila$sd_x / fila$sd_y)
}

#' Coeficiente estandarizado (DE del resultado por DE del regresor).
estandarizar <- function(fila) transform(fila, coef_std = coef * sd_x / sd_y, ic_inf_std = ic_inf * sd_x / sd_y, ic_sup_std = ic_sup * sd_x / sd_y)

#' Regresión de mediana con errores bootstrap (para la malla de robustez).
mediana <- function(f, d, var, etiqueta) {
  d <- d[complete.cases(d[, all.vars(f)]), , drop = FALSE]
  s <- summary(quantreg::rq(f, tau = .5, data = d), se = "boot", R = 999)$coefficients
  data.frame(modelo = etiqueta, termino = var, n = nrow(d), coef = s[var, 1], ee_cr2 = s[var, 2], df = nrow(d) - nrow(s),
             p_cr2 = s[var, 4], ic_inf = s[var, 1] - 1.96 * s[var, 2], ic_sup = s[var, 1] + 1.96 * s[var, 2])
}
