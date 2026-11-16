# =====================================================================
# 02_figuras.R — Figuras (PNG 600 dpi y PDF) en escala de grises (requisito de Cuadernos de
# Economía); los grupos se distinguen por forma y tipo de línea, no solo por tono.
# Coma decimal y signo menos tipográfico. Numeración según el orden de aparición en el texto.
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(scales) })
dir.create("figuras", showWarnings = FALSE)
res <- readRDS("resultados/resultados.rds"); d <- res$d
oi <- c(azul = "grey55", bermellon = "black", verde = "grey30", naranja = "grey70", gris = "grey60")
n0 <- label_number(decimal.mark = ",", big.mark = ".", style_negative = "minus", accuracy = 1)
n1 <- label_number(decimal.mark = ",", big.mark = ".", style_negative = "minus", accuracy = 0.1)
n2 <- label_number(decimal.mark = ",", big.mark = ".", style_negative = "minus", accuracy = 0.01)
tema <- theme_classic(base_size = 10, base_family = "serif") +
  theme(axis.text = element_text(colour = "black"), legend.position = "bottom", legend.title = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = .3), plot.margin = margin(6, 10, 6, 6))
guarda <- function(p, n, w = 16, h = 10) {
  ggsave(file.path("figuras", paste0(n, ".png")), p, width = w, height = h, units = "cm", dpi = 600)
  ggsave(file.path("figuras", paste0(n, ".pdf")), p, width = w, height = h, units = "cm", device = cairo_pdf)
}
d <- d %>% mutate(grupo = ifelse(meta == 1, "Meta", "Resto del país"))

# Figura 1. Distribución del rezago en el giro al cierre de cada bienio
f1 <- d %>% select(grupo, `Cierre 2021-2022` = rezago_b1, `Cierre 2023-2024` = rezago_b2) %>%
  pivot_longer(-grupo, names_to = "bienio", values_to = "rezago")
p1 <- ggplot(f1, aes(rezago, colour = grupo, linetype = grupo)) +
  stat_ecdf(linewidth = .6) + facet_wrap(~ bienio) +
  scale_colour_manual(values = unname(oi[c("bermellon", "azul")])) + scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(labels = n1, breaks = seq(0, 1, .2)) + scale_y_continuous(labels = n1) +
  labs(x = "Rezago en el giro (proporción de la apropiación no girada)", y = "Proporción acumulada de municipios") + tema
guarda(p1, "figuraA1_distribucion_rezago", 16, 8.5)

# Figura 2. Relación condicional entre rezago 2022 y cambio de la brecha DE (Frisch-Waugh-Lovell, 20 intervalos)
fw <- function(y, x) {
  ctrl <- "de_2022 + log_pob_2022 + mdm_gestion_2022 + log_aprop_pc_b1 + productor_crudo + pdet + factor(departamento) + categoria + ref_2022 + ref_2024"
  ry <- resid(lm(as.formula(paste(y, "~", ctrl)), d)) + mean(d[[y]]); rx <- resid(lm(as.formula(paste(x, "~", ctrl)), d)) + mean(d[[x]])
  tibble(rx, ry, grupo = d$grupo)
}
r2 <- fw("d_de_24_22", "rezago_b1")
bins <- r2 %>% mutate(bin = ntile(rx, 20)) %>% group_by(bin) %>% summarise(rx = mean(rx), ry = mean(ry))
p2 <- ggplot(r2, aes(rx, ry)) +
  geom_point(data = filter(r2, grupo == "Meta"), aes(shape = "Municipios del Meta", colour = "Municipios del Meta"), size = 1.6) +
  geom_point(data = bins, aes(shape = "Promedio por grupos de 5 % de los municipios", colour = "Promedio por grupos de 5 % de los municipios"), size = 2) +
  geom_smooth(data = r2, method = "lm", formula = y ~ x, se = TRUE, colour = "black", fill = "grey60", alpha = .25, linewidth = .5, linetype = "dashed") +
  scale_shape_manual(name = NULL, values = c("Municipios del Meta" = 17, "Promedio por grupos de 5 % de los municipios" = 16)) +
  scale_colour_manual(name = NULL, values = c("Municipios del Meta" = unname(oi["bermellon"]), "Promedio por grupos de 5 % de los municipios" = "grey45")) +
  scale_x_continuous(labels = n2, breaks = seq(0, 1, .25)) + scale_y_continuous(labels = n1) +
  labs(x = "Rezago al cierre de 2022 (residualizado)", y = "Cambio de la brecha DE\n2022-2024 (residualizado, puntos)") + tema
guarda(p2, "figura2_rezago_cambio_brecha")

# Figura 3. Coeficientes estandarizados con IC 95 % y banda de equivalencia (±0,10 DE)
ps <- res$principales_std %>% select(modelo, coef_std, ic_inf_std, ic_sup_std)
sec <- res$sectorial %>% transmute(modelo, coef_std = coef * sd_x / sd_y, ic_inf_std = ic_inf * sd_x / sd_y, ic_sup_std = ic_sup * sd_x / sd_y)
pl <- res$placebo[1, ] %>% transmute(modelo, coef_std = coef * sd_x / sd_y, ic_inf_std = ic_inf * sd_x / sd_y, ic_sup_std = ic_sup * sd_x / sd_y)
f3 <- bind_rows(ps, sec, pl) %>%
  mutate(etiqueta = c("H1: rezago 2022 → Δ brecha", "H2: Δ rezago → Δ brecha", "H3: brecha 2022 → rezago 2022",
                      "H3: brecha 2022 → rezago 2024", "H1 sectorial (triple diferencia)", "Falsación: sectores no prioritarios", "Falsación: anticipación"),
         tipo = ifelse(grepl("Falsación", etiqueta), "Prueba de falsación", "Hipótesis"),
         etiqueta = factor(etiqueta, levels = rev(etiqueta)))
p3 <- ggplot(f3, aes(coef_std, etiqueta)) +
  annotate("rect", xmin = -.1, xmax = .1, ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_vline(xintercept = 0, linewidth = .3) +
  geom_errorbarh(aes(xmin = ic_inf_std, xmax = ic_sup_std, linetype = tipo), height = .25, linewidth = .4) +
  geom_point(aes(shape = tipo, colour = tipo), size = 2) +
  scale_colour_manual(values = unname(oi[c("azul", "bermellon")])) + scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) + scale_x_continuous(labels = n2) +
  labs(x = "Coeficiente estandarizado\n(DE del resultado por DE del regresor)", y = NULL) + tema
guarda(p3, "figura4_coeficientes", 16, 9)

# Figura 4. Inferencia por aleatorización: interacción por departamento (H3, cierre 2022)
f4 <- res$ri$H3_b1 %>% mutate(grupo = ifelse(meta, "Meta", "Otros departamentos")) %>% arrange(coef) %>%
  mutate(orden = row_number())
p4 <- ggplot(f4, aes(orden, coef * 10)) +
  geom_hline(yintercept = 0, linewidth = .3) +
  geom_segment(aes(xend = orden, y = 0, yend = coef * 10, linetype = grupo), colour = "grey50") +
  geom_point(aes(colour = grupo, shape = grupo), size = 2.2) +
  scale_colour_manual(values = unname(oi[c("bermellon", "azul")])) + scale_shape_manual(values = c(17, 16)) +
  scale_linetype_manual(values = c("solid", "dotted")) + scale_y_continuous(labels = n2) +
  scale_x_continuous(breaks = NULL) +
  labs(x = "Departamentos con al menos 15 municipios (ordenados)", y = "Cambio del rezago por 10 puntos\nde brecha (interacción)") + tema
guarda(p4, "figura3_aleatorizacion_meta", 16, 9)
message("Figuras generadas")
