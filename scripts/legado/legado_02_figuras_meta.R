# =====================================================================
# 02_figuras.R — Figuras del artículo (PNG 600 dpi y PDF)
# Paleta Okabe-Ito (apta para daltonismo), codificación secundaria por
# forma/tipo de línea, coma decimal y signo menos tipográfico.
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(ggrepel); library(scales) })
dir.create("figuras", showWarnings = FALSE)
res <- readRDS("resultados/resultados.rds"); tv <- res$tv

oi <- c(negro = "#000000", naranja = "#E69F00", celeste = "#56B4E9", verde = "#009E73",
        azul = "#0072B2", bermellon = "#D55E00", purpura = "#CC79A7", gris = "#999999")
num <- label_number(decimal.mark = ",", big.mark = ".", style_negative = "minus", accuracy = 1)
num1 <- label_number(decimal.mark = ",", big.mark = ".", style_negative = "minus", accuracy = 0.1)
tema <- theme_classic(base_size = 10, base_family = "serif") +
  theme(axis.text = element_text(colour = "black"), legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
        plot.margin = margin(6, 10, 6, 6))
guarda <- function(p, nombre, w = 16, h = 10) {
  ggsave(file.path("figuras", paste0(nombre, ".png")), p, width = w, height = h, units = "cm", dpi = 600)
  ggsave(file.path("figuras", paste0(nombre, ".pdf")), p, width = w, height = h, units = "cm", device = cairo_pdf)
}

# Figura 1. Distribución de la brecha DE por año
d1 <- bind_rows(lapply(c(2022, 2023, 2024), function(a)
  data.frame(anio = factor(a), brecha = tv[[paste0("v_", a)]], cod = tv$cod_dane)))
med <- d1 %>% group_by(anio) %>% summarise(m = median(brecha), mu = mean(brecha))
p1 <- ggplot(d1, aes(anio, brecha)) +
  geom_line(aes(group = cod), colour = oi["gris"], linewidth = 0.25, alpha = 0.6) +
  geom_point(aes(shape = anio, colour = anio), size = 1.8,
             position = position_jitter(width = 0.06, seed = 1)) +
  geom_crossbar(data = med, aes(y = m, ymin = m, ymax = m), width = 0.4, colour = "black", linewidth = 0.4) +
  scale_colour_manual(values = unname(oi[c("azul", "naranja", "verde")]), guide = "none") +
  scale_shape_manual(values = c(16, 17, 15), guide = "none") +
  scale_y_continuous(labels = num, limits = c(0, 80), breaks = seq(0, 80, 20)) +
  labs(x = NULL, y = "Brecha de desarrollo económico (índice 0-100)") + tema
guarda(p1, "figura1_brecha_por_anio", 14, 9)

# Figura 2. Rezago al cierre de 2022 y cambio de la brecha 2022-2024
m <- lm(d_brecha_24_22 ~ rezago_b1, tv)
grid <- data.frame(rezago_b1 = seq(min(tv$rezago_b1), max(tv$rezago_b1), length.out = 100))
pr <- predict(m, grid, se.fit = TRUE)
grid$fit <- pr$fit; grid$lo <- pr$fit - qt(.975, m$df.residual) * pr$se.fit
grid$hi <- pr$fit + qt(.975, m$df.residual) * pr$se.fit
etiq <- tv %>% filter(rank(rezago_b1) <= 2 | rank(-rezago_b1) <= 2 |
                      rank(d_brecha_24_22) <= 2 | rank(-d_brecha_24_22) <= 2) %>%
  mutate(nom = gsub("\\bDe\\b", "de", tools::toTitleCase(tolower(municipio))))
p2 <- ggplot(tv, aes(rezago_b1, d_brecha_24_22)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = 1, colour = "grey40", linewidth = 0.3, linetype = "dotted") +
  geom_ribbon(data = grid, aes(x = rezago_b1, ymin = lo, ymax = hi), inherit.aes = FALSE,
              fill = oi["celeste"], alpha = 0.25) +
  geom_line(data = grid, aes(rezago_b1, fit), colour = oi["azul"], linetype = "dashed", linewidth = 0.6) +
  geom_point(colour = oi["bermellon"], shape = 16, size = 1.8) +
  geom_text_repel(data = etiq, aes(label = nom), size = 2.6, family = "serif", seed = 2, min.segment.length = 0) +
  scale_x_continuous(labels = num1) + scale_y_continuous(labels = num) +
  labs(x = "Rezago al cierre de 2022 (disponibilidad inicial / asignación)",
       y = "Cambio de la brecha 2022-2024 (puntos)") + tema
guarda(p2, "figura2_rezago_cambio_brecha")

# Figura 3. Malla de robustez: coeficiente y IC 95 %
rb <- bind_rows(res$H1 %>% mutate(grupo = "H1"), res$rob[-1, ] %>% mutate(grupo = "Robustez")) %>%
  filter(!grepl("Indicador|log DI", modelo))   # escalas no comparables; se reportan en la tabla
rb$modelo <- factor(rb$modelo, levels = rev(rb$modelo))
delta <- res$eq$delta[res$eq$hipotesis == "H1 (2)" & res$eq$puntos_por_DE == 3]
p3 <- ggplot(rb, aes(coef, modelo)) +
  annotate("rect", xmin = -delta, xmax = delta, ymin = -Inf, ymax = Inf, fill = "grey88") +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = ic95_inf, xmax = ic95_sup, linetype = grupo), height = 0.25, linewidth = 0.4) +
  geom_point(aes(shape = grupo, colour = grupo), size = 2) +
  scale_colour_manual(values = unname(oi[c("azul", "bermellon")]), name = NULL) +
  scale_shape_manual(values = c(16, 17), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_x_continuous(labels = num) +
  labs(x = "Coeficiente del rezago\n(puntos de brecha por unidad de rezago)", y = NULL) + tema
guarda(p3, "figura3_robustez", 16, 11)

# Figura 4. Cambio medio de la brecha 2022-2024 por sector
sc <- res$sec_camb %>% mutate(sector = paste0(toupper(substr(sector, 1, 1)), substring(sector, 2)),
                              sector = sub("Tecnologías de la información y las comunicaciones", "TIC", sector),
                              de = ifelse(grepl("Desarrollo econ", sector), "Desarrollo económico", "Otros sectores"))
sc$sector <- factor(sc$sector, levels = sc$sector[order(sc$cambio_medio)])
p4 <- ggplot(sc, aes(cambio_medio, sector)) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_segment(aes(x = 0, xend = cambio_medio, yend = sector, linetype = de), colour = "grey50") +
  geom_point(aes(colour = de, shape = de), size = 2.2) +
  scale_colour_manual(values = unname(oi[c("bermellon", "azul")]), name = NULL) +
  scale_shape_manual(values = c(17, 16), name = NULL) +
  scale_linetype_manual(values = c("solid", "dotted"), name = NULL) +
  scale_x_continuous(labels = num) +
  labs(x = "Cambio medio de la brecha\n2022-2024 (puntos)", y = NULL) + tema
guarda(p4, "figura4_cambio_sectores", 16, 10)
message("Figuras generadas")
