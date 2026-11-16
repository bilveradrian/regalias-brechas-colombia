# =====================================================================
# 01b_robustez_adicional.R — Forma funcional del regresor (montos per cápita, logaritmos,
# asinh, término cuadrático), controles adicionales de capacidad fiscal y desempeño (MDM 2022,
# densidad) y cota de Oster (2019) para variables omitidas. Insumo de la tabla A3.
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(readxl) })
source("scripts/funciones.R")
res <- readRDS("resultados/resultados.rds"); d <- res$d
# MDM 2022: subcomponentes y resultados
mdm <- read_excel("datos_originales/nacional/Controles/Medicion Desempe#U00f1o Municipal.xlsx", guess_max = 1e6) %>%
  filter(Año == 2022, Indicador %in% c("Movilización de recursos", "Ejecución de recursos", "Componente de resultados",
                                       "Gobierno abierto y transparencia", "Recaudo con instrumentos de ordenamiento territorial")) %>%
  transmute(cod_dane = sprintf("%05d", as.integer(`Código Entidad`)), ind = Indicador, v = as.numeric(gsub(",", ".", gsub("\\.", "", `Dato Numérico`)))) %>%
  tidyr::pivot_wider(names_from = ind, values_from = v)
names(mdm) <- c("cod_dane", "movil", "ejec", "result", "gobab", "pot")[match(names(mdm), c("cod_dane","Movilización de recursos","Ejecución de recursos","Componente de resultados","Gobierno abierto y transparencia","Recaudo con instrumentos de ordenamiento territorial"))]
ig <- read_excel("datos_originales/nacional/Controles/Indicadores Generales.xlsx", guess_max = 1e6) %>%
  filter(Indicador %in% c("Densidad poblacional", "Extensión"), Año %in% c(2022, 2024)) %>%
  transmute(cod_dane = sprintf("%05d", as.integer(`Código Entidad`)), ind = Indicador, v = as.numeric(gsub(",", ".", gsub("\\.", "", `Dato Numérico`)))) %>%
  group_by(cod_dane, ind) %>% summarise(v = first(v), .groups = "drop") %>% tidyr::pivot_wider(names_from = ind, values_from = v)
names(ig) <- c("cod_dane", "densidad", "extension")[match(names(ig), c("cod_dane", "Densidad poblacional", "Extensión"))]
d <- d %>% left_join(mdm, by = "cod_dane") %>% left_join(ig, by = "cod_dane") %>%
  mutate(pop = poblacion_2022,
         di_pc = di_b1 / pop / 1e3, log_di_pc = log1p(di_b1 / pop), giros_pc = giros_b1 / pop / 1e3, log_giros_pc = log1p(giros_b1 / pop),
         log_di = log1p(di_b1), log_densidad = log(densidad), rezago_sq = rezago_b1^2,
         asinh_di_pc = asinh(di_b1 / pop))
cat("faltantes movil/ejec/result/densidad:", colSums(is.na(d[, c("movil","ejec","result","densidad")])), "\n")
CT <- "de_2022 + log_pob_2022 + mdm_gestion_2022 + log_aprop_pc_b1 + productor_crudo + pdet + factor(departamento) + categoria + ref_2022 + ref_2024"
f <- function(x, extra = "") as.formula(paste("d_de_24_22 ~", x, "+", CT, extra))
filas <- list(
  estimar(f("rezago_b1"), d, "rezago_b1", etiqueta = "Base: rezago (proporción)"),
  estimar(f("di_pc"), d, "di_pc", etiqueta = "DI per cápita (miles $)"),
  estimar(f("log_di_pc"), d, "log_di_pc", etiqueta = "log(1 + DI per cápita)"),
  estimar(f("asinh_di_pc"), d, "asinh_di_pc", etiqueta = "asinh(DI per cápita)"),
  estimar(f("log_di"), d, "log_di", etiqueta = "log(1 + DI total)"),
  estimar(f("giros_pc"), d, "giros_pc", etiqueta = "Giros per cápita (miles $)"),
  estimar(f("log_giros_pc"), d, "log_giros_pc", etiqueta = "log(1 + giros per cápita)"),
  estimar(f("rezago_b1", "+ rezago_sq"), d, "rezago_b1", etiqueta = "Rezago + rezago² (término lineal)"),
  estimar(f("rezago_b1", "+ movil"), d, "rezago_b1", etiqueta = "+ movilización de recursos 2022"),
  estimar(f("rezago_b1", "+ ejec"), d, "rezago_b1", etiqueta = "+ ejecución de recursos 2022"),
  estimar(f("rezago_b1", "+ result"), d, "rezago_b1", etiqueta = "+ componente de resultados MDM 2022"),
  estimar(f("rezago_b1", "+ log_densidad"), d, "rezago_b1", etiqueta = "+ log densidad poblacional"),
  estimar(f("rezago_b1", "+ movil + ejec + result + gobab + log_densidad"), d, "rezago_b1", etiqueta = "+ todas las anteriores"))
out <- bind_rows(filas) %>% mutate(coef_std = coef * sd_x / sd_y) %>% select(modelo, n, coef, ee_cr2, p_cr2, ic_inf, ic_sup, coef_std, r2)
print(out, digits = 3)
# Estabilidad del coeficiente (Oster, 2019): delta tal que beta = 0, con Rmax = min(1, 1.3 R2)
m0 <- lm(d_de_24_22 ~ rezago_b1 + de_2022 + factor(departamento) + ref_2022 + ref_2024 + categoria, d)
m1 <- attr(filas[[1]], "modelo")
b0 <- coef(m0)["rezago_b1"]; r0 <- summary(m0)$r.squared; b1 <- coef(m1)["rezago_b1"]; r1 <- summary(m1)$r.squared
rmax <- min(1, 1.3 * r1); oster <- b1 - (b0 - b1) * (rmax - r1) / (r1 - r0)
write.csv(data.frame(b_corto = b0, r2_corto = r0, b_largo = b1, r2_largo = r1, rmax = rmax, beta_oster = oster),
          "resultados/oster.csv", row.names = FALSE)
cat(sprintf("Oster: b_corto=%.3f R2=%.3f; b_largo=%.3f R2=%.3f; Rmax=%.4f; beta* (delta=1)=%.3f\n",
  b0, r0, b1, r1, rmax, b1 - (b0 - b1) * (rmax - r1) / (r1 - r0)))
# Brecha en H3 con variables de capacidad fiscal: ¿cambia la conclusión?
h3 <- estimar(as.formula(paste("rezago_b1 ~ de_2022 + movil + ejec + log_densidad + log_pob_2022 + mdm_gestion_2022 + log_aprop_pc_b1 + productor_crudo + pdet + factor(departamento) + categoria + ref_2022 + ref_2024")), d, "de_2022", etiqueta = "H3 2022 + capacidad fiscal")
print(h3[, c("modelo","n","coef","ee_cr2","p_cr2")], digits = 3)
write.csv(out, "resultados/exploratorio_forma_omitidas.csv", row.names = FALSE)
