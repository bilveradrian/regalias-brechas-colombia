# =====================================================================
# 02b_mapas.R — Mapas municipales: brecha DE 2022, cambio 2022-2024 y
# rezago en el giro al cierre de 2022. El Meta se resalta con borde grueso.
# Límites: paquete colmaps (Marco Geoestadístico Nacional, DANE).
# =====================================================================
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(sf); library(sp); library(scales); library(patchwork) })
sf_use_s2(FALSE)
load("datos_originales/geo/municipios.rda"); load("datos_originales/geo/departamentos.rda")
mun <- st_as_sf(municipios) %>% st_make_valid() %>% st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>% select(cod_dane = id)
dep <- st_as_sf(departamentos) %>% st_make_valid() %>% st_simplify(preserveTopology = TRUE, dTolerance = 0.01)
meta <- filter(dep, id == "50")

d <- read.csv("datos/base_transversal.csv", colClasses = c(cod_dane = "character"))
m <- mun %>% left_join(d %>% select(cod_dane, de_2022, d_de_24_22, rezago_b1, muestra), by = "cod_dane")
stopifnot(sum(!is.na(m$de_2022)) == 1102)

n0 <- label_number(decimal.mark = ",", style_negative = "minus", accuracy = 1)
n1 <- label_number(decimal.mark = ",", style_negative = "minus", accuracy = 0.1)
tema <- theme_void(base_size = 9, base_family = "serif") +
  theme(legend.position = "bottom", legend.title = element_text(size = 7.5, hjust = .5), legend.text = element_text(size = 7),
        legend.key.width = unit(.75, "cm"), legend.key.height = unit(.22, "cm"), plot.title = element_text(size = 8, hjust = .5))
capa <- function(var, titulo, escala, sin_dato = "grey85") {
  ggplot(m) + geom_sf(aes(fill = .data[[var]]), colour = NA) +
    geom_sf(data = dep, fill = NA, colour = "grey55", linewidth = .15) +
    geom_sf(data = meta, fill = NA, colour = "white", linewidth = 1.1) +
    geom_sf(data = meta, fill = NA, colour = "black", linewidth = .5) +
    escala + labs(title = titulo) + tema + coord_sf(datum = NA, xlim = c(-79.1, -66.8), ylim = c(-4.3, 12.6)) +
    guides(fill = guide_colourbar(title.position = "top"))
}
# Escala de grises (requisito de Cuadernos de Economía): rampas secuenciales de luminosidad; más oscuro = valor más alto,
# salvo en el panel B, donde más oscuro = mayor cierre de la brecha (valores negativos).
pA <- capa("de_2022", "A. Brecha DE, 2022",
           scale_fill_gradient(low = "grey97", high = "grey10", labels = n0, name = "Índice 0-100", na.value = "white"))
pB <- capa("d_de_24_22", "B. Cambio de la brecha DE, 2022-24",
           scale_fill_gradient(low = "grey10", high = "grey97", limits = c(-30, 30),
                                oob = squish, labels = n0, breaks = c(-30, -15, 0, 15, 30), name = "Puntos (negativo = cierre)", na.value = "white"))
m$rezago_b1[m$muestra != 1] <- NA
pC <- capa("rezago_b1", "C. Rezago en el giro, 2022",
           scale_fill_gradient(low = "grey97", high = "grey10", labels = n1, breaks = seq(0, 1, .2), limits = c(0, 1), name = "Proporción no girada",
                                na.value = "white"))
p <- pA + pB + pC + plot_layout(ncol = 3)
ggsave("figuras/figura1_mapas.png", p, width = 17, height = 8.5, units = "cm", dpi = 600)
ggsave("figuras/figura1_mapas.pdf", p, width = 17, height = 8.5, units = "cm", device = cairo_pdf)
message("Mapa generado")
