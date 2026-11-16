# Especificación econométrica (fijada antes de estimar; 10-nov-2026)

## Unidad, muestra y variables
- Unidad: municipio beneficiario de la AIL en ambos bienios con datos completos (n = 1.038; 27 del Meta).
- Tratamiento: rezago en el giro R_b = disponibilidad inicial / apropiación definitiva al cierre del bienio b
  (b1: 31-dic-2022; b2: 31-dic-2024). Rango 0-1.
- Resultado principal: brecha de Desarrollo Económico (DE) de la MCB (0-100; mayor = más brecha).
- Advertencia de medición documentada antes de estimar: la brecha DE 2022 usa valor agregado 2017-2020,
  ingresos 2018-2021 y densidad empresarial 2021; la de 2024 usa valor agregado 2018-2021, ingresos 2023 y
  densidad 2021, y cambia el grupo de referencia (G1-G5 → T1-T5). El canal de corto plazo AIL → brecha DE solo
  puede operar por ingresos 2023 y valor agregado 2021.

## Controles (predeterminados, 2022) y efectos fijos
X: log población 2022, MDM-gestión 2022 (capacidad administrativa), log apropiación per cápita del bienio 1,
productor de crudo (ANH), PDET. Efectos fijos: departamento; categoría Ley 617 de 2022; celda grupo de
referencia 2022 × tipología 2024 (absorbe el cambio de referente del DNP).

## Ecuaciones
[1] H1  ΔDE_m(2022→2024) = β1·R1_m + θ·DE_m,2022 + X_m'γ + μ_d + κ_c + ε_m          (H1: β1 > 0)
[2] H2  ΔDE_m(2022→2024) = β2·ΔR_m + θ·DE_m,2022 + X_m'γ + μ_d + κ_c + ε_m          (H2: β2 > 0)
    Equivale a un panel de dos periodos con efectos fijos de municipio y de año por departamento/celda.
[3] H3  R_m,b = π_b·DE_m,2022 + X_m'γ + μ_d + κ_c + ε_m,  b = 1, 2                  (H3: π_b > 0)
    Hipótesis de capacidad de absorción, fijada antes de estimar tras documentar la temporalidad del indicador.
[4] H1 sectorial (triple diferencia dentro del municipio):
    Δb_ms = β4·(R1_m × P_ms) + α_m + λ_s + θ_s·b_ms,2022 + (P_ms × X_m)'φ + ε_ms
    P_ms = 1 si el sector s está entre los tres de mayor brecha del municipio en 2022 (los que la Resolución
    DNP 2993/2021 obliga a priorizar). α_m absorbe todo choque municipal; λ_s, cambios de medición por sector.
[5] Heterogeneidad: [1] y [3] con interacciones R1×Meta, R1×Productor y DE2022×Meta.

## Inferencia
Errores CR2 agrupados por departamento con grados de libertad de Satterthwaite (pocos conglomerados);
wild cluster bootstrap-t (pesos de Webb, nula impuesta, 9.999 réplicas) para los coeficientes principales;
HC3 como contraste. En [4], errores agrupados por municipio y por departamento.
Equivalencia (TOST, α = 0,05): umbral de 0,10 desviaciones estándar del resultado por desviación estándar del
regresor. Efecto mínimo detectable con potencia 0,80.

## Validación y robustez
Placebo temporal: ΔDE(2022→2023) sobre R2 (el rezago futuro no puede explicar el cambio pasado).
Placebo sectorial: [4] con P = tres sectores de menor brecha.
Malla: definiciones de R (solo AIL; frente a la ley, winsorizado); sin controles; sin efectos fijos; sin Meta;
sin productores; excluye R1 = 1; resultado en niveles; regresión de mediana; errores HC3.
No aplican: raíz unitaria, VAR, proyecciones locales (corte transversal con dos periodos).
