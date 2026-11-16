# Regalías no giradas y brechas de desarrollo municipal en Colombia: código de replicación

Código del artículo **«¿Frenan las regalías no giradas el cierre de brechas? Evidencia municipal para Colombia»**
(en evaluación en *Cuadernos de Economía*, Universidad Nacional de Colombia).

## Datos: acceso restringido

La principal contribución del trabajo es una base municipal nueva de la Asignación para la Inversión Local (AIL)
del Sistema General de Regalías: apropiación definitiva, giros y disponibilidad inicial de los bienios 2021-2022 y
2023-2024. Los valores se extrajeron de los anexos escaneados de las Leyes 2072 de 2020 y 2279 de 2022 y de los
Decretos 0363 de 2023 y 0379 de 2025, con validación contable y revisión visual documentada.

**Este repositorio publica solo el código.** Los datos originales, la base construida, la bitácora de lecturas
visuales y los resultados están depositados en Zenodo con **acceso restringido**:

> DOI: [10.5281/zenodo.24287432] (https://doi.org/10.5281/zenodo.24287432).

El acceso se concede a editores y evaluadores de la revista, y a investigadores que lo soliciten con un propósito
de verificación o replicación, mediante el botón «Request access» del registro de Zenodo.

## Reproducción

1. Descargar el depósito de Zenodo (una vez aprobado el acceso) y copiar en la raíz del repositorio las carpetas
   `datos_originales/` y, si se desea partir de la base ya construida, `datos/`.
2. Ejecutar desde la raíz:

```
Rscript scripts/ejecutar_todo.R
```

El flujo extrae los PDF (Python 3 y poppler-utils), construye la base maestra, estima los modelos y genera las
tablas y figuras del artículo. Los requisitos y la descripción completa de cada script están en el archivo
`LEEME.md` del depósito de Zenodo y en `especificacion_econometrica.md`.

## Estructura

```
scripts/pdf/     Extracción y validación de los PDF de leyes y decretos del SGR
scripts/         Base maestra (00), análisis (01, 01b), figuras (02, 02b), tablas (03), funciones y flujo completo
scripts/legado/  Versión inicial limitada al Meta (no se usa en el artículo)
texto/           Armado del manuscrito y verificaciones automáticas de cifras y citas
```

## Cita

Ver `CITATION.cff`. Licencia del código: MIT (archivo `LICENSE`). Los datos del depósito de Zenodo no se rigen por
esta licencia.
