#!/usr/bin/env python3
"""
extraer_pdf.py — Extracción de anexos municipales de la AIL desde los PDF
escaneados (capa OCR) de las Leyes 2072/2020 y 2279/2022 y de los Decretos
0363/2023 y 0379/2025.

Salida: datos/pdf/extraccion_cruda.csv (una fila por municipio y fuente, con
página, línea original y valores leídos) y datos/pdf/subtotales_crudos.csv
(subtotales departamentales impresos en el documento).

Uso (desde la raíz del proyecto):
    python3 -I scripts/pdf/extraer_pdf.py

Requiere pdftotext (poppler-utils). No modifica los PDF originales.
"""
import csv, os, re, statistics, subprocess, sys

RAIZ = os.getcwd()
PDF_DIR = os.path.join(RAIZ, "datos_originales", "nacional", "Asignaciones_y_Disponibilidad")
OUT_DIR = os.path.join(RAIZ, "datos", "pdf")
os.makedirs(OUT_DIR, exist_ok=True)

# fuente: (archivo, página inicial, página final [1-indexadas], columnas, tipo de AIL)
FUENTES = {
    # En las leyes los anexos 3 (AIL) y 4 (AIL-Ambiente) comparten páginas de
    # frontera; el tipo se asigna por la etiqueta de cada fila ("MIXTO").
    "ley2072":        ("LEY BIENIO 2021 - 2022 SGR.pdf",              86, 182, ["asignacion_ley"], "MIXTO"),
    "ley2279":        ("LEY BIENIO 2023 - 2024 SGR.pdf",              60, 142, ["asignacion_ley"], "MIXTO"),
    "d0363_a2":       ("DECRETO 0363 DEL 16 DE MARZO DE 2023.pdf",   288, 375, ["apropiacion", "giros", "di"], "MIXTO"),
    "d0363_a5_ail":   ("DECRETO 0363 DEL 16 DE MARZO DE 2023.pdf",   459, 503, ["di", "reintegros", "multas"], "AIL"),
    "d0363_a6_ad":    ("DECRETO 0363 DEL 16 DE MARZO DE 2023.pdf",   504, 548, ["di", "multas"], "AD"),
    "d0379_a3_ail":   ("DECRETO 0379  DE 2025.pdf",                    81, 111, ["apropiacion", "giros", "di"], "AIL"),
    "d0379_a4_ad":    ("DECRETO 0379  DE 2025.pdf",                   112, 142, ["apropiacion", "giros", "di"], "AD"),
    "d0379_a8_ail":   ("DECRETO 0379  DE 2025.pdf",                   205, 235, ["di", "multas"], "AIL"),
    "d0379_a9_ad":    ("DECRETO 0379  DE 2025.pdf",                   236, 266, ["di", "multas"], "AD"),
}

COD_MUN = re.compile(r"^\s{0,12}([0-9OoIl]{5})(?=\s|$)")
COD_DEP = re.compile(r"^\s{0,12}([0-9]{2})\s{2,}([A-ZÁÉÍÓÚÑ][A-ZÁÉÍÓÚÑa-záéíóúñ .,()\-]+?)\s{2,}(.*)$")
NUMCHUNK = re.compile(r"^[\d.,'´`\s]+$")


def paginas(pdf):
    """Texto con layout, una cadena por página."""
    txt = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True,
                         check=True).stdout.decode("utf-8", "replace")
    return txt.split("\f")


def trozos_numericos(linea, desde=0):
    """Fragmentos separados por 2+ espacios que solo contienen dígitos y
    separadores; devuelve (valor_entero, posición_final, texto)."""
    out = []
    for m in re.finditer(r"\S+(?:\s\S+)*", linea):
        if m.start() < desde:
            continue
        s = m.group(0)
        if s.strip() in ("-", "–", "—", "_", "­"):      # guion = cero en los anexos
            out.append((0, m.end(), s)); continue
        # admite espacios simples internos ("1.352.481 .430"), no letras
        if NUMCHUNK.match(s) and re.search(r"\d", s):
            dig = re.sub(r"\D", "", s)
            if dig:
                out.append((int(dig), m.end(), s))
    return out


def tipo_contexto(texto):
    u = texto.upper()
    if "FONPET" in u or "DIRECTAS" in u:
        return "OTRA"
    if "AMBIENTE" in u:
        return "AD"
    if re.search(r"\b(NBI|N81|NEI|NB!|NBl)\b", u) or "CUARTA" in u or "SEXTA" in u:
        return "AIL"
    return None


def nombre_municipio(texto):
    """Devuelve todos los nombres 'MUNICIPIO DE X' del contexto, separados por '|'."""
    u = re.sub(r"\s+", " ", texto.upper())
    noms = []
    for m in re.finditer(r"MUNICIPIO\s+DE\s*(.+?)(?=\(|ASIGNACI|MUNICIPIO|$)", u):
        n = re.sub(r"[^A-ZÁÉÍÓÚÑÜ ]", "", m.group(1)).strip()
        if n:
            noms.append(n)
    return "|".join(noms)


HEADER_KW = re.compile(r"SECCI|ENTIDAD|APROPIACI|GIROS|DISPONIBILIDAD|INICIAL|DEFINITIVA|MULTAS|"
                       r"REINTEGROS|VALOR|P.GINA|ANEXO|DECRETO|SANCIONES|^\s*20\d\d\s*$|^\s*2021|^\s*2023")
ANIOS = {str(a) for a in range(2019, 2027)}
MAX_DIST = 2   # distancia vertical máxima (líneas) entre un número y su código


def extraer(fuente, archivo, p0, p1, cols, tipo):
    """Lee las páginas del anexo. Cada fragmento numérico se asigna al código
    municipal más cercano en sentido vertical (misma línea primero; en empate,
    al código siguiente, porque la etiqueta de la fila antecede al código)."""
    pags = paginas(os.path.join(PDF_DIR, archivo))
    filas, subt = [], []
    seccion = tipo if tipo != "MIXTO" else None
    for p in range(p0, p1 + 1):
        lineas = pags[p - 1].splitlines()
        # fin del encabezado: última línea con palabras de encabezado entre las primeras 25
        fin_enc = -1
        for i, l in enumerate(lineas[:25]):
            if HEADER_KW.search(l.upper()) and not COD_MUN.match(l):
                fin_enc = i
        codigos, numeros, deps = [], [], set()
        for i, l in enumerate(lineas):
            u = l.upper()
            if "BENEFICIARIAS DE" in u or re.search(r"AN[EÉ]XO\s*N", u) or "CIERRE DE" in u:
                t = tipo_contexto(" ".join(lineas[i:i + 3]))
                if tipo == "MIXTO" and t:
                    seccion = t
            if i <= fin_enc:
                continue
            md = COD_DEP.match(l)
            if md and not COD_MUN.match(l):
                nums = trozos_numericos(l, md.start(3))
                if nums:
                    subt.append(dict(fuente=fuente, pagina=p, cod_dep=md.group(1),
                                     departamento=md.group(2).strip(), valores=";".join(str(v) for v, _, _ in nums),
                                     fines=";".join(str(e) for _, e, _ in nums), seccion=seccion or ""))
                deps.add(i)
                continue
            mc = COD_MUN.match(l)
            if mc:
                cod_txt = mc.group(1)
                t_row = (tipo_contexto(" ".join(lineas[max(0, i - 1):i + 2])) or seccion) if tipo == "MIXTO" else tipo
                codigos.append(dict(i=i, cod_txt=cod_txt, tipo=t_row,
                                    contexto=" ".join(lineas[max(0, i - 2):min(len(lineas), i + 3)])))
                for v, e, s_ in trozos_numericos(l, mc.end()):
                    numeros.append((i, v, e, s_))
            else:
                ch = trozos_numericos(l)
                # línea de encabezado con años sueltos ("2022   2024")
                if ch and all(str(v) in ANIOS for v, _, _ in ch):
                    continue
                # número de página al pie (fragmento corto aislado en las últimas líneas)
                if ch and len(ch) == 1 and len(str(ch[0][0])) <= 3 and i >= len(lineas) - 6:
                    continue
                for v, e, s_ in ch:
                    numeros.append((i, v, e, s_))
        asign = {k: [] for k in range(len(codigos))}
        for (i, v, e, s_) in numeros:
            if not codigos:
                break
            cand = sorted(range(len(codigos)),
                          key=lambda k: (abs(codigos[k]["i"] - i), 0 if codigos[k]["i"] >= i else 1))
            k = cand[0]
            if abs(codigos[k]["i"] - i) <= MAX_DIST:
                asign[k].append((e, v, s_))
        for k, c in enumerate(codigos):
            nums = sorted(asign[k])        # orden horizontal
            cod = c["cod_txt"].translate(str.maketrans("OoIl", "0011"))
            filas.append(dict(fuente=fuente, pagina=p, linea=c["i"] + 1, cod_dane=cod,
                              cod_ocr=c["cod_txt"], nombre_ocr=nombre_municipio(c["contexto"]),
                              tipo=c["tipo"] or "", n_valores=len(nums),
                              valores=";".join(str(v) for _, v, _ in nums),
                              fines=";".join(str(e) for e, _, _ in nums),
                              texto_ocr=" | ".join(s_ for _, _, s_ in nums)))
    return filas, subt


def asignar_columnas(filas, cols):
    """Asigna cada valor a su columna según la posición horizontal del
    número (borde derecho), tomando como ancla la mediana de las filas
    completas de la misma página (o del documento si la página no tiene)."""
    k = len(cols)
    completas = [f for f in filas if f["n_valores"] == k]
    def anclas(fs):
        if not fs:
            return None
        return [statistics.median(int(f["fines"].split(";")[c]) for f in fs) for c in range(k)]
    anc_doc = anclas(completas)
    por_pag = {}
    for f in completas:
        por_pag.setdefault(f["pagina"], []).append(f)
    for f in filas:
        vals = [int(v) for v in f["valores"].split(";")] if f["valores"] else []
        fines = [int(e) for e in f["fines"].split(";")] if f["fines"] else []
        res = {c: "" for c in cols}
        estado = "ok"
        if len(vals) == k:
            res = dict(zip(cols, vals))
        elif 0 < len(vals) < k:
            a = anclas(por_pag.get(f["pagina"], [])) or anc_doc
            usados = set()
            for v, e in zip(vals, fines):
                c = min((c for c in range(k) if c not in usados), key=lambda c: abs(a[c] - e))
                usados.add(c); res[cols[c]] = v
            estado = "columnas_por_posicion"
        elif len(vals) > k:
            res = dict(zip(cols, vals[-k:]))
            estado = "valores_de_mas"
        else:
            estado = "sin_valores"
        f.update(res); f["estado_lectura"] = estado
    return filas


def main():
    todas, subts = [], []
    for fuente, (arch, p0, p1, cols, tipo) in FUENTES.items():
        filas, subt = extraer(fuente, arch, p0, p1, cols, tipo)
        filas = asignar_columnas(filas, cols)
        todas += filas; subts += subt
        n_ok = sum(f["estado_lectura"] == "ok" for f in filas)
        print(f"{fuente:14s} filas={len(filas):5d} completas={n_ok:5d} subtotales={len(subt)}", file=sys.stderr)
    campos = ["fuente", "pagina", "linea", "cod_dane", "cod_ocr", "nombre_ocr", "tipo", "n_valores",
              "asignacion_ley", "apropiacion", "giros", "di", "reintegros", "multas",
              "estado_lectura", "valores", "fines", "texto_ocr"]
    with open(os.path.join(OUT_DIR, "extraccion_cruda.csv"), "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=campos, extrasaction="ignore"); w.writeheader(); w.writerows(todas)
    with open(os.path.join(OUT_DIR, "subtotales_crudos.csv"), "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=["fuente", "pagina", "cod_dep", "departamento", "seccion", "valores", "fines"])
        w.writeheader(); w.writerows(subts)


if __name__ == "__main__":
    main()
