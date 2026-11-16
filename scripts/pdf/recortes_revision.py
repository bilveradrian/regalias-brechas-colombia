#!/usr/bin/env python3
"""Genera recortes de imagen de las filas que requieren revisión visual.
Cada recorte muestra la franja del PDF original alrededor del código DANE
(dos filas arriba y abajo) para leer los valores a ojo.
Salida: revision/recortes/<bienio>_<que>_<cod>_<fuente>.png y un mosaico por lote."""
import csv, os, re, subprocess, sys, html
import pandas as pd
from PIL import Image, ImageDraw

RAIZ = os.getcwd()
PDF_DIR = os.path.join(RAIZ, "datos_originales", "nacional", "Asignaciones_y_Disponibilidad")
OUT = os.path.join(RAIZ, "revision", "recortes"); os.makedirs(OUT, exist_ok=True)
TMP = os.path.join(RAIZ, "revision", "tmp"); os.makedirs(TMP, exist_ok=True)
DPI = 130
ARCH = {"ley2072": "LEY BIENIO 2021 - 2022 SGR.pdf", "ley2279": "LEY BIENIO 2023 - 2024 SGR.pdf",
        "d0363": "DECRETO 0363 DEL 16 DE MARZO DE 2023.pdf", "d0379": "DECRETO 0379  DE 2025.pdf"}
FU = {(1, "LEY"): ["ley2072"], (2, "LEY"): ["ley2279"],
      (1, "AIL"): ["d0363_a2", "d0363_a5_ail"], (1, "AD"): ["d0363_a2", "d0363_a6_ad"],
      (2, "AIL"): ["d0379_a3_ail", "d0379_a8_ail"], (2, "AD"): ["d0379_a4_ad", "d0379_a9_ad"]}

cr = pd.read_csv(os.path.join(RAIZ, "datos/pdf/extraccion_cruda.csv"), dtype=str)
cc = pd.read_csv(os.path.join(RAIZ, "datos/pdf/correccion_codigos.csv"), dtype=str).fillna("")
alias = {}
for _, r in cc.iterrows():
    if r.cod_final:
        alias.setdefault(r.cod_final, set()).add(r.cod_ocr)
rv = pd.read_csv(os.path.join(RAIZ, "datos/pdf/para_revision.csv"), dtype={"cod_dane": str})
ig = pd.read_excel(os.path.join(RAIZ, "datos_originales/nacional/Controles/Indicadores Generales.xlsx"), dtype=str)
NOMBRE = dict(zip(ig["Código Entidad"], ig["Entidad"]))

_bbox = {}
def palabras(pdf, p):
    k = (pdf, p)
    if k not in _bbox:
        out = subprocess.run(["pdftotext", "-f", str(p), "-l", str(p), "-bbox", os.path.join(PDF_DIR, ARCH[pdf]), "-"],
                             capture_output=True).stdout.decode("utf-8", "replace")
        pw = re.search(r'<page width="([\d.]+)" height="([\d.]+)"', out)
        ws = [(float(a), float(b), float(c), float(d), html.unescape(w)) for a, b, c, d, w in
              re.findall(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">([^<]*)</word>', out)]
        _bbox[k] = (float(pw.group(1)), float(pw.group(2)), ws)
    return _bbox[k]

_img = {}
def imagen(pdf, p):
    k = (pdf, p)
    if k not in _img:
        base = os.path.join(TMP, f"{pdf}_{p}")
        if not os.path.exists(base + ".png"):
            subprocess.run(["pdftoppm", "-f", str(p), "-l", str(p), "-r", str(DPI), "-png", "-singlefile",
                            os.path.join(PDF_DIR, ARCH[pdf]), base], check=True)
        _img[k] = Image.open(base + ".png")
    return _img[k]

def recorte(pdf, p, codigos, nombre):
    W, H, ws = palabras(pdf, p)
    ys = [w for w in ws if w[4] in codigos]
    if not ys and nombre:
        tok = nombre.upper().split()[0]
        ys = [w for w in ws if w[4].upper().startswith(tok[:6])]
    if not ys:
        return None
    w = ys[0]; sc = DPI / 72
    y0, y1 = max(0, (w[1] - 34) * sc), min(H * sc, (w[3] + 34) * sc)
    im = imagen(pdf, p).crop((0, int(y0), int(W * sc), int(y1)))
    return im

RANGOS = {("ley2072", "AIL"): (88, 136), ("ley2072", "AD"): (135, 182),
          ("ley2279", "AIL"): (61, 102), ("ley2279", "AD"): (101, 142),
          ("d0363_a2", "AIL"): (288, 331), ("d0363_a2", "AD"): (331, 375),
          ("d0363_a5_ail", "AIL"): (459, 503), ("d0363_a6_ad", "AD"): (504, 548),
          ("d0379_a3_ail", "AIL"): (81, 111), ("d0379_a4_ad", "AD"): (112, 142),
          ("d0379_a8_ail", "AIL"): (205, 235), ("d0379_a9_ad", "AD"): (236, 266)}
FU = {(1, "LEY"): [("ley2072", "AIL"), ("ley2072", "AD")], (2, "LEY"): [("ley2279", "AIL"), ("ley2279", "AD")],
      (1, "AIL"): [("d0363_a2", "AIL"), ("d0363_a5_ail", "AIL")], (1, "AD"): [("d0363_a2", "AD"), ("d0363_a6_ad", "AD")],
      (2, "AIL"): [("d0379_a3_ail", "AIL"), ("d0379_a8_ail", "AIL")], (2, "AD"): [("d0379_a4_ad", "AD"), ("d0379_a9_ad", "AD")]}
no_hallados = []
lotes, actual = [], []
for _, r in rv.iterrows():
    cod = str(r.cod_dane).zfill(5)
    cods = {cod} | alias.get(cod, set())
    for f, t in FU[(r.bienio, r.que)]:
        pdf = f.split("_")[0]; a_, b_ = RANGOS[(f, t)]
        hall = []
        for p in range(a_, b_ + 1):
            _, _, ws = palabras(pdf, p)
            if any(w[4] in cods for w in ws):
                hall.append(p)
        if not hall:
            no_hallados.append(dict(cod_dane=cod, bienio=r.bienio, que=r.que, fuente=f, tipo=t)); continue
        for p in hall[:2]:
            im = recorte(pdf, p, cods, "")
            if im is None:
                continue
            lab = f"{r.bienio}|{r.que}|{cod} {NOMBRE.get(cod,'')}|{f}/{t} p{p}|{r.estado}"
            actual.append((lab, im))
            if len(actual) == 6:
                lotes.append(actual); actual = []
if actual:
    lotes.append(actual)
pd.DataFrame(no_hallados).to_csv(os.path.join(RAIZ, "revision", "codigos_no_hallados.csv"), index=False)
for i, lote in enumerate(lotes):
    ancho = max(im.width for _, im in lote); alto = sum(im.height + 22 for _, im in lote)
    M = Image.new("RGB", (ancho, alto), "white"); d = ImageDraw.Draw(M); y = 0
    for lab, im in lote:
        d.rectangle([0, y, ancho, y + 20], fill=(255, 235, 150)); d.text((4, y + 4), lab, fill="black")
        M.paste(im, (0, y + 22)); y += im.height + 22
    M.save(os.path.join(OUT, f"lote_{i:03d}.png"))
print(len(lotes), "lotes")
