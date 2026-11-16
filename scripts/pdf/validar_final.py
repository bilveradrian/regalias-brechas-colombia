#!/usr/bin/env python3
"""validar_final.py — Construye la tabla final AIL (municipio × bienio) desde la
conciliación y la valida contra (i) subtotales departamentales impresos y
(ii) la base del Meta digitada a mano en la tesis de origen (control externo)."""
import os
import pandas as pd
RAIZ = os.getcwd(); D = os.path.join(RAIZ, "datos", "pdf")
r = pd.read_csv(os.path.join(D, "conciliacion.csv"), dtype={"cod_dane": str})

OKD = {"ok_R2", "ok_R2_reparado_R3", "reparado_R2", "reparado_DI", "ok_DI_cero", "ok_DI_doble_G_implicito",
       "ok_R2_adicion_inconsistente", "ok_R2_DI_adicion", "cero_guiones"}
r["beneficiario_ley"] = r.estado_ley.ne("pendiente").astype(int)
r["calidad_ley"] = r.estado_ley.map(lambda e: "exacta" if e in ("ok_R1", "reparado_R1") else
                                    ("sin_asignacion" if e == "pendiente" else "imputada_2_13"))
for t in ["ail", "ad"]:
    r[f"calidad_dec_{t}"] = r[f"estado_dec_{t}"].map(lambda e: "validada" if e in OKD else "no_validada")
final = r[["cod_dane", "bienio", "beneficiario_ley", "ley_ail", "ley_ad", "calidad_ley",
           "apropiacion_ail", "giros_ail", "di_ail", "calidad_dec_ail",
           "apropiacion_ad", "giros_ad", "di_ad", "calidad_dec_ad", "estado_ley", "estado_dec_ail", "estado_dec_ad"]].copy()
final.to_csv(os.path.join(D, "ail_municipal_final.csv"), index=False)

# ---------- R4: subtotales departamentales (todas las columnas impresas) ----------
st = pd.read_csv(os.path.join(D, "subtotales_crudos.csv"), dtype={"cod_dep": str})
COLS = {("ley2072", "AIL"): (1, ["ley_ail"]), ("ley2072", "AD"): (1, ["ley_ad"]),
        ("ley2279", "AIL"): (2, ["ley_ail"]), ("ley2279", "AD"): (2, ["ley_ad"]),
        ("d0363_a2", "AIL"): (1, ["apropiacion_ail", "giros_ail", "di_ail"]),
        ("d0363_a2", "AD"): (1, ["apropiacion_ad", "giros_ad", "di_ad"]),
        ("d0379_a3_ail", "AIL"): (2, ["apropiacion_ail", "giros_ail", "di_ail"]),
        ("d0379_a4_ad", "AD"): (2, ["apropiacion_ad", "giros_ad", "di_ad"]),
        ("d0363_a5_ail", "AIL"): (1, ["di_ail"]), ("d0363_a6_ad", "AD"): (1, ["di_ad"]),
        ("d0379_a8_ail", "AIL"): (2, ["di_ail"]), ("d0379_a9_ad", "AD"): (2, ["di_ad"])}
def dd(a, b):
    prev = list(range(len(b) + 1))
    for i, ca in enumerate(a, 1):
        cur = [i]
        for j, cb in enumerate(b, 1):
            cur.append(min(prev[j] + 1, cur[j - 1] + 1, prev[j - 1] + (ca != cb)))
        prev = cur
    return prev[-1]
chk = []
for _, s in st.iterrows():
    sec = s.seccion if isinstance(s.seccion, str) and s.seccion else None
    if s.fuente in ("d0363_a5_ail", "d0379_a3_ail", "d0379_a8_ail"): sec = "AIL"
    if s.fuente in ("d0363_a6_ad", "d0379_a4_ad", "d0379_a9_ad"): sec = "AD"
    if (s.fuente, sec) not in COLS: continue
    b, vars_ = COLS[(s.fuente, sec)]
    sub = final[(final.bienio == b) & (final.cod_dane.str[:2] == s.cod_dep)]
    sumas = [int(round(sub[v].fillna(0).sum())) for v in vars_]
    concat = "".join(str(int(v)) for v in str(s.valores).split(";"))
    # división óptima de la cadena impresa en k columnas (los números pueden venir pegados por OCR)
    import itertools
    k = len(sumas); mejor = None
    def cortes(rest, objetivos):
        if not objetivos:
            yield []
            return
        L = len(str(objetivos[0]))
        opciones = {L - 1, L, L + 1} | ({0} if objetivos[0] == 0 else set())
        for l in opciones:
            if (l == 0 and objetivos[0] == 0) or 0 < l <= len(rest):
                for tail in cortes(rest[l:], objetivos[1:]):
                    yield [rest[:l]] + tail
    for piezas in cortes(concat, sumas):
        err = sum(abs(int(x or 0) - t) for x, t in zip(piezas, sumas))
        if mejor is None or err < mejor[0]:
            mejor = (err, piezas)
    if mejor is None:
        res, piezas, difs = "sin_lectura", [], []
    else:
        piezas = mejor[1]; difs = [int(x or 0) - t for x, t in zip(piezas, sumas)]
        if all(abs(d_) <= 100 for d_ in difs):
            res = "coincide"
        elif all(abs(d_) <= 100 or dd(str(t), x) <= 2 for d_, x, t in zip(difs, piezas, sumas)):
            res = "difiere_1_digito_en_subtotal_impreso"
        else:
            res = "difiere"
    chk.append(dict(fuente=s.fuente, seccion=sec, cod_dep=s.cod_dep, departamento=s.departamento,
                    columnas="+".join(vars_), subtotal_impreso_ocr=";".join(piezas), sumas_municipales=";".join(map(str, sumas)),
                    diferencias=";".join(map(str, difs)), resultado=res))
chk = pd.DataFrame(chk).drop_duplicates(["fuente", "seccion", "cod_dep"])
chk.to_csv(os.path.join(D, "validacion_subtotales.csv"), index=False)
print(chk.resultado.value_counts())
print(chk[chk.resultado != "coincide"][["fuente", "seccion", "cod_dep", "departamento", "subtotal_impreso_ocr", "sumas_municipales", "diferencias"]].to_string())

# ---------- Control externo: base del Meta digitada en la tesis ----------
cmp = []
for b, f in [(1, "disponibilidad_inicial_1.xlsx"), (2, "disponibilidad_inicial_2.xlsx")]:
    m = pd.read_excel(os.path.join(RAIZ, "datos_originales", f)); m["cod_dane"] = m.ciudad.astype(str).str.zfill(5)
    x = m.merge(final[final.bienio == b], on="cod_dane")
    x = x[~x.cod_dane.isin(["50006", "50001"])]
    for a, c in [("AIL", "ley_ail"), ("AIL-AD", "ley_ad"), ("D_AIL", "di_ail"), ("D_AIL-AD", "di_ad")]:
        dif = (x[a] - x[c]).abs()
        cmp.append(dict(bienio=b, variable_tesis=a, variable_pdf=c, n=len(x), coinciden=int((dif <= 2).sum()),
                        no_coinciden=";".join(x.loc[dif > 2, "cod_dane"] + ":" + x.loc[dif > 2, a].astype(str) + "≠" + x.loc[dif > 2, c].astype(str))))
cmp = pd.DataFrame(cmp); cmp.to_csv(os.path.join(D, "control_meta_tesis.csv"), index=False)
print(cmp.to_string())
print(final.groupby("bienio")[["beneficiario_ley"]].sum())
print(final.groupby("bienio").calidad_dec_ail.value_counts(), final.groupby("bienio").calidad_dec_ad.value_counts())
