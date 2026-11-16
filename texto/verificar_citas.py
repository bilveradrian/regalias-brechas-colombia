"""Cruce automático citas <-> referencias del manuscrito.
Revisa: (1) toda cita del texto, tablas y figuras tiene referencia; (2) toda referencia
se cita; (3) ninguna obra se cita más de dos veces. Uso: python3 -I texto/verificar_citas.py"""
import glob, json, os, re, sys, unicodedata
from collections import Counter

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
def norm(s):
    return unicodedata.normalize('NFKD', s).encode('ascii', 'ignore').decode().lower()

cuerpo = ''
for f in sorted(glob.glob(os.path.join(RAIZ, 'texto', '0[1-6]_*.md'))):
    cuerpo += open(f, encoding='utf-8').read() + '\n'
for f in glob.glob(os.path.join(RAIZ, 'tablas', 'json', '*.json')):
    t = json.load(open(f, encoding='utf-8')); cuerpo += ' ' + t['notas'] + ' ' + t['fuente']
refs = [b.strip() for b in open(os.path.join(RAIZ, 'texto', '07_referencias.md'), encoding='utf-8').read().split('\n\n')[1:] if b.strip()]

# Clave de cada referencia: primer apellido (o norma) + año
claves = {}
for r in refs:
    m = re.match(r'(Ley|Decreto|Resolución) (\d+) de (\d{4})', r)
    if m:
        claves[r] = (norm(f'{m.group(2)} de {m.group(3)}'), None); continue
    sigla = re.search(r'\[([A-Z]+)\]', r)
    autor = re.split(r',| \[|\. \(', r)[0]
    anio = re.search(r'\((\d{4})\)', r).group(1)
    claves[r] = (norm(sigla.group(1)) if sigla else norm(autor), anio)

# Citas autor-año en el cuerpo: "Apellido (2010)", "(Apellido, 2010)", "Apellido et al. (2010)", "Apellido y Otro (2010)"
pat = re.compile(r"((?:van der )?[A-ZÁÉÍÓÚ][\w'\-áéíóúñ]+)(?:,? (?:y|&) (?:van der )?[A-ZÁÉÍÓÚ][\w'\-áéíóúñ]+| et al\.)?,? \(?((?:19|20)\d{2})\)?")
usos = Counter(); sin_ref = []
for m in pat.finditer(cuerpo):
    ap, anio = norm(m.group(1)), m.group(2)
    if ap in ('ley', 'decreto', 'resolucion') or not re.match(r'[a-z]', ap):
        continue
    hit = [r for r, (a, y) in claves.items() if y == anio and a.endswith(ap)]
    if hit:
        usos[hit[0]] += 1
    elif ap not in ('entre', 'meta', 'de', 'en', 'cierre', 'bienio', 'brecha', 'mediciones') and len(ap) > 3:
        sin_ref.append(m.group(0))
for r, (a, y) in claves.items():
    if y is None:
        usos[r] += len(re.findall(re.escape(a), norm(cuerpo)))
problemas = 0
for r in refs:
    n = usos.get(r, 0)
    autor_anio = r[:70]
    if n == 0:
        print('SIN CITA EN EL TEXTO:', autor_anio); problemas += 1
    elif n > 2 and claves[r][1] is not None:
        print(f'CITADA {n} VECES (> 2):', autor_anio); problemas += 1
posibles = sorted(set(s for s in sin_ref if not re.search(r'\d{4}\)?$', s) is None))
print('Citas sin referencia (revisar):', sorted(set(sin_ref)))
print(f'Referencias: {len(refs)}; académicas: {sum(1 for r in refs if claves[r][1])}; problemas: {problemas}')
