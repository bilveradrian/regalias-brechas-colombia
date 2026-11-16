"""Verifica que cada cifra decimal del texto aparezca, con el mismo redondeo, en alguna tabla
(cuerpo o notas). Uso: python3 -I texto/verificar_cifras.py"""
import glob, json, os, re
RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
num = re.compile(r'[−-]?\d{1,3}(?:\.\d{3})*,\d+|[−-]?\d+,\d+')
def val(s): return float(s.replace('−', '-').replace('.', '').replace(',', '.'))
tabla = []
for f in glob.glob(os.path.join(RAIZ, 'tablas/json/*.json')):
    t = json.load(open(f, encoding='utf-8'))
    txt = ' '.join(' '.join(str(c) for c in fila) for fila in t['cuerpo']) + ' ' + t['notas']
    tabla += [val(x) for x in num.findall(txt)]
faltan = []
for f in sorted(glob.glob(os.path.join(RAIZ, 'texto/0[1-6]_*.md'))):
    for linea in open(f, encoding='utf-8'):
        if linea.startswith('$$') or linea.startswith('<<'):
            continue
        for x in num.findall(linea):
            v = val(x); dec = len(x.split(',')[1])
            ok = any(round(abs(t), dec) == round(abs(v), dec) for t in tabla) or \
                 any(round(abs(100 * t), dec) == round(abs(v), dec) for t in tabla)
            if not ok:
                faltan.append((os.path.basename(f), x))
print('Cifras del texto sin respaldo en tablas:', faltan if faltan else 'ninguna')
