"""Arma el manuscrito en la plantilla de Cuadernos de Economía a partir de las
secciones en Markdown de texto/. Reutiliza los párrafos modelo de la plantilla
(título, resumen, encabezados, cuerpo, tabla y figura) para heredar su formato.

Marcado admitido en los .md:
  # 1. Título           encabezado de primer nivel (numeración de la plantilla)
  ## 1.1. Subtítulo     encabezado de segundo nivel
  *texto*               cursiva;  _{x} subíndice;  ^{x} superíndice
  $$ ecuación | n       ecuación centrada con número [n] alineado a la derecha
  <<TABLA n>>           inserta tablas/json/tabla_n.json (título, fuente, notas)
  <<FIGURA n|ruta|título|fuente>>  inserta la figura (PNG) con título y fuente
Uso: python3 -I texto/armar_docx.py salida.docx seccion1.md [seccion2.md ...]"""
import copy, json, os, re, sys
import docx
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PLANTILLA = ('/mnt/user-data/attachments/354d07c0-dae7-5ea1-8dbc-119ca9027acf/'
             '6b598553-CE_-_Plantilla_para_postulacio_n_Cuadernos-OTH.docx')
TITULO = '¿Frenan las regalías no giradas el cierre de brechas? Evidencia municipal para Colombia'
RESUMEN = ('Este artículo examina si los saldos no girados de la Asignación para la Inversión Local del Sistema General de Regalías explican el cierre de brechas de desarrollo económico en 1.037 municipios de Colombia entre 2022 y 2024. Con datos de los decretos de cierre, diferencias largas y pruebas de equivalencia, se encuentra que el rezago en el giro no se asocia con el cierre de la brecha y que los saldos se concentran en municipios pequeños. El Meta muestra indicios de una restricción de capacidad de absorción. La medición oficial de brechas no permite evaluar la inversión de corto plazo.')
CLAVES = 'regalías; brechas de desarrollo; inversión local; capacidad de absorción; Colombia.'
JEL = 'H72, H77, Q33.'
ANCHO_TEXTO = 15.5  # cm (carta con márgenes de 3 cm)
TOKENS = re.compile(r'(\*[^*]+\*|_\{[^}]*\}|\^\{[^}]*\})')


def poner_texto(par, texto, tam=None):
    """Sustituye el texto del párrafo conservando el formato del primer run."""
    r0 = par.runs[0]._r if par.runs else None
    for r in list(par.runs)[1:]:
        r._r.getparent().remove(r._r)
    primero = True
    for p in TOKENS.split(texto):
        if not p:
            continue
        estilo = None
        if p.startswith('*') and p.endswith('*'):
            estilo, p = 'it', p[1:-1]
        elif p.startswith('_{'):
            estilo, p = 'sub', p[2:-1]
        elif p.startswith('^{'):
            estilo, p = 'sup', p[2:-1]
        if primero and r0 is not None:
            run = par.runs[0]; primero = False
        elif r0 is not None:
            nuevo = copy.deepcopy(r0); par._p.append(nuevo); run = docx.text.run.Run(nuevo, par)
        else:
            run = par.add_run(); r0 = run._r; primero = False
        run.text = p
        run.italic = True if estilo == 'it' else None
        # vertAlign es un solo atributo: asignar None a uno borra el otro
        if estilo == 'sub':
            run.font.subscript = True
        elif estilo == 'sup':
            run.font.superscript = True
        else:
            run.font.subscript = None
        if tam:
            run.font.size = Pt(tam)


def etiqueta_y_texto(par, etiqueta, texto):
    """Párrafo tipo 'Fuente: ...': etiqueta en negrita y texto normal."""
    poner_texto(par, etiqueta)
    par.runs[0].bold = True
    nuevo = copy.deepcopy(par.runs[0]._r); par._p.append(nuevo)
    run = docx.text.run.Run(nuevo, par); run.text = ' ' + texto; run.bold = False


def borde(celda, lados, grosor=8):
    tcPr = celda._tc.get_or_add_tcPr()
    bordes = tcPr.find(qn('w:tcBorders'))
    if bordes is None:
        bordes = OxmlElement('w:tcBorders'); tcPr.append(bordes)
    for lado in lados:
        e = OxmlElement(f'w:{lado}')
        e.set(qn('w:val'), 'single'); e.set(qn('w:sz'), str(grosor)); e.set(qn('w:color'), '000000')
        bordes.append(e)


def main(salida, secciones):
    d = docx.Document(PLANTILLA)
    P = d.paragraphs
    modelos = {'h1': P[13], 'cuerpo': P[15], 'vacio': P[14], 'h2': P[46], 'tab_num': P[35], 'tab_tit': P[36],
               'fuente': P[37], 'nota': P[38], 'fig_num': P[50], 'fig_tit': P[51], 'ref_tit': P[81], 'ref': P[83]}
    poner_texto(P[1], TITULO)
    poner_texto(P[6], RESUMEN)
    for par, valor in ((P[8], CLAVES), (P[10], JEL)):
        etiqueta_y_texto(par, par.text.split(':')[0] + ':', valor)
    tpl = {k: copy.deepcopy(v._p) for k, v in modelos.items()}
    cuerpo = d.element.body
    ancla = P[12]._p
    for el in list(cuerpo):
        if not el.tag.endswith('sectPr') and cuerpo.index(el) > cuerpo.index(ancla):
            cuerpo.remove(el)
    sect = [el for el in cuerpo if el.tag.endswith('sectPr')][0]

    def agrega(clave, texto=None):
        e = copy.deepcopy(tpl[clave]); sect.addprevious(e)
        par = docx.text.paragraph.Paragraph(e, d._body)
        if texto is not None:
            poner_texto(par, texto)
        return par

    def ecuacion(texto, num):
        par = agrega('cuerpo')
        par.alignment = WD_ALIGN_PARAGRAPH.LEFT
        par.paragraph_format.tab_stops.add_tab_stop(Cm(ANCHO_TEXTO / 2), WD_TAB_ALIGNMENT.CENTER)
        par.paragraph_format.tab_stops.add_tab_stop(Cm(ANCHO_TEXTO), WD_TAB_ALIGNMENT.RIGHT)
        poner_texto(par, '\t' + texto + '\t[' + num + ']')
        agrega('vacio')

    def tabla(num):
        t = json.load(open(os.path.join(RAIZ, 'tablas/json', f'tabla_{num}.json'), encoding='utf-8'))
        agrega('tab_num', f'Tabla {t["num"]}.').paragraph_format.keep_with_next = True
        agrega('tab_tit', t['titulo']).paragraph_format.keep_with_next = True
        filas = [t['encabezado']] + t['cuerpo']
        tb = d.add_table(rows=len(filas), cols=len(t['encabezado']))
        sect.addprevious(tb._tbl)
        tb.alignment = 1
        anchos = t.get('anchos') or [ANCHO_TEXTO / len(t['encabezado'])] * len(t['encabezado'])
        escala = ANCHO_TEXTO / sum(anchos)
        tb.autofit = False
        tblPr = tb._tbl.tblPr
        lay = OxmlElement('w:tblLayout'); lay.set(qn('w:type'), 'fixed'); tblPr.append(lay)
        for gc, a in zip(tb._tbl.tblGrid.findall(qn('w:gridCol')), anchos):
            gc.set(qn('w:w'), str(int(a * escala / 2.54 * 1440)))
        for i, fila in enumerate(filas):
            for j, valor in enumerate(fila):
                c = tb.cell(i, j); c.width = Cm(anchos[j] * escala)
                par = c.paragraphs[0]
                poner_texto(par, '' if valor is None else str(valor), tam=9)
                par.alignment = WD_ALIGN_PARAGRAPH.LEFT if j == 0 else WD_ALIGN_PARAGRAPH.CENTER
                par.paragraph_format.space_after = Pt(0); par.paragraph_format.line_spacing = 1.0
                if i == 0:
                    for r in par.runs:
                        r.bold = True
                    borde(c, ['top', 'bottom'], 8)
                if i == len(filas) - 1:
                    borde(c, ['bottom'], 8)
                else:
                    par.paragraph_format.keep_with_next = True  # la tabla no se parte entre páginas
        etiqueta_y_texto(agrega('fuente'), 'Fuente:', t['fuente'])
        nota = agrega('nota', t['notas'])
        nota.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
        for r in nota.runs:
            r.font.size = Pt(9)
        agrega('vacio')

    def figura(num, ruta, titulo, fuente):
        agrega('fig_num', f'Figura {num}.').paragraph_format.keep_with_next = True
        agrega('fig_tit', titulo).paragraph_format.keep_with_next = True
        par = agrega('vacio'); par.alignment = WD_ALIGN_PARAGRAPH.CENTER; par.paragraph_format.keep_with_next = True
        par.add_run().add_picture(os.path.join(RAIZ, ruta), width=Cm(ANCHO_TEXTO))
        etiqueta_y_texto(agrega('fuente'), 'Fuente:', fuente)
        agrega('vacio')

    for f in secciones:
        es_ref = os.path.basename(f).startswith('07_')
        es_anexo = os.path.basename(f).startswith('08_')
        bloques = [b.strip() for b in open(f, encoding='utf-8').read().split('\n\n') if b.strip()]
        for b in bloques:
            if b.startswith('## '):
                agrega('h2', re.sub(r'^##\s+[\d.]+\s*', '', b)); agrega('vacio')
            elif b.startswith('# ') and (es_ref or es_anexo):
                agrega('ref_tit', b[2:].strip()); agrega('vacio')
            elif es_ref:
                agrega('ref', b.replace('\n', ' ')).alignment = WD_ALIGN_PARAGRAPH.LEFT
            elif b.startswith('# '):
                agrega('h1', re.sub(r'^#\s+\d+\.\s*', '', b)); agrega('vacio')
            elif b.startswith('$$'):
                texto, num = b[2:].rsplit('|', 1); ecuacion(texto.strip(), num.strip())
            elif b.startswith('<<TABLA'):
                tabla(b[7:-2].strip())
            elif b.startswith('<<FIGURA'):
                figura(*[x.strip() for x in b[8:-2].split('|')])
            else:
                agrega('cuerpo', b.replace('\n', ' ')); agrega('vacio')
    d.save(salida)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2:])
