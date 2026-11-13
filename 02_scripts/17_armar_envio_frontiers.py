"""
17_armar_envio_frontiers.py
Arma el paquete de envío a Frontiers in Public Health en 06_Envio_Frontiers/ y el ZIP para Doris.

  01_Cover_letter.docx, 02_Scope_statement.txt   (de 06_Envio_Frontiers/fuente/, con el conversor del kit)
  03_Manuscript.docx                              (copia del manuscrito vigente)
  04_Figure1.tif ... 04_Figure6.tif               (TIFF de 600 ppp de 04_figuras/)
  05_Supplementary_Material.docx                  (copia del suplementario vigente)
  06_RECORD_checklist.docx                        (copia de la lista RECORD vigente; segundo archivo suplementario)
  figuras_editables/                              (PDF, SVG y EPS de las mismas figuras)

El ZIP (Paquete_envio_Frontiers_tosferina_vN.zip, en la raíz) lleva esos archivos, la guía para Doris,
las fuentes de los requisitos y, en 07_Datos_y_codigo/, la misma selección de código y datos que el
repositorio de GitHub. Deja MANIFIESTO_SHA256.txt con la huella de cada archivo del ZIP.
Ejecutar desde la raíz: ~/herramientas/py-ciencia/bin/python -I 02_scripts/17_armar_envio_frontiers.py
Con --sin-zip solo copia los archivos a 06_Envio_Frontiers/, para revisarlos antes con chequear_paquete.py.
"""
import hashlib
import pathlib
import shutil
import sys
import zipfile

MANUSCRITO = "Manuscrito_Frontiers_tosferina_v5.docx"          # v5: sobre la plantilla de Word de Frontiers
SUPLEMENTO = "Material_suplementario_tosferina_v4.docx"      # v4: sobre la plantilla de suplementos de Frontiers
RECORD = "Lista_RECORD_tosferina_v3.docx"                   # v3: ídem
ZIP = "Paquete_envio_Frontiers_tosferina_v3.zip"
FIGURAS = ["Figura1_dinamica_deficit", "Figura2_estructura_edad", "Figura3_estudio_anio_por_anio",
           "Figura4_mapas_riesgo_municipal", "Figura5_control_prenatal_por_edad",
           "Figura6_muerte_infantil_desigualdad"]

raiz = pathlib.Path.cwd()
env = raiz / "06_Envio_Frontiers"
if not (raiz / MANUSCRITO).exists():
    sys.exit("Correr desde la raíz del proyecto (no encuentro " + MANUSCRITO + ")")
for f in (MANUSCRITO, SUPLEMENTO, RECORD):
    if (raiz / ("~$" + f[2:])).exists() or (raiz / ("~$" + f)).exists():
        sys.exit(f + " está abierto en Word")
SIN_ZIP = "--sin-zip" in sys.argv
if (raiz / ZIP).exists() and not SIN_ZIP:
    sys.exit(ZIP + " ya existe; para una versión nueva, cambie ZIP a la siguiente _vN")


def sha(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()


# 1. Archivos del portal
shutil.copy2(raiz / MANUSCRITO, env / "03_Manuscript.docx")
shutil.copy2(raiz / SUPLEMENTO, env / "05_Supplementary_Material.docx")
shutil.copy2(raiz / RECORD, env / "06_RECORD_checklist.docx")
(env / "figuras_editables").mkdir(exist_ok=True)
for i, f in enumerate(FIGURAS, 1):
    shutil.copy2(raiz / "04_figuras" / f"{f}.tiff", env / f"04_Figure{i}.tif")
    for ext in ("pdf", "svg", "eps"):
        shutil.copy2(raiz / "04_figuras" / f"{f}.{ext}", env / "figuras_editables" / f"Figure{i}.{ext}")
for f in ("01_Cover_letter.docx", "02_Scope_statement.txt", "00_Como_enviar_Frontiers.docx"):
    if not (env / f).exists():
        sys.exit("Falta " + f + " en 06_Envio_Frontiers (se arma desde fuente/ con el conversor del kit)")
if SIN_ZIP:
    sys.exit(print("Archivos copiados en 06_Envio_Frontiers/ (sin ZIP)"))

# 2. Código y datos: la misma selección que el repositorio de GitHub
def del_repositorio():
    sel = [raiz / "README.md", raiz / "CITATION.cff", raiz / ".gitignore", raiz / "LICENSE", raiz / "LICENSE-DATA.md"]
    for d in ("02_scripts", "01_datos/procesados", "01_datos/raw", "03_resultados", "04_figuras"):
        for p in sorted((raiz / d).rglob("*")):
            r = p.relative_to(raiz).as_posix()
            if (not p.is_file() or p.name.startswith(".") or "__pycache__" in r or r.startswith("01_datos/raw/dane_proyecciones")
                    or "/extraido/" in r or p.name == "sivigila_800_2007_2025_raw.rds" or p.name.startswith("antioquia_")
                    or p.suffix in (".tiff", ".eps")
                    or r == "02_scripts/07_documentos.R"):   # lleva los correos de las coautoras; no va al repositorio público
                continue
            sel.append(p)
    return sel

# 3. ZIP
entradas = []
for p in sorted(env.iterdir()):
    if p.name in ("fuente", "revision_referencias", ".DS_Store") or p.name.startswith("00_Plan") or p.name.startswith("~$"):
        continue
    if p.is_dir():
        entradas += [(q, "Paquete_Frontiers/" + q.relative_to(env).as_posix()) for q in sorted(p.rglob("*")) if q.is_file() and q.name != ".DS_Store"]
    else:
        entradas.append((p, "Paquete_Frontiers/" + p.name))
entradas += [(p, "Paquete_Frontiers/07_Datos_y_codigo/" + p.relative_to(raiz).as_posix()) for p in del_repositorio()]
manifiesto = "\n".join(f"{sha(p)}  {a}" for p, a in entradas) + "\n"
with zipfile.ZipFile(raiz / ZIP, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as z:
    for p, a in entradas:
        z.write(p, a)
    z.writestr("Paquete_Frontiers/MANIFIESTO_SHA256.txt", manifiesto)
tam = (raiz / ZIP).stat().st_size / 1e6
print(f"{ZIP}: {len(entradas) + 1} archivos, {tam:.1f} MB")
