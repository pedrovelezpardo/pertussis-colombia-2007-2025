# =============================================================================
# 15_referencias.py — construye la bibliografía (estilo Vancouver) desde PubMed.
# Las referencias sin PMID se escriben a mano. Salida: 05_manuscrito/referencias.json
# Uso (desde la raíz): python3 -I 02_scripts/15_referencias.py
# =============================================================================
import json, time, urllib.request, urllib.parse, xml.etree.ElementTree as ET

PMID = {
 "who2015": "26320265", "ismail2026": "41565439", "nordholm2024": "38577803", "fu2024": "39209267",
 "zhangh2026": "41802571", "ai2026": "42000803", "madden2026": "42811542", "zhangs2026": "41638914",
 "vargas2026": "41324875", "cohen2021": "33991720", "sansone2025": "40496447", "silvadirzo2026": "40928235",
 "villacis2025": "40774871", "juscamayta2026": "42811428", "jeanbaptiste2026": "41878584",
 "cardona2016": "27869387", "idrovo2026": "42757791", "arango2022": "36423493", "hoyos2021": "34751533",
 "carrasquilla2020": "33012607", "hincapie2018": "29789240", "gentile2025": "39951079", "vizzotti2016": "27847175",
 "juscamayta2023": "37469614", "guzman2021": "33773845", "santana2021": "33446384", "amirthalingam2014": "25037990",
 "baxter2017": "28557752", "skoff2017": "29028938", "zhangx2025": "41333898", "mcdermid2024": "38355320",
 "klein2012": "22970945", "warfel2014": "24277828", "mcgirr2015": "25560446", "wearing2009": "19876392",
 "rohani2010": "21071671", "skoff2015": "26347437", "guzman2023": "38095808", "nunes2021": "33734002",
 "rodriguez2019": "31129271", "bonilla2025": "40938105", "kolos2007": "16971026", "mchugh2024": "38353124",
 "victora2000": "11009159", "riebler2016": "27566770", "assuncao1999": "10441770", "heinze2002": "12210625",
 "lipsitch2010": "20335814", "mackenbach1997": "9080560", "benchimol2015": "26440803", "altman2003": "12543843",
 "carstensen2007": "17177166", "knorrheld2001": "12933560", "burton2009": "19649368", "wilkinson2022": "35045177",
 "kim2021": "34412447", "condratovici2026": "42506632",
 "holford1983": "6626659", "dupont2022": "35258102", "farrington1993": "8225751",   # v4
}
MANUAL = {
 "paho2026": "Pan American Health Organization. Epidemiological Update: Pertussis (whooping cough) in the Americas Region, 25 March 2026. Washington, D.C.: PAHO/WHO; 2026. Available from: https://www.paho.org/en/documents/epidemiological-update-pertussis-whooping-cough-americas-region-25-march-2026 (accessed 3 November 2026).",
 "ins2024": "Instituto Nacional de Salud. Protocolo de vigilancia en salud pública: tos ferina (código 800). Versión 2024. Bogotá: INS; 2024. Available from: https://www.ins.gov.co/buscador-eventos/Lineamientos/Pro_Tos%20ferina%202024.pdf (accessed 3 November 2026).",
 "minsalud2025circ": "Ministerio de Salud y Protección Social, Instituto Nacional de Salud. Circular Externa Conjunta No. 011 de 2025: instrucciones para fortalecer las acciones de vigilancia, prevención y control de la tos ferina. Bogotá; 28 March 2025.",
 "minsalud2025info": "Ministerio de Salud y Protección Social. Acortamiento del esquema de vacunación ante la presencia de casos de tos ferina [infographic]. Bogotá: MinSalud; 2025. Available from: https://www.minsalud.gov.co/sites/rid/Lists/BibliotecaDigital/RIDE/VS/PP/PAI/infografia-acortamiento-esquema-2025.pdf (accessed 3 November 2026).",
 "sivigila_micro": "Instituto Nacional de Salud. Portal SIVIGILA: microdatos de eventos de interés en salud pública, evento 800 (tos ferina), 2007–2025. Bogotá: INS. Available from: https://portalsivigila.ins.gov.co (accessed 3 November 2026).",
 "dane_proj": "Departamento Administrativo Nacional de Estadística (DANE). Proyecciones y retroproyecciones de población nacional, departamental y municipal por área, sexo y edad, con base en el CNPV 2018 (actualización post-COVID-19). Bogotá: DANE; 2025. Available from: https://www.dane.gov.co/index.php/estadisticas-por-tema/demografia-y-poblacion/proyecciones-de-poblacion (accessed 3 November 2026).",
 "dane_ipm": "Departamento Administrativo Nacional de Estadística (DANE). Medida de pobreza multidimensional municipal de fuente censal 2018. Bogotá: DANE; 2020. Available from: https://www.dane.gov.co (accessed 3 November 2026).",
 "dane_mgn": "Departamento Administrativo Nacional de Estadística (DANE). Marco Geoestadístico Nacional (MGN), nivel municipio, versión 2018. Bogotá: DANE. Available from: https://portalgis.dane.gov.co (accessed 3 November 2026).",
 "terridata": "Departamento Nacional de Planeación (DNP). TerriData: dimensión Salud (fuente: Ministerio de Salud y Protección Social). Bogotá: DNP; 2026. Available from: https://terridata.dnp.gov.co (accessed 3 November 2026).",
 "sivirep": "Epiverse-TRACE. sivirep: data wrangling and automated reports from SIVIGILA source [R package]. Available from: https://CRAN.R-project.org/package=sivirep (accessed 3 November 2026).",
 "rcore": "R Core Team. R: a language and environment for statistical computing. Version 4.6.1. Vienna: R Foundation for Statistical Computing; 2026. Available from: https://www.R-project.org.",
 "rue2009": "Rue H, Martino S, Chopin N. Approximate Bayesian inference for latent Gaussian models by using integrated nested Laplace approximations. J R Stat Soc Series B Stat Methodol (2009) 71:319–92. doi: 10.1111/j.1467-9868.2008.00700.x",
 "simpson2017": "Simpson D, Rue H, Riebler A, Martins TG, Sørbye SH. Penalising model component complexity: a principled, practical approach to constructing priors. Stat Sci (2017) 32:1–28. doi: 10.1214/16-STS576",
 "kulldorff1997": "Kulldorff M. A spatial scan statistic. Commun Stat Theory Methods (1997) 26:1481–96. doi: 10.1080/03610929708831995",
 "firth1993": "Firth D. Bias reduction of maximum likelihood estimates. Biometrika (1993) 80:27–38. doi: 10.1093/biomet/80.1.27",
}

def efetch(ids):
    u = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?" + urllib.parse.urlencode({"db": "pubmed", "id": ",".join(ids), "retmode": "xml"})
    return ET.fromstring(urllib.request.urlopen(u, timeout=120).read())

def fmt(art):
    """Formato Frontiers (Vancouver): Autores. Título. Revista. (Año) Vol:Págs. doi: ..."""
    a = art.find(".//Article")
    names = []
    for au in a.findall(".//AuthorList/Author"):
        ln = au.findtext("LastName"); ini = au.findtext("Initials"); cn = au.findtext("CollectiveName")
        if ln: names.append(f"{ln} {ini or ''}".strip())
        elif cn: names.append(cn)
    auth = ", ".join(names[:6]) + (", et al" if len(names) > 6 else "")
    title = "".join(a.find("ArticleTitle").itertext()).strip().rstrip(".")
    j = a.findtext("Journal/ISOAbbreviation") or a.findtext("Journal/Title")
    ji = a.find("Journal/JournalIssue")
    year = ji.findtext("PubDate/Year") or (ji.findtext("PubDate/MedlineDate") or "")[:4]
    vol = ji.findtext("Volume") or ""; pg = a.findtext("Pagination/MedlinePgn") or ""
    if not pg:
        eid = [e.text for e in a.findall("ELocationID") if e.get("EIdType") == "pii"]
        pg = eid[0] if eid else ""
    doi = ""   # solo el identificador propio (no los de la lista de referencias del artículo)
    own = art.find("PubmedData/ArticleIdList")
    if own is not None:
        for e in own.findall("ArticleId"):
            if e.get("IdType") == "doi": doi = e.text
    if not doi:
        for e in a.findall("ELocationID"):
            if e.get("EIdType") == "doi": doi = e.text
    if not auth: auth = "World Health Organization"
    pg = pg.replace("-", "–")
    s = f"{auth}. {title}. {j.replace('.', '')} ({year})"
    if vol: s += f" {vol}" + (f":{pg}" if pg else "")
    elif pg: s += f" {pg}"
    s += "."
    if doi: s += f" doi: {doi}"
    return s

refs = dict(MANUAL)
ids = list(PMID.values())
root = efetch(ids)
by = {art.findtext(".//PMID"): art for art in root.findall("PubmedArticle")}
for k, p in PMID.items():
    refs[k] = fmt(by[p]) if p in by else f"[PMID {p} no recuperado]"
# Verificación del 12-11-2026 contra Crossref, OpenAlex, PubMed y retractaciones (detalle en
# 06_Envio_Frontiers/revision_referencias/): sus cadenas, en formato Frontiers Vancouver, reemplazan
# las armadas desde PubMed. Corrigen, entre otros, el título de who2015 (PubMed dice septiembre; el
# número del WER es de agosto) y el año de hoyos2021 (el volumen es de 2022).
import csv, os
# v4 (12-11-2026): cuatro referencias nuevas verificadas en PubMed y Crossref, incluida la fuente WUENIC
for VERIF in ["05_manuscrito/referencias_verificadas_12-11-2026.csv", "05_manuscrito/referencias_verificadas_v4_12-11-2026.csv"]:
    if os.path.exists(VERIF):
        for r in csv.DictReader(open(VERIF, encoding="utf-8")):
            refs[r["clave"]] = r["referencia_propuesta"]
json.dump(refs, open("05_manuscrito/referencias.json", "w", encoding="utf-8"), ensure_ascii=False, indent=1)
for k in list(PMID)[:6] + ["madden2026", "riebler2016", "who2015"]:
    print(k, "→", refs[k])
print("total:", len(refs), "| faltantes:", [k for k, v in refs.items() if v.startswith("[PMID")])
