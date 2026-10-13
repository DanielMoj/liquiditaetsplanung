# ============================================================================
#  build_workbook_neuer_mandant.py
#  Erzeugt eine LEERE Mandatsmappe — Struktur, Formate, Farbcodierung und
#  Token-Referenz, aber OHNE Beispieldaten. Fuer das Onboarding eines neuen
#  Mandanten, bevor irgendetwas eingetragen ist.
#
#  Gefuellt bleiben nur:
#    - strukturelle Schluessel/Beschriftungen (Engine-Vertrag),
#    - gesetzliche Steuersaetze in 08 (gelten ueberall gleich),
#    - der Finanzierungsplan 05 mit 0 (neutral; verhindert NA in der Engine).
#  Alles Mandantenspezifische ist leer und gelb/blau markiert.
#
#  Layout identisch zur Beispielmappe: Titel Z1, Untertitel Z2, Kopfzeile Z4
#  (die Engine liest mit read_excel(skip=3)). Blattnamen exakt wie erwartet.
#
#  Aufruf:   python3 build_workbook_neuer_mandant.py
#  Ergebnis: Mandant_NEU.xlsx
# ============================================================================
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill
from openpyxl.utils import get_column_letter

# ---------------- Konfiguration (vor dem Erzeugen anpassen) ----------------
IST_JAHR  = 2025               # Kalenderjahr der 12 Ist-Monatsspalten in Blatt 01
N_POS     = 60                 # Anzahl vorformatierter Leerzeilen in 01_Positionen
DATEINAME = "Mandant_NEU.xlsx" # Ausgabedatei (je Mandant umbenennen)

# ---------------- Farben (identisch zur Beispielmappe) ----------------
YEL = PatternFill("solid", fgColor="FFFF00")   # Eingabe: du setzt den Wert
BLU = PatternFill("solid", fgColor="DDEBF7")   # Pflichtdaten: Ist/BWA + Klassifikation
GRY = PatternFill("solid", fgColor="D9D9D9")   # von der Engine erzeugt / strukturell fix
CTX = PatternFill("solid", fgColor="EDE7DD")   # reiner Kontext: Engine liest es nicht
AR  = Font(name="Arial", size=10)
ARB = Font(name="Arial", size=10, bold=True)
ARI = Font(name="Arial", size=10, italic=True)

EUR  = '#,##0" €"'
PCT  = '0.0%'
CORR = '0.00'
DAYS = '0" Tage"'
MON  = '0" Mon."'
HEB  = '0"%"'
DATE = 'DD.MM.YYYY'

def put(ws, r, c, v, yellow=False, blue=False, grey=False, context=False, bold=False, text=False, fmt=None):
    cell = ws.cell(r, c); cell.value = v
    cell.font = ARB if bold else (ARI if context else AR)
    if yellow:    cell.fill = YEL
    elif blue:    cell.fill = BLU
    elif grey:    cell.fill = GRY
    elif context: cell.fill = CTX
    if text:                       # auch leere Textzellen als "@" -> getippte Werte bleiben Text
        cell.number_format = "@"
    elif fmt is not None:
        cell.number_format = fmt
    return cell

def header_sheet(ws, title, subtitle, header):
    put(ws, 1, 1, title, bold=True)
    put(ws, 2, 1, subtitle)
    for j, h in enumerate(header, start=1):
        put(ws, 4, j, h, bold=True, text=True)

def paint(ws, rows, fmt_map, fill):
    """Leere Eingabezellen vorformatieren und faerben (Einheit sichtbar, bevor getippt wird)."""
    for r in rows:
        for c, fm in fmt_map.items():
            cell = ws.cell(r, c); cell.font = AR; cell.number_format = fm; cell.fill = fill

wb = Workbook()

# =========================== 00_Anleitung ===========================
ws = wb.active; ws.title = "00_Anleitung"
steps = [
 ("Liquiditätsmodell — Mappe für einen NEUEN Mandanten (leer)", True),
 ("Eine Mappe je Mandat. Reihenfolge unten einhalten. Gelb/Blau füllst du; Grau/Beige nicht.", False),
 ("", False),
 ("Onboarding-Reihenfolge", True),
 ("1.  01_Positionen: je BWA-/GuV-Zeile eine Zeile. Klassifizieren (grp, cashprofile, driver, ust) + 12 Ist-Monate "
  "(EUR, Aufwand positiv). Es müssen GENAU 12 Monate sein.", False),
 ("2.  02_Zahlungsprofile: Forderungs-/Verbindlichkeitskurven aus dem OPOS-Aging. f1 = Anteil im Monat, f2 = Folgemonat … "
  "Summe je Profil ≈ 1, lückenlos ab f1. Profilname = was du in 01 unter cashprofile referenzierst.", False),
 ("3.  03_Stammdaten & Eröffnung: Forecast-Beginn, Zahlungsmittel-Anfangsbestand (open_cash), KK-Linie/-Zins, Mindestreserve.", False),
 ("4.  04_Annahmen: vorwärtsgerichtete Treiber (menge, preis, tarif …) als Punktwerte + hmax (Forecast-Monate, ≤ Spalten in 05).", False),
 ("5.  05_Finanzierungsplan: Investitionen, Fördermittel, Tilgung, Darlehensaufnahme je Monat. 0 lassen, wo nichts passiert.", False),
 ("6.  08_Steuern: Rechtsform + Status + Sätze. Gesetzliche Sätze sind vorbelegt; mandantenspezifisch sind rechtsform, "
  "gewst_status, ust_status, ust_methode, gewst_hebesatz, entnahme_quote.", False),
 ("7.  Phase 1:  Rscript 01_forecast.R " + DATEINAME + "   — bis Validierung & Cash-Massenerhaltung sauber sind.", False),
 ("8.  Phase 2:  Rscript 02_sensitivitaet.R " + DATEINAME + "   — der Tornado zeigt, welche Annahmen den Tiefpunkt bewegen (braucht 06 NICHT).", False),
 ("9.  06_Verteilungen: NUR die Treiber aus dem Tornado eintragen, mit p_min/p_max. Danach Sobol (liest 06) zum Schärfen; Unwichtiges wieder raus.", False),
 ("10. Phase 3:  Rscript 03_verteilungen.R " + DATEINAME + "   — Verteilungen der gewählten Treiber.", False),
 ("11. 07_Korrelation (nur bei ≥2 abhängigen Treibern) füllen, dann Phase 4:  Rscript 04_abhaengigkeit.R " + DATEINAME, False),
 ("12. Phase 5:  Rscript 05_simulation.R " + DATEINAME + "   — Monte-Carlo (Endbestand-Verteilung, Schwellen). Optional 05b_varianzzerlegung.R.", False),
 ("Optional jederzeit: 09_Darlehen (Kredite mit Tilgungsplan), 10_OPOS (offene Posten, Nahbereich), 01b_wochen.R (Wochensicht).", False),
 ("", False),
 ("Minimal lauffähig (Phase 1): 01, 02, 03, 04, 05, 08. Die Blätter 06/07/09/10 dürfen zunächst leer bleiben.", False),
 ("Engine + Klassifikation bleiben über Mandate gleich: für denselben Typ kannst du die Klassifikation (Spalten C–F in 01) "
  "aus einer bestehenden Mappe übernehmen und nur die Monate neu eintragen.", False),
 ("", False),
]
for i,(t,b) in enumerate(steps, start=1):
    put(ws, i, 1, t, bold=b)
base = len(steps)

put(ws, base+1, 1, "Farbcodierung der Felder", bold=True)
put(ws, base+2, 1, "Gelb — Eingabe: du setzt den Wert (Annahmen, Plan, Eröffnungssalden, Steuersätze, Bandbreiten).", yellow=True)
put(ws, base+3, 1, "Blau — Pflichtdaten, beobachtet/strukturell: Ist-/BWA-Werte und Klassifikation in 01.", blue=True)
put(ws, base+4, 1, "Grau — von der Engine erzeugt oder strukturell fix; stehen lassen (geschätzte cv in 06, Diagonale 1,0 in 07).", grey=True)
put(ws, base+5, 1, "Beige/kursiv — reiner Kontext: Engine liest es nicht; nur Orientierung (untere Zeilen in 03).", context=True)
put(ws, base+6, 1, "Ohne Farbe — Beschriftungen, Schlüssel, Hinweise, Kopfzeilen. Nicht ausfüllen.")

t = base+8
put(ws, t,   1, "Erlaubte Werte (von der Engine erkannt)", bold=True)
put(ws, t+1, 1, "grp  (Spalte C in 01) — bestimmt die GuV-Zuordnung. Genau einer von:")
put(ws, t+2, 1, "      umsatz · bestand · material · betr_ertrag · sopo · personal · sachkosten · afa · zins_e · zins_a · neutral_e · neutral_a · steuer")
put(ws, t+3, 1, "driver  (Spalte E) — bestimmt das Jahreswachstum. Einer von (Tippfehler wächst STILL nicht — also exakt schreiben):")
put(ws, t+4, 1, "      menge_preis (Menge×Preis) · menge · material (Menge×Materialinflation) · tarif · energie · pflegebudget · infl (allg. Inflation) · flat (kein Wachstum)")
put(ws, t+5, 1, "cashprofile  (Spalte D) — Zahlungszeitpunkt. Schlüsselwort ODER ein Profilname aus 02 (unbekannt = harter Abbruch):")
put(ws, t+6, 1, "      inmonth (sofort) · noncash (nie) · annual_jan (Jahresbetrag im M1) · abschlag (gleichmäßig 1/12) · quarterly (Quartalsende) · <Profilname aus 02>")
put(ws, t+7, 1, "ust  (Spalte F) — 0 / 0.07 / 0.19. Nur wirksam, wenn 08:ust_status = pflichtig.")
put(ws, t+8, 1, "Hinweis: 06_Verteilungen nutzt als driver die ANNAHMENAMEN aus 04 (menge, preis, tarif, energie, material, pflegebudget, inflation) — "
                "nicht die Transmissions-Tokens der 01-Spalte (menge_preis/infl/flat).")
put(ws, t+10, 1, "Farben sind rein visuell; die R-Engine liest die Werte unverändert (read_excel ignoriert Zellfarben).")
ws.column_dimensions["A"].width = 122

# =========================== 01_Positionen ===========================
ws = wb.create_sheet("01_Positionen")
months = [f"{IST_JAHR}-{m:02d}" for m in range(1, 13)]
header_sheet(ws, "01_Positionen — Ist-Werte je Monat + Klassifikation (leer)",
    f"Eine Zeile je BWA-Position. Spalten C–F klassifizieren (gültige Werte siehe 00_Anleitung). Monatsspalten = Ist in EUR, "
    f"Aufwand positiv. GENAU 12 Monate. Monatslabels = {IST_JAHR}; oben im Skript IST_JAHR ändern, um das Jahr zu setzen.",
    ["pos", "label", "grp", "cashprofile", "driver", "ust"] + months)
rows = range(5, 5 + N_POS)
for r in rows:                                    # Klassifikation C,D,E als Text-blau
    for c in (3, 4, 5):
        cell = ws.cell(r, c); cell.font = AR; cell.fill = BLU; cell.number_format = "@"
paint(ws, rows, {6: PCT}, BLU)                    # ust
paint(ws, rows, {j: EUR for j in range(7, 19)}, BLU)   # 12 Monatsspalten
ws.freeze_panes = "C5"
for col, w in {"A": 10, "B": 30, "C": 13, "D": 13, "E": 14, "F": 7}.items():
    ws.column_dimensions[col].width = w
for j in range(7, 19):
    ws.column_dimensions[get_column_letter(j)].width = 12

# =========================== 02_Zahlungsprofile ===========================
ws = wb.create_sheet("02_Zahlungsprofile")
header_sheet(ws, "02_Zahlungsprofile — Anteil der Zahlung je Monat ab Leistung/Rechnung (leer)",
    "Aus OPOS-Aging ableiten. f1 = Anteil im Monat, f2 = Folgemonat usw. Summe je Profil ≈ 1, lückenlos ab f1. "
    "Der Profilname ist genau das, was du in 01 unter cashprofile schreibst.",
    ["profile", "f1", "f2", "f3", "f4", "Anwendung"])
paint(ws, range(5, 15), {2: PCT, 3: PCT, 4: PCT, 5: PCT}, YEL)
put(ws, 16, 1, "Schlüsselwörter (kein Lag, brauchen keine Zeile): inmonth · noncash · annual_jan · abschlag · quarterly")
ws.column_dimensions["A"].width = 14; ws.column_dimensions["F"].width = 42

# =========================== 03_Stammdaten_Eroeffnung ===========================
ws = wb.create_sheet("03_Stammdaten_Eroeffnung")
header_sheet(ws, "03_Stammdaten & Eröffnung (leer)",
    "Gelb = von der Engine gelesen, ausfüllen. Beige/kursiv = reiner Kontext (nicht gerechnet).",
    ["key", "wert", "Hinweis"])
stamm = [   # key, Hinweis, art(none|yellow|ctx), text?
 ("Mandant",         "Name des Mandanten",                                  "none",   True),
 ("Branche",         "Branche / Versorgungsstufe",                          "none",   True),
 ("Ist-Zeitraum",    f"z. B. {IST_JAHR}-01 bis {IST_JAHR}-12",              "none",   True),
 ("Forecast-Beginn", f"erster Forecast-Monat, z. B. {IST_JAHR+1}-01",       "yellow", True),
 ("open_cash",       "Zahlungsmittel zu Forecast-Beginn (= Endbestand Vorperiode)", "yellow", False),
 ("kk_limit",        "Kontokorrentlinie",                                   "yellow", False),
 ("kk_zins",         "KK-Zinssatz p.a.",                                    "yellow", False),
 ("mindestreserve",  "angestrebte Mindestliquidität",                       "yellow", False),
 ("Forderungen",     "Kontext (nicht gerechnet)",                           "ctx",    False),
 ("Verbindlichkeiten","Kontext",                                            "ctx",    False),
 ("Vorraete",        "Kontext",                                             "ctx",    False),
 ("Rueckstellungen", "Kontext",                                             "ctx",    False),
 ("Darlehen",        "Kontext",                                             "ctx",    False),
]
_f03 = {"open_cash": EUR, "kk_limit": EUR, "mindestreserve": EUR, "kk_zins": PCT,
        "Forderungen": EUR, "Verbindlichkeiten": EUR, "Vorraete": EUR, "Rueckstellungen": EUR, "Darlehen": EUR}
r = 5
for k, h, kind, txt in stamm:
    is_ctx = (kind == "ctx")
    put(ws, r, 1, k, context=is_ctx)
    put(ws, r, 2, None, yellow=(kind == "yellow"), context=is_ctx, text=txt, fmt=_f03.get(k))
    put(ws, r, 3, h, context=is_ctx)
    r += 1
ws.column_dimensions["A"].width = 18; ws.column_dimensions["B"].width = 22; ws.column_dimensions["C"].width = 48

# =========================== 04_Annahmen ===========================
ws = wb.create_sheet("04_Annahmen")
header_sheet(ws, "04_Annahmen — vorwärtsgerichtete Treiber (leer)",
    "Punktwerte des Szenarios. Ab Phase 3 wird der ausgewählte Treiber durch eine Verteilung ersetzt.",
    ["key", "wert", "Bedeutung"])
ann = [
 ("menge",        "Fallzahl-/Mengenentwicklung vs. Vorjahr"),
 ("preis",        "Erlöspreis / Landesbasisfallwert"),
 ("tarif",        "Tarifabschluss Personal"),
 ("energie",      "Energiepreis-Pfad"),
 ("material",     "Materialkosteninflation (zusätzlich zur Menge)"),
 ("pflegebudget", "Anpassung Pflegebudget"),
 ("inflation",    "allg. Kostentrend (übrige Sachkosten)"),
 ("hmax",         "Forecast-Monate (ganzzahlig, ≤ Spaltenzahl in 05)"),
]
r = 5
for k, b in ann:
    put(ws, r, 1, k); put(ws, r, 2, None, yellow=True, fmt=(MON if k == "hmax" else PCT)); put(ws, r, 3, b)
    r += 1
ws.column_dimensions["A"].width = 14; ws.column_dimensions["B"].width = 10; ws.column_dimensions["C"].width = 48

# =========================== 05_Finanzierungsplan ===========================
ws = wb.create_sheet("05_Finanzierungsplan")
header_sheet(ws, "05_Finanzierungsplan — je Forecast-Monat (EUR)",
    "Auszahlungen/Zuflüsse außerhalb der GuV. 0 lassen, wo nichts passiert. M1 = erster Forecast-Monat.",
    ["Position"] + [f"M{i}" for i in range(1, 13)])
r = 5
for name in ["Investitionen", "Foerdermittel", "Tilgung", "Darlehensaufnahme"]:
    put(ws, r, 1, name)
    for j in range(2, 14):
        put(ws, r, j, 0, yellow=True, fmt=EUR)
    r += 1
ws.column_dimensions["A"].width = 20
for j in range(2, 14):
    ws.column_dimensions[get_column_letter(j)].width = 12

# =========================== 06_Verteilungen ===========================
ws = wb.create_sheet("06_Verteilungen")
header_sheet(ws, "06_Verteilungen — Unsicherheit der AUSGEWÄHLTEN Treiber (leer, Phase 3)",
    "ERST nach dem Tornado (Phase 2) füllen: nur die Treiber eintragen, die den Tiefpunkt bewegen. "
    "mode = Punktwert aus 04. p_min/p_max = epistemische Bandbreite (PERT). cv = aleatorische Monatsstreuung (0 bei reinen Jahresgrößen).",
    ["driver", "p_min", "p_max", "cv", "Hinweis"])
paint(ws, range(5, 13), {2: PCT, 3: PCT, 4: PCT}, YEL)
put(ws, 14, 1, "Treiber ohne Zeile bleiben deterministisch. driver = Annahmename aus 04 "
               "(menge, preis, tarif, energie, material, pflegebudget, inflation).")
ws.column_dimensions["A"].width = 14; ws.column_dimensions["E"].width = 60

# =========================== 07_Korrelation ===========================
ws = wb.create_sheet("07_Korrelation")
header_sheet(ws, "07_Korrelation — Abhängigkeit der epistemischen Treiber (leer, Phase 4)",
    "Erst sinnvoll bei ≥2 abhängigen Treibern. Zeilen UND Spalten = dieselben Treiber wie in 06, gleiche Reihenfolge.",
    ["driver"])
put(ws, 6, 1, "Aufbau nach 06: in A5 abwärts die Treiber, in der Kopfzeile (B4 …) dieselben Namen in gleicher Reihenfolge. "
              "Diagonale 1,0 (grau), übrige Zellen gelb mit Wert in [-1, 1], symmetrisch.")
ws.column_dimensions["A"].width = 14

# =========================== 08_Steuern ===========================
ws = wb.create_sheet("08_Steuern")
header_sheet(ws, "08_Steuern — Rechtsform, Status und Sätze",
    "Gesetzliche Sätze sind vorbelegt (selten zu ändern). Mandantenspezifisch: rechtsform, gewst_status, ust_status, "
    "ust_methode, gewst_hebesatz, entnahme_quote.",
    ["key", "wert", "Hinweis"])
tax = [   # key, default, Hinweis, kind
 ("rechtsform",       "kapital",    "kapital | personen | einzel  (transparent = personen/einzel)",        "sw"),
 ("gewst_status",     "gewerblich", "gewerblich | freiberuflich | gemeinnuetzig",                          "sw"),
 ("ust_status",       "pflichtig",  "pflichtig | frei (§4 Nr.14 Heilbehandlung = frei)",                    "sw"),
 ("ust_methode",      "soll",       "soll (nach Leistung) | ist (nach Vereinnahmung, §20 UStG)",            "sw"),
 ("kst_satz",         0.15,         "Körperschaftsteuersatz — gesetzlich (nur Kapitalgesellschaft)",        "pct"),
 ("soli_satz",        0.055,        "Solidaritätszuschlag auf die KSt — gesetzlich",                        "pct"),
 ("gewst_messzahl",   0.035,        "Steuermesszahl Gewerbesteuer — gesetzlich",                            "pct"),
 ("gewst_hebesatz",   400,          "Hebesatz in % der Gemeinde (z. B. 400 = 400%) — mandantenspezifisch",  "heb"),
 ("gewst_freibetrag", 24500,        "GewSt-Freibetrag — gesetzlich (nur EK/Personengesellschaft)",          "eur"),
 ("ust_lag",          1,            "Monate bis USt-Abführung (1; mit Dauerfristverlängerung 2)",           "mon"),
 ("entnahme_quote",   0.30,         "Steuerentnahme als Anteil des Plangewinns (nur transparente Formen)",  "pct"),
]
_fmt08 = {"pct": PCT, "heb": HEB, "eur": EUR, "mon": MON}
r = 5
for k, v, h, kind in tax:
    is_text = (kind == "sw")
    put(ws, r, 1, k)
    put(ws, r, 2, v, yellow=True, text=is_text, fmt=(None if is_text else _fmt08.get(kind)))
    put(ws, r, 3, h)
    r += 1
ws.column_dimensions["A"].width = 18; ws.column_dimensions["B"].width = 14; ws.column_dimensions["C"].width = 60

# =========================== 09_Darlehen ===========================
ws = wb.create_sheet("09_Darlehen")
header_sheet(ws, "09_Darlehen — explizite Kredite mit Tilgungsplan (optional, leer)",
    "Je Zeile ein Kredit. tilgungsart: annuitaet | linear | endfaellig | nur_zins. start_m = Auszahlungsmonat (1..hmax); "
    "<=0 = bereits valutiert. disagio mindert nur die Auszahlung. Leer = ohne Darlehensmodul.",
    ["id", "betrag", "zinssatz", "tilgungsart", "laufzeit_m", "start_m", "disagio"])
paint(ws, range(5, 11), {1: "@", 2: EUR, 3: PCT, 4: "@", 5: MON, 6: MON, 7: PCT}, YEL)
ws.column_dimensions["A"].width = 16
for col, w in {"B": 12, "C": 10, "D": 13, "E": 12, "F": 9, "G": 9}.items():
    ws.column_dimensions[col].width = w

# =========================== 10_OPOS ===========================
ws = wb.create_sheet("10_OPOS")
header_sheet(ws, "10_OPOS — offene Forderungen/Verbindlichkeiten mit Zahlungsziel (optional, leer)",
    "Aus der OPOS-Liste. art = forderung | verbindlichkeit. betrag brutto. Termin per faellig_am (ISO-Datum) ODER "
    "zahlungsziel_tage. wahrscheinlichkeit = Quote (default 1). verzug_tage (default 0). ust_satz nur bei ust_methode=ist. "
    "Leer = statistische Profile (Carry-in).",
    ["art", "bezeichnung", "betrag", "faellig_am", "zahlungsziel_tage", "wahrscheinlichkeit", "verzug_tage", "ust_satz"])
paint(ws, range(5, 31), {1: "@", 2: "@", 3: EUR, 4: DATE, 5: DAYS, 6: PCT, 7: DAYS, 8: PCT}, YEL)
ws.column_dimensions["A"].width = 15
for col, w in {"B": 26, "C": 12, "D": 13, "E": 17, "F": 17, "G": 12, "H": 10}.items():
    ws.column_dimensions[col].width = w

wb.save(DATEINAME)
print("Leere Mandatsmappe geschrieben:", DATEINAME)
print("Blätter:", wb.sheetnames)
