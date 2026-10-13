# ============================================================================
#  tests/test_new_mandate.R  —  SPEZIFIKATION (rot zuerst) fuer
#  phase-10-generator
#
#  Die Tests MUESSEN gegen den aktuellen Stand FEHLSCHLAGEN (R/new_mandate.R
#  existiert noch nicht -> source() bricht ab; jeder Test spezifiziert
#  zusaetzlich das Zielverhalten). Kein skip(), kein succeed(), keine
#  Existenz-Weiche, keine Tier-1-/Anker-Literale.
#
#  Vertrag (handoff/phase-10-generator.json; Vorlage
#  legacy/build_workbook_neuer_mandant.py):
#   - new_mandate(pfad, ist_jahr = 2025, n_pos = 60) schreibt via openxlsx2
#     eine LEERE Mandatsmappe nach exakt dem Vertrag der Python-Vorlage:
#     gleiche 11 Blattnamen in gleicher Reihenfolge, je Blatt Titel Z1 /
#     Untertitel Z2 / Kopfzeile Z4 (die Engine liest read_excel(skip = 3));
#     gefuellt bleiben NUR strukturelle Schluessel/Beschriftungen, die
#     gesetzlichen Steuersaetze in 08 und der Finanzierungsplan 05 mit 0;
#     alles Mandantenspezifische ist leer, aber vorformatiert (Zahlformate)
#     und farbcodiert (gelb FFFF00 Eingabe, blau DDEBF7 Pflichtdaten, grau
#     D9D9D9 strukturell, Kontext EDE7DD). Rueckgabe: list(pfad, blaetter).
#     Deterministisch: kein Zeitstempel und kein pfadabhaengiger Inhalt in
#     den Zellen (zwei Laeufe in verschiedene tempfiles -> identischer
#     Zellinhalt).
#
#  ORAKEL-PRINZIP: fixtures/Mandant_NEU.xlsx ist die eingefrorene
#  Python-Referenzmappe — ein Setup-Snapshot des Laufs von
#  legacy/build_workbook_neuer_mandant.py (python3/openpyxl; IST_JAHR = 2025,
#  N_POS = 60), beim Einfrieren mtime-gleich zur legacy-Quelle gesetzt.
#  Sie ist die WAHRHEIT: der Test erzeugt das R-Generat in tempfile() und
#  vergleicht ZELLWEISE GEGEN DIE REFERENZ (wb_load einmal memoisiert),
#  nicht gegen eigene Literale. Eigene Literale existieren nur als
#  Referenz-WAECHTER (damit ein Pin nicht still auf eine falsche Zelle
#  driftet) und fuer die Parameter-Gegenproben (n_pos = 5, ist_jahr = 2027).
#
#  Leseformen (dokumentiert; im openxlsx2 des Repos — lokal 1.27 — verfuegbar):
#   - ROHZELLEN: openxlsx2::wb_to_df(wb, sheet, col_names = FALSE,
#     skip_empty_rows = FALSE, skip_empty_cols = FALSE) liefert das
#     unverschobene Zellgitter: rownames = Blattzeilennummern, Spaltennamen
#     = Excel-Spaltenbuchstaben. So sind Z1/Z2/Z4 und beliebige Koordinaten
#     direkt adressierbar (bewusst OHNE skip: skip = 3 ist der
#     Engine-Lesepfad, hier wird die Mappe roh geprueft). Werte werden fuer
#     den Vergleich ueber as.character normalisiert — Referenz und Generat
#     laufen durch DIESELBE Leseform, damit ist der Vergleich typfest;
#     zusaetzlich prueft je ein getypter wb_to_df-Bereichs-Read (05/08),
#     dass die Strukturwerte numerisch ankommen.
#   - STYLES: openxlsx2::wb_get_cell_style(wb, sheet, dims) liefert den
#     cellXfs-Index der Zelle; aufgeloest wird ueber die style-Objekte des
#     wbWorkbook (wb$styles_mgr$styles$cellXfs / $numFmts / $fills —
#     XML-Strings) per Regex: numFmtId -> formatCode (Builtins normalisiert,
#     insbesondere 49 -> "@"; XML-Entities entescaped), fillId -> fgColor-rgb,
#     verglichen als RGB6 OHNE Alphakanal (openpyxl schreibt Alpha "00",
#     openxlsx2 "FF" — dieselbe Farbe). Harter Vergleich Generat == Referenz
#     an derselben Koordinate, KEIN skip.
#
#  Hinweis CORR ("0.00"): diese Format-Klasse ist in der Python-Vorlage
#  DEFINIERT, aber an KEINE Zelle vergeben (07_Korrelation traegt keine
#  vorformatierten Zellen) — es existiert keine Referenzkoordinate, darum
#  bewusst kein Pin. Gepinnt sind die tatsaechlich vergebenen Klassen
#  EUR, PCT, DAYS, MON, HEB, DATE und Text "@".
#
#  Laufzeit klein: Referenz-Workbook und alle Generate werden lazy
#  memoisiert (delayedAssign-Muster wie tests/test_export_xlsx.R); das
#  Schreiben einer leeren Mappe ist billig, keine Engine, keine Simulation.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "new_mandate.R"))   # <- existiert noch nicht: rot zuerst

.REF <- file.path(.root, "fixtures", "Mandant_NEU.xlsx")

# ---------------------------------------------------------------------------
# Leseform 1: Rohzellen-Gitter (s. Kopf). Zugriff ueber Blattzeilennummer
# (rownames-Match) und Excel-Spaltenbuchstabe — nie ueber Positionsindizes,
# damit ein Leseform-Drift laut statt still scheitert.
# ---------------------------------------------------------------------------
.roh <- function(wb, blatt) {
  openxlsx2::wb_to_df(wb, sheet = blatt, col_names = FALSE,
                      skip_empty_rows = FALSE, skip_empty_cols = FALSE)
}

.zelle_a1 <- function(roh, a1) {
  sp <- gsub("[0-9]", "", a1)
  z  <- as.integer(gsub("[A-Z]", "", a1))
  i  <- match(as.character(z), rownames(roh))
  if (is.na(i) || !sp %in% colnames(roh)) return(NA_character_)
  v <- roh[[sp]][i]
  if (is.na(v)) NA_character_ else as.character(v)
}

# Eine Blattzeile als benannter character-Vektor der Nicht-NA-Zellen
# (Name = Spaltenbuchstabe), namenssortiert — vergleichbar per identical.
.zeilen_map <- function(roh, zeile) {
  i <- match(as.character(zeile), rownames(roh))
  if (is.na(i)) return(character(0))
  v <- vapply(colnames(roh), function(sp) {
    x <- roh[[sp]][i]
    if (is.na(x)) NA_character_ else as.character(x)
  }, character(1))
  v <- v[!is.na(v)]
  v[order(names(v))]
}

# ALLE Nicht-NA-Zellen eines Blatts als benannter Vektor "zeile|spalte" ->
# Wert. identical(Generat, Referenz) prueft damit Position + Wert UND
# verbietet zugleich Extra-Zellen im Generat.
.blatt_map <- function(roh) {
  out <- character(0)
  for (sp in colnames(roh)) {
    v <- roh[[sp]]
    n <- which(!is.na(v))
    if (length(n))
      out[paste0(rownames(roh)[n], "|", sp)] <- as.character(v[n])
  }
  out[order(names(out))]
}

# ---------------------------------------------------------------------------
# Leseform 2: Zellstil (Zahlformat + Fuellfarbe) — s. Kopf. Builtin-numFmts
# werden auf ihren kanonischen Format-Code normalisiert, damit "@" als
# Builtin 49 (openpyxl-Referenz) und "@" als custom numFmt (moegliche
# openxlsx2-Schreibform) gleich verglichen werden.
# ---------------------------------------------------------------------------
.BUILTIN_NUMFMT <- c(`0` = "General", `1` = "0", `2` = "0.00", `3` = "#,##0",
                     `4` = "#,##0.00", `9` = "0%", `10` = "0.00%",
                     `14` = "mm-dd-yy", `49` = "@")

.unescape <- function(x) {
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'",  x, fixed = TRUE)
  x <- gsub("&lt;",   "<",  x, fixed = TRUE)
  x <- gsub("&gt;",   ">",  x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

.xml_attr1 <- function(xml, attr) {
  m <- regmatches(xml, regexpr(paste0(attr, '="[^"]*"'), xml))
  if (!length(m)) return(NA_character_)
  sub('"$', "", sub(paste0("^", attr, '="'), "", m))
}

.stil <- function(wb, blatt, zelle) {
  sid <- suppressWarnings(as.integer(
    openxlsx2::wb_get_cell_style(wb, sheet = blatt, dims = zelle)))
  if (!length(sid) || is.na(sid)) sid <- 0L
  xfs <- wb$styles_mgr$styles$cellXfs
  xf  <- if (sid + 1L <= length(xfs)) xfs[[sid + 1L]] else ""

  nid <- .xml_attr1(xf, "numFmtId")
  if (is.na(nid)) nid <- "0"
  nf  <- wb$styles_mgr$styles$numFmts
  hit <- if (length(nf)) nf[grepl(paste0('numFmtId="', nid, '"'), nf,
                                  fixed = TRUE)] else character(0)
  numfmt <- if (length(hit)) {
    .unescape(.xml_attr1(hit[[1]], "formatCode"))
  } else if (nid %in% names(.BUILTIN_NUMFMT)) {
    unname(.BUILTIN_NUMFMT[[nid]])
  } else {
    paste0("builtin#", nid)
  }

  fid  <- .xml_attr1(xf, "fillId")
  fill <- "keinfill"
  if (!is.na(fid)) {
    fills <- wb$styles_mgr$styles$fills
    i <- as.integer(fid) + 1L
    if (i >= 1L && i <= length(fills)) {
      m <- regmatches(fills[[i]],
                      regexpr('fgColor[^>]*rgb="[0-9A-Fa-f]+"', fills[[i]]))
      if (length(m)) {
        h <- .xml_attr1(m, "rgb")
        fill <- toupper(substring(h, nchar(h) - 5L))   # RGB6 ohne Alpha
      }
    }
  }
  list(numfmt = numfmt, fill = fill)
}

# ---------------------------------------------------------------------------
# Gepinnte Koordinaten-Konstanten (aus legacy/build_workbook_neuer_mandant.py
# abgeleitet; jede Erwartung wird trotzdem GEGEN DIE REFERENZ geprueft, die
# Literale hier sind nur Referenz-Waechter bzw. Adressen).
# ---------------------------------------------------------------------------

# Leere Eingabezellen (put/paint fuellt sie NICHT): je relevantem Blatt 3 Pins.
#  01: paint rows 5..64 (n_pos = 60); C = grp (Text-blau), G = 1. Ist-Monat,
#      R = 12. Ist-Monat (Spalten 7..18) -> C5, G5, R64.
#  02: paint rows 5..14, Spalten B..E (f1..f4 PCT gelb)      -> B5, C5, E14.
#  03: wert-Spalte B; Zeilen 5 Mandant .. 12 mindestreserve  -> B8 (Forecast-
#      Beginn), B9 (open_cash), B12 (mindestreserve).
#  04: wert-Spalte B; Zeilen 5 menge .. 12 hmax              -> B5, B11, B12.
#  06: paint rows 5..12, Spalten B..D (p_min/p_max/cv)       -> B5, C5, D12.
#  09: paint rows 5..10, Spalten A..G                        -> A5, B5, G10.
#  10: paint rows 5..30, Spalten A..H                        -> A5, C5, H30.
.LEER_PINS <- list(
  "01_Positionen"            = c("C5", "G5", "R64"),
  "02_Zahlungsprofile"       = c("B5", "C5", "E14"),
  "03_Stammdaten_Eroeffnung" = c("B8", "B9", "B12"),
  "04_Annahmen"              = c("B5", "B11", "B12"),
  "06_Verteilungen"          = c("B5", "C5", "D12"),
  "09_Darlehen"              = c("A5", "B5", "G10"),
  "10_OPOS"                  = c("A5", "C5", "H30")
)

# Zahlformat-Pins: je vergebener Format-Klasse eine Stichprobenzelle
# (08: Zeile 5 rechtsform .. Zeile 15 entnahme_quote; 10: Datenzeile 5).
# CORR ("0.00") ist an keine Zelle vergeben — s. Kopf, bewusst kein Pin.
.EUR <- paste0('#,##0" ', intToUtf8(0x20AC), '"')   # '#,##0" <EUR>"' — Euro via Codepoint, kein Nicht-ASCII im Quelltext
.FMT_PINS <- list(
  list(klasse = "EUR",  blatt = "08_Steuern", zelle = "B13", code = .EUR),        # gewst_freibetrag 24500
  list(klasse = "PCT",  blatt = "08_Steuern", zelle = "B9",  code = "0.0%"),      # kst_satz 0.15
  list(klasse = "DAYS", blatt = "10_OPOS",    zelle = "E5",  code = '0" Tage"'),  # zahlungsziel_tage (leer)
  list(klasse = "MON",  blatt = "08_Steuern", zelle = "B14", code = '0" Mon."'),  # ust_lag 1
  list(klasse = "HEB",  blatt = "08_Steuern", zelle = "B12", code = '0"%"'),      # gewst_hebesatz 400
  list(klasse = "DATE", blatt = "10_OPOS",    zelle = "D5",  code = "DD.MM.YYYY"),# faellig_am (leer)
  list(klasse = "TEXT", blatt = "08_Steuern", zelle = "B5",  code = "@")          # rechtsform "kapital"
)

# Fuellfarben-Pins: je Farbklasse eine Stichprobenzelle.
#  GRAU: einzige D9D9D9-Zelle der Mappe ist die Legenden-Zeile "Grau — ..."
#  in 00_Anleitung, Zeile base+4 mit base = 21 Steps -> A25 (Text-Waechter
#  unten sichert die Zeilenzaehlung ab).
#  KONTEXT: 03, Zeile 13 "Forderungen" (erste ctx-Zeile), Spalte A.
.FILL_PINS <- list(
  list(klasse = "GELB",    blatt = "04_Annahmen",              zelle = "B5",  rgb = "FFFF00"),
  list(klasse = "BLAU",    blatt = "01_Positionen",            zelle = "C5",  rgb = "DDEBF7"),
  list(klasse = "GRAU",    blatt = "00_Anleitung",             zelle = "A25", rgb = "D9D9D9"),
  list(klasse = "KONTEXT", blatt = "03_Stammdaten_Eroeffnung", zelle = "A13", rgb = "EDE7DD")
)

# ---------------------------------------------------------------------------
# Lazy memoisierte Workbooks: Referenz einmal, Generate einmal je Parametrik
# (delayedAssign-Muster wie test_export_xlsx.R).
# ---------------------------------------------------------------------------
.cache <- new.env(parent = emptyenv())
delayedAssign("ref", local({
  wb <- openxlsx2::wb_load(.REF)
  list(wb = wb, blaetter = unname(openxlsx2::wb_get_sheet_names(wb)))
}), assign.env = .cache)
delayedAssign("gen", local({
  tf <- tempfile(fileext = ".xlsx")
  r  <- new_mandate(tf)
  list(rueck = r, pfad = tf, wb = openxlsx2::wb_load(tf))
}), assign.env = .cache)
delayedAssign("gen_b", local({
  tf <- tempfile(fileext = ".xlsx")
  r  <- new_mandate(tf)
  list(rueck = r, pfad = tf, wb = openxlsx2::wb_load(tf))
}), assign.env = .cache)
delayedAssign("gen5", local({
  tf <- tempfile(fileext = ".xlsx")
  r  <- new_mandate(tf, n_pos = 5)
  list(rueck = r, pfad = tf, wb = openxlsx2::wb_load(tf))
}), assign.env = .cache)
delayedAssign("gen27", local({
  tf <- tempfile(fileext = ".xlsx")
  r  <- new_mandate(tf, ist_jahr = 2027)
  list(rueck = r, pfad = tf, wb = openxlsx2::wb_load(tf))
}), assign.env = .cache)
ref   <- function() .cache$ref
gen   <- function() .cache$gen
gen_b <- function() .cache$gen_b
gen5  <- function() .cache$gen5
gen27 <- function() .cache$gen27

# ===========================================================================
#  (1) Rueckgabe-Vertrag und Blattliste
# ===========================================================================

# ---------------------------------------------------------------------------
# pfad existiert und ist der uebergebene Zielpfad; blaetter ist identical zur
# Blattliste der Referenz UND zur openxlsx2-Blattliste der geschriebenen
# Datei. Referenz-Waechter: 11 Blaetter, alle in Pins adressierten Blaetter
# existieren (sonst waeren die Folgetests blind).
# ---------------------------------------------------------------------------
test_that("Rueckgabe: pfad existiert, blaetter identical Referenz-Blattliste == Datei-Blattliste", {
  g <- gen()
  expect_identical(g$rueck$pfad, g$pfad)
  expect_true(file.exists(g$rueck$pfad))
  expect_length(ref()$blaetter, 11L)
  expect_true(all(c(names(.LEER_PINS), "00_Anleitung", "05_Finanzierungsplan",
                    "07_Korrelation", "08_Steuern") %in% ref()$blaetter))
  expect_identical(g$rueck$blaetter, ref()$blaetter)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(g$wb)), ref()$blaetter)
})

# ===========================================================================
#  (2) Titel Z1 / Untertitel Z2 / Kopfzeile Z4 — je Blatt gegen die Referenz
# ===========================================================================

# ---------------------------------------------------------------------------
# Fuer JEDES Blatt der Referenz: die Nicht-NA-Zellen der Zeilen 1, 2 und 4
# sind als (Spalte -> Wert)-Map identical zur Referenz — das deckt Titel,
# Untertitel und die KOMPLETTE Kopfzeile (inkl. der 12 Ist-Monats-Koepfe
# in 01 mit dem Default ist_jahr = 2025) und verbietet Extra-Kopfzellen.
# ---------------------------------------------------------------------------
test_that("Je Blatt: Zeile 1 (Titel), Zeile 2 (Untertitel), Zeile 4 (Kopfzeile) identical zur Referenz", {
  for (b in ref()$blaetter) {
    rr <- .roh(ref()$wb, b)
    gg <- .roh(gen()$wb, b)
    for (z in c(1L, 2L, 4L)) {
      expect_identical(.zeilen_map(gg, z), .zeilen_map(rr, z),
                       info = sprintf("%s Zeile %d", b, z))
    }
  }
})

# ===========================================================================
#  (3) Strukturzellen 08_Steuern: gesetzliche Steuersaetze bleiben gefuellt
# ===========================================================================

# ---------------------------------------------------------------------------
# ALLE Nicht-NA-Zellen der Referenz sind im Generat identisch (Position +
# Wert; die Map verbietet zugleich Extra-Zellen). Kreuz-Waechter gegen ein
# blindes Orakel: die Referenz traegt in der wert-Spalte B9:B15 mindestens
# 3 numerische Satz-Zellen (tatsaechlich 7: kst/soli/messzahl/hebesatz/
# freibetrag/ust_lag/entnahme_quote), und der getypte Read beweist, dass
# die Werte im Generat NUMERISCH ankommen (nicht als Text).
# ---------------------------------------------------------------------------
test_that("08_Steuern: alle Nicht-NA-Referenzzellen im Generat identisch; Saetze numerisch", {
  mr <- .blatt_map(.roh(ref()$wb, "08_Steuern"))
  mg <- .blatt_map(.roh(gen()$wb, "08_Steuern"))
  expect_identical(mg, mr)
  br <- openxlsx2::wb_to_df(ref()$wb, sheet = "08_Steuern",
                            dims = "B9:B15", col_names = FALSE)[["B"]]
  bg <- openxlsx2::wb_to_df(gen()$wb, sheet = "08_Steuern",
                            dims = "B9:B15", col_names = FALSE)[["B"]]
  expect_true(is.numeric(br))
  expect_gte(sum(!is.na(br)), 3L)          # Kreuz-Waechter: sonst blind
  expect_true(is.numeric(bg))
  expect_identical(bg, br)
})

# ===========================================================================
#  (4) Strukturzellen 05_Finanzierungsplan: die neutralen Nullen
# ===========================================================================

# ---------------------------------------------------------------------------
# ALLE Nicht-NA-Zellen der Referenz im Generat identisch (Position + Wert,
# keine Extra-Zellen). Kreuz-Waechter: der Datenbereich B5:M8 (4 Positionen
# x 12 Monate) traegt in der Referenz mindestens 1 Null-Zelle (tatsaechlich
# 48), und der getypte Read beweist numerische Nullen im Generat.
# ---------------------------------------------------------------------------
test_that("05_Finanzierungsplan: alle Nicht-NA-Referenzzellen im Generat identisch; Nullen numerisch", {
  mr <- .blatt_map(.roh(ref()$wb, "05_Finanzierungsplan"))
  mg <- .blatt_map(.roh(gen()$wb, "05_Finanzierungsplan"))
  expect_identical(mg, mr)
  dr <- openxlsx2::wb_to_df(ref()$wb, sheet = "05_Finanzierungsplan",
                            dims = "B5:M8", col_names = FALSE)
  dg <- openxlsx2::wb_to_df(gen()$wb, sheet = "05_Finanzierungsplan",
                            dims = "B5:M8", col_names = FALSE)
  vr <- unlist(dr, use.names = FALSE)
  vg <- unlist(dg, use.names = FALSE)
  expect_true(is.numeric(vr))
  expect_gte(sum(vr == 0, na.rm = TRUE), 1L)   # Kreuz-Waechter: sonst blind
  expect_true(is.numeric(vg))
  expect_identical(vg, vr)
})

# ===========================================================================
#  (5) Leerheit: Mandantenspezifisches bleibt leer
# ===========================================================================

# ---------------------------------------------------------------------------
# An den gepinnten Eingabezellen (3 je relevantem Blatt, Ableitung s.
# .LEER_PINS) ist die REFERENZ NA (Waechter: Pin sitzt auf einer wirklich
# leeren Eingabezelle) und das GENERAT ebenso NA.
# ---------------------------------------------------------------------------
test_that("Leerheit: gepinnte Eingabezellen sind NA — in Referenz (Waechter) und Generat", {
  for (b in names(.LEER_PINS)) {
    rr <- .roh(ref()$wb, b)
    gg <- .roh(gen()$wb, b)
    for (a1 in .LEER_PINS[[b]]) {
      expect_true(is.na(.zelle_a1(rr, a1)),
                  info = sprintf("Referenz-Waechter %s!%s", b, a1))
      expect_true(is.na(.zelle_a1(gg, a1)),
                  info = sprintf("Generat %s!%s", b, a1))
    }
  }
})

# ===========================================================================
#  (6) Zahlformate: je Format-Klasse eine gepinnte Stichprobe
# ===========================================================================

# ---------------------------------------------------------------------------
# Je Pin: der Referenz-Waechter prueft, dass die Referenzzelle wirklich den
# Format-Code der Klasse traegt (Pin-Drift wird laut), dann muss das
# number_format des Generats an derselben Koordinate identical zur Referenz
# sein. Leseform s. Kopf (wb_get_cell_style + wb$styles_mgr, KEIN skip).
# ---------------------------------------------------------------------------
test_that("Zahlformate: gepinnte Stichproben (EUR/PCT/DAYS/MON/HEB/DATE/@) identical zur Referenz", {
  for (p in .FMT_PINS) {
    sr <- .stil(ref()$wb, p$blatt, p$zelle)
    sg <- .stil(gen()$wb, p$blatt, p$zelle)
    expect_identical(sr$numfmt, p$code,
                     info = sprintf("Referenz-Waechter %s %s!%s", p$klasse, p$blatt, p$zelle))
    expect_identical(sg$numfmt, sr$numfmt,
                     info = sprintf("Generat %s %s!%s", p$klasse, p$blatt, p$zelle))
  }
})

# ===========================================================================
#  (7) Fuellfarben: je Farbklasse eine gepinnte Stichprobe + Kreuz-Check
# ===========================================================================

# ---------------------------------------------------------------------------
# Je Pin: Referenz-Waechter (Zelle traegt wirklich das RGB6 der Klasse; fuer
# GRAU/KONTEXT zusaetzlich Text-Waechter gegen Zeilenzaehl-Drift), dann Fill
# des Generats identical zur Referenz. Kreuz-Check: die gelbe Referenzzelle
# 05!B5 (Finanzierungsplan-Null) ist im Generat gelb wie die Referenz und
# traegt NICHT blau.
# ---------------------------------------------------------------------------
test_that("Fuellfarben: gepinnte Stichproben (FFFF00/DDEBF7/D9D9D9/EDE7DD) identical zur Referenz", {
  for (p in .FILL_PINS) {
    sr <- .stil(ref()$wb, p$blatt, p$zelle)
    sg <- .stil(gen()$wb, p$blatt, p$zelle)
    expect_identical(sr$fill, p$rgb,
                     info = sprintf("Referenz-Waechter %s %s!%s", p$klasse, p$blatt, p$zelle))
    expect_identical(sg$fill, sr$fill,
                     info = sprintf("Generat %s %s!%s", p$klasse, p$blatt, p$zelle))
  }
  # Text-Waechter der zaehl-abhaengigen Pins:
  expect_true(startsWith(.zelle_a1(.roh(ref()$wb, "00_Anleitung"), "A25"), "Grau"))
  expect_identical(.zelle_a1(.roh(ref()$wb, "03_Stammdaten_Eroeffnung"), "A13"),
                   "Forderungen")
  # Kreuz-Check gelb != blau:
  sr5 <- .stil(ref()$wb, "05_Finanzierungsplan", "B5")
  sg5 <- .stil(gen()$wb, "05_Finanzierungsplan", "B5")
  expect_identical(sr5$fill, "FFFF00")
  expect_identical(sg5$fill, sr5$fill)
  expect_false(identical(sg5$fill, "DDEBF7"))
})

# ===========================================================================
#  (8) Parameter n_pos: Zahl der vorformatierten Leerzeilen in 01_Positionen
# ===========================================================================

# ---------------------------------------------------------------------------
# Kriterium (aus der Python-paint-Logik gepinnt): Zeile r ist vorformatiert
# <=> die Klassifikationszelle C_r traegt Fill DDEBF7 und Zahlformat "@"
# (Spalten C..E Text-blau, F PCT, G..R EUR — C ist der Marker). Waechter am
# Referenz-Rand: N_POS = 60 -> letzte vorformatierte Zeile 64, Zeile 65
# nicht mehr; dasselbe gilt fuer das Default-Generat. Generat mit n_pos = 5
# (zweites wb_load des EIGENEN Outputs): genau die Zeilen 5..9 sind
# vorformatiert, Zeile 10 nicht.
# ---------------------------------------------------------------------------
test_that("n_pos: genau n_pos vorformatierte Leerzeilen ab Zeile 5 in 01_Positionen", {
  # Kriteriums-Waechter an der Referenz (und am Default-Generat, n_pos = 60):
  expect_identical(.stil(ref()$wb, "01_Positionen", "C64")$fill, "DDEBF7")
  expect_false(identical(.stil(ref()$wb, "01_Positionen", "C65")$fill, "DDEBF7"))
  expect_identical(.stil(gen()$wb, "01_Positionen", "C64")$fill, "DDEBF7")
  expect_false(identical(.stil(gen()$wb, "01_Positionen", "C65")$fill, "DDEBF7"))

  g5 <- gen5()
  expect_identical(g5$rueck$blaetter, ref()$blaetter)
  for (r in 5:9) {
    s <- .stil(g5$wb, "01_Positionen", paste0("C", r))
    expect_identical(s$fill, "DDEBF7", info = sprintf("n_pos=5, C%d Fill", r))
    expect_identical(s$numfmt, "@",    info = sprintf("n_pos=5, C%d Format", r))
  }
  expect_false(identical(.stil(g5$wb, "01_Positionen", "C10")$fill, "DDEBF7"))
})

# ===========================================================================
#  (9) Parameter ist_jahr: die 12 Ist-Monats-Spaltenkoepfe in Blatt 01
# ===========================================================================

# ---------------------------------------------------------------------------
# Format-Ableitung AUS DER REFERENZ mit Waechter: die Koepfe der Spalten
# G..R (7..18) sind dort "2025-01".."2025-12" (sprintf "%d-%02d"). Generat
# mit ist_jahr = 2027: dieselben Koordinaten tragen "2027-01".."2027-12",
# die Kopfzeile hat dieselben belegten Spalten wie die Referenz und ist
# ausserhalb der Monatsspalten unveraendert.
# ---------------------------------------------------------------------------
test_that("ist_jahr: 2027 erscheint in den 12 Ist-Monats-Koepfen, Rest der Kopfzeile gleich", {
  monat_sp <- openxlsx2::int2col(7:18)
  mref <- .zeilen_map(.roh(ref()$wb, "01_Positionen"), 4L)
  expect_identical(unname(mref[monat_sp]), sprintf("%d-%02d", 2025L, 1:12))

  m27 <- .zeilen_map(.roh(gen27()$wb, "01_Positionen"), 4L)
  expect_identical(unname(m27[monat_sp]), sprintf("%d-%02d", 2027L, 1:12))
  expect_identical(names(m27), names(mref))
  rest <- setdiff(names(mref), monat_sp)
  expect_identical(m27[rest], mref[rest])
  expect_identical(gen27()$rueck$blaetter, ref()$blaetter)
})

# ===========================================================================
#  (10) Determinismus
# ===========================================================================

# ---------------------------------------------------------------------------
# Zwei Erzeugungen mit Default-Parametern in ZWEI verschiedene tempfiles
# (verschiedene Basenames!): die zurueckgelesenen Zellgitter ALLER Blaetter
# sind identical — das pinnt "kein Zeitstempel" UND "kein pfadabhaengiger
# Zellinhalt" (Datei-Bytes duerfen abweichen, der Zellinhalt nicht).
# ---------------------------------------------------------------------------
test_that("Determinismus: zweimal erzeugen -> identisch zurueckgelesene Zellinhalte", {
  a <- gen()
  b <- gen_b()
  expect_identical(a$rueck$blaetter, b$rueck$blaetter)
  ga <- lapply(a$rueck$blaetter, function(bl) .roh(a$wb, bl))
  gb <- lapply(b$rueck$blaetter, function(bl) .roh(b$wb, bl))
  expect_identical(ga, gb)
})

# ===========================================================================
#  (11) Fehlerfaelle
# ===========================================================================

# ---------------------------------------------------------------------------
# n_pos < 1 sowie ist_jahr, das kein EINZELNES ganzzahliges Jahr ist
# (Bruchzahl, Vektor, String) -> expect_error.
# ---------------------------------------------------------------------------
test_that("Fehlerfaelle: n_pos < 1, ist_jahr kein einzelnes ganzzahliges Jahr", {
  tf <- tempfile(fileext = ".xlsx")
  expect_error(new_mandate(tf, n_pos = 0))
  expect_error(new_mandate(tf, ist_jahr = 2025.5))
  expect_error(new_mandate(tf, ist_jahr = c(2025, 2026)))
  expect_error(new_mandate(tf, ist_jahr = "2025"))
})
