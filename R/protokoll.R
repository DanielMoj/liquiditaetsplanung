# ============================================================================
#  R/protokoll.R -- Verfahrensprotokoll der GoBD-Schicht (GoBD Rz. 58 ff.)
#
#  Diese Einheit (phase-12-protokoll-hash) enthaelt ausschliesslich das
#  Hash-Primitive der Protokollkette. Die Kettenglied-Erzeugung
#  (protokoll_anhaengen) und die Kettenpruefung (protokoll_pruefen) folgen in
#  den beiden naechsten Phasen und werden diese Datei erweitern -- beide
#  TEILEN dann .protokoll_hash: der Anhaenger berechnet den Hash, der Pruefer
#  rechnet ihn nach. Genau deshalb steht die Formel hier als eigenstaendige,
#  extern pinnbare Einheit und nicht inline im Anhaenger: kein Hash prueft
#  sich selbst.
#
#  Vertrag: handoff/phase-12-protokoll-hash.json; Spezifikation:
#  tests/test_protokoll_hash.R (Primitive gegen NIST FIPS 180-4, Komposition
#  gegen ein unabhaengig nachgebautes kanon-Orakel).
#
#  Setzt kein anderes Repo-Skript voraus (engine-unabhaengig); benoetigt das
#  Paket digest (seit phase-12-runmanifest-kern im need-Vektor von
#  tests/deps.R).
# ============================================================================

# Normierung eines skalaren daten-Wertes zur Zeichenkette. logical wird
# 'TRUE'/'FALSE' (nicht 1/0, sonst kollidiert FALSE mit der Zahl 0), character
# geht durch enc2utf8 (Kanon v2: die Bytes des Wertes sind damit UTF-8-fest,
# unabhaengig vom nativen Encoding der Plattform), alles Uebrige (numeric)
# ueber sprintf('%.15g') -- dieselbe Zahl-Normierung wie .gobd_str in
# R/manifest_anbindung.R, hier um den character-Zweig erweitert, weil
# protokoll-daten Freiform-Strings tragen (run_id, Dateiname), die .gobd_str
# nie sah. logical- und numeric-Zweig sind unveraendert Bestand.
.pstr <- function(x) {
  if (is.logical(x))   return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.character(x)) return(enc2utf8(as.character(x)))
  sprintf("%.15g", x)
}

# Length-Prefix-Token '<bytes>:<utf8-string>' (Kanon v2). Praefixfrei
# dekodierbar: die Byte-Laenge steht vor dem Inhalt, damit ist jede Tokenfolge
# aus dem Bytestrom eindeutig rekonstruierbar -- das Kanon wird injektiv.
.ptok <- function(s) {
  s2 <- enc2utf8(s)
  paste0(nchar(s2, type = "bytes"), ":", s2)
}

# ---------------------------------------------------------------------------
#  .protokoll_hash(seq, typ, zeitstempel, user, daten, vorheriger_hash)
#
#  Kanonischer Ereignis-Hash eines Protokollgliedes nach KANON v2. Rein und
#  zustandslos: kein Datei-IO, kein RNG, kein Sys.time(), keine
#  Eingabevalidierung -- die leisten protokoll_anhaengen / protokoll_pruefen.
#
#    seq             Skalar, ganzzahlig (laufende Nummer des Ereignisses)
#    typ             Skalar-String (einer der sechs Ereignistypen; hier
#                    fliesst nur der Wert ein, er wird nicht geprueft)
#    zeitstempel     Skalar-String, bereits UTC-normiert '%Y-%m-%dT%H:%M:%SZ'
#    user            Skalar-String
#    daten           benannte Liste skalarer Atome (logical/numeric/character),
#                    darf leer sein: list() -> daten_kanon ''
#    vorheriger_hash Skalar-String: 64-Hex oder Genesis-Sentinel strrep('0',64)
#
#  Rueckgabe: 64-stelliger Kleinbuchstaben-Hexstring.
#
#  KANON v2 -- die vollstaendige Formel, Schritt fuer Schritt (ein externer
#  Pruefer kann den Hash allein aus diesem Kommentar nachbauen):
#
#    tok(s):  s2 <- enc2utf8(s)
#             paste0(nchar(s2, type = "bytes"), ":", s2)
#             (implementiert als .ptok oben)
#
#    wert(x): logical   -> "TRUE"/"FALSE"
#             character -> enc2utf8(as.character(x))
#             sonst     -> sprintf("%.15g", x)
#             (implementiert als .pstr oben)
#
#    daten_kanon:
#             bei length(daten) == 0 der Leerstring "";
#             sonst nm2 <- enc2utf8(names(daten));
#                   o   <- order(nm2, method = "radix");
#             Konkatenation OHNE Trennzeichen ueber die sortierten Felder,
#             je Feld: paste0(tok(nm2[i]), tok(wert(daten[[i]])))
#
#    kanon:   paste0(tok(wert(seq)), tok(typ), tok(zeitstempel), tok(user),
#                    tok(daten_kanon), tok(vorheriger_hash))
#             -- feste Sechs-Token-Aussenstruktur; daten_kanon geht als
#             GANZES nochmals durch tok(), leere daten verschwinden also
#             nicht aus dem Kanon (Token "0:").
#
#    hash:    digest::digest(charToRaw(enc2utf8(kanon)), algo = "sha256",
#                            serialize = FALSE)
#
#  Warum diese Struktur (Review-Nachholungs-Befund
#  .orch/review-pakete/befund-paket-phase-12-protokoll-hash.md; Spezifikation
#  eingefroren in tests/test_protokoll_hash.R, Spec-Commit 0b9d612):
#   - Length-Prefix-Tokens sind praefixfrei dekodierbar, die Tokenfolge ist
#     aus dem Bytestrom eindeutig rekonstruierbar -> das Kanon ist INJEKTIV.
#     Das v1-Kanon ('name=wert' mit '&' verkettet, Felder mit '|', keine
#     Escapes) hatte Kollisionspaare wie list(a='1&b=2') vs. list(a='1', b=2);
#     mit v2 ist kein '&'/'='/'|'-Kollisionspaar mehr moeglich.
#   - enc2utf8 vor nchar/charToRaw plus order(..., method = 'radix') machen
#     die Bytes und die Namenssortierung locale- und plattformfest.
#   - Die typ-uebergreifende Wert-Normierung (logical TRUE und String 'TRUE'
#     fallen zusammen) ist BEWUSST unveraendert Bestand: sie macht den Hash
#     robust gegen die jsonlite-Rundreise, die Typen nicht exakt zusichert.
#
#  Die daten-Namen werden VOR der Verkettung sortiert. Damit haengt der Hash
#  nicht an der Einfuegereihenfolge und ueberlebt eine jsonlite-Rundreise, die
#  die Feldreihenfolge nicht zusichert.
# ---------------------------------------------------------------------------
.protokoll_hash <- function(seq, typ, zeitstempel, user, daten, vorheriger_hash) {
  # names(list()) ist NULL, und order(NULL) bricht ab -- der Grenzfall leerer
  # daten wird deshalb vor der Sortierung abgefangen: daten_kanon ''.
  if (length(daten) == 0L) {
    daten_kanon <- ""
  } else {
    nm2 <- enc2utf8(names(daten))
    o   <- order(nm2, method = "radix")
    daten_kanon <- paste0(vapply(o, function(i)
      paste0(.ptok(nm2[i]), .ptok(.pstr(daten[[i]]))),
      character(1)), collapse = "")
  }
  kanon <- paste0(.ptok(.pstr(seq)), .ptok(typ), .ptok(zeitstempel),
                  .ptok(user), .ptok(daten_kanon), .ptok(vorheriger_hash))
  digest::digest(charToRaw(enc2utf8(kanon)), algo = "sha256",
                 serialize = FALSE)
}

# Die sechs zulaessigen Ereignistypen -- der Lebenszyklus eines Laufs: Import
# der Eingabe, Simulationslauf, IKS-Kontrolle, Export, Finalisierung, Korrektur
# (als neuer Lauf). Feste Konstante; jeder andere typ wird abgewiesen.
.protokoll_typen <- c("import", "lauf", "kontrolle", "export",
                      "finalisierung", "korrektur")

# Genesis-Sentinel: markierte Luecke 'kein Vorgaenger' am Kettenanfang. KEIN zu
# verifizierender Hash, sondern ein fester 64-Nullen-String -- dieselbe Breite
# wie ein sha256-Hex, damit die Kettenpruefung strukturell einheitlich bleibt.
.protokoll_genesis <- strrep("0", 64)

# Befund-Text fuer einen nicht skalar-atomaren daten-Wert, oder NULL wenn ok.
# Zulaessig: logical/numeric/character der Laenge 1, nicht NA, bei numeric
# endlich (kein Inf/NaN). Alles Uebrige (Vektor >1, Liste, data.frame, NA) ist
# unzulaessig -- der Aufrufer benennt das Feld daten$<name> im stop().
.protokoll_daten_befund <- function(x) {
  if (!is.atomic(x) || length(x) != 1L) return("muss ein skalarer Wert sein")
  if (!(is.logical(x) || is.numeric(x) || is.character(x))) {
    return("muss logical, numeric oder character sein")
  }
  if (is.na(x)) return("darf nicht NA sein")
  if (is.numeric(x) && !is.finite(x)) return("muss endlich sein")
  NULL
}

# ---------------------------------------------------------------------------
#  protokoll_anhaengen(protokoll_pfad, typ, daten, zeit, user)
#
#  Revisionsfester Writer der Protokollkette (GoBD Rz. 58 ff./107: lueckenlose,
#  manipulationssichere Nachweisfolge). Haengt GENAU EINE Ereigniszeile (JSONL:
#  ein JSON-Objekt je Zeile) APPEND-ONLY an protokoll_pfad an -- bestehende
#  Bytes werden nie beruehrt, nur das Dateiende waechst. Append-only gilt auf
#  ANWENDUNGSEBENE; Dateisystem-/WORM-Schutz bleibt Betreiberpflicht der
#  Verfahrensdokumentation (phase-14), genau wie bei manifest_schreiben.
#
#  Rueckgabe: invisible das angehaengte Ereignis = list mit den SIEBEN Feldern
#  seq, typ, zeitstempel, user, daten, vorheriger_hash, hash (in dieser
#  Reihenfolge).
#
#    protokoll_pfad  nicht leerer String. Fehlt die Datei -> neue Kette
#                    (Genesis). Existiert sie -> letzte Zeile wird verkettet;
#                    existiert sie aber leer (0 Zeilen) -> stop('Protokoll leer')
#    typ             genau einer von .protokoll_typen (sonst stop() mit Wert und
#                    zulaessiger Sechser-Menge)
#    daten           benannte Liste skalarer Atome (darf leer sein); befuellt ->
#                    Namen eindeutig und nicht leer, jeder Wert skalar-atomar
#    zeit            einzelner POSIXct; zeitstempel = UTC-Normierung (wie im
#                    Manifest-Kern R/manifest.R Z. 166)
#    user            nicht leerer String
#
#  Der Hash wird ueber den oracle-gepinnten Primitive .protokoll_hash gebildet
#  (eine Wahrheit pro Fakt -- die kanonische Formel steht allein dort). KEIN
#  RNG, kein Sys.time() im Kern: zeit und user werden injiziert wie beim
#  Manifest. Setzt digest (via .protokoll_hash) und jsonlite voraus -- beide
#  seit phase-12-runmanifest-kern im need-Vektor von tests/deps.R.
# ---------------------------------------------------------------------------
protokoll_anhaengen <- function(protokoll_pfad, typ, daten, zeit, user) {
  if (!is.character(protokoll_pfad) || length(protokoll_pfad) != 1L ||
      is.na(protokoll_pfad) || !nzchar(protokoll_pfad)) {
    stop("protokoll_anhaengen: protokoll_pfad muss ein nicht leerer String sein.",
         call. = FALSE)
  }
  if (!is.character(typ) || length(typ) != 1L || is.na(typ) ||
      !(typ %in% .protokoll_typen)) {
    stop("protokoll_anhaengen: typ '", typ, "' unzulaessig; erlaubt ist genau ",
         "einer von: ", paste(.protokoll_typen, collapse = "/"), ".",
         call. = FALSE)
  }
  if (!is.list(daten) || is.data.frame(daten)) {
    stop("protokoll_anhaengen: daten muss eine benannte Liste sein.",
         call. = FALSE)
  }
  if (length(daten) > 0L) {
    nm <- names(daten)
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm) > 0L) {
      stop("protokoll_anhaengen: daten braucht eindeutige, nicht leere Namen.",
           call. = FALSE)
    }
    for (k in seq_along(daten)) {
      befund <- .protokoll_daten_befund(daten[[k]])
      if (!is.null(befund)) {
        stop("protokoll_anhaengen: daten$", nm[k], " ", befund, ".",
             call. = FALSE)
      }
    }
  }
  if (!inherits(zeit, "POSIXct") || length(zeit) != 1L || anyNA(zeit)) {
    stop("protokoll_anhaengen: zeit muss ein einzelner POSIXct-Zeitpunkt sein.",
         call. = FALSE)
  }
  if (!is.character(user) || length(user) != 1L || is.na(user) ||
      !nzchar(user)) {
    stop("protokoll_anhaengen: user muss ein nicht leerer String sein.",
         call. = FALSE)
  }

  # Kettenfortsetzung: fehlt die Datei -> Genesis; sonst letzte Zeile verketten.
  if (!file.exists(protokoll_pfad)) {
    seq             <- 1L
    vorheriger_hash <- .protokoll_genesis
  } else {
    vorhandene <- readLines(protokoll_pfad, warn = FALSE)
    if (length(vorhandene) == 0L) {
      stop("protokoll_anhaengen: Protokoll leer -- ", protokoll_pfad,
           " existiert, enthaelt aber keine Ereigniszeile.", call. = FALSE)
    }
    letztes         <- jsonlite::fromJSON(vorhandene[[length(vorhandene)]])
    seq             <- as.integer(letztes$seq) + 1L
    vorheriger_hash <- letztes$hash
  }

  # UTC-Normierung macht den Stempel zeitzonenfrei reproduzierbar.
  zeitstempel <- format(zeit, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # Hash ueber den oracle-gepinnten Primitive (Delegation; die kanon-Formel
  # steht allein in .protokoll_hash).
  hash <- .protokoll_hash(seq, typ, zeitstempel, user, daten, vorheriger_hash)

  # Ereignis in fester Feldreihenfolge; genau diese Reihenfolge wird als JSON
  # geschrieben und von der Kettenpruefung erwartet.
  ereignis <- list(seq = seq, typ = typ, zeitstempel = zeitstempel,
                   user = user, daten = daten,
                   vorheriger_hash = vorheriger_hash, hash = hash)

  # APPEND-ONLY: eine JSON-Zeile ausschliesslich ans Dateiende. auto_unbox
  # unboxt die skalaren Felder. daten ist eine Map -> als JSON-Objekt zu
  # schreiben. jsonlite kodiert die LEERE Liste list() deterministisch (und
  # versionsunabhaengig) als Array [] -- die Objektform {} eines benannten
  # Leer-list() haengt dagegen an einer versionsabhaengigen Heuristik. Deshalb
  # nicht darauf verlassen: das leere daten-Feld nach der Serialisierung fest
  # von "daten":[] auf "daten":{} normieren (fixed = TRUE, genau ein Vorkommen).
  # Das zurueckgegebene ereignis (Original-daten) bleibt unangetastet.
  json <- jsonlite::toJSON(ereignis, auto_unbox = TRUE, pretty = FALSE,
                           null = "null")
  if (length(daten) == 0L) {
    json <- sub('"daten":[]', '"daten":{}', json, fixed = TRUE)
  }
  cat(json, "\n", file = protokoll_pfad, append = TRUE, sep = "")

  invisible(ereignis)
}

# ---------------------------------------------------------------------------
#  protokoll_pruefen(protokoll_pfad)
#
#  Tamper-evidente Verifikation der Hash-Kette OHNE LLM (GoBD Rz. 107/152:
#  lueckenlose, benennbar-nachpruefbare Protokollfolge). Liest saemtliche
#  JSONL-Zeilen und liefert invisible(TRUE), wenn die Kette in JEDER Zeile
#  intakt ist -- oder bricht bei der ERSTEN Verletzung mit stop() und einem
#  BENANNTEN Befund (Zeilennummer + Feldname, Stil wie manifest_validieren in
#  R/manifest.R) ab.
#
#  Pruefungen je Zeile i (1-basiert), Abbruch bei der ersten Verletzung:
#    seq             identical i               (sonst 'Zeile <i>, seq')
#    typ             in .protokoll_typen        (sonst 'Zeile <i>, typ')
#    zeitstempel     ~ UTC-Muster               (sonst 'Zeile <i>, zeitstempel')
#    user            nicht leerer String        (sonst 'Zeile <i>, user')
#    vorheriger_hash identical (i==1 ? Genesis : hash von Zeile i-1)
#                                               (sonst 'Zeile <i>, verkettung')
#    hash            identical .protokoll_hash(seq, typ, zeitstempel, user,
#                    daten, vorheriger_hash)    (sonst 'Zeile <i>, hash')
#
#  Weil der Hash JEDES Feld (seq/typ/zeitstempel/user/daten/vorheriger_hash)
#  abdeckt, macht jede nachtraegliche Aenderung an einem Ereignis die Kette an
#  genau dieser Stelle nachweisbar -- inklusive der Rueckwirkung auf den
#  Nachfolger, dessen vorheriger_hash dann nicht mehr zum (veraenderten) hash
#  der Vorgaengerzeile passt.
#
#  Die kanonische Hash-Formel wird NICHT dupliziert: pruefen ruft den
#  oracle-gepinnten Primitive .protokoll_hash auf (eine Wahrheit pro Fakt).
#  KEIN RNG, kein Sys.time(), reines Lesen. Datei fehlt -> stop() mit Pfad im
#  Text; existiert aber leer (0 Zeilen) -> stop('Protokoll leer'). Setzt
#  jsonlite (Zeilen-Parsing) und digest (via .protokoll_hash) voraus.
# ---------------------------------------------------------------------------
protokoll_pruefen <- function(protokoll_pfad) {
  fehl <- function(i, feld, warum) {
    stop("Protokoll ungueltig: Zeile ", i, ", ", feld, " ", warum, ".",
         call. = FALSE)
  }

  if (!is.character(protokoll_pfad) || length(protokoll_pfad) != 1L ||
      is.na(protokoll_pfad) || !nzchar(protokoll_pfad)) {
    stop("protokoll_pruefen: protokoll_pfad muss ein nicht leerer String sein.",
         call. = FALSE)
  }
  if (!file.exists(protokoll_pfad)) {
    stop("protokoll_pruefen: Protokolldatei existiert nicht: ", protokoll_pfad,
         call. = FALSE)
  }
  zeilen <- readLines(protokoll_pfad, warn = FALSE)
  if (length(zeilen) == 0L) {
    stop("protokoll_pruefen: Protokoll leer -- ", protokoll_pfad,
         " existiert, enthaelt aber keine Ereigniszeile.", call. = FALSE)
  }

  # Verkettungs-Sollwert der aktuellen Zeile: Zeile 1 gegen den Genesis-
  # Sentinel, jede weitere gegen den (bereits verifizierten) hash der
  # Vorgaengerzeile. Da bei der ersten Verletzung abgebrochen wird, ist der
  # gespeicherte hash der Vorgaengerzeile beim Erreichen von Zeile i schon
  # gegen seine eigene Rekonstruktion geprueft und damit vertrauenswuerdig.
  vorheriger_soll <- .protokoll_genesis

  # Exakt diese sieben Top-Level-Felder traegt ein wohlgeformtes Ereignis (die
  # Schreibreihenfolge aus protokoll_anhaengen). Jedes Zusatz- oder Fehlfeld
  # wird abgewiesen, BEVOR der Hash rekonstruiert wird: der Hash deckt nur diese
  # sechs Kanon-Felder ab, ein ungeprueftes Achtfeld liefe sonst weder durch die
  # Schema- noch durch die Hash-Pruefung und koennte still mitreisen.
  erlaubte_felder <- c("seq", "typ", "zeitstempel", "user", "daten",
                       "vorheriger_hash", "hash")

  for (i in seq_along(zeilen)) {
    # Ungueltiges JSON faellt nicht als generischer jsonlite-Parsefehler durch,
    # sondern als benannter Befund mit Zeilennummer (Stil wie jede andere
    # Verletzung: 'Zeile <i>, json ...').
    z <- tryCatch(
      jsonlite::fromJSON(zeilen[[i]]),
      error = function(e) fehl(i, "json", "ist kein gueltiges JSON"))

    # Schema-Riegel: genau die sieben erlaubten Felder, nicht mehr, nicht
    # weniger -- vor der Hash-Rekonstruktion, damit Zusatzfelder benannt
    # auffallen statt ungehasht durchzurutschen.
    if (!identical(sort(names(z)), sort(erlaubte_felder))) {
      fehl(i, "schema",
           paste0("muss genau die Felder ",
                  paste(erlaubte_felder, collapse = "/"), " tragen"))
    }

    # seq identical i (numerisch, damit der JSON-Roundtrip double vs. integer
    # nicht faelschlich als Befund zaehlt; ein manipulierter seq weicht im Wert
    # ab und faellt so oder so auf).
    if (!(is.numeric(z$seq) && length(z$seq) == 1L && !is.na(z$seq) &&
          z$seq == i)) {
      fehl(i, "seq", "muss identisch der laufenden Zeilennummer sein")
    }
    # typ in der zulaessigen Sechser-Menge.
    if (!(is.character(z$typ) && length(z$typ) == 1L && !is.na(z$typ) &&
          z$typ %in% .protokoll_typen)) {
      fehl(i, "typ", paste0("muss einer von ",
                            paste(.protokoll_typen, collapse = "/"), " sein"))
    }
    # zeitstempel im UTC-Muster JJJJ-MM-TTThh:mm:ssZ.
    if (!(is.character(z$zeitstempel) && length(z$zeitstempel) == 1L &&
          !is.na(z$zeitstempel) &&
          grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
                z$zeitstempel))) {
      fehl(i, "zeitstempel", "entspricht nicht dem UTC-Format JJJJ-MM-TTThh:mm:ssZ")
    }
    # user nicht leerer String.
    if (!(is.character(z$user) && length(z$user) == 1L && !is.na(z$user) &&
          nzchar(z$user))) {
      fehl(i, "user", "muss ein nicht leerer String sein")
    }
    # Verkettung: vorheriger_hash gegen den Sollwert (Genesis bzw. Vorgaenger).
    if (!(is.character(z$vorheriger_hash) && length(z$vorheriger_hash) == 1L &&
          !is.na(z$vorheriger_hash) &&
          identical(z$vorheriger_hash, vorheriger_soll))) {
      fehl(i, "verkettung",
           "vorheriger_hash passt nicht zum hash der Vorgaengerzeile")
    }
    # Hash-Rekonstruktion ueber den oracle-gepinnten Primitive (Delegation --
    # die kanon-Formel steht allein in .protokoll_hash). Da der Hash jedes Feld
    # abdeckt, faellt jede Feld-Mutatio ohne Neu-Hash hier auf.
    rekonstruiert <- .protokoll_hash(z$seq, z$typ, z$zeitstempel, z$user,
                                     z$daten, z$vorheriger_hash)
    if (!(is.character(z$hash) && length(z$hash) == 1L && !is.na(z$hash) &&
          identical(z$hash, rekonstruiert))) {
      fehl(i, "hash", "stimmt nicht mit dem rekonstruierten Ereignis-Hash ueberein")
    }

    vorheriger_soll <- z$hash
  }

  invisible(TRUE)
}
