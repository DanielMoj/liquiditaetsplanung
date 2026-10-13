# ============================================================================
#  R/finalisierung.R -- Lauf-Finalisierung der GoBD-Schicht (phase-13-finalisierung)
#
#  Zweck (GoBD Rz. 58 ff. / IDW PS 880-Anschluss): ein Rechenlauf hat einen
#  Lock-Zustand entwurf/final. Ein finalisierter Lauf ist fuer die
#  Berichtserstellung eingefroren und darf app-seitig nicht mehr veraendert
#  werden; eine Korrektur ist allein als NEUER, rueckverfolgbarer Lauf mit
#  vorgaenger_run_id zulaessig (lueckenlose Herkunft).
#
#  Vier Primitive, alle rein-injiziert (zeit/user hereingereicht wie bei
#  R/manifest.R und R/protokoll.R -- KEIN RNG, kein Sys.time() im Kern):
#    lauf_status(run_dir)               Lock-Zustand EINES Laufs (Marker-Lesung)
#    lauf_finalisieren(run_dir, ...)    entwurf -> final (write-once + Protokoll)
#    lauf_vorgaenger(run_dir)           Korrektur-Herkunft EINES Laufs (Lesung)
#    korrektur_verketten(vor, neu, ...) neuer Lauf als Korrektur (write-once + Protokoll)
#
#  Zwei Marker je Lauf, beide write-once im Run-Verzeichnis:
#    final.json      Existenz == 'final' (Lock-Zustand, aktueller Fakt)
#    korrektur.json  vorgaenger_run_id (Herkunft, aktueller Fakt)
#  Eine Wahrheit pro Fakt: der Lock-Zustand steht ALLEIN im Marker final.json,
#  NIE im Protokoll. Marker (write-once je Lauf, aktueller Zustand) und
#  Protokoll (tamper-evidente, globale Audit-Kette) sind zwei Fakten mit
#  unterschiedlichem Zweck -- dieselbe Trennung wie manifest gegenueber
#  protokoll. Die kettenfoermige Verifikation des Finalisierungs-Ereignisses
#  obliegt allein protokoll_pruefen (R/protokoll.R).
#
#  Vertrag: handoff/phase-13-finalisierung.json; Spezifikation:
#  tests/test_finalisierung.R. Setzt R/manifest.R (manifest_lesen) und
#  R/protokoll.R (protokoll_anhaengen) voraus sowie jsonlite (seit
#  phase-12-runmanifest-kern im need-Vektor von tests/deps.R). Beruehrt weder
#  kernel/ noch R/manifest.R noch R/protokoll.R -- ruft nur deren Funktionen.
# ============================================================================

# ---------------------------------------------------------------------------
#  .finalis_run_pruefen(run_dir, feld)
#
#  Gemeinsame Precondition der vier Primitive: run_dir ist ein nicht leerer
#  String, existiert und ist ein ECHTER Lauf (manifest.json liegt). Bei
#  Verletzung ein BENANNTER Fehler MIT dem Pfad im Text (Rz.-152-Pruefbarkeit).
#  feld benennt das verletzte Argument im Aufrufer (run_dir /
#  run_dir_vorgaenger / run_dir_neu), damit die Befunde isoliert zuordenbar
#  bleiben. Rueckgabe: der Pfad zur manifest.json (fuer den Aufrufer).
# ---------------------------------------------------------------------------
.finalis_run_pruefen <- function(run_dir, feld = "run_dir") {
  if (!is.character(run_dir) || length(run_dir) != 1L || is.na(run_dir) ||
      !nzchar(run_dir)) {
    stop("finalisierung: ", feld, " muss ein nicht leerer String sein.",
         call. = FALSE)
  }
  if (!dir.exists(run_dir)) {
    stop("finalisierung: ", feld, " existiert nicht: ", run_dir, call. = FALSE)
  }
  manifest_pfad <- file.path(run_dir, "manifest.json")
  if (!file.exists(manifest_pfad)) {
    stop("finalisierung: kein Lauf -- manifest.json fehlt in ", run_dir, ".",
         call. = FALSE)
  }
  manifest_pfad
}

# ---------------------------------------------------------------------------
#  .finalis_marker_transaktion(marker_pfad, marker_daten, schon_da, effekt)
#
#  Kanonischer, transaktionaler Effekt-Pfad fuer BEIDE write-once-Marker
#  (final.json / korrektur.json). Loest zwei Fehlerklassen, die ein nackter
#  check-then-write-Guard offen laesst:
#
#   (1) ATOMARE write-once-Sperre. Der Guard 'file.exists -> write' ist NICHT
#       atomar: zwei parallele Finalisierungen (mirai/future-Worker) koennten
#       beide file.exists(FALSE) sehen und je einen Marker samt Ereignis
#       anhaengen. dir.create() ist auf einem Verzeichnis atomar -- genau EIN
#       Aufrufer legt das Lock an, jeder parallele scheitert. Der write-once-
#       Guard wird UNTER der Sperre ERNEUT geprueft (double check), damit
#       'pruefen -> schreiben' unteilbar wird.
#
#   (2) TRANSAKTIONALER Protokoll-Append. Der Marker steht (Marker-vor-Protokoll
#       ist Vertrag) VOR dem Ereignis. Scheitert protokoll_anhaengen, bliebe der
#       Lauf final/korrektur markiert OHNE Audit-Ereignis, und der write-once-
#       Guard blockierte jeden Retry. Deshalb wird der frisch geschriebene
#       Marker bei einem Append-Fehler zurueckgerollt (unlink) und der Fehler
#       benannt weitergereicht: Marker UND Ereignis -- oder keins von beiden.
#
#  schon_da() ist der aufrufer-spezifische BENANNTE Fehler (mit run_id) fuer den
#  Write-once-Verstoss; effekt() haengt das Protokoll-Ereignis an. Kein RNG,
#  kein Sys.time() -- reiner Datei-Effekt.
# ---------------------------------------------------------------------------
.finalis_marker_transaktion <- function(marker_pfad, marker_daten, schon_da,
                                         effekt) {
  # (1) Exklusive Sperre: dir.create ist atomar; ein paralleler Zweit-Aufruf
  # scheitert hier, statt einen zweiten Marker durchzulassen.
  lock_pfad <- paste0(marker_pfad, ".lock")
  if (!isTRUE(dir.create(lock_pfad, showWarnings = FALSE))) {
    stop("finalisierung: Marker ", basename(marker_pfad), " wird gerade ",
         "geschrieben (Sperre ", basename(lock_pfad), " gehalten -- ",
         "parallele Finalisierung).", call. = FALSE)
  }
  on.exit(unlink(lock_pfad, recursive = TRUE), add = TRUE)

  # (2) Write-once-Guard ERNEUT unter der Sperre (double check): erst jetzt ist
  # 'pruefen -> schreiben' atomar gegen Parallelitaet.
  if (file.exists(marker_pfad)) schon_da()

  # (3) Marker write-once schreiben ...
  jsonlite::write_json(marker_daten, marker_pfad, auto_unbox = TRUE)

  # (4) ... DANN das Protokoll-Ereignis transaktional anhaengen: bei Append-
  # Fehler den Marker zuruecksetzen (nie Marker ohne Audit-Ereignis).
  tryCatch(
    effekt(),
    error = function(e) {
      unlink(marker_pfad)
      stop("finalisierung: Protokoll-Append fehlgeschlagen -- Marker ",
           basename(marker_pfad), " zurueckgerollt (kein final/korrektur ohne ",
           "Audit-Ereignis): ", conditionMessage(e), call. = FALSE)
    })
}

# ---------------------------------------------------------------------------
#  lauf_status(run_dir) -> 'entwurf' | 'final'
#
#  Liest den Lock-Zustand EINES Laufs. STATUS-QUELLE ist ALLEIN der write-once
#  Marker final.json im Run-Verzeichnis: existiert er -> 'final', sonst
#  'entwurf'. Liest NIE das Protokoll (eine Wahrheit pro Fakt). run_dir muss
#  existieren und ein echter Lauf sein (manifest.json). Reines Lesen: KEIN
#  RNG, kein Sys.time(), kein Schreiben.
# ---------------------------------------------------------------------------
lauf_status <- function(run_dir) {
  .finalis_run_pruefen(run_dir)
  if (file.exists(file.path(run_dir, "final.json"))) "final" else "entwurf"
}

# ---------------------------------------------------------------------------
#  lauf_finalisieren(run_dir, protokoll_pfad, zeit, user)
#
#  Friert einen Lauf ein: entwurf -> final in EINEM kanonischen Effekt-Pfad.
#  Reihenfolge Marker-vor-Protokoll ist Bestandteil des Vertrags; der
#  Idempotenz-Guard feuert VOR protokoll_anhaengen -> eine Zweit-Finalisierung
#  haengt KEIN zweites Ereignis an, sondern ist ein BENANNTER Fehler (kein
#  silent succeed, kein No-Op).
#
#  Rueckgabe: invisible list(run_id, status = 'final', marker_pfad,
#  protokoll_pfad). zeit/user INJIZIERT, KEIN RNG im Kern.
# ---------------------------------------------------------------------------
lauf_finalisieren <- function(run_dir, protokoll_pfad, zeit = Sys.time(),
                              user = Sys.info()[["user"]]) {
  # (1) Precondition: echter Lauf; run_id UNABHAENGIG via manifest_lesen.
  manifest_pfad <- .finalis_run_pruefen(run_dir)

  # Argumentpruefung wie bei den Nachbar-Primitiven -- benannt und VOR jedem
  # Seiteneffekt, damit ein delegierter Fehler keinen halben Marker hinterlaesst.
  if (!is.character(protokoll_pfad) || length(protokoll_pfad) != 1L ||
      is.na(protokoll_pfad) || !nzchar(protokoll_pfad)) {
    stop("lauf_finalisieren: protokoll_pfad muss ein nicht leerer String sein.",
         call. = FALSE)
  }
  if (!inherits(zeit, "POSIXct") || length(zeit) != 1L || anyNA(zeit)) {
    stop("lauf_finalisieren: zeit muss ein einzelner POSIXct-Zeitpunkt sein.",
         call. = FALSE)
  }
  if (!is.character(user) || length(user) != 1L || is.na(user) ||
      !nzchar(user)) {
    stop("lauf_finalisieren: user muss ein nicht leerer String sein.",
         call. = FALSE)
  }

  run_id      <- manifest_lesen(manifest_pfad)$run_id
  marker_pfad <- file.path(run_dir, "final.json")

  # (2) Status muss 'entwurf' sein (Marker fehlt). Der Guard feuert VOR jedem
  # Schreiben -- die Idempotenz ist ein BENANNTER Fehler mit run_id, kein
  # stiller Erfolg (Lehre: succeed in Fallback ist unsichtbares Immer-Gruen).
  # Fast-Fail OHNE Sperr-Overhead; der AUTORITATIVE, atomare Guard sitzt unter
  # der Sperre in .finalis_marker_transaktion (double check) -- erst dort ist
  # 'pruefen -> schreiben' gegen parallele Worker unteilbar.
  schon_final <- function() {
    stop("lauf_finalisieren: Lauf ", run_id, " ist bereits finalisiert ",
         "(final.json write-once, kein zweites Einfrieren).", call. = FALSE)
  }
  if (file.exists(marker_pfad)) schon_final()

  # (3) Marker final.json write-once (UTC-Normierung wie R/manifest.R Z. 166)
  # und (4) Protokoll-Ereignis FINALISIERUNG als EIN transaktionaler, atomar
  # gesperrter Effekt: Marker-vor-Protokoll bleibt Vertrag, der Guard feuert vor
  # dem Append, und ein fehlgeschlagener Append rollt den Marker zurueck (loest
  # zugleich die aus phase-12 aufgeschobene Emission des finalisierung-Ereignisses ein).
  zeitstempel <- format(zeit, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  .finalis_marker_transaktion(
    marker_pfad,
    list(run_id = run_id, zeitstempel = zeitstempel, user = user),
    schon_final,
    function() protokoll_anhaengen(protokoll_pfad, "finalisierung",
                                   list(run_id = run_id, status = "final"),
                                   zeit, user))

  invisible(list(run_id = run_id, status = "final",
                 marker_pfad = marker_pfad, protokoll_pfad = protokoll_pfad))
}

# ---------------------------------------------------------------------------
#  lauf_vorgaenger(run_dir) -> vorgaenger_run_id (String) | NA_character_
#
#  Liest die Korrektur-Herkunft EINES Laufs. Existiert der Marker
#  korrektur.json -> sein Feld vorgaenger_run_id; sonst NA_character_ (kein
#  Korrekturlauf). Symmetrisch zu lauf_status (korrektur.json wird von
#  korrektur_verketten GESCHRIEBEN und hier GELESEN -- Roundtrip-Disziplin,
#  keine write-only-Hinterlassenschaft). run_dir muss existieren und ein
#  echter Lauf sein. Reines Lesen: KEIN RNG, kein Sys.time().
# ---------------------------------------------------------------------------
lauf_vorgaenger <- function(run_dir) {
  .finalis_run_pruefen(run_dir)
  marker_pfad <- file.path(run_dir, "korrektur.json")
  if (!file.exists(marker_pfad)) return(NA_character_)
  km <- jsonlite::read_json(marker_pfad, simplifyVector = TRUE)
  as.character(km$vorgaenger_run_id)
}

# ---------------------------------------------------------------------------
#  korrektur_verketten(run_dir_vorgaenger, run_dir_neu, protokoll_pfad, zeit, user)
#
#  Verkettet einen NEUEN Lauf als Korrektur eines finalen Vorgaengers. Beide
#  run_dir muessen echte Laeufe sein; die run_ids kommen UNABHAENGIG via
#  manifest_lesen.
#
#  PRECONDITIONS in Vertragsreihenfolge (je BENANNTER Fehler, jede einzeln
#  isoliert pruefbar):
#    (a) run_id_neu != run_id_vorgaenger      -> 'Korrektur muss ein neuer Lauf'
#    (b) lauf_status(vorgaenger) == 'final'    -> 'Vorgaenger nicht finalisiert'
#    (c) lauf_status(neu)        == 'entwurf'  -> 'Korrekturlauf bereits finalisiert'
#    (d) korrektur.json write-once (file.exists) -> 'bereits als Korrektur verzeichnet'
#
#  Effekt: Marker korrektur.json schreiben (vorgaenger_run_id, run_id,
#  zeitstempel UTC, user) und Protokoll-Ereignis KORREKTUR anhaengen -- loest
#  die aufgeschobene Emission des korrektur-Ereignisses ein. Rueckgabe:
#  invisible list(run_id = run_id_neu, vorgaenger_run_id, marker_pfad,
#  protokoll_pfad). zeit/user INJIZIERT, KEIN RNG.
# ---------------------------------------------------------------------------
korrektur_verketten <- function(run_dir_vorgaenger, run_dir_neu, protokoll_pfad,
                                zeit = Sys.time(), user = Sys.info()[["user"]]) {
  # Beide run_dir echte Laeufe (benannter Fehler MIT Pfad, isoliert je Argument).
  manifest_vor <- .finalis_run_pruefen(run_dir_vorgaenger, "run_dir_vorgaenger")
  manifest_neu <- .finalis_run_pruefen(run_dir_neu, "run_dir_neu")

  # Argumentpruefung (benannt, VOR jedem Seiteneffekt).
  if (!is.character(protokoll_pfad) || length(protokoll_pfad) != 1L ||
      is.na(protokoll_pfad) || !nzchar(protokoll_pfad)) {
    stop("korrektur_verketten: protokoll_pfad muss ein nicht leerer String sein.",
         call. = FALSE)
  }
  if (!inherits(zeit, "POSIXct") || length(zeit) != 1L || anyNA(zeit)) {
    stop("korrektur_verketten: zeit muss ein einzelner POSIXct-Zeitpunkt sein.",
         call. = FALSE)
  }
  if (!is.character(user) || length(user) != 1L || is.na(user) ||
      !nzchar(user)) {
    stop("korrektur_verketten: user muss ein nicht leerer String sein.",
         call. = FALSE)
  }

  run_id_vorgaenger <- manifest_lesen(manifest_vor)$run_id
  run_id_neu        <- manifest_lesen(manifest_neu)$run_id

  # (a) Korrektur ist ein NEUER Lauf -- distinct run_id.
  if (identical(run_id_neu, run_id_vorgaenger)) {
    stop("korrektur_verketten: Korrektur muss ein neuer Lauf sein ",
         "(run_id_neu == run_id_vorgaenger: ", run_id_neu, ").", call. = FALSE)
  }
  # (b) Vorgaenger muss eingefroren sein.
  if (lauf_status(run_dir_vorgaenger) != "final") {
    stop("korrektur_verketten: Vorgaenger nicht finalisiert (", run_id_vorgaenger,
         " ist noch entwurf -- ein entwurf-Lauf wird direkt geaendert, nicht ",
         "korrigiert).", call. = FALSE)
  }
  # (c) Herkunft VOR dem Einfrieren des Korrekturlaufs verzeichnen.
  if (lauf_status(run_dir_neu) != "entwurf") {
    stop("korrektur_verketten: Korrekturlauf ", run_id_neu, " bereits finalisiert ",
         "(keine nachtraegliche Herkunfts-Injektion in einen gesperrten Lauf).",
         call. = FALSE)
  }
  # (d) Marker write-once. Fast-Fail OHNE Sperr-Overhead; der AUTORITATIVE,
  # atomare Guard sitzt unter der Sperre in .finalis_marker_transaktion (double
  # check) -- erst dort ist 'pruefen -> schreiben' gegen parallele
  # korrektur_verketten-Aufrufe unteilbar.
  marker_pfad <- file.path(run_dir_neu, "korrektur.json")
  schon_korrektur <- function() {
    stop("korrektur_verketten: Lauf ", run_id_neu, " ist bereits als Korrektur ",
         "verzeichnet (korrektur.json write-once).", call. = FALSE)
  }
  if (file.exists(marker_pfad)) schon_korrektur()

  # Effekt: Marker korrektur.json write-once (UTC-Normierung wie R/manifest.R)
  # UND Protokoll-Ereignis KORREKTUR als EIN transaktionaler, atomar gesperrter
  # Effekt: ein fehlgeschlagener Append rollt den Marker zurueck (nie Korrektur-
  # Marker ohne Audit-Ereignis; loest zugleich die aus phase-12 aufgeschobene
  # Emission des korrektur-Ereignisses ein).
  zeitstempel <- format(zeit, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  .finalis_marker_transaktion(
    marker_pfad,
    list(vorgaenger_run_id = run_id_vorgaenger, run_id = run_id_neu,
         zeitstempel = zeitstempel, user = user),
    schon_korrektur,
    function() protokoll_anhaengen(protokoll_pfad, "korrektur",
                                   list(vorgaenger_run_id = run_id_vorgaenger,
                                        run_id = run_id_neu), zeit, user))

  invisible(list(run_id = run_id_neu, vorgaenger_run_id = run_id_vorgaenger,
                 marker_pfad = marker_pfad, protokoll_pfad = protokoll_pfad))
}
