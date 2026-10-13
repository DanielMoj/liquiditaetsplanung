# ============================================================================
#  R/manifest.R -- Run-Manifest (Kern) der GoBD-Schicht (phase-12-runmanifest-kern)
#
#  Zweck (GoBD Rz. 152/154): jeder Rechenlauf wird nachpruefbar dokumentiert --
#  jede Eingabedatei ueber ihre rohen Bytes SHA-256-gehasht, die Umgebung
#  erfasst, alles in einem DETERMINISTISCHEN Manifest-Objekt gebuendelt,
#  write-once als JSON persistiert und feldtreu zurueckgelesen (retrograde
#  Pruefung). Vertrag: handoff/phase-12-runmanifest-kern.json; Spezifikation:
#  tests/test_manifest.R. Kein Engine-Bezug, kein RNG, kein Sys.time() im
#  reinen Kern (manifest_erstellen bekommt zeit und umgebung INJIZIERT).
#
#  Schema-Erweiterung v2 (phase-12-runmanifest-anbindung, GoBD Rz. 100/152):
#  manifest_erstellen traegt ein optionales kontrollen-Argument (IKS-
#  Kontrollzeilen). kontrollen = NULL liefert das BIT-IDENTISCHE v1-Objekt
#  wie bisher; ein kontrollen-data.frame liefert manifest_version 2L mit dem
#  ZEHNTEN Top-Level-Feld kontrollen (nach outputs). Die run_id-Ableitung ist
#  davon unberuehrt (kontrollen gehen wie outputs NICHT in die kanonische
#  Verkettung ein). Vertrag: handoff/phase-12-runmanifest-anbindung.json;
#  Spezifikation: tests/test_manifest_anbindung.R.
#
#  Kanon-Praezisierung (Review-Fix 12.07.2026): die run_id-Verkettung ist
#  injektiv und byte-fest (Length-Prefix ueber enc2utf8-Bytes, radix-
#  Sortierung). Suffixe identischer Inputs aendern sich gegenueber der
#  Erstfassung; Alt-Manifeste bleiben pruefbar, da keine retrograde
#  run_id-Nachrechnung existiert.
# ============================================================================

.manifest_felder_top <- c("manifest_version", "run_id", "zeitstempel", "user",
                          "umgebung", "eingaben", "parameter", "seeds", "outputs")
.manifest_df_spalten <- c("name", "pfad", "sha256", "groesse_bytes")
.manifest_umg_felder <- c("r_version", "plattform", "git_sha", "git_dirty",
                          "renv_lock_sha256")
.manifest_k_spalten  <- c("name", "bestanden", "soll", "ist", "details")

# SHA-256 ueber die ROHEN BYTES einer Datei, Rueckgabe als 64-stelliger
# Kleinbuchstaben-Hexstring (Orakel: NIST FIPS 180-4).
sha256_datei <- function(pfad) {
  if (!file.exists(pfad)) stop("sha256_datei: Datei nicht gefunden: ", pfad, call. = FALSE)
  digest::digest(pfad, algo = "sha256", file = TRUE)
}

# Duenner Sammler der Verarbeitungsumgebung (GoBD Rz. 152: identifizierbare
# Umgebung je Lauf). Wirft NIE -- alle Fehlerpfade werden abgefangen;
# Determinismus-Feinheiten (dirty-Flag) liegen bewusst hier, nicht im Kern.
umgebung_erfassen <- function() {
  git_sha <- tryCatch({
    out <- as.character(suppressWarnings(system2(
      "git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)))
    if (length(out) == 1L && grepl("^[0-9a-f]{40}$", out)) out else NA_character_
  }, error = function(e) NA_character_)
  git_dirty <- tryCatch({
    out <- suppressWarnings(system2(
      "git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE))
    st <- attr(out, "status")
    if (!is.null(st) && st != 0L) NA else length(out) > 0L && any(nzchar(out))
  }, error = function(e) NA)
  # Das Repo traegt aktuell KEIN renv.lock; der Paketstand ist ueber
  # tests/deps.R im git_sha mitversioniert (dokumentierte Luecke bis renv).
  renv_lock <- tryCatch(
    if (file.exists("renv.lock")) sha256_datei("renv.lock") else NA_character_,
    error = function(e) NA_character_)
  list(r_version = as.character(getRversion()), plattform = R.version$platform,
       git_sha = git_sha, git_dirty = git_dirty, renv_lock_sha256 = renv_lock)
}

# Hilfsfunktion: baut aus einem Pfadvektor den data.frame-Vertrag mit exakt
# den Spalten name/pfad/sha256/groesse_bytes; character(0) ergibt den
# 0-Zeilen-data.frame MIT den vier Spalten.
.manifest_df_aus_pfaden <- function(pfade, feld) {
  pfade <- unname(pfade)
  if (length(pfade) == 0L) {
    return(data.frame(name = character(0), pfad = character(0),
                      sha256 = character(0), groesse_bytes = numeric(0),
                      stringsAsFactors = FALSE))
  }
  fehlt <- pfade[!file.exists(pfade)]
  if (length(fehlt)) {
    stop("manifest_erstellen: ", feld, "-Datei nicht gefunden: ",
         paste(fehlt, collapse = ", "), call. = FALSE)
  }
  data.frame(name = basename(pfade), pfad = pfade,
             sha256 = vapply(pfade, sha256_datei, character(1), USE.NAMES = FALSE),
             groesse_bytes = as.numeric(file.info(pfade)$size),
             stringsAsFactors = FALSE)
}

# Gemeinsamer seeds-Pruefer fuer manifest_erstellen und manifest_validieren:
# liefert den Befund als String oder NULL wenn in Ordnung.
.manifest_seeds_befund <- function(seeds) {
  if (!is.numeric(seeds) || length(seeds) < 1L) {
    return("muss ein numerischer Vektor mit Laenge >= 1 sein")
  }
  nm <- names(seeds)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm) > 0L) {
    return("braucht eindeutige, nicht leere Namen")
  }
  if (any(!is.finite(seeds))) return("muss endliche Werte tragen")
  if (any(seeds != round(seeds))) return("muss ganzzahlige Werte tragen")
  NULL
}

# Gemeinsamer kontrollen-Pruefer (v2) fuer manifest_erstellen und
# manifest_validieren: liefert list(feld, warum) mit dem verletzten Feldnamen
# oder NULL wenn in Ordnung.
.manifest_kontrollen_befund <- function(k) {
  b <- function(feld, warum) list(feld = feld, warum = warum)
  if (!is.data.frame(k)) return(b("kontrollen", "muss ein data.frame sein"))
  if (!identical(names(k), .manifest_k_spalten)) {
    return(b("kontrollen", paste0("braucht exakt die Spalten ",
                                  paste(.manifest_k_spalten, collapse = "/"))))
  }
  if (nrow(k) < 1L) return(b("kontrollen", "braucht mindestens 1 Zeile"))
  if (!is.character(k$name) || anyNA(k$name) || any(!nzchar(k$name)) ||
      anyDuplicated(k$name) > 0L) {
    return(b("kontrollen$name", "muss eindeutige, nicht leere Strings tragen"))
  }
  if (!is.logical(k$bestanden) || anyNA(k$bestanden)) {
    return(b("kontrollen$bestanden", "muss logical ohne NA sein"))
  }
  for (sp in c("soll", "ist", "details")) {
    if (!is.character(k[[sp]]) || anyNA(k[[sp]])) {
      return(b(paste0("kontrollen$", sp), "muss character ohne NA sein"))
    }
  }
  NULL
}

# Injektive, byte-feste Kanonisierung fuer die run_id-Ableitung: je Token
# Laengen-Praefix "n:" (Dezimal, Bytelaenge der UTF-8-Repraesentation) vor den
# enc2utf8-normalisierten Bytes -- Segmentgrenzen sind damit unfaelschbar, die
# Byte-Folge locale- und encoding-unabhaengig. Schema analog .ptok aus
# R/protokoll.R (protokoll v2), aber als BEWUSSTE lokale Kopie OHNE
# Datei-Abhaengigkeit zu R/protokoll.R -- manifest.R bleibt eigenstaendig
# sourcebar.
.manifest_kanon_bytes <- function(tokens) {
  teile <- lapply(tokens, function(x) {
    b <- charToRaw(enc2utf8(x))
    c(charToRaw(paste0(length(b), ":")), b)
  })
  do.call(c, teile)
}

# Reiner, deterministischer Kern -- zweimal dieselben Argumente liefern
# identical() dasselbe Objekt. Kein Sys.time(), kein RNG. kontrollen = NULL
# (Default) liefert das v1-Objekt wie bisher; ein kontrollen-data.frame nach
# dem Spaltenvertrag liefert manifest_version 2L mit kontrollen als zehntem
# Feld nach outputs (Argument bewusst ANS ENDE der Signatur: bestehende
# Positionsaufrufe bleiben gueltig).
manifest_erstellen <- function(eingaben, parameter, seeds,
                               outputs = character(0), zeit, user,
                               umgebung = umgebung_erfassen(),
                               kontrollen = NULL) {
  # Argumentpruefungen zuerst (benennbare Befunde).
  if (!is.character(eingaben) || length(eingaben) < 1L || anyNA(eingaben)) {
    stop("manifest_erstellen: eingaben muss ein character-Vektor mit Laenge >= 1 ",
         "sein (ein Lauf ohne Eingaben ist nicht rekonstruierbar).", call. = FALSE)
  }
  if (!is.character(outputs) || anyNA(outputs)) {
    stop("manifest_erstellen: outputs muss ein character-Vektor ohne NA sein.", call. = FALSE)
  }
  if (!is.list(parameter) || is.data.frame(parameter)) {
    stop("manifest_erstellen: parameter muss eine Liste sein.", call. = FALSE)
  }
  if (length(parameter) > 0L) {
    nm <- names(parameter)
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm) > 0L) {
      stop("manifest_erstellen: parameter braucht eindeutige, nicht leere Namen.", call. = FALSE)
    }
  }
  seeds_befund <- .manifest_seeds_befund(seeds)
  if (!is.null(seeds_befund)) stop("manifest_erstellen: seeds ", seeds_befund, ".", call. = FALSE)
  if (!inherits(zeit, "POSIXct") || length(zeit) != 1L || anyNA(zeit)) {
    stop("manifest_erstellen: zeit muss ein einzelner POSIXct-Zeitpunkt sein.", call. = FALSE)
  }
  if (!is.character(user) || length(user) != 1L || is.na(user) || !nzchar(user)) {
    stop("manifest_erstellen: user muss ein nicht leerer String sein.", call. = FALSE)
  }
  if (!is.null(kontrollen)) {
    kb <- .manifest_kontrollen_befund(kontrollen)
    if (!is.null(kb)) stop("manifest_erstellen: ", kb$feld, " ", kb$warum, ".", call. = FALSE)
  }

  eingaben_df <- .manifest_df_aus_pfaden(eingaben, "eingaben")
  outputs_df  <- .manifest_df_aus_pfaden(outputs, "outputs")

  # UTC-Normierung macht den Stempel zeitzonenfrei reproduzierbar.
  zeitstempel <- format(zeit, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # run_id deterministisch: UTC-Zeitanteil plus 8 Hexzeichen aus sha256 der
  # kanonischen Verkettung (sortierte Eingabe-Hashes, Seed-Namen, Seed-Werte,
  # user) -- gleicher Input gleiche id, anderer Seed andere id. Die
  # Verkettung ist jetzt INJEKTIV (Length-Prefix je Token) und BYTE-FEST
  # (enc2utf8, radix-Sortierung der Hashes) -- Review-Fix 12.07.2026, Spec
  # tests/test_manifest.R Abschnitt (8); Komponenten und Reihenfolge
  # unveraendert, Suffixe identischer Inputs aendern sich gegenueber der
  # Erstfassung.
  kanon_tokens <- c(sort(eingaben_df$sha256, method = "radix"), names(seeds),
                    sprintf("%.0f", seeds), user)
  suffix <- substr(digest::digest(.manifest_kanon_bytes(kanon_tokens),
                                  algo = "sha256", serialize = FALSE), 1, 8)
  run_id <- paste0("run-", format(zeit, "%Y%m%dT%H%M%SZ", tz = "UTC"), "-", suffix)

  m <- list(manifest_version = 1L, run_id = run_id, zeitstempel = zeitstempel,
            user = user, umgebung = umgebung, eingaben = eingaben_df,
            parameter = parameter, seeds = seeds, outputs = outputs_df)
  if (is.null(kontrollen)) return(m)   # v1 BIT-IDENTISCH wie vor der Erweiterung
  m$manifest_version <- 2L
  m$kontrollen <- kontrollen           # zehntes Feld ANS ENDE (nach outputs)
  m
}

# Deterministischer Schema-Validator ohne LLM. stop() nennt den VERLETZTEN
# FELDNAMEN (Rz.-152-Pruefbarkeit heisst benennbare Befunde), sonst
# invisible(TRUE).
manifest_validieren <- function(m) {
  fehl <- function(feld, warum) {
    stop("Manifest ungueltig: Feld '", feld, "' ", warum, call. = FALSE)
  }
  # nicht leerer Ein-Element-String
  chr1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  # NA oder Kleinbuchstaben-Hexstring der Laenge n
  na_oder_hex <- function(x, n) {
    length(x) == 1L && (is.na(x) || (is.character(x) &&
      grepl(paste0("^[0-9a-f]{", n, "}$"), x)))
  }
  pruef_df <- function(df, feld, min_zeilen) {
    if (!is.data.frame(df)) fehl(feld, "muss ein data.frame sein")
    if (!identical(names(df), .manifest_df_spalten)) {
      fehl(feld, paste0("braucht exakt die Spalten ",
                        paste(.manifest_df_spalten, collapse = "/")))
    }
    if (nrow(df) < min_zeilen) fehl(feld, paste0("braucht mindestens ", min_zeilen, " Zeile(n)"))
    if (nrow(df) > 0L) {
      if (!is.character(df$name) || anyNA(df$name) || any(!nzchar(df$name)))
        fehl(paste0(feld, "$name"), "muss nicht leere Strings tragen")
      if (!is.character(df$pfad) || anyNA(df$pfad) || any(!nzchar(df$pfad)))
        fehl(paste0(feld, "$pfad"), "muss nicht leere Strings tragen")
      if (!is.character(df$sha256) || anyNA(df$sha256) ||
          !all(grepl("^[0-9a-f]{64}$", df$sha256)))
        fehl(paste0(feld, "$sha256"),
             "muss je Zeile 64 Kleinbuchstaben-Hexzeichen tragen")
      if (!is.numeric(df$groesse_bytes) || any(!is.finite(df$groesse_bytes)) ||
          any(df$groesse_bytes < 0))
        fehl(paste0(feld, "$groesse_bytes"), "muss numerisch >= 0 sein")
    }
  }

  # Pruefreihenfolge wie im Handoff-Vertrag festgelegt. Versionsbewusst
  # (Anbindungs-Einheit): die manifest_version legt die EXAKTE Soll-
  # Namensfolge fest -- 1L die neun Kernfelder (ein kontrollen-Feld ist dann
  # ueber die Namensfolge ein benannter Befund), 2L zehn Felder mit
  # kontrollen an Position 10 (nach outputs).
  if (!is.list(m) || is.data.frame(m)) fehl("manifest", "muss eine Liste sein")
  if (!identical(m$manifest_version, 1L) && !identical(m$manifest_version, 2L)) {
    fehl("manifest_version", "muss identical 1L (ohne kontrollen) oder 2L (mit kontrollen) sein")
  }
  felder_soll <- if (identical(m$manifest_version, 2L)) {
    c(.manifest_felder_top, "kontrollen")
  } else .manifest_felder_top
  if (!identical(names(m), felder_soll)) {
    abw <- unique(c(setdiff(felder_soll, names(m)),
                    setdiff(names(m), felder_soll)))
    if (!length(abw)) abw <- felder_soll
    fehl(paste(abw, collapse = ", "),
         paste0("-- Top-Level-Namen muessen exakt ",
                paste(felder_soll, collapse = "/"), " sein"))
  }
  if (!chr1(m$run_id) || !grepl("^run-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{8}$", m$run_id)) {
    fehl("run_id", "entspricht nicht run-<UTC-Zeitanteil>-<8 Hexzeichen>")
  }
  if (!chr1(m$zeitstempel) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", m$zeitstempel)) {
    fehl("zeitstempel", "entspricht nicht dem UTC-Format JJJJ-MM-TTThh:mm:ssZ")
  }
  if (!chr1(m$user)) fehl("user", "muss ein nicht leerer String sein")

  u <- m$umgebung
  if (!is.list(u) || is.data.frame(u)) fehl("umgebung", "muss eine Liste sein")
  if (!identical(names(u), .manifest_umg_felder)) {
    abw <- unique(c(setdiff(.manifest_umg_felder, names(u)),
                    setdiff(names(u), .manifest_umg_felder)))
    if (!length(abw)) abw <- .manifest_umg_felder
    fehl("umgebung", paste0("braucht exakt die Felder ",
                            paste(.manifest_umg_felder, collapse = "/"),
                            " -- abweichend: ", paste(abw, collapse = ", ")))
  }
  if (!chr1(u$r_version)) fehl("umgebung$r_version", "muss ein nicht leerer String sein")
  if (!chr1(u$plattform)) fehl("umgebung$plattform", "muss ein nicht leerer String sein")
  if (!na_oder_hex(u$git_sha, 40L)) {
    fehl("umgebung$git_sha", "muss NA oder 40 Kleinbuchstaben-Hexzeichen sein")
  }
  if (!(is.logical(u$git_dirty) && length(u$git_dirty) == 1L)) {
    fehl("umgebung$git_dirty", "muss logical der Laenge 1 sein (NA erlaubt)")
  }
  if (!na_oder_hex(u$renv_lock_sha256, 64L)) {
    fehl("umgebung$renv_lock_sha256", "muss NA oder 64 Kleinbuchstaben-Hexzeichen sein")
  }

  pruef_df(m$eingaben, "eingaben", 1L)
  pruef_df(m$outputs, "outputs", 0L)

  if (!is.list(m$parameter) || is.data.frame(m$parameter)) {
    fehl("parameter", "muss eine Liste sein")
  }
  if (length(m$parameter) > 0L) {
    nm <- names(m$parameter)
    if (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm) > 0L) {
      fehl("parameter", "braucht eindeutige, nicht leere Namen")
    }
  }
  seeds_befund <- .manifest_seeds_befund(m$seeds)
  if (!is.null(seeds_befund)) fehl("seeds", seeds_befund)

  if (identical(m$manifest_version, 2L)) {
    kb <- .manifest_kontrollen_befund(m$kontrollen)
    if (!is.null(kb)) fehl(kb$feld, kb$warum)
  }

  invisible(TRUE)
}

# WRITE-ONCE-Persistierung (GoBD Rz. 58 ff./107-Anschluss: keine
# Ueberschreibung von Nachweisen). Write-once sichert auf ANWENDUNGSEBENE;
# Dateisystem-/WORM-Schutz ist Betreiberpflicht der Verfahrensdokumentation
# (phase-14), die Funktion ersetzt ihn nicht.
manifest_schreiben <- function(m, pfad) {
  # Validierung ZUERST: bei invalidem Objekt entsteht KEINE Datei.
  manifest_validieren(m)
  if (file.exists(pfad)) {
    stop("manifest_schreiben: write-once verletzt, Datei existiert bereits: ",
         pfad, call. = FALSE)
  }
  m_json <- m
  # JSON-Grenzfall: benannte atomare Vektoren verlieren bei jsonlite ihre
  # Namen -- seeds als benannte Liste serialisieren, damit die Objektform die
  # Seed-Namen traegt; manifest_lesen macht sie per unlist() rueckgaengig.
  m_json$seeds <- as.list(m$seeds)
  jsonlite::write_json(m_json, pfad, auto_unbox = TRUE, digits = NA,
                       pretty = TRUE, na = "null")
  invisible(pfad)
}

# Ruecklese fuer die retrograde Pruefung -- Normalisierung nach dem
# JSON-Roundtrip, sodass das Ergebnis manifest_validieren besteht und
# feldtreu zum Original ist.
manifest_lesen <- function(pfad) {
  if (!file.exists(pfad)) stop("manifest_lesen: Datei nicht gefunden: ", pfad, call. = FALSE)
  m <- jsonlite::read_json(pfad, simplifyVector = TRUE)
  m$manifest_version <- as.integer(m$manifest_version)
  # seeds: JSON-Objekt kommt als benannte Liste zurueck -> benannter
  # numerischer Vektor.
  m$seeds <- unlist(m$seeds)
  # umgebung: JSON-null kommt als NULL zurueck -> je Feld auf den passenden
  # NA-Typ abbilden, Feldreihenfolge wiederherstellen.
  u <- m$umgebung
  als_chr <- function(x) {
    if (is.null(x) || length(x) == 0L || is.na(x[1])) NA_character_ else as.character(x)
  }
  m$umgebung <- list(
    r_version        = als_chr(u$r_version),
    plattform        = als_chr(u$plattform),
    git_sha          = als_chr(u$git_sha),
    git_dirty        = if (is.null(u$git_dirty) || length(u$git_dirty) == 0L) NA
                       else as.logical(u$git_dirty),
    renv_lock_sha256 = als_chr(u$renv_lock_sha256))
  # eingaben/outputs: ein leeres JSON-Array traegt keine Spaltennamen ->
  # 0-Zeilen-data.frame MIT den vier Spalten rekonstruieren (dokumentierter
  # JSON-Grenzfall); sonst Spaltentypen normalisieren. parameter bleibt
  # Liste, numerische Werte bleiben numerisch (jsonlite).
  norm_df <- function(x) {
    if (is.null(x) || (is.data.frame(x) && nrow(x) == 0L) ||
        (!is.data.frame(x) && length(x) == 0L)) {
      return(data.frame(name = character(0), pfad = character(0),
                        sha256 = character(0), groesse_bytes = numeric(0),
                        stringsAsFactors = FALSE))
    }
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    df$groesse_bytes <- as.numeric(df$groesse_bytes)
    df
  }
  m$eingaben <- norm_df(m$eingaben)
  m$outputs  <- norm_df(m$outputs)
  # kontrollen (nur v2): nach dem JSON-Roundtrip wieder als data.frame mit
  # den fuenf Spalten, bestanden wieder logical, uebrige Spalten character.
  # Typen werden NUR bei exakt passender Spaltenfolge normalisiert -- eine
  # abweichende Struktur bleibt stehen, damit der Validator sie benennt.
  if (identical(m$manifest_version, 2L) && !is.null(m$kontrollen)) {
    k <- as.data.frame(m$kontrollen, stringsAsFactors = FALSE)
    if (identical(names(k), .manifest_k_spalten)) {
      k$name      <- as.character(k$name)
      k$bestanden <- as.logical(k$bestanden)
      k$soll      <- as.character(k$soll)
      k$ist       <- as.character(k$ist)
      k$details   <- as.character(k$details)
    }
    m$kontrollen <- k
  }
  # Retrograde Pruefung: das rueckgelesene Manifest muss den Vertrag
  # bestehen, sonst benannter Befund statt stiller Weiterverarbeitung.
  manifest_validieren(m)
  m
}
