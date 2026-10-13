# ============================================================================
#  tests/test_protokoll_hash.R  —  SPEZIFIKATION fuer phase-12-protokoll-hash,
#  gehoben auf das Protokoll-Kanon v2 (rot zuerst).
#
#  Review-Nachholungs-Befund (GPT-5.5,
#  .orch/review-pakete/befund-paket-phase-12-protokoll-hash.md): das Kanon v1
#  ('name=wert'-Paare mit '&' verkettet, Felder mit '|' — keine Escapes) ist
#  KOLLISIONSFAEHIG (High), und order() ohne method = 'radix' bzw. charToRaw
#  auf der nativen String-Repraesentation sind nicht byte-deterministisch
#  (Medium Locale-Sortierung / Medium natives Encoding). Diese Spezifikation
#  pinnt das injektive, byte-feste Kanon v2. R/protokoll.R bleibt in DIESEM
#  Schritt bewusst unveraendert (rot zuerst): die v2-Orakel- und
#  Kollisionstests MUESSEN gegen den aktuellen v1-Stand FEHLSCHLAGEN; die
#  Implementierung folgt als eigener Commit. Kein skip(), kein succeed(),
#  keine Existenz-Weiche.
#
#  Vertrag KANON v2 (wortgenau):
#   - tok(s):  s2 <- enc2utf8(s);  paste0(nchar(s2, type = "bytes"), ":", s2)
#   - wert(x): logical -> 'TRUE'/'FALSE'; character -> enc2utf8(as.character(x));
#     sonst (numeric) -> sprintf('%.15g', x). Die typ-uebergreifende
#     Wert-Normierung (logical TRUE und String 'TRUE' fallen zusammen) ist
#     BEWUSST unveraendert Bestand (jsonlite-Roundtrip-Robustheit).
#   - daten_kanon: bei length(daten)==0 der Leerstring ''; sonst
#     nm2 <- enc2utf8(names(daten)); o <- order(nm2, method = 'radix');
#     Konkatenation OHNE Trennzeichen ueber die sortierten Felder:
#     je Feld paste0(tok(nm2[i]), tok(wert(daten[[i]]))).
#   - kanon: paste0(tok(wert(seq)), tok(typ), tok(zeitstempel), tok(user),
#     tok(daten_kanon), tok(vorheriger_hash)) — feste Sechs-Token-
#     Aussenstruktur, daten_kanon als GANZES nochmals durch tok().
#   - hash: digest::digest(charToRaw(enc2utf8(kanon)), algo = 'sha256',
#     serialize = FALSE). Rueckgabe: 64-stelliger Kleinbuchstaben-Hexstring.
#  Warum diese Struktur: Length-Prefix-Tokens sind praefixfrei dekodierbar —
#  die Tokenfolge ist aus dem Bytestrom eindeutig rekonstruierbar, damit ist
#  das Kanon INJEKTIV (kein '&'/'='/'|'-Kollisionspaar mehr moeglich);
#  enc2utf8 + radix-Sortierung machen es locale- und plattformfest.
#
#  .protokoll_hash(seq, typ, zeitstempel, user, daten, vorheriger_hash) bleibt
#  ein reiner, zustandsloser Hash-Primitive; kein Datei-IO, kein RNG, kein
#  Sys.time(), KEINE Eingabevalidierung (die leisten protokoll_anhaengen /
#  protokoll_pruefen).
#
#  Orakel-Politik: KEIN Hash prueft sich selbst (Blockprinzip; Lehre:
#  Treue-Tests haben keine Mutationskraft, wenn beide Seiten denselben Code
#  ziehen). Deshalb zwei getrennte Ebenen:
#   (a) Das PRIMITIVE wird gegen EXTERNE, eingefrorene Literale gepinnt: die
#       SHA-256-Testvektoren fuer 'abc' und die leere Nachricht aus NIST
#       FIPS 180-4 (woertlich uebernommen, repo-verifiziert in
#       tests/test_manifest.R). UNVERAENDERT gegenueber v1.
#   (b) Die KOMPOSITION (Sechs-Token-Aussenstruktur, Length-Prefix-Tokens,
#       radix-Namenssortierung, Normierungs-Zweige, UTF-8-Byte-Festigkeit)
#       wird gegen ein INLINE aus dem Kanon-v2-Wortlaut nachgebautes
#       kanon-Orakel gepinnt (.orakel_tok/.orakel_wert/.orakel_hash unten) —
#       nie durch Ablesen aus R/protokoll.R, nie durch Aufruf der getesteten
#       Funktion oder ihrer internen Helfer (Generator-Verifier-Trennung).
#       digest ist dabei zulaessiges Orakel-Primitiv, weil (a) seine
#       sha256-Korrektheit unabhaengig festnagelt.
#  Erwartete Hashes sind NIE hartcodierte Literale aus einem Probelauf der
#  Implementierung — das waere Selbstbezug.
#
#  Keine Anker-/Tier-1-Literale, keine Fixture-Mappe, kein MC, kein
#  Engine-Aufruf, kein Datei-IO — nur deterministische Eingaben, kleine
#  Laufzeit.
#
#  Bestandsschutz: beruehrt weder kernel/ noch R/manifest.R noch
#  R/manifest_anbindung.R noch eine der neun test_manifest*-Dateien
#  (Blockprinzip c17d29a: Erweiterung statt Bruch). Keine neuen Pakete —
#  digest ist seit phase-12-runmanifest-kern im need-Vektor von tests/deps.R.
# ============================================================================

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "protokoll.R"))  # v1-Stand: v2-Orakeltests unten sind rot zuerst

# ---------------------------------------------------------------------------
# Gepinnte Hash-Orakel (LITERALE, Herkunft: NIST FIPS 180-4, eingefrorene
# SHA-256-Testvektoren).
# ---------------------------------------------------------------------------
.hash_leer <- "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
.hash_abc  <- "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

# Genesis-Sentinel und feste Ereignis-Koordinaten (handgesetzte Literale).
.genesis <- strrep("0", 64)
.z       <- "2026-07-09T08:00:00Z"
.user    <- "anker"

# ---------------------------------------------------------------------------
# UNABHAENGIGES kanon-Orakel: aus dem Kanon-v2-Wortlaut des Befund-Pakets
# nachgebaut, NICHT aus R/protokoll.R importiert. Bewusst eigene Namen
# (.orakel_*), damit kein Symbol der Implementierung durchschlaegt.
# ---------------------------------------------------------------------------

# Length-Prefix-Token: '<bytes>:<utf8-string>'. Praefixfrei dekodierbar, damit
# ist jede Tokenfolge aus dem Bytestrom eindeutig rekonstruierbar (Injektivitaet).
.orakel_tok <- function(s) {
  s2 <- enc2utf8(s)
  paste0(nchar(s2, type = "bytes"), ":", s2)
}

# Wert-Normierung (typ-uebergreifend, bewusst wie v1 — nur der character-Zweig
# wird UTF-8-fest): logical -> 'TRUE'/'FALSE', character -> enc2utf8, sonst
# (numeric) sprintf('%.15g').
.orakel_wert <- function(x) {
  if (is.logical(x))   return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.character(x)) return(enc2utf8(as.character(x)))
  sprintf("%.15g", x)
}

.orakel_hash <- function(seq, typ, zeitstempel, user, daten, vorheriger_hash) {
  if (length(daten) == 0L) {
    daten_kanon <- ""
  } else {
    nm2 <- enc2utf8(names(daten))
    o <- order(nm2, method = "radix")
    daten_kanon <- paste0(vapply(o, function(i)
      paste0(.orakel_tok(nm2[i]), .orakel_tok(.orakel_wert(daten[[i]]))),
      character(1)), collapse = "")
  }
  kanon <- paste0(.orakel_tok(.orakel_wert(seq)), .orakel_tok(typ),
                  .orakel_tok(zeitstempel), .orakel_tok(user),
                  .orakel_tok(daten_kanon), .orakel_tok(vorheriger_hash))
  digest::digest(charToRaw(enc2utf8(kanon)), algo = "sha256",
                 serialize = FALSE)
}

# Formvertrag: 64-stelliger Kleinbuchstaben-Hexstring, Skalar.
.erwarte_hexhash <- function(h) {
  expect_type(h, "character")
  expect_length(h, 1L)
  expect_identical(nchar(h), 64L)
  expect_true(grepl("^[0-9a-f]{64}$", h))
}

# Ereignis-daten der Spec-Beispiele (Einfuegereihenfolge ABSICHTLICH
# unsortiert, damit die Namenssortierung Mutationskraft bekommt).
.d_anker <- list(name = "M.xlsx", menge = -0.1)


# ===========================================================================
#  (1) Roh-SHA-256-Primitive gegen NIST FIPS 180-4
#      'kein Hash prueft sich selbst' beginnt beim Primitive.
# ===========================================================================

test_that("protokoll_hash: SHA-256-Primitive gegen die NIST-FIPS-180-4-Testvektoren gepinnt", {
  expect_identical(
    digest::digest(charToRaw("abc"), algo = "sha256", serialize = FALSE),
    .hash_abc)
  expect_identical(
    digest::digest(charToRaw(""), algo = "sha256", serialize = FALSE),
    .hash_leer)
})


# ===========================================================================
#  (2) Formvertrag der Rueckgabe
# ===========================================================================

test_that("protokoll_hash: liefert einen 64-stelligen Kleinbuchstaben-Hexstring", {
  h <- .protokoll_hash(1L, "import", .z, .user, .d_anker, .genesis)
  .erwarte_hexhash(h)
})


# ===========================================================================
#  (3) Unabhaengige Neuberechnung der KOMPOSITION (Mutationskraft)
#      Feld vergessen, Reihenfolge vertauscht, Token-Struktur geaendert,
#      Normierung verbogen, character-Zweig fehlt -> hier rot.
# ===========================================================================

test_that("protokoll_hash: Ereignis-Hash identisch zum INLINE nach der Spec nachgerechneten kanon-Orakel", {
  h   <- .protokoll_hash(1L, "import", .z, .user, .d_anker, .genesis)
  exp <- .orakel_hash(1L, "import", .z, .user, .d_anker, .genesis)
  expect_identical(h, exp)

  # Der typ-Wert fliesst ein (nur der Wert, nicht validiert): ein anderer der
  # sechs zulaessigen Typen ergibt einen anderen Hash — und trifft wieder das
  # unabhaengige Orakel. Negativ-Pin gegen 'typ ignoriert'.
  h_lauf <- .protokoll_hash(1L, "lauf", .z, .user, .d_anker, .genesis)
  expect_identical(h_lauf, .orakel_hash(1L, "lauf", .z, .user, .d_anker, .genesis))
  expect_false(identical(h_lauf, h))

  # seq, zeitstempel und user fliessen ebenfalls ein (Negativ-Pins gegen
  # 'Feld aus der Verkettung gefallen').
  expect_false(identical(.protokoll_hash(2L, "import", .z, .user, .d_anker, .genesis), h))
  expect_false(identical(
    .protokoll_hash(1L, "import", "2026-07-09T08:00:01Z", .user, .d_anker, .genesis), h))
  expect_false(identical(.protokoll_hash(1L, "import", .z, "pruefer", .d_anker, .genesis), h))
})


# ===========================================================================
#  (4) Normierungs-Abdeckung: alle drei wert()-Zweige klingen im Lauf
# ===========================================================================

test_that("protokoll_hash: character-, logical- und numeric-Normierung — je Zweig gepinnt", {
  # Vier Werte, Einfuegereihenfolge != sortierte Reihenfolge:
  #   character -> enc2utf8(as.character); TRUE/FALSE -> 'TRUE'/'FALSE';
  #   numeric   -> sprintf('%.15g')  (1/3 unterscheidet %.15g von %.14g)
  # Ein Builder ohne character-Zweig laeuft hier in sprintf('%.15g', "lauf.rds")
  # und stirbt — der fehlende Zweig ist damit beobachtbar, nicht still.
  d_alle <- list(name = "lauf.rds", ok = TRUE, dirty = FALSE, betrag = 1/3)
  h_alle <- .protokoll_hash(7L, "kontrolle", .z, .user, d_alle, .hash_abc)
  .erwarte_hexhash(h_alle)
  expect_identical(h_alle, .orakel_hash(7L, "kontrolle", .z, .user, d_alle, .hash_abc))

  # Ereignis OHNE character-Wert (nur numeric/logical): haelt den
  # numeric/logical-Pfad separat fest — gueltiger, aber VERSCHIEDENER Hash.
  d_ohne <- list(ok = TRUE, betrag = 1/3)
  h_ohne <- .protokoll_hash(7L, "kontrolle", .z, .user, d_ohne, .hash_abc)
  .erwarte_hexhash(h_ohne)
  expect_identical(h_ohne, .orakel_hash(7L, "kontrolle", .z, .user, d_ohne, .hash_abc))
  expect_false(identical(h_ohne, h_alle))

  # Praezisions-Pin: %.15g, nicht %.14g. Zwei numerisch VERSCHIEDENE Werte,
  # die erst ab der 15. signifikanten Stelle auseinandergehen, muessen
  # verschiedene Hashes ergeben.
  a <- .protokoll_hash(1L, "lauf", .z, .user, list(x = 0.333333333333333), .genesis)
  b <- .protokoll_hash(1L, "lauf", .z, .user, list(x = 0.33333333333333), .genesis)
  expect_false(identical(a, b))

  # logical wird 'TRUE'/'FALSE', nicht 1/0: FALSE und 0 sind unterscheidbar.
  expect_false(identical(
    .protokoll_hash(1L, "lauf", .z, .user, list(f = FALSE), .genesis),
    .protokoll_hash(1L, "lauf", .z, .user, list(f = 0),     .genesis)))
})


# ===========================================================================
#  (5) Namensreihenfolge-Invarianz (Negativ-Pin gegen Reihenfolge-Abhaengigkeit)
# ===========================================================================

test_that("protokoll_hash: Hash ist unabhaengig von der Einfuegereihenfolge der daten-Namen", {
  expect_identical(
    .protokoll_hash(1L, "import", .z, .user, list(b = 2, a = 1), .genesis),
    .protokoll_hash(1L, "import", .z, .user, list(a = 1, b = 2), .genesis))

  # Die Sortierung ist ECHT (nicht 'Reihenfolge egal, weil Namen ignoriert'):
  # gleiche Werte unter getauschten Namen ergeben einen ANDEREN Hash.
  expect_false(identical(
    .protokoll_hash(1L, "import", .z, .user, list(a = 1, b = 2), .genesis),
    .protokoll_hash(1L, "import", .z, .user, list(a = 2, b = 1), .genesis)))

  # Und sie trifft das unabhaengige Orakel (das ebenfalls radix-sortiert).
  expect_identical(
    .protokoll_hash(1L, "import", .z, .user, list(b = 2, a = 1), .genesis),
    .orakel_hash(1L, "import", .z, .user, list(a = 1, b = 2), .genesis))
})


# ===========================================================================
#  (6) vorheriger_hash fliesst ein; leere daten sind ein gueltiger Grenzfall
# ===========================================================================

test_that("protokoll_hash: vorheriger_hash veraendert den Hash (Kettenglied, nicht Dekoration)", {
  h_genesis <- .protokoll_hash(1L, "import", .z, .user, .d_anker, .genesis)
  h_kette   <- .protokoll_hash(1L, "import", .z, .user, .d_anker, .hash_abc)
  .erwarte_hexhash(h_kette)
  expect_false(identical(h_kette, h_genesis))   # Negativ-Pin: nicht ignoriert
  expect_identical(h_kette, .orakel_hash(1L, "import", .z, .user, .d_anker, .hash_abc))
})

test_that("protokoll_hash: leere daten list() -> daten_kanon '' und gueltiger 64-Hex", {
  h_leer <- .protokoll_hash(1L, "finalisierung", .z, .user, list(), .genesis)
  .erwarte_hexhash(h_leer)

  # Orakel-Grenzfall, handverkettet: daten_kanon ist der Leerstring, sein
  # Token damit '0:'. Die Sechs-Token-Aussenstruktur bleibt vollstaendig —
  # leere daten verschwinden NICHT aus dem Kanon (Injektivitaet).
  kanon_leer <- paste0(.orakel_tok(.orakel_wert(1L)), .orakel_tok("finalisierung"),
                       .orakel_tok(.z), .orakel_tok(.user),
                       .orakel_tok(""), .orakel_tok(.genesis))
  expect_identical(
    h_leer,
    digest::digest(charToRaw(enc2utf8(kanon_leer)), algo = "sha256",
                   serialize = FALSE))

  # Und der length(daten)==0-Zweig des Orakels trifft denselben Wert (K4).
  expect_identical(
    h_leer, .orakel_hash(1L, "finalisierung", .z, .user, list(), .genesis))

  # Leere daten sind nicht dasselbe wie irgendein befuelltes daten-Feld.
  expect_false(identical(
    h_leer,
    .protokoll_hash(1L, "finalisierung", .z, .user, list(a = 1), .genesis)))
})


# ===========================================================================
#  (7) Determinismus und RNG-Waechter: kein RNG im GoBD-Pfad
# ===========================================================================

test_that("protokoll_hash: deterministisch und RNG-frei", {
  # Zweimal dieselben Argumente -> derselbe Hash.
  expect_identical(
    .protokoll_hash(3L, "export", .z, .user, .d_anker, .hash_abc),
    .protokoll_hash(3L, "export", .z, .user, .d_anker, .hash_abc))

  # .Random.seed initialisieren, dann Snapshot.
  set.seed(1)
  invisible(runif(1))
  rng_vor <- .Random.seed

  invisible(.protokoll_hash(3L, "korrektur", .z, .user, .d_anker, .hash_abc))

  expect_identical(.Random.seed, rng_vor)       # kein RNG-Verbrauch
})


# ===========================================================================
#  (8) KANON-v2-RIEGEL: Injektivitaet und Byte-Festigkeit
#      (Review-Nachholungs-Befund; gegen den v1-Stand ROT — rot zuerst)
# ===========================================================================

test_that("protokoll_hash: K1 — daten-Injektivitaet, kein '&'/'='-Kollisionspaar", {
  # v1 verkettete 'name=wert'-Paare mit '&' ohne Escapes: list(a = '1&b=2')
  # und list(a = '1', b = 2) ergaben BEIDE den daten_kanon 'a=1&b=2' und
  # damit denselben Hash. Die Length-Prefix-Tokens von v2 trennen beide.
  h_a <- .protokoll_hash(1L, "import", "2026-01-01T00:00:00Z", "u",
                         list(a = "1&b=2"), strrep("0", 64))
  h_b <- .protokoll_hash(1L, "import", "2026-01-01T00:00:00Z", "u",
                         list(a = "1", b = 2), strrep("0", 64))
  expect_false(identical(h_a, h_b))

  # Beide treffen zudem das unabhaengige v2-Orakel.
  expect_identical(h_a, .orakel_hash(1L, "import", "2026-01-01T00:00:00Z", "u",
                                     list(a = "1&b=2"), strrep("0", 64)))
  expect_identical(h_b, .orakel_hash(1L, "import", "2026-01-01T00:00:00Z", "u",
                                     list(a = "1", b = 2), strrep("0", 64)))
})

test_that("protokoll_hash: K2 — Feld-Injektivitaet, kein '|'-Kollisionspaar", {
  # v1 verkettete die sechs Kanon-Felder mit '|' ohne Escapes: (typ 'a|b',
  # zeitstempel 'c') und (typ 'a', zeitstempel 'b|c') ergaben dasselbe kanon
  # '...|a|b|c|...'. Die praefixfreien Tokens von v2 halten die Feldgrenzen
  # fest — die Hashes MUESSEN verschieden sein. (.protokoll_hash validiert
  # bewusst nicht; nur die Werte fliessen ein.)
  h_a <- .protokoll_hash(1L, "a|b", "c", .user, list(), .genesis)
  h_b <- .protokoll_hash(1L, "a", "b|c", .user, list(), .genesis)
  expect_false(identical(h_a, h_b))

  # Beide treffen zudem das unabhaengige v2-Orakel.
  expect_identical(h_a, .orakel_hash(1L, "a|b", "c", .user, list(), .genesis))
  expect_identical(h_b, .orakel_hash(1L, "a", "b|c", .user, list(), .genesis))
})

test_that("protokoll_hash: K3 — Umlaut-Determinismus (UTF-8-Bytes, locale-/plattformfest)", {
  # Die Umlaute werden via intToUtf8() konstruiert, damit die Datei ASCII-rein
  # bleibt und das Parsen nicht an der Quelldatei-Encoding haengt.
  ue <- intToUtf8(0xFC)   # u-Umlaut
  ae <- intToUtf8(0xE4)   # a-Umlaut
  oe <- intToUtf8(0xF6)   # o-Umlaut

  # Length-Prefix zaehlt UTF-8-BYTES, nicht Zeichen: 'schluessel' mit
  # Umlaut-ue hat 9 Zeichen, aber 10 Bytes (Inline-Pin auf das Orakel-Token).
  schluessel <- paste0("schl", ue, "ssel")
  expect_identical(.orakel_tok(schluessel), paste0("10:", schluessel))

  # Umlaut in daten-Name, daten-Wert UND user: der Hash trifft den im Test
  # unabhaengig rekonstruierten v2-Orakelwert — enc2utf8 vor nchar/charToRaw
  # macht das Ergebnis unabhaengig von der nativen Encoding der Plattform.
  d_uml <- setNames(list(paste0(ue, "ber", ae, "nderung")), schluessel)
  u_uml <- paste0("pr", ue, "fer-", oe)
  h <- .protokoll_hash(5L, "kontrolle", .z, u_uml, d_uml, .genesis)
  .erwarte_hexhash(h)
  expect_identical(h, .orakel_hash(5L, "kontrolle", .z, u_uml, d_uml, .genesis))
})

test_that("protokoll_hash: K4 — Bestandsgrenzfaelle gegen das v2-Orakel gehoben", {
  # Genesis-Sentinel als vorheriger_hash.
  expect_identical(
    .protokoll_hash(1L, "import", .z, .user, list(a = 1), .genesis),
    .orakel_hash(1L, "import", .z, .user, list(a = 1), .genesis))

  # Ein-Feld-daten.
  expect_identical(
    .protokoll_hash(2L, "lauf", .z, .user,
                    list(run_id = "run-20260709T080000Z-abcd1234"), .hash_abc),
    .orakel_hash(2L, "lauf", .z, .user,
                 list(run_id = "run-20260709T080000Z-abcd1234"), .hash_abc))

  # Mehr-Feld-daten, Einfuegereihenfolge unsortiert -> radix-Sortierung
  # greift: beide Reihenfolgen treffen denselben Orakelwert.
  expect_identical(
    .protokoll_hash(3L, "export", .z, .user,
                    list(z = 1, a = "b", m = TRUE), .hash_abc),
    .orakel_hash(3L, "export", .z, .user,
                 list(a = "b", m = TRUE, z = 1), .hash_abc))

  # Leere daten (daten_kanon '') gegen den Orakel-Leerzweig.
  expect_identical(
    .protokoll_hash(4L, "finalisierung", .z, .user, list(), .genesis),
    .orakel_hash(4L, "finalisierung", .z, .user, list(), .genesis))
})
