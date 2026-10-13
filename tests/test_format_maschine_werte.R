# ============================================================================
#  tests/test_format_maschine_werte.R  —  Werte-Guard fuer fmt_maschine
#  (R/format.R): sperrt die 15-signifikante-Stellen-Zusage der Maschinen-
#  Serialisierung mit Werten, die eine Off-by-one-Verengung sichtbar machen.
#
#  Herkunft: Mutgate .orch/mut-phase-13-z3-csv, Survivor M2 (digits = 15L ->
#  14L in fmt_maschine). Kein bestehender Test unterscheidet 15 von 14
#  signifikanten Stellen — die Tier-1-Zahlen und Spec-Werte tragen hoechstens
#  ~7 Stellen. Der 15-Steller ist der Unterscheider: 123456789012345 ist als
#  double exakt darstellbar (< 2^53) und braucht ALLE 15 Stellen; formatC mit
#  format = "fg" und digits = 14 rundet auf 14 signifikante Stellen, die
#  letzte Ziffer kippt ("...12350" statt "...12345") — bei digits = 15 bleibt
#  der Wert ziffernidentisch. Analog auf der Nachkommaseite.
# ============================================================================

.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "R", "format.R"))

test_that("fmt_maschine traegt 15 signifikante Stellen (Mutgate-Survivor M2: digits-Off-by-one 15L->14L)", {
  # 15-Steller, exakt als double (< 2^53): unterscheidet digits = 15
  # (ziffernidentisch) von digits = 14 (fg rundet, letzte Ziffer kippt).
  expect_identical(fmt_maschine(123456789012345), "123456789012345")
  expect_identical(fmt_maschine(0.123456789012345), "0,123456789012345")

  # Roundtrip-Probe ohne Toleranz: String -> Zahl reproduziert den
  # Eingangsvektor bitgenau.
  eingang <- c(123456789012345, 0.123456789012345)
  expect_identical(
    as.numeric(gsub(",", ".", fmt_maschine(eingang), fixed = FALSE)),
    eingang
  )
})
