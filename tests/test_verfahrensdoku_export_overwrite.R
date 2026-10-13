# ============================================================================
#  tests/test_verfahrensdoku_export_overwrite.R
#
#  Mutgate-Nachschub (Rerun-Befund M2, 12.07.2026): overwrite-Vertrag von
#  export_verfahrensdoku — der Re-Export auf denselben Pfad muss gelingen
#  (wb_save overwrite = TRUE). Der Bool-Flip auf FALSE ueberlebte die Suite,
#  weil alle Bestandstests frische tempfiles nutzen; dieser Test macht den
#  Unterschied beobachtbar. Kill-Logik als rot-zuerst-Ersatz: auf dem Mutanten
#  rot (wb_save wirft bei existierender Datei), auf main gruen.
#
#  Kein skip(), kein succeed(), kein Engine-Aufruf, kein Tier-1-Zahlen-Literal,
#  kleine Laufzeit (die Funktion rechnet nicht).
# ============================================================================

library(testthat)

# Repo-Wurzel finden (testthat wechselt beim Lauf ins tests/-Verzeichnis).
.root <- getwd()
for (i in 1:6) {
  if (file.exists(file.path(.root, "kernel", "engine.R"))) break
  .root <- dirname(.root)
}
source(file.path(.root, "kernel", "engine.R"))
source(file.path(.root, "kernel", "distributions.R"))
source(file.path(.root, "R", "scenario.R"))
source(file.path(.root, "R", "manifest.R"))
source(file.path(.root, "R", "verfahrensdoku.R"))

test_that("export_verfahrensdoku ueberschreibt eine existierende Zieldatei (overwrite-Vertrag, Mutgate-Nachschub M2)", {
  # INJIZIERTE umgebung als eigene Quelle (deterministisch, kein Live-Erfassen).
  u <- list(r_version        = as.character(getRversion()),
            plattform        = "overwriteTest",
            git_sha          = NA_character_,
            git_dirty        = NA,
            renv_lock_sha256 = NA_character_)
  vd <- verfahrensdoku_erstellen(umgebung = u)
  tf <- tempfile(fileext = ".xlsx")

  # Erster Export: legt die Zieldatei an.
  r1 <- export_verfahrensdoku(vd, tf)
  expect_true(file.exists(tf))

  # ZWEITER Export AUF DENSELBEN PFAD — der Kill-Punkt: auf dem Mutanten
  # (wb_save overwrite = FALSE) wirft wb_save hier, weil tf existiert.
  expect_no_error(r2 <- export_verfahrensdoku(vd, tf))

  # Rueckgabe-Vertrag des Zweitexports (kein Torso-Rueckgabeobjekt).
  expect_identical(r2$pfad, tf)
  expect_identical(r2$blaetter, names(vd))

  # Ruecklese nach dem Zweitexport (wb_load + wb_to_df, die im Repo etablierte
  # Ruecklese): der ueberschriebene Stand ist der frische Export, kein Torso.
  wb <- openxlsx2::wb_load(tf)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), names(vd))
  review_df <- openxlsx2::wb_to_df(wb, sheet = "review", detectDates = FALSE)
  expect_true(isTRUE(all.equal(as.numeric(review_df$nummer),
                               as.numeric(1:8), tolerance = 1e-12)))

  unlink(tf)
})
