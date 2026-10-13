# ============================================================================
#  tests/test_phase_gc_seam.R  —  SPEICHER-HYGIENE-NAHT der DOD-Suite
#  (Gate-A-Fix fuer phase-13-z3-csv, KEIN Produkt-Test — bewusst OHNE
#  test_that: eine gezaehlte Erwartung haette hier keine fachliche
#  Mutationskraft und koennte nur immer-gruen degradieren. test_dir SOURCT
#  die Datei, die Naht laeuft als Top-Level-Nebenwirkung; gezaehlt wird
#  nichts.)
#
#  WARUM DIESE DATEI EXISTIERT
#  ---------------------------
#  phase-13-z3-csv haengt eine weitere openxlsx2-schwere Naht in die lange
#  DOD-Suite: die geschuetzte Spec tests/test_export_z3_csv.R laedt die
#  Tier-1-Mappe (scenario_from_workbook -> wb_load) UND liest in ihrer
#  Datentreue-Pruefung ein export_xlsx wieder ein (export_xlsx + wb_load).
#  openxlsx2 haelt seinen Arbeitsspeicher hinter externen Zeigern (Rcpp/
#  pugixml), deren Umfang R's automatischer gc-Ausloeser UNTERZAEHLT — die
#  C++-Bytes bleiben bis zur Finalisierung liegen und heben die Basis-RSS.
#  Diese Bytes AKKUMULIEREN ueber die ~50 Dateien der Suite: neben der Spec
#  laden u.a. test_finalisierung*, test_manifest*, test_new_mandate*,
#  test_phase_forecast und test_phase_kop je eine Mappe (load_model /
#  scenario_from_workbook), OHNE dass R sie zwischen den Dateien freigibt.
#
#  Symptom (Gate A, run_all.R): die testthat-Ausgabe endet MITTEN in der
#  Suite — nur Punkte, KEIN F/E, KEINE Zusammenfassung. Kein Testfehler,
#  sondern ein Kill des Rscript-Prozesses (OOM), sobald der speicher-schwere
#  Schwanz der Suite grossen Speicher anfordert: der chromote-Browser der
#  shinytest2-E2E-Controls (test_phase_kop_e2e / test_phase_kor_e2e) und die
#  Monte-Carlo-Tests (test_phase_sim*). Der Kill-Punkt VARIIERT zwischen
#  Laeufen (phase_sim_deckung, phase_kor_e2e) — nicht-deterministisch, also
#  Ressourcen/Last, kein Testfehler.
#
#  DIE NAHT (und warum GENAU HIER)
#  -------------------------------
#  Ein einzelner gc(full = TRUE) an der richtigen Stelle stoesst die
#  openxlsx2-Finalizer an und gibt die akkumulierten C++-Bytes frei, BEVOR der
#  schwere Schwanz Speicher anfordert. "Richtige Stelle" = unmittelbar vor dem
#  phase_*-Schwanz: testthat::test_dir laeuft alphabetisch, und dieser
#  Dateiname sortiert robust ZWISCHEN test_phase_forecast.R ("...phase_f...")
#  und test_phase_kop.R ("...phase_g..." < "...phase_k...") — in JEDER Locale
#  gilt f < g < k. Zu diesem Zeitpunkt sind die Umgebungen aller vorherigen
#  Testdateien bereits unreferenziert, ihr openxlsx2-Speicher also
#  finalisierbar.
#
#  Generalisierbar (LESSONS): nach openxlsx2-schweren Testdateien in einer
#  langen, in EINEM Prozess laufenden Suite explizit gc(full = TRUE) an der
#  naechsten Naht — R's automatischer Ausloeser unterzaehlt den externen
#  Speicher und raeumt ihn nicht rechtzeitig ab.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei. Sourct nichts, laedt kein Paket, ruft keine Engine-/
#  Wrapper-Funktion — reine Speicher-Hygiene ueber base::gc.
# ============================================================================

# Die Naht selbst: openxlsx2-Finalizer anstossen, akkumulierten externen
# Speicher freigeben, BEVOR der phase_*-Schwanz (chromote-E2E / Monte-Carlo)
# ihn anfordert. Laeuft beim Sourcen der Datei, also an der Naht-Position.
invisible(gc(full = TRUE))
