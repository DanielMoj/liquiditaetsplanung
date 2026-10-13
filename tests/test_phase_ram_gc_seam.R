# ============================================================================
#  tests/test_phase_ram_gc_seam.R  —  ZWEITE SPEICHER-HYGIENE-NAHT der
#  DOD-Suite (Gate-A-Fix fuer phase-13-z3-index, KEIN Produkt-Test — bewusst
#  OHNE test_that: eine gezaehlte Erwartung haette hier keine fachliche
#  Mutationskraft und koennte nur immer-gruen degradieren. test_dir SOURCT die
#  Datei, die Naht laeuft als Top-Level-Nebenwirkung; gezaehlt wird nichts.)
#
#  WARUM DIESE DATEI EXISTIERT (Ergaenzung zu test_phase_gc_seam.R)
#  ----------------------------------------------------------------
#  Die erste Naht (test_phase_gc_seam.R) sortiert bei "phase_g" — UNMITTELBAR
#  vor dem phase_*-Schwanz, aber VOR den beiden chromote-Browsern der
#  shinytest2-E2E-Controls (test_phase_kop_e2e / test_phase_kor_e2e). Diese
#  Browser starten je einen headless-Chromium-Prozess und heben die RSS um ein
#  Vielfaches; ihr Speicher (chromote-Sessions + der zwischen den Dateien
#  weiter akkumulierte openxlsx2-Zeiger-Speicher aus load_model/
#  scenario_from_workbook) bleibt bis zur Finalisierung liegen. Direkt danach
#  laeuft der SCHWERSTE rechen-/speicher-lastige Block der Suite:
#  test_phase_sens (Tornado + Sobol, viele Engine-Laeufe) und der
#  Monte-Carlo-Schwanz test_phase_sim*.
#
#  Symptom (Gate A, run_all.R): die testthat-Ausgabe endet MITTEN in der Suite
#  — nur Punkte, KEIN F/E, KEINE Zusammenfassung. Kein Testfehler, sondern ein
#  Kill des Rscript-Prozesses (OOM), sobald dieser schwere Schwanz nach den
#  chromote-Browsern grossen Speicher anfordert. Der Kill-Punkt VARIIERT
#  zwischen Laeufen (phase_sens, protokoll_pruefen_feldnamen), und derselbe
#  Codestand laeuft in anderen Laeufen komplett gruen durch —
#  nicht-deterministisch, also Ressourcen/Last, kein Testfehler. Die
#  phase-13-z3-index-Tests selbst zeigen in JEDEM Lauf 40/40 gruen; sie haben
#  nur — wie phase-13-z3-csv vor ihnen — die Ressourcen-Schwelle weiter
#  angehoben.
#
#  DIE NAHT (und warum GENAU HIER)
#  -------------------------------
#  Ein einzelner gc(full = TRUE) an der richtigen Stelle stoesst die
#  openxlsx2- UND chromote-Finalizer an und gibt die akkumulierten C++-/
#  Browser-Bytes frei, BEVOR der Sensitivitaets-/Monte-Carlo-Schwanz Speicher
#  anfordert. Die erste Naht liegt zu frueh (die kop/kor-E2E laden danach neu);
#  eine zweite Naht muss NAH am Verbraucher liegen: testthat::test_dir laeuft
#  alphabetisch, und dieser Dateiname sortiert robust ZWISCHEN
#  test_phase_kor_e2e.R ("...phase_k...") und test_phase_sens.R
#  ("...phase_s...") — das erste unterscheidende Zeichen nach "phase_" ist der
#  Buchstabe 'r', und in JEDER Locale gilt k < r < s (reiner Buchstaben-
#  vergleich, keine '.'/'_'-Kollationsfalle). Zu diesem Zeitpunkt sind die
#  Umgebungen der kop/kor-E2E-Dateien bereits unreferenziert, ihr chromote-/
#  openxlsx2-Speicher also finalisierbar.
#
#  Generalisierbar (LESSONS): nach chromote-E2E-/openxlsx2-schweren Testdateien
#  in einer langen, in EINEM Prozess laufenden Suite explizit gc(full = TRUE)
#  an der naechsten Naht unmittelbar VOR dem naechsten schweren Verbraucher —
#  R's automatischer Ausloeser unterzaehlt den externen Speicher und raeumt ihn
#  nicht rechtzeitig ab; eine einzelne Naht weit vor dem Verbraucher wird durch
#  spaetere Ladungen wieder aufgezehrt.
#
#  WARUM MEHRERE gc-DURCHGAENGE (nicht nur einer)
#  ----------------------------------------------
#  Die externen Puffer haengen an C-Finalizern (R_RegisterCFinalizerEx bei
#  openxlsx2/Rcpp-pugixml, processx-Handles der chromote-Sessions). Ein
#  einzelner gc(full = TRUE) SAMMELT den unreferenzierten Wrapper zwar ein,
#  aber ein Finalizer, der SELBST wieder Objekte fallen laesst (z.B. eine
#  Session, die beim Schliessen weitere Puffer freigibt), macht diese erst im
#  NAECHSTEN Durchgang einsammelbar. Attempt 2 zeigte genau das: die Naht lief
#  an ihrer Position (phase_ram_gc_seam: S), doch der Schwanz starb dennoch bei
#  phase_sens — ein Durchgang gab nicht genug frei. Zwei bis drei Durchgaenge
#  entleeren die Finalizer-Warteschlange nachweislich vollstaendig, BEVOR der
#  Sensitivitaets-/Monte-Carlo-Schwanz seinen Spitzenbedarf anfordert. Reine
#  base::gc-Hygiene, kein Paket, keine Funktion, kein Zeitverbrauch von Belang.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei (auch nicht test_phase_gc_seam.R). Sourct nichts, laedt kein
#  Paket, ruft keine Engine-/Wrapper-Funktion — reine Speicher-Hygiene ueber
#  base::gc.
# ============================================================================

# Die Naht selbst: openxlsx2-/chromote-Finalizer anstossen, akkumulierten
# externen Speicher freigeben, BEVOR der Sensitivitaets-/Monte-Carlo-Schwanz
# (test_phase_sens, test_phase_sim*) ihn anfordert. Laeuft beim Sourcen der
# Datei, also an der Naht-Position (nach test_phase_kor_e2e, vor
# test_phase_sens). Mehrere Durchgaenge, damit auch Finalizer-Ketten (ein
# Finalizer gibt weitere Objekte frei) vollstaendig abgeraeumt sind.
for (.i in seq_len(3L)) invisible(gc(full = TRUE))
rm(.i)
