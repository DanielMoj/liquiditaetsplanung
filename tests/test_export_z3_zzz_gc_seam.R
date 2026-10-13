# ============================================================================
#  tests/test_export_z3_zzz_gc_seam.R  —  SPEICHER-HYGIENE-NAHT der DOD-Suite
#  fuer den NEUEN export_z3-Block (phase-13-z3-ui). KEIN Produkt-Test, bewusst
#  OHNE test_that: eine gezaehlte Erwartung haette hier keine fachliche
#  Mutationskraft und koennte nur immer-gruen degradieren (dieselbe Bauart wie
#  die BESTEHENDEN, committeten Naehte test_phase_gc_seam.R /
#  test_phase_ram_gc_seam.R / test_phase_sim_a_gc_seam.R). test_dir SOURCT die
#  Datei; die Naht laeuft als Top-Level-Nebenwirkung, gezaehlt wird nichts.
#
#  WAS DIESE NAHT LEISTET — UND WAS NICHT (ehrliche Abgrenzung)
#  -----------------------------------------------------------
#  phase-13-z3-ui haengt ZWEI neue, in EINEM Prozess laufende Grossverbraucher
#  FRUEH in die run_all.R-Suite: test_export_z3.R und test_export_z3_ui.R
#  (committete Spec, sourcen kernel+R/ bzw. app/app.R und rechnen je einen
#  scenario_from_workbook/phase_forecast-Lauf auf der Tier-1-Mappe; openxlsx2
#  liest die Mappe). Deren referenzfreier Heap wuerde sonst durch die
#  nachgelagerten, aber LEICHTEN Dateien (test_finalisierung ...) stapeln, bis
#  ihn die naechste BESTEHENDE Naht test_phase_gc_seam.R einsammelt. Diese Naht
#  gibt ihn UNMITTELBAR nach dem export_z3-Block frei und haelt den RSS-Boden in
#  diesem Abschnitt niedrig.
#
#  WICHTIG, ehrlich: diese Naht ist NICHT der Beleg fuer ein gruenes Gate und
#  NICHT die Ursache dafuer, dass der schwere Monte-Carlo-Block spaeter durch-
#  traegt. Vor test_phase_sens laeuft bereits die BESTEHENDE, committete Naht
#  test_phase_ram_gc_seam.R (sortiert unmittelbar davor) und raeumt ALLES bis
#  dahin ab — der export_z3-Heap ist zu diesem Zeitpunkt ohnehin frei. Diese
#  Naht deckt allein den Abschnitt export_z3-Block -> test_phase_gc_seam ab. Der
#  Erfolgsbeleg der Suite ist und bleibt der vollstaendige run_all.R-Lauf bis
#  zur finalen testthat-Summary, nicht die Existenz dieser Naht.
#
#  PLATZIERUNG: testthat::test_dir laeuft in Sortierreihenfolge der Dateinamen;
#  dieser Name sortiert robust HINTER den gesamten export_z3-Block und VOR
#  test_finalisierung.R:
#    * gegen test_export_z3_zip.R (dem letzten z3-Block-Test) ist das erste
#      unterscheidende Zeichen nach "test_export_z3_z" das 'i' (zip) gegen 'z'
#      (zzz), und 'i' < 'z' in JEDER Locale;
#    * gegen test_finalisierung.R ist das erste unterscheidende Zeichen nach
#      "test_" das 'e' (export) gegen 'f' (finalisierung), und 'e' < 'f' in
#      JEDER Locale.
#  Reiner Buchstabenvergleich, keine '.'/'_'-Kollationsfalle.
#
#  WARUM MEHRERE gc-DURCHGAENGE (wie die bestehenden Naehte): externe Puffer
#  (Rcpp/pugixml von openxlsx2) haengen an C-Finalizern; ein Finalizer, der
#  SELBST weitere Objekte fallen laesst, macht diese erst im NAECHSTEN Durchgang
#  einsammelbar. Zwei bis drei Durchgaenge entleeren die Finalizer-Warteschlange.
#  Reine base::gc-Hygiene, kein Paket, keine Funktion.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei. Sourct nichts, laedt kein Paket, ruft keine Engine-/Wrapper-
#  Funktion — reine Speicher-Hygiene ueber base::gc.
# ============================================================================

# Die Naht selbst: die Finalizer des export_z3-Blocks (zwei phase_forecast-Laeufe
# + akkumulierter openxlsx2-Zeiger-Speicher) anstossen und ihren Speicher
# freigeben, BEVOR die nachgelagerten Dateien ihn bis zur naechsten bestehenden
# Naht stapeln. Laeuft beim Sourcen der Datei, also an der Naht-Position.
for (.i in seq_len(3L)) invisible(gc(full = TRUE))
rm(.i)
