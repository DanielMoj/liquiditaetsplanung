# ============================================================================
#  tests/test_phase_sim_zzz_gc_seam.R  —  SPEICHER-HYGIENE-NAHT der DOD-Suite
#  fuer den UNGESICHERTEN Schwanz NACH dem Monte-Carlo-Block (Gate-A-Fix beim
#  Landen von phase-14-verfahrensdoku). KEIN Produkt-Test, bewusst OHNE
#  test_that: eine gezaehlte Erwartung haette hier keine fachliche
#  Mutationskraft und koennte nur immer-gruen degradieren — dieselbe Bauart wie
#  die BESTEHENDEN, committeten Naehte test_phase_gc_seam.R /
#  test_phase_ram_gc_seam.R / test_phase_sim_a_gc_seam.R /
#  test_export_z3_zzz_gc_seam.R. test_dir SOURCT die Datei; die Naht laeuft als
#  Top-Level-Nebenwirkung, gezaehlt wird nichts.
#
#  WARUM DIESE NAHT (die Luecke, die phase-14 sichtbar gemacht hat)
#  ---------------------------------------------------------------
#  Die letzte BESTEHENDE Naht test_phase_sim_a_gc_seam.R sortiert bei
#  "phase_sim_a" — also VOR dem Monte-Carlo-Block. Danach laeuft der SCHWERSTE
#  rechen-/speicher-lastige Abschnitt der Suite (test_phase_sim* + der
#  Verteilungs-Schirm test_phase_vert) und anschliessend ein LANGER Schwanz
#  (test_phase_var*, test_phase_vert*, test_phase_wochen*, test_protokoll_*,
#  test_sobol_*, test_verteilungsschicht, test_verfahrensdoku*, test_z3_*) —
#  und in diesem ganzen Schwanz gibt es KEINE weitere gc-Naht mehr. Der
#  referenzfreie openxlsx2-/Plot-Zeiger-Speicher des sim/vert-Blocks bleibt
#  liegen und stapelt ueber den restlichen Schwanz.
#
#  Symptom (Gate A, run_all.R): die testthat-Ausgabe endet MITTEN in der Suite
#  — nur Punkte, KEIN F/E, KEINE Zusammenfassung. Kein Testfehler, sondern ein
#  Kill des Rscript-Prozesses (OOM). Weil R's stdout unter einer Pipe
#  block-gepuffert ist, markiert die letzte SICHTBARE Zeile (hier: phase_vert)
#  NICHT den Kill-Punkt — der liegt im ungepufferten Rest dahinter. Der
#  Kill-Punkt VARIIERT zwischen Laeufen (frueher phase_sens /
#  protokoll_pruefen_feldnamen, jetzt der phase_vert-Schwanz), und derselbe
#  Codestand laeuft in anderen Laeufen komplett gruen durch — nicht-
#  deterministisch, also Ressourcen/Last, kein Testfehler. phase-14 selbst
#  fuegt nur LEICHTE Dateien hinzu (test_verfahrensdoku* rechnen nichts, sie
#  bauen ein reines Objekt), verlaengert damit aber den ohnehin ungesicherten
#  Schwanz und hebt seinen RSS-Boden — genau der Tropfen, der die Schwelle in
#  Versuch 1 und 2 riss.
#
#  DIE NAHT (und warum GENAU HIER)
#  -------------------------------
#  Ein gc(full = TRUE) stoesst die openxlsx2-/Plot-Finalizer des sim-Blocks an
#  und gibt die akkumulierten externen Bytes frei, BEVOR der gesamte restliche
#  Schwanz sie weiter stapelt. testthat::test_dir laeuft in Sortierreihenfolge
#  der Dateinamen; dieser Name sortiert robust HINTER den gesamten
#  phase_sim-Block und VOR test_phase_var.R:
#    * gegen test_phase_sim_ui.R (dem letzten phase_sim-Test) ist das erste
#      unterscheidende Zeichen nach "test_phase_sim_" das 'u' (ui) gegen 'z'
#      (zzz), und 'u' < 'z' in JEDER Locale;
#    * gegen test_phase_var.R ist das erste unterscheidende Zeichen nach
#      "test_phase_" das 's' (sim) gegen 'v' (var), und 's' < 'v' in JEDER
#      Locale.
#  Reiner Buchstabenvergleich am unterscheidenden Zeichen, keine '.'/'_'-
#  Kollationsfalle.
#
#  WARUM MEHRERE gc-DURCHGAENGE (wie die bestehenden Naehte): externe Puffer
#  (Rcpp/pugixml von openxlsx2, Grafik-Devices) haengen an C-Finalizern; ein
#  Finalizer, der SELBST weitere Objekte fallen laesst, macht diese erst im
#  NAECHSTEN Durchgang einsammelbar. Zwei bis drei Durchgaenge entleeren die
#  Finalizer-Warteschlange. Reine base::gc-Hygiene, kein Paket, keine Funktion.
#
#  EHRLICHE ABGRENZUNG: diese Naht ist NICHT der Beleg fuer ein gruenes Gate.
#  Der Erfolgsbeleg der Suite ist und bleibt der vollstaendige run_all.R-Lauf
#  bis zur finalen testthat-Summary. Die Naht senkt nur den RSS-Boden des
#  ungesicherten Schwanzes, in dem phase-14 lebt.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei. Sourct nichts, laedt kein Paket, ruft keine Engine-/Wrapper-
#  Funktion — reine Speicher-Hygiene ueber base::gc.
# ============================================================================

# Die Naht selbst: die Finalizer des Monte-Carlo-/Verteilungs-Blocks anstossen
# und ihren externen Speicher freigeben, BEVOR der restliche ungesicherte
# Schwanz (phase_var* ... verfahrensdoku* ... z3_*) ihn weiter stapelt. Laeuft
# beim Sourcen der Datei, also an der Naht-Position (nach dem phase_sim-Block,
# vor test_phase_var.R). Mehrere Durchgaenge, damit auch Finalizer-Ketten
# vollstaendig abgeraeumt sind.
for (.i in seq_len(3L)) invisible(gc(full = TRUE))
rm(.i)
