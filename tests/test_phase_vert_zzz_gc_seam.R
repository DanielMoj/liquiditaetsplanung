# ============================================================================
#  tests/test_phase_vert_zzz_gc_seam.R  —  SPEICHER-HYGIENE-NAHT der DOD-Suite
#  fuer den Uebergang vom Monte-Carlo-/Schirm-Block IN den langen, ungesicherten
#  Schwanz (Gate-A-Fix beim Landen von phase-14-verfahrensdoku, Nachschub zu
#  test_phase_sim_zzz_gc_seam.R). KEIN Produkt-Test, bewusst OHNE test_that: eine
#  gezaehlte Erwartung haette hier keine fachliche Mutationskraft und koennte nur
#  immer-gruen degradieren — dieselbe Bauart wie die BESTEHENDEN, committeten
#  Naehte test_phase_gc_seam.R / test_phase_ram_gc_seam.R /
#  test_phase_sim_a_gc_seam.R / test_export_z3_zzz_gc_seam.R und die
#  Schwester-Naht test_phase_sim_zzz_gc_seam.R. test_dir SOURCT die Datei; die
#  Naht laeuft als Top-Level-Nebenwirkung, gezaehlt wird nichts.
#
#  WARUM DIESE ZWEITE NAHT (die Restluecke, die Versuch 3 noch riss)
#  ----------------------------------------------------------------
#  Die erste neue Naht test_phase_sim_zzz_gc_seam.R sortiert VOR test_phase_var.R
#  und raeumt den phase_sim-Block ab. Danach folgen aber ZWEI weitere schwere,
#  rechen-/speicher-lastige Bloecke OHNE jede Naht: der Varianz-Schirm
#  test_phase_var* und der Verteilungs-Schirm test_phase_vert* (beide mit
#  Engine-Lauf + openxlsx2-/Plot-Zeigern). Ihr referenzfreier externer Speicher
#  stapelt auf den ganzen restlichen Schwanz (test_phase_wochen*,
#  test_protokoll_*, test_sobol_*, test_verfahrensdoku*, test_verteilungsschicht,
#  test_z3_*). Symptom in Gate A: die testthat-Ausgabe endet MITTEN in der Suite
#  — nur Punkte, KEIN F/E, KEINE Zusammenfassung; die letzte SICHTBARE Zeile
#  (phase_vert_mutgate) ist wegen der block-gepufferten stdout NICHT der
#  Kill-Punkt, der liegt im ungepufferten Schwanz dahinter. Kein Testfehler,
#  sondern ein OOM-Kill des Rscript-Prozesses; nicht-deterministisch (derselbe
#  Codestand laeuft in anderen Laeufen gruen), also Ressourcen/Last.
#
#  DIE NAHT (und warum GENAU HIER)
#  -------------------------------
#  Ein gc(full = TRUE) stoesst die openxlsx2-/Plot-Finalizer des
#  sim-/var-/vert-Blocks an und gibt deren akkumulierte externe Bytes frei, BEVOR
#  der gesamte restliche Schwanz sie weiter stapelt. testthat::test_dir laeuft in
#  Sortierreihenfolge der Dateinamen; dieser Name sortiert robust HINTER den
#  gesamten phase_vert-Block und VOR test_phase_wochen.R:
#    * gegen test_phase_vert_mutgate.R ist das erste unterscheidende Zeichen nach
#      "test_phase_vert_" das 'm' (mutgate) gegen 'z' (zzz), und 'm' < 'z' in
#      JEDER Locale;
#    * gegen test_phase_wochen.R ist das erste unterscheidende Zeichen nach
#      "test_phase_" das 'v' (vert) gegen 'w' (wochen), und 'v' < 'w' in JEDER
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
#  Der Erfolgsbeleg der Suite ist und bleibt der vollstaendige run_all.R-Lauf bis
#  zur finalen testthat-Summary. Die Naht senkt nur den RSS-Boden des
#  ungesicherten Schwanzes, in dem phase-14 lebt, unter den Vor-phase-14-Stand.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei. Sourct nichts, laedt kein Paket, ruft keine Engine-/Wrapper-
#  Funktion — reine Speicher-Hygiene ueber base::gc.
# ============================================================================

# Die Naht selbst: die Finalizer des sim-/var-/vert-Blocks anstossen und ihren
# externen Speicher freigeben, BEVOR der restliche ungesicherte Schwanz
# (phase_wochen* ... verteilungsschicht ... z3_*) ihn weiter stapelt. Laeuft beim
# Sourcen der Datei, also an der Naht-Position (nach dem phase_vert-Block, vor
# test_phase_wochen.R). Mehrere Durchgaenge, damit auch Finalizer-Ketten
# vollstaendig abgeraeumt sind.
for (.i in seq_len(3L)) invisible(gc(full = TRUE))
rm(.i)
