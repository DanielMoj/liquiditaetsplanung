# ============================================================================
#  tests/test_phase_wochen_zzz_gc_seam.R  —  SPEICHER-HYGIENE-NAHT der DOD-Suite
#  NACH dem schwersten Schwanz-Block (test_phase_wochen*) und VOR dem leichten
#  Rest-Schwanz (Gate-A-Fix beim Landen von phase-14-verfahrensdoku, Nachschub zu
#  test_phase_sim_zzz_gc_seam.R / test_phase_vert_zzz_gc_seam.R). KEIN
#  Produkt-Test, bewusst OHNE test_that: eine gezaehlte Erwartung haette hier
#  keine fachliche Mutationskraft und koennte nur immer-gruen degradieren —
#  dieselbe Bauart wie die BESTEHENDEN, committeten Naehte test_phase_gc_seam.R /
#  test_phase_ram_gc_seam.R / test_phase_sim_a_gc_seam.R /
#  test_export_z3_zzz_gc_seam.R. test_dir SOURCT die Datei; die Naht laeuft als
#  Top-Level-Nebenwirkung, gezaehlt wird nichts.
#
#  WARUM DIESE DRITTE NAHT (der letzte schwere Block vor dem Rest-Schwanz)
#  ---------------------------------------------------------------------
#  Nach der Naht test_phase_vert_zzz_gc_seam.R folgt mit test_phase_wochen* der
#  DICHTESTE rechen-/speicher-lastige Block des Schwanzes (Engine-Lauf +
#  openxlsx2-/Plot-Zeiger, die meisten schweren Aufrufe je Datei). Sein
#  referenzfreier externer Speicher wuerde sonst ueber den restlichen Schwanz
#  (test_protokoll_*, test_sobol_*, test_verfahrensdoku*, test_verteilungsschicht,
#  test_z3_*) weiterstapeln — genau der Schwanz, in dem der block-gepufferte
#  OOM-Kill aus Gate A (Ausgabe endet nach phase_vert_mutgate ohne Summary)
#  landet. Diese Naht raeumt phase_wochen* ab, sodass der Rest-Schwanz von einem
#  niedrigen RSS-Boden startet — unter dem Vor-phase-14-Stand, der (nicht-
#  deterministisch) gruen durchlief.
#
#  DIE NAHT (und warum GENAU HIER)
#  -------------------------------
#  Ein gc(full = TRUE) stoesst die openxlsx2-/Plot-Finalizer des
#  phase_wochen-Blocks an und gibt deren externe Bytes frei. testthat::test_dir
#  laeuft in Sortierreihenfolge der Dateinamen; dieser Name sortiert robust
#  HINTER den gesamten phase_wochen-Block und VOR test_protokoll_anhaengen.R:
#    * gegen test_phase_wochen_mutgate.R ist das erste unterscheidende Zeichen
#      nach "test_phase_wochen_" das 'm' (mutgate) gegen 'z' (zzz), und 'm' < 'z'
#      in JEDER Locale;
#    * gegen test_protokoll_anhaengen.R ist das erste unterscheidende Zeichen
#      nach "test_p" das 'h' (phase) gegen 'r' (protokoll), und 'h' < 'r' in
#      JEDER Locale.
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
#  ungesicherten Schwanzes, in dem phase-14 lebt.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei. Sourct nichts, laedt kein Paket, ruft keine Engine-/Wrapper-
#  Funktion — reine Speicher-Hygiene ueber base::gc.
# ============================================================================

# Die Naht selbst: die Finalizer des phase_wochen-Blocks anstossen und ihren
# externen Speicher freigeben, BEVOR der restliche ungesicherte Schwanz
# (protokoll_* ... verfahrensdoku* ... verteilungsschicht ... z3_*) ihn weiter
# stapelt. Laeuft beim Sourcen der Datei, also an der Naht-Position (nach dem
# phase_wochen-Block, vor test_protokoll_anhaengen.R). Mehrere Durchgaenge, damit
# auch Finalizer-Ketten vollstaendig abgeraeumt sind.
for (.i in seq_len(3L)) invisible(gc(full = TRUE))
rm(.i)
