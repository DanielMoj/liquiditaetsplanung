# ============================================================================
#  tests/test_phase_sim_a_gc_seam.R  —  DRITTE SPEICHER-HYGIENE-NAHT der
#  DOD-Suite (Gate-A-Fix fuer phase-13-z3-index, KEIN Produkt-Test — bewusst
#  OHNE test_that: eine gezaehlte Erwartung haette hier keine fachliche
#  Mutationskraft und koennte nur immer-gruen degradieren. test_dir SOURCT die
#  Datei, die Naht laeuft als Top-Level-Nebenwirkung; gezaehlt wird nichts.)
#
#  WARUM DIESE DATEI EXISTIERT (Ergaenzung zu test_phase_gc_seam.R und
#  test_phase_ram_gc_seam.R)
#  ----------------------------------------------------------------
#  Die erste Naht (test_phase_gc_seam.R) sortiert bei "phase_g" — vor den
#  chromote-Browsern der E2E-Controls; die zweite (test_phase_ram_gc_seam.R)
#  bei "phase_ram" — NACH den beiden E2E-Browsern (test_phase_kop_e2e /
#  test_phase_kor_e2e), aber VOR test_phase_sens. Damit lief die Suite (Gate A,
#  Versuch 3 fuer phase-13-z3-index) erstmals durch test_phase_sens (Tornado +
#  Sobol) UND test_phase_sim (Monte-Carlo) hindurch — genau die beiden
#  schwersten RECHEN-/SPEICHER-Bloecke der Suite mit vielen Engine-Laeufen und
#  grossen MC-Matrizen. Der Kill ruecke daraufhin an ihr Ende: die testthat-
#  Ausgabe endet MITTEN in der Suite bei test_phase_sim_breach — nur Punkte,
#  KEIN F/E, KEINE Zusammenfassung. Kein Testfehler, sondern ein Kill des
#  Rscript-Prozesses (OOM), sobald der phase_sim_*-Schwanz nach sens + sim
#  weiteren Speicher anfordert.
#
#  DIE NAHT (und warum GENAU HIER)
#  -------------------------------
#  test_phase_sens und test_phase_sim sind die groessten Einzel-Allokatoren des
#  Schwanzes; ihr Speicher (Engine-Laeufe, MC-Matrizen) bleibt referenzfrei
#  liegen, bis ein gc ihn einsammelt. Die zweite Naht liegt VOR sens und gibt
#  ihn daher NICHT frei. Eine dritte Naht muss NAH am naechsten Verbraucher und
#  NACH den beiden Grossallokatoren liegen: unmittelbar hinter test_phase_sim,
#  vor dem uebrigen phase_sim_*-Schwanz (test_phase_sim_breach,
#  test_phase_sim_deckung*, test_phase_sim_monate*, test_phase_sim_noise,
#  test_phase_sim_ui). testthat::test_dir laeuft in der Sortierreihenfolge der
#  Dateinamen; die Gate-A-Ausgabe belegt fuer diese Umgebung die Ordnung
#  ... test_phase_sim.R  <  test_phase_sim_breach.R ... (das erste
#  unterscheidende Zeichen nach "test_phase_sim" ist '.' gegen '_', und hier
#  gilt '.' < '_'). Dieser Dateiname traegt hinter "test_phase_sim_" ein 'a':
#  gegen "test_phase_sim_breach.R" ist das erste unterscheidende Zeichen 'a'
#  gegen 'b', und 'a' < 'b' in JEDER Locale (reiner Buchstabenvergleich, keine
#  Kollationsfalle mehr). Damit sortiert die Naht robust ZWISCHEN
#  test_phase_sim.R und test_phase_sim_breach.R — nach beiden Grossallokatoren,
#  vor dem uebrigen MC-Schwanz. Zu diesem Zeitpunkt sind die Umgebungen von
#  test_phase_sens und test_phase_sim bereits unreferenziert, ihr Speicher also
#  finalisierbar.
#
#  WARUM MEHRERE gc-DURCHGAENGE (wie die zweite Naht)
#  -------------------------------------------------
#  Externe Puffer (Rcpp/pugixml von openxlsx2, processx-Handles der chromote-
#  Sessions) haengen an C-Finalizern. Ein Finalizer, der SELBST weitere Objekte
#  fallen laesst, macht diese erst im NAECHSTEN Durchgang einsammelbar; zwei bis
#  drei Durchgaenge entleeren die Finalizer-Warteschlange nachweislich
#  vollstaendig, BEVOR der phase_sim_*-Schwanz seinen Spitzenbedarf anfordert.
#  Reine base::gc-Hygiene, kein Paket, keine Funktion, kein Zeitverbrauch von
#  Belang.
#
#  BESTANDSSCHUTZ: beruehrt weder kernel/ noch R/ noch eine bestehende
#  test_*-Datei (auch nicht test_phase_gc_seam.R / test_phase_ram_gc_seam.R).
#  Sourct nichts, laedt kein Paket, ruft keine Engine-/Wrapper-Funktion — reine
#  Speicher-Hygiene ueber base::gc.
# ============================================================================

# Die Naht selbst: die Finalizer der Grossallokatoren (test_phase_sens /
# test_phase_sim) anstossen und ihren akkumulierten Speicher freigeben, BEVOR
# der uebrige phase_sim_*-Schwanz ihn anfordert. Laeuft beim Sourcen der Datei,
# also an der Naht-Position (nach test_phase_sim, vor test_phase_sim_breach).
# Mehrere Durchgaenge, damit auch Finalizer-Ketten vollstaendig abgeraeumt sind.
for (.i in seq_len(3L)) invisible(gc(full = TRUE))
rm(.i)
