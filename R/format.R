# ============================================================================
#  R/format.R  —  zentrale Format-Heimat der MASCHINENLESBAREN Serialisierung
#
#  Zwei Formatierungswelten, EINE Heimat je Welt:
#    * ANZEIGE: fmt() aus kernel/engine.R (format(round(x), big.mark = ".",
#      decimal.mark = ",")) — gerundete, gruppierte Zahlen fuer Menschen.
#      Die Regel "Zahlenformat DE ausschliesslich ueber fmt()"
#      (IMPLEMENTATION_RULES.md) bindet diese Anzeige-Schicht.
#    * MASCHINE: fmt_maschine() (hier) — verlustfreie, gruppierungsfreie
#      Serialisierung fuer Datenexporte (GoBD-CSV, Datentraegerueberlassung).
#      Exportschichten (R/z3.R usw.) formatieren NIE lokal, sondern
#      delegieren hierher — eine Wahrheit, ein Modul.
#
#  Warum fmt() auf der Datenschicht strukturell unbrauchbar ist (und der
#  eingefrorene kernel nicht um einen GoBD-Modus erweitert werden darf):
#  round() wuerfe jede Nachkommastelle weg (KK_Zins 0.065 -> "0", verletzt
#  die 1e-12-Genauigkeitsprobe der Spec), und big.mark = "." SETZTE erst
#  Tausenderpunkte ("-5.135.990", verletzt den quoting-freien
#  Dezimalkomma-Vertrag). fmt_maschine teilt mit fmt() die
#  Dezimalkomma-Konvention, verzichtet aber auf round() und Gruppierung.
#
#  Verhalten: formatC mit format = "fg" (fixed-notation, nie wissenschaftlich,
#  unabhaengig von der Groessenordnung), 15 signifikante Stellen wie die
#  R-eigene Darstellung, big.mark leer (KEINE Gruppierung), Dezimalkomma
#  direkt ueber decimal.mark. drop0trailing haelt Integrale punkt- UND
#  kommalos (-5235600 bleibt "-5235600"); ein Dezimalwert traegt genau ein
#  ',' (0.065 -> "0,065"). Vektorisiert, deterministisch, reines Basis-R.
# ============================================================================

fmt_maschine <- function(x) {
  s <- formatC(x, format = "fg", digits = 15L, big.mark = "",
               decimal.mark = ",", drop0trailing = TRUE)
  trimws(s)
}
