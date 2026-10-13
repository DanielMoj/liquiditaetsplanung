# Motivation und Hintergrund — Stochastische Liquiditätsplanung im Restrukturierungskontext

*Konzeptpapier. Die technische Umsetzung beschreibt die Methodendokumentation.*

## Das Problem

Jede Sanierungs- und Insolvenzentscheidung hängt an einer Frage über die Zukunft: Bleibt das Unternehmen über den Prognosezeitraum zahlungsfähig? § 18 InsO (drohende Zahlungsunfähigkeit) und § 19 InsO (Überschuldung mit Fortbestehensprognose) verlangen beide ein Urteil über künftige Liquidität. Der gesetzliche Maßstab ist ausdrücklich ein Wahrscheinlichkeitsmaßstab: die Fortführung muss *überwiegend wahrscheinlich* sein — eine Wahrscheinlichkeit über 50 Prozent.

Die übliche Praxis beantwortet diese Wahrscheinlichkeitsfrage mit einem deterministischen Plan: einer einzigen Liquiditätslinie, dem Base Case, ergänzt um ein, zwei Szenarien. Ein Punktwert kann aber keine Wahrscheinlichkeit ausdrücken. „Wir erwarten, zahlungsfähig zu bleiben" ist nicht dasselbe wie „die Wahrscheinlichkeit der Zahlungsfähigkeit beträgt X Prozent". Das ist der Ausgangspunkt: die rechtliche Frage ist probabilistisch, das gängige Werkzeug ist deterministisch.

Best-/Worst-Case-Szenarien schließen die Lücke nicht. Sie sind willkürlich gegriffen, sie aggregieren sich nicht zu einer Wahrscheinlichkeit, und sie lenken meist alle Inputs gleichgerichtet aus — was das Risiko je nach Konstruktion über- oder unterzeichnet und die Abhängigkeiten zwischen den Treibern ignoriert.

## Die Idee

Das Werkzeug ersetzt den deterministischen Plan nicht — es legt eine stochastische Schicht darum. Die Unsicherheit in den Eingaben des Plans wird durch dieselbe buchhalterische Logik propagiert, die auch den Punktplan erzeugt. Aus der einen Linie wird eine Verteilung möglicher Verläufe und damit eine Wahrscheinlichkeit, die kritische Schwelle zu reißen.

So wird der gesetzliche Maßstab unmittelbar operationalisierbar: nicht „der Plan zeigt einen positiven Saldo", sondern „P(zahlungsfähig über den Prognosezeitraum) = X Prozent", direkt vergleichbar mit der 50-Prozent-Schwelle. Das Produkt ist nicht die Bandbreite als Selbstzweck, sondern die Schwellenwahrscheinlichkeit.

## Zwei Arten von Unsicherheit

Nicht jede Unsicherheit ist gleich. Das Werkzeug trennt zwei Quellen, weil die Unterscheidung steuerungsrelevant ist:

- **Epistemische Unsicherheit** — Unwissen über die künftigen Jahresannahmen (Tarifabschluss, Preisniveau, Mengenentwicklung). Sie ist reduzierbar; mehr Information verengt sie. Modelliert als Bandbreite über die Annahme.
- **Aleatorische Variabilität** — die irreduzible Monat-zu-Monat-Schwankung des Betriebs, die selbst dann bleibt, wenn die Jahresannahmen feststehen. Aus der Historie geschätzt, nicht geraten.

Die Trennung beantwortet eine praktische Frage: Woher kommt das Risiko? Stammt es überwiegend aus den unbekannten Annahmen, lohnt es, diese durch Information zu schärfen. Stammt es aus der Betriebsvolatilität, hilft kein Verhandeln, sondern nur Liquiditätspuffer und Linienreserve. Ein Punktplan kann diese Unterscheidung nicht treffen.

## Herkunft der Methode

Die Technik ist nicht neu — sie ist ein Transfer aus der quantitativen Pharmakologie. Dort wiederholt sich dieselbe Problemform: ein mechanistisches Modell, unsichere Parameter, und die Frage nach der Wahrscheinlichkeit, dass eine Zielgröße in einem Korridor bleibt. Monte-Carlo-Propagation, Parameterunsicherheit gegenüber Populationsvariabilität, Aggregation über korrelierte Einzelgrößen — dort Standard. Die Liquiditätsprognose hat dieselbe Struktur: ein Rechenwerk aus Buchhaltungsidentitäten, unsichere Eingaben, eine Schwellenwahrscheinlichkeit.

In der deutschen Restrukturierungs- und Risikoliteratur entspricht das der Risikoaggregation nach Gleißner und den Standards IDW PS 340 und IDW S 11: Einzelrisiken werden mit ihren Abhängigkeiten zu einer Verteilung des Ergebnisses bzw. der Liquidität aggregiert, woraus sich die Wahrscheinlichkeit einer Bestandsgefährdung ablesen lässt. Das Werkzeug steht in dieser Linie, nicht gegen sie. Es führt die etablierte pharmakometrische Technik und die etablierte Restrukturierungstheorie an der konkreten Prognosefrage zusammen.

## Einordnung

Ergänzung, nicht Konkurrenz. Der deterministische Punktplan — das Handwerk des Restrukturierers — bleibt das Rückgrat. Die stochastische Schicht ist ein Mantel, der seine Unsicherheit quantifiziert. Das Urteil des Planers geht hinein, als Basisannahmen und als plausible Bandbreiten; das Werkzeug propagiert es, es ersetzt es nicht.

Tragfähig vor Sachwalter, Gericht und Bank wird die Aussage durch die Disziplin dahinter: strikte Trennung von beobachteten Daten und Urteilseingaben, durchgehende Nachvollziehbarkeit, und die Validierung gegen die Historie, bevor überhaupt prognostiziert wird. Jede Zahl im Ergebnis lässt sich auf eine Eingabe, eine Annahme oder eine dokumentierte Formel zurückführen. Weil ein Kern alle Mandate bedient und nur die Daten getauscht werden, wird aus einer einmaligen Analyse ein wiederverwendbares Werkzeug.

## Was es liefert

- Eine Wahrscheinlichkeitsaussage statt einer Behauptung: P(zahlungsfähig) und P(Liquiditätslinie reißt) je Monat, direkt am gesetzlichen Maßstab.
- Die Treiber, die zählen: welche wenigen Annahmen das Ergebnis bewegen — und welche man fixieren kann.
- Die Quelle des Risikos: ob die Unsicherheit aus den Annahmen oder aus der Betriebsvolatilität stammt, und damit, wo Aufwand wirkt.

## Grenzen

Das Werkzeug erzeugt keine Sicherheit — es macht die vorhandene Unsicherheit explizit und ehrlich. Eine breite Bandbreite mit klarer Wahrscheinlichkeit ist wahrhaftiger als eine selbstbewusste Einzellinie. Die Ergebnisse sind nur so gut wie die Eingaben; die eigentliche Arbeit ist saubere Datenbeschaffung und ehrliche Bandbreiten, nicht die Mathematik. Und die ausgewiesene Wahrscheinlichkeit ist bedingt — auf das Modell und die angenommenen Bandbreiten. Sie ist Entscheidungsunterstützung, kein Orakel.
