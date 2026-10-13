# ============================================================================
#  engine.R  —  wiederverwendbarer Kern der GuV-/Liquiditaets-Engine
#
#  Stellt bereit:
#    load_model(WB)          liest die Mandatsmappe -> Modellobjekt (Daten/Mapping/Annahmen)
#    run_model(mdl, assume)  f(params): -> list(guv, cash, kpis)
#    validate_history(mdl)   rechnet die Ist-Historie nach (Konsistenz-Gate)
#    fmt(x)                  deutsche Zahlenformatierung
#
#  Wird von 01_forecast.R und 02_sensitivitaet.R (und spaeter 03_simulation.R)
#  per source("engine.R") eingebunden. Enthaelt KEINE mandantenspezifischen Zahlen.
# ============================================================================
suppressMessages(library(readxl))

load_model <- function(WB) {
  rd <- function(sheet) as.data.frame(read_excel(WB, sheet, skip = 3))
  pos <- rd("01_Positionen"); prof_raw <- rd("02_Zahlungsprofile")
  stamm <- rd("03_Stammdaten_Eroeffnung"); ann <- rd("04_Annahmen"); fin_raw <- rd("05_Finanzierungsplan")
  if (!"pos" %in% names(pos)) stop("01_Positionen: Spalte 'pos' fehlt (Kopfzeile in Zeile 4?).")
  pos <- pos[!is.na(pos[["pos"]]), ]; fin_raw <- fin_raw[!is.na(fin_raw[[1]]), ]
  prof_raw <- prof_raw[!is.na(prof_raw[[1]]), ]; stamm <- stamm[!is.na(stamm[[1]]), ]; ann <- ann[!is.na(ann[[1]]), ]
  if (!nrow(pos)) stop("01_Positionen: keine Positionszeilen nach Filterung -- Kopfzeile/pos-Spalte pruefen.")
  # INVARIANTE: Pflichtspalten vorhanden; jede Position traegt eine bekannte GuV-Gruppe und einen bekannten Treiber —
  # eine unbekannte grp fiele stumm aus GuV und Cash heraus, ein unbekannter Treiber bekaeme still Wachstum 1.
  need_cols <- c("grp","cashprofile","driver")
  col_miss <- setdiff(need_cols, names(pos))
  if (length(col_miss)) stop(sprintf("load_model: INVARIANTE VERLETZT — 01_Positionen: Pflichtspalte(n) fehlen: %s.", paste(col_miss, collapse=", ")))
  known_grp <- c("umsatz","bestand","betr_ertrag","sopo","material","personal","sachkosten","afa","zins_e","zins_a","neutral_e","neutral_a","steuer")
  bad_grp <- which(!pos$grp %in% known_grp)
  if (length(bad_grp)) stop(sprintf("load_model: INVARIANTE VERLETZT — 01_Positionen: unbekannte grp '%s' bei Position '%s' (erlaubt: %s).",
                                    pos$grp[bad_grp[1]], pos$pos[bad_grp[1]], paste(known_grp, collapse=", ")))
  known_driver <- c("menge_preis","menge","material","tarif","energie","pflegebudget","infl","flat")
  bad_drv <- which(!pos$driver %in% known_driver)
  if (length(bad_drv)) stop(sprintf("load_model: INVARIANTE VERLETZT — 01_Positionen: unbekannter driver '%s' bei Position '%s' (erlaubt: %s).",
                                    pos$driver[bad_drv[1]], pos$pos[bad_drv[1]], paste(known_driver, collapse=", ")))
  classcols <- c("pos","label","grp","cashprofile","driver","ust")
  mcols <- setdiff(names(pos), classcols)
  ust <- if ("ust" %in% names(pos)) { u <- suppressWarnings(as.numeric(pos[["ust"]])); u[is.na(u)] <- 0; u } else rep(0, nrow(pos))
  # INVARIANTE: USt-Saetze sind Dezimalsaetze in [0, 1) — "19" statt "0.19" wuerde Brutto-Cash und Zahllast um Faktor ~20 verfaelschen.
  if (any(ust < 0 | ust >= 1)) { ub <- which(ust < 0 | ust >= 1)[1]
    stop(sprintf("load_model: INVARIANTE VERLETZT — 01_Positionen: ust-Satz von Position '%s' ist %s, soll in [0, 1) (19%% = 0.19).", pos$pos[ub], format(ust[ub]))) }
  Hist <- matrix(as.numeric(as.matrix(pos[, mcols])), nrow = nrow(pos)); rownames(Hist) <- pos$pos
  nb <- ncol(Hist)
  if (nb < 12) stop(sprintf("01_Positionen: Historie hat %d Monate, benoetigt 12. Mehr Ist liefern oder Basisjahr explizit setzen.", nb))
  n_hist <- nb   # rohe Monatszahl der Blatt-Historie (vor der Basisjahr-Rotation) — reiner Ausweis, keine Rechenlogik
  base <- Hist[, (nb-11):nb, drop = FALSE]
  profiles <- list()
  for (i in seq_len(nrow(prof_raw))) { v <- suppressWarnings(as.numeric(unlist(prof_raw[i, -1]))); v <- v[!is.na(v)]
    profiles[[as.character(prof_raw[i, 1])]] <- v }
  kv <- function(df) setNames(as.character(df[[2]]), as.character(df[[1]])); S <- kv(stamm); A <- kv(ann)
  # INVARIANTE: Pflichtschluessel der Stammdaten/Annahmen sind vorhanden und numerisch — ein fehlender Schluessel
  # wuerde sonst als kryptischer Subscript-Fehler oder stilles NA durch die ganze Rechnung laufen.
  s_need <- c("open_cash","kk_limit","kk_zins","mindestreserve","Forecast-Beginn")
  s_miss <- setdiff(s_need, names(S))
  if (length(s_miss)) stop(sprintf("load_model: INVARIANTE VERLETZT — 03_Stammdaten_Eroeffnung: Schluessel fehlen: %s.", paste(s_miss, collapse=", ")))
  a_need <- c("menge","preis","tarif","energie","material","pflegebudget","inflation","hmax")
  a_miss <- setdiff(a_need, names(A))
  if (length(a_miss)) stop(sprintf("load_model: INVARIANTE VERLETZT — 04_Annahmen: Schluessel fehlen: %s.", paste(a_miss, collapse=", ")))
  s_num <- c("open_cash","kk_limit","kk_zins","mindestreserve")
  s_bad <- s_num[!is.finite(suppressWarnings(vapply(s_num, function(k) as.numeric(S[[k]]), numeric(1))))]
  if (length(s_bad)) stop(sprintf("load_model: INVARIANTE VERLETZT — 03_Stammdaten_Eroeffnung: nicht numerisch: %s (ist Text/leer, soll Zahl).", paste(s_bad, collapse=", ")))
  a_bad <- a_need[!is.finite(suppressWarnings(vapply(a_need, function(k) as.numeric(A[[k]]), numeric(1))))]
  if (length(a_bad)) stop(sprintf("load_model: INVARIANTE VERLETZT — 04_Annahmen: nicht numerisch: %s (ist Text/leer, soll Zahl).", paste(a_bad, collapse=", ")))
  # INVARIANTE: kk_zins ist Dezimal-Jahreszins in [0, 1) und kk_limit >= 0 — beide steuern die vorzeichenkritische KK-Zins-Rekursion.
  if (suppressWarnings(as.numeric(S[["kk_zins"]])) < 0 || suppressWarnings(as.numeric(S[["kk_zins"]])) >= 1)
    stop(sprintf("load_model: INVARIANTE VERLETZT — 03_Stammdaten_Eroeffnung: kk_zins ist %s, soll in [0, 1) (6.5%% p.a. = 0.065).", S[["kk_zins"]]))
  if (suppressWarnings(as.numeric(S[["kk_limit"]])) < 0)
    stop(sprintf("load_model: INVARIANTE VERLETZT — 03_Stammdaten_Eroeffnung: kk_limit ist %s, soll >= 0.", S[["kk_limit"]]))
  num <- function(m, k) as.numeric(m[[k]])
  HMAX <- as.integer(num(A, "hmax"))
  # INVARIANTE: Der Planhorizont ist eine ganze Zahl >= 1 — alle Vektoren/Matrizen der Engine haengen an dieser Laenge.
  if (is.na(HMAX) || HMAX < 1)
    stop(sprintf("load_model: INVARIANTE VERLETZT — 04_Annahmen: hmax ist %s, soll ganze Zahl >= 1.", A[["hmax"]]))
  assume <- list(menge=num(A,"menge"), preis=num(A,"preis"), tarif=num(A,"tarif"), energie=num(A,"energie"),
                 material=num(A,"material"), pflegebudget=num(A,"pflegebudget"), inflation=num(A,"inflation"))
  rownames(fin_raw) <- fin_raw[[1]]
  finm <- matrix(as.numeric(as.matrix(fin_raw[, -1])), nrow = nrow(fin_raw), dimnames = list(fin_raw[[1]], NULL))
  need_fin <- c("Investitionen","Foerdermittel","Tilgung","Darlehensaufnahme")
  miss_fin <- setdiff(need_fin, rownames(finm))
  if (length(miss_fin)) stop(sprintf("05_Finanzierungsplan: Pflichtzeile(n) fehlen: %s", paste(miss_fin, collapse=", ")))
  if (HMAX > ncol(finm)) stop(sprintf("hmax (=%d) ueberschreitet die %d Finanzplanmonate. Plan verlaengern oder hmax<=%d.", HMAX, ncol(finm), ncol(finm)))
  # INVARIANTE: Die vier Finanzplanzeilen sind im Planhorizont 1..HMAX lueckenlos numerisch — eine leere Zelle
  # wuerde als NA den NettoCashflow ab diesem Monat stumm in NaN verwandeln.
  finh <- finm[need_fin, 1:HMAX, drop = FALSE]
  if (any(!is.finite(finh))) { fb <- which(!is.finite(finh), arr.ind = TRUE)
    stop(sprintf("load_model: INVARIANTE VERLETZT — 05_Finanzierungsplan: Zeile '%s' hat in Planmonat %d keinen numerischen Wert (ist NA/leer, soll Zahl, ggf. 0).",
                 rownames(finh)[fb[1,1]], fb[1,2])) }
  fin <- list(capex=finm["Investitionen",1:HMAX], foerder=finm["Foerdermittel",1:HMAX],
              tilg=finm["Tilgung",1:HMAX], aufn=finm["Darlehensaufnahme",1:HMAX])
  dists_raw <- tryCatch({ x <- as.data.frame(read_excel(WB, "06_Verteilungen", skip = 3)); x[!is.na(x[[1]]), ] },
                        error = function(e) NULL)
  corr_raw <- tryCatch({ x <- as.data.frame(read_excel(WB, "07_Korrelation", skip = 3)); x[!is.na(x[[1]]), ] },
                       error = function(e) NULL)
  # Forecast-Beginn -> Startmonat (1..12), fuer die Steuer-Voranmeldungstermine
  pm <- function(x) { g <- regmatches(x, regexpr("-([0-9]{1,2})", x)); if (length(g)) { v <- as.integer(sub("-", "", g)); if (!is.na(v) && v >= 1 && v <= 12) return(v) }; 1L }
  start_month <- pm(as.character(S[["Forecast-Beginn"]]))
  start_date <- { x <- as.character(S[["Forecast-Beginn"]]); d <- tryCatch(as.Date(x), error=function(e) NA)
    if (is.na(d)) d <- tryCatch(as.Date(paste0(x, "-01")), error=function(e) NA)   # "2026-01" -> "2026-01-01"
    if (is.na(d)) d <- as.Date(sprintf("%04d-%02d-01", as.integer(format(Sys.Date(),"%Y")), start_month)); d }
  # Kalenderausrichtung: das 12-Monats-Template auf den Forecast-Beginn rotieren (F-01).
  # Kalendermonat je Basisspalte aus den Spaltenkoepfen (mcols der letzten 12 Spalten) lesen;
  # nicht lesbar -> Annahme Jan..Dez (mit Warnung). Bei Januar-Start ist die Rotation die Identitaet.
  base_cols <- mcols[(nb-11):nb]
  base_cal  <- suppressWarnings(as.integer(sub(".*-(\\d{1,2}).*", "\\1", base_cols)))
  if (any(is.na(base_cal)) || length(unique(base_cal)) != 12) {
    warning("Basisspalten nicht als 12 Kalendermonate lesbar - Annahme Jan..Dez.")
    base_cal <- 1:12
  }
  col_for <- match(1:12, base_cal)                 # Kalendermonat -> Basisspalte
  cal12   <- ((start_month - 1 + 0:11) %% 12) + 1  # Forecast-Reihenfolge ab start_month
  base    <- base[, col_for[cal12], drop = FALSE]  # 12 Spalten, ausgerichtet
  # INVARIANTE: Das rotierte 12-Monats-Basisjahr ist vollstaendig numerisch — ein NA hier liefe als NaN
  # stumm durch Projektion, GuV und Cash bis in die KPIs.
  if (any(is.na(base))) { bb <- which(is.na(base), arr.ind = TRUE)
    stop(sprintf("load_model: INVARIANTE VERLETZT — 01_Positionen: Basisjahr enthaelt %d nicht-numerische Zelle(n), z.B. Position '%s' / Basismonat %d (ist NA/Text, soll Zahl). Letzte 12 Ist-Monate pruefen.",
                 nrow(bb), rownames(base)[bb[1,1]], bb[1,2])) }
  # 08_Steuern (optional): drei Schalter + Saetze. Fehlt das Blatt -> tax = NULL (Verhalten exakt wie zuvor).
  tax <- tryCatch({
    tx <- as.data.frame(read_excel(WB, "08_Steuern", skip = 3)); tx <- tx[!is.na(tx[[1]]), ]
    Tv <- setNames(as.character(tx[[2]]), as.character(tx[[1]]))
    gk  <- function(k) if (k %in% names(Tv)) Tv[[k]] else NA
    numk <- function(k, d) { v <- suppressWarnings(as.numeric(gk(k))); if (is.na(v)) d else v }
    chr  <- function(k, d) { v <- gk(k); if (is.na(v)) d else tolower(trimws(v)) }
    list(rechtsform=chr("rechtsform","kapital"), gewst_status=chr("gewst_status","gewerblich"),
         ust_status=chr("ust_status","frei"), ust_methode=chr("ust_methode","soll"),
         kst_satz=numk("kst_satz",0.15), soli_satz=numk("soli_satz",0.055),
         gewst_messzahl=numk("gewst_messzahl",0.035), gewst_hebesatz=numk("gewst_hebesatz",0),
         gewst_freibetrag=numk("gewst_freibetrag",24500), ust_lag=max(1L, as.integer(round(numk("ust_lag",1)))),
         entnahme_quote=numk("entnahme_quote",0))
  }, error = function(e) NULL)
  # INVARIANTE: Steuerparameter liegen im Sinnbereich (Saetze als Dezimale, Hebesatz in Prozentpunkten, Lag 1..12) —
  # geprueft NACH dem tryCatch, damit ein Verstoss ein benannter Fehler wird statt still tax=NULL (Steuern lautlos aus).
  if (!is.null(tax)) {
    t_rates <- c(kst_satz=tax$kst_satz, soli_satz=tax$soli_satz, gewst_messzahl=tax$gewst_messzahl)
    t_bad <- names(t_rates)[!is.finite(t_rates) | t_rates < 0 | t_rates >= 1]
    if (length(t_bad)) stop(sprintf("load_model: INVARIANTE VERLETZT — 08_Steuern: %s ist %s, soll Dezimalsatz in [0, 1) (15%% = 0.15).",
                                    t_bad[1], format(t_rates[[t_bad[1]]])))
    if (!is.finite(tax$entnahme_quote) || tax$entnahme_quote < 0 || tax$entnahme_quote > 1)
      stop(sprintf("load_model: INVARIANTE VERLETZT — 08_Steuern: entnahme_quote ist %s, soll Quote in [0, 1].", format(tax$entnahme_quote)))
    if (!is.finite(tax$gewst_hebesatz) || tax$gewst_hebesatz < 0 || tax$gewst_hebesatz > 2000)
      stop(sprintf("load_model: INVARIANTE VERLETZT — 08_Steuern: gewst_hebesatz ist %s, soll Prozentpunkte in [0, 2000] (z.B. 400).", format(tax$gewst_hebesatz)))
    if (!is.finite(tax$gewst_freibetrag) || tax$gewst_freibetrag < 0)
      stop(sprintf("load_model: INVARIANTE VERLETZT — 08_Steuern: gewst_freibetrag ist %s, soll >= 0.", format(tax$gewst_freibetrag)))
    if (tax$ust_lag < 1 || tax$ust_lag > 12)
      stop(sprintf("load_model: INVARIANTE VERLETZT — 08_Steuern: ust_lag ist %d, soll in 1..12 (Voranmeldungs-Lag in Monaten).", tax$ust_lag))
  }
  # 09_Darlehen (optional): explizite Kredite mit Tilgungsplan. Fehlt das Blatt -> loans = NULL (additiv, kein Effekt).
  loans <- tryCatch({
    dl <- as.data.frame(read_excel(WB, "09_Darlehen", skip = 3)); dl <- dl[!is.na(dl[[1]]), ]
    if (!nrow(dl)) NULL else dl
  }, error = function(e) NULL)
  # 10_OPOS (optional): offene Forderungen/Verbindlichkeiten mit Zahlungsziel. Fehlt -> opos = NULL (Carry-in wie bisher).
  opos <- tryCatch({
    op <- as.data.frame(read_excel(WB, "10_OPOS", skip = 3)); op <- op[!is.na(op[[1]]), ]
    if (!nrow(op)) NULL else op
  }, error = function(e) NULL)
  list(pos=pos, base=base, profiles=profiles, assume=assume, fin=fin, dists_raw=dists_raw, corr_raw=corr_raw, WB=WB,
       ust=ust, start_month=start_month, start_date=start_date, tax=tax, loans=loans, opos=opos,
       open_cash=num(S,"open_cash"), kk_limit=num(S,"kk_limit"), kk_zins=num(S,"kk_zins"),
       mindestreserve=num(S,"mindestreserve"), HMAX=HMAX, n_hist=n_hist)
}

growth_of <- function(driver, a) switch(driver,
  menge_preis=(1+a$menge)*(1+a$preis), menge=(1+a$menge), material=(1+a$menge)*(1+a$material),
  tarif=(1+a$tarif), energie=(1+a$energie), pflegebudget=(1+a$pflegebudget), infl=(1+a$inflation), flat=1, 1)

project <- function(base, dat, a, hmax, noise = NULL) {
  g   <- vapply(dat$driver, growth_of, numeric(1), a = a)
  # INVARIANTE: Jeder Wachstumsfaktor ist endlich und > 0 — ein Faktor <= 0 (Annahme <= -100%) wuerde
  # Vorzeichen der Position kippen statt Wachstum zu skalieren.
  if (any(!is.finite(g) | g <= 0)) { gb <- which(!is.finite(g) | g <= 0)[1]
    stop(sprintf("project: INVARIANTE VERLETZT — Wachstumsfaktor von Position '%s' (Treiber '%s') ist %s, soll endlich und > 0.",
                 dat$pos[gb], dat$driver[gb], format(g[gb]))) }
  idx <- ((0:(hmax-1)) %% ncol(base)) + 1
  acc <- base[, idx, drop = FALSE] * g
  if (!is.null(noise)) {
    # INVARIANTE: Noise ist eine endliche, strikt positive Matrix in Positions-x-Monats-Form — ein Vektor
    # wuerde in R spaltenweise recycelt (stille Fehlzuordnung), ein Faktor <= 0 kippt Vorzeichen statt zu streuen.
    if (!is.matrix(noise) || !all(dim(noise) == dim(acc)))
      stop(sprintf("project: INVARIANTE VERLETZT — noise hat Form %s, soll Matrix %d x %d (Positionen x Monate).",
                   if (is.matrix(noise)) paste(dim(noise), collapse=" x ") else sprintf("Vektor[%d]", length(noise)),
                   nrow(acc), ncol(acc)))
    if (any(!is.finite(noise) | noise <= 0)) { nb <- which(!is.finite(noise) | noise <= 0, arr.ind = TRUE)
      stop(sprintf("project: INVARIANTE VERLETZT — noise[%d, %d] ist %s, soll endlich und > 0.",
                   nb[1,1], nb[1,2], format(noise[nb[1,1], nb[1,2]]))) }
  }
  if (!is.null(noise)) acc <- acc * noise
  # INVARIANTE: Die projizierte Accrual-Matrix ist vollstaendig endlich — ein NA/NaN liefe sonst stumm
  # durch GuV-Summen, Cash-Profile und KPIs, statt hier benannt zu sterben.
  if (any(!is.finite(acc))) { ab <- which(!is.finite(acc), arr.ind = TRUE)
    stop(sprintf("project: INVARIANTE VERLETZT — acc['%s', Monat %d] ist %s, soll endliche Zahl.",
                 rownames(acc)[ab[1,1]], ab[1,2], format(acc[ab[1,1], ab[1,2]]))) }
  list(acc = acc, g = g)
}
gsum <- function(acc, dat, grp, hmax) { idx <- which(dat$grp %in% grp)
  if (!length(idx)) return(rep(0, hmax)); colSums(acc[idx, , drop = FALSE]) }
build_guv <- function(acc, dat, hmax) {
  ums<-gsum(acc,dat,"umsatz",hmax); best<-gsum(acc,dat,"bestand",hmax); gl<-ums+best
  mat<-gsum(acc,dat,"material",hmax); roh<-gl-mat
  broh<-roh+gsum(acc,dat,"betr_ertrag",hmax)+gsum(acc,dat,"sopo",hmax)
  pers<-gsum(acc,dat,"personal",hmax); sach<-gsum(acc,dat,"sachkosten",hmax)+gsum(acc,dat,"afa",hmax)
  be<-broh-pers-sach
  ebt<-be+gsum(acc,dat,"zins_e",hmax)-gsum(acc,dat,"zins_a",hmax)+gsum(acc,dat,"neutral_e",hmax)-gsum(acc,dat,"neutral_a",hmax)
  erg<-ebt-gsum(acc,dat,"steuer",hmax)
  # INVARIANTE: GuV-Massenerhaltung — das Ergebnis der Staffel entspricht je Monat der Direktsumme aller
  # Ertragsgruppen minus aller Aufwandsgruppen. Bricht die Staffel (Vorzeichen gedreht, Gruppe vergessen,
  # Gruppe doppelt), feuert dieser Abgleich mit dem betroffenen Monat.
  ein_g <- c("umsatz","bestand","betr_ertrag","sopo","zins_e","neutral_e")
  aus_g <- c("material","personal","sachkosten","afa","zins_a","neutral_a","steuer")
  erg_direkt <- colSums(acc[dat$grp %in% ein_g, , drop=FALSE]) - colSums(acc[dat$grp %in% aus_g, , drop=FALSE])
  guv_dev <- abs(erg - erg_direkt)
  guv_tol <- 1e-6 * pmax(abs(erg_direkt), 1)
  if (any(guv_dev > guv_tol)) { gm <- which(guv_dev > guv_tol)[1]
    stop(sprintf("build_guv: INVARIANTE VERLETZT — GuV-Massenerhaltung in Monat %d: Ergebnis (Staffel) ist %.2f, Direktsumme Ertraege-Aufwaende soll %.2f (Abweichung %.6g).",
                 gm, erg[gm], erg_direkt[gm], guv_dev[gm])) }
  rbind(Umsatzerloese=ums, Bestandsveraenderung=best, Gesamtleistung=gl, Materialaufwand=mat, Rohertrag=roh,
        Betr.Rohertrag=broh, Personalaufwand=pers, Sach.Betriebskosten=sach, Betriebsergebnis=be, EBT=ebt, Ergebnis=erg)
}
conv_lag <- function(s, hist12, prof, hmax, carryin = TRUE) {
  # INVARIANTE: Lag-Profile haben 1..13 endliche Gewichte (der Carry-in greift auf hist12[(14-L):12] zu,
  # braucht also L-1 <= 12 Basismonate) und das Basisjahr exakt 12 Monate — sonst faltet der Kern mit
  # negativen/NA-Indizes stumm daneben. Leeres Profil (z.B. Doku-Zeile aus 02_Zahlungsprofile) stirbt hier benannt.
  if (length(prof) < 1 || length(prof) > 13 || any(!is.finite(prof)))
    stop(sprintf("conv_lag: INVARIANTE VERLETZT — Profil hat %d Gewicht(e) (davon nicht-endlich: %d), soll 1..13 endliche Gewichte.",
                 length(prof), sum(!is.finite(prof))))
  if (length(hist12) != 12)
    stop(sprintf("conv_lag: INVARIANTE VERLETZT — Basisjahr-Vektor hat Laenge %d, soll exakt 12 Monatswerte.", length(hist12)))
  L<-length(prof); carry<-if(L>1 && carryin) hist12[(14-L):12] else rep(0, max(0, L-1)); ext<-c(carry, s[1:hmax]); out<-numeric(hmax)
  for(m in 1:hmax){ acc<-0; for(k in 1:L) acc<-acc+ext[L+m-k]*prof[k]; out[m]<-acc }; out
}
apply_profile <- function(s, hist12, profile, g, hmax, profiles, carryin = TRUE) {
  if(profile=="inmonth")  return(s)
  if(profile=="noncash")  return(rep(0,hmax))
  if(profile=="annual_jan"){o<-rep(0,hmax); o[1]<-sum(hist12)*g; return(o)}
  if(profile=="abschlag")  return(rep(sum(hist12)*g/12, hmax))
  if(profile=="quarterly"){o<-rep(0,hmax); for(q in 1:4){e<-q*3; if(hmax>=e) o[e]<-sum(s[(e-2):e])}; return(o)}
  pr<-profiles[[profile]]; if(is.null(pr)) stop(paste("Unbekanntes Profil:",profile)); conv_lag(s, hist12, pr, hmax, carryin)
}
build_cash <- function(pr, base, dat, fin, hmax, profiles, open_cash, kk_limit, kk_zins = 0,
                       ust_rates = NULL, ust_active = FALSE, tax_cash = NULL, darlehen = NULL, opos = NULL,
                       ust_methode = "soll", ust_lag = 1) {
  acc<-pr$acc; g<-pr$g
  # USt-pflichtig: operative Zu-/Abfluesse brutto rechnen (inkl. USt ueber die Lag-Profile);
  # die Zahllast wird separat als eigene Cash-Zeile abgefuehrt (Soll: build_ust; Ist: aus dem Cashflow hier).
  if (ust_active && !is.null(ust_rates)) { acc <- acc * (1 + ust_rates); base <- base * (1 + ust_rates) }
  # OPOS aktiv: die expliziten offenen Posten ersetzen den statistischen Carry-in (Eroeffnungsbestand).
  # Je Seite getrennt: nur abschalten, wo OPOS Zeilen liefert -> keine Doppelerfassung.
  inflow<-c("umsatz","betr_ertrag","zins_e"); outflow<-c("material","personal","sachkosten","zins_a","steuer")
  fo_off <- !is.null(opos) && isTRUE(opos$fo_active); ve_off <- !is.null(opos) && isTRUE(opos$ve_active)
  cf<-matrix(0,nrow=nrow(dat),ncol=hmax,dimnames=list(dat$pos,NULL))
  for(i in seq_len(nrow(dat))) {
    ci <- TRUE
    if (fo_off && dat$grp[i] %in% c("umsatz","betr_ertrag")) ci <- FALSE
    if (ve_off && dat$grp[i] %in% c("material","sachkosten")) ci <- FALSE
    cf[i,]<-apply_profile(acc[i,], base[i,], dat$cashprofile[i], g[i], hmax, profiles, carryin = ci)
  }
  ein<-colSums(cf[dat$grp %in% inflow,,drop=FALSE]); aus<-colSums(cf[dat$grp %in% outflow,,drop=FALSE])
  of<-numeric(hmax); ov<-numeric(hmax)
  if (!is.null(opos)) { of<-opos$forderungen; ov<-opos$verbindlichkeiten; ein<-ein+of; aus<-aus+ov }
  netcf<-ein-aus-fin$capex+fin$foerder-fin$tilg+fin$aufn
  # Darlehensmodul: Auszahlung (+), Zins (-), Tilgung (-) als eigene Cash-Zeilen.
  dz<-numeric(hmax); dt<-numeric(hmax); da<-numeric(hmax)
  if (!is.null(darlehen)) { da<-darlehen$auszahlung; dz<-darlehen$zins; dt<-darlehen$tilgung
                            netcf <- netcf + da - dz - dt }
  ust_c<-numeric(hmax); kst_c<-numeric(hmax); gst_c<-numeric(hmax); ent_c<-numeric(hmax)
  if (!is.null(tax_cash)) {
    if (!is.null(tax_cash$ust)) ust_c <- tax_cash$ust
    # Ist-Versteuerung: USt-Zahllast aus dem Brutto-Cashflow (operativ + OPOS) statt aus der Leistung.
    if (ust_active && identical(ust_methode, "ist")) {
      of_vat <- if (!is.null(opos) && !is.null(opos$fo_vat)) opos$fo_vat else numeric(hmax)
      ov_vat <- if (!is.null(opos) && !is.null(opos$ve_vat)) opos$ve_vat else numeric(hmax)
      ust_c <- ust_ist_remit(cf, base, dat, ust_rates, of_vat, ov_vat, ust_lag, hmax, fo_off = fo_off, ve_off = ve_off)
    }
    kst_c <- -tax_cash$kst_vz; gst_c <- -tax_cash$gewst_vz; ent_c <- -tax_cash$entnahme
    netcf <- netcf + ust_c + kst_c + gst_c + ent_c
  }
  # INVARIANTE: Kompositions-Identitaet des NettoCashflows — netcf entspricht je Monat exakt der Summe seiner
  # Komponenten (operativ inkl. OPOS, Finanzplan, Darlehen, Steuern). Ein gedrehtes Vorzeichen oder eine beim
  # Aufsummieren vergessene Komponente feuert hier, bevor sie den Endbestand erreicht.
  netcf_soll <- ein - aus - fin$capex + fin$foerder - fin$tilg + fin$aufn + da - dz - dt + ust_c + kst_c + gst_c + ent_c
  cf_dev <- abs(netcf - netcf_soll)
  cf_tol <- 1e-6 * pmax(abs(netcf_soll), 1)
  if (any(cf_dev > cf_tol)) { cm <- which(cf_dev > cf_tol)[1]
    stop(sprintf("build_cash: INVARIANTE VERLETZT — NettoCashflow-Komposition in Monat %d: netcf ist %.2f, Komponentensumme soll %.2f (Abweichung %.6g).",
                 cm, netcf[cm], netcf_soll[cm], cf_dev[cm])) }
  # INVARIANTE: KK-Zins-Vertragsformel Monat — kkz[m] = max(0, -Vorsaldo) * kk_zins/12; Endbestand-Rekursion
  # end[m] = Vorsaldo + netcf[m] - kkz[m]. Teleskop-Abgleich nach der Schleife.
  end<-numeric(hmax); kkz<-numeric(hmax); prev<-open_cash
  for(m in 1:hmax){ kkz[m]<-max(0,-prev)*kk_zins/12; end[m]<-prev+netcf[m]-kkz[m]; prev<-end[m] }
  # INVARIANTE: KK-Zins ist nie negativ (er entsteht nur aus negativem Vorsaldo), und der letzte Endbestand
  # erfuellt die Teleskopsumme Eroeffnung + Summe(netcf) - Summe(kkz) — ein Indexfehler in der Rekursion feuert hier.
  if (any(kkz < 0)) { km <- which(kkz < 0)[1]
    stop(sprintf("build_cash: INVARIANTE VERLETZT — KK_Zins in Monat %d ist %s, soll >= 0.", km, format(kkz[km]))) }
  end_soll <- open_cash + sum(netcf) - sum(kkz)
  if (abs(end[hmax] - end_soll) > 1e-6 * max(abs(end_soll), 1))
    stop(sprintf("build_cash: INVARIANTE VERLETZT — Endbestand-Teleskop: Endbestand[Monat %d] ist %.2f, Eroeffnung + Summe(netcf) - Summe(KK_Zins) soll %.2f (Abweichung %.6g).",
                 hmax, end[hmax], end_soll, abs(end[hmax] - end_soll)))
  rows <- rbind(Einzahlungen=ein, Auszahlungen=aus, Investitionen=fin$capex, Foerdermittel=fin$foerder,
                Tilgung=fin$tilg, Darlehensaufnahme=fin$aufn)
  if (!is.null(opos))
    rows <- rbind(rows, OPOS_Forderungen=of, OPOS_Verbindlichkeiten=ov)
  if (!is.null(darlehen))
    rows <- rbind(rows, Darlehen_Auszahlung=da, Darlehen_Zins=-dz, Darlehen_Tilgung=-dt)
  if (!is.null(tax_cash))
    rows <- rbind(rows, USt_Zahllast=ust_c, KSt_Voraus=kst_c, GewSt_Voraus=gst_c, Steuerentnahme=ent_c)
  rbind(rows, NettoCashflow=netcf, KK_Zins=kkz, Endbestand=end,
        KK_Inanspruchnahme=pmax(0,-end), Freie_Kreditlinie=kk_limit-pmax(0,-end)+pmax(0,end))
}

# --- Steuer-Bausteine (Phase: Steuern). Nur aktiv, wenn Blatt 08_Steuern vorhanden. ---
# USt (Soll-Versteuerung): Zahllast = Ausgangs- minus Vorsteuer auf der Leistungs-/Accrual-Basis,
# abgefuehrt mit Lag. Carry-in: die USt auf die letzten Basisjahresmonate wird in den ersten
# Planmonaten faellig (Dezember-USt -> Januar). Rueckgabe = Cash-Effekt (Abfuehrung negativ).
build_ust <- function(acc, base, dat, ust, lag, hmax) {
  # INVARIANTE: Der Voranmeldungs-Lag liegt in 1..12 — sonst griffe der Carry-in-Index zext[12+m-lag]
  # vor den 12-Monats-Puffer (Index <= 0) und lieferte stumm leere Werte statt der Dezember-Zahllast.
  if (!is.finite(lag) || lag < 1 || lag > 12)
    stop(sprintf("build_ust: INVARIANTE VERLETZT — ust_lag ist %s, soll in 1..12.", format(lag)))
  r <- ust
  rev <- dat$grp %in% c("umsatz","betr_ertrag"); exp <- dat$grp %in% c("material","sachkosten")
  zfc <- colSums((acc * r)[rev,,drop=FALSE]) - colSums((acc * r)[exp,,drop=FALSE])   # Planmonate
  zb  <- colSums((base * r)[rev,,drop=FALSE]) - colSums((base * r)[exp,,drop=FALSE])  # Basisjahr (12)
  zext <- c(zb, zfc)                       # Index 1..12 = Basis, 13..(12+hmax) = M1..Mhmax
  remit <- vapply(1:hmax, function(m) zext[12 + m - lag], numeric(1))
  -remit
}
# USt nach Ist-Versteuerung: die Zahllast folgt der Vereinnahmung/Verausgabung (Cash), nicht der
# Leistung. Ausgangs-USt auf die Brutto-Einzahlungen (operativ + OPOS-Forderungen), Vorsteuer auf
# die Brutto-Auszahlungen (operativ + OPOS-Verbindlichkeiten) -> die USt folgt automatisch den OPOS-
# Terminen. cf/base sind brutto, VAT-Anteil je Position = Betrag * r/(1+r). Basisjahr-Carry-in wie
# Soll approximiert. Abfuehrung mit Lag. Rueckgabe = Cash-Effekt (Abfuehrung negativ).
ust_ist_remit <- function(cf, base, dat, r, of_vat, ov_vat, lag, hmax,
                          fo_off = FALSE, ve_off = FALSE) {
  # INVARIANTE: Wie build_ust — Lag in 1..12, sonst greift zext[12+m-lag] vor den Basisjahr-Puffer.
  if (!is.finite(lag) || lag < 1 || lag > 12)
    stop(sprintf("ust_ist_remit: INVARIANTE VERLETZT — ust_lag ist %s, soll in 1..12.", format(lag)))
  rr <- r/(1+r); rr[is.na(rr) | is.infinite(rr)] <- 0
  rev <- dat$grp %in% c("umsatz","betr_ertrag"); exp <- dat$grp %in% c("material","sachkosten")
  out_fc <- colSums((cf*rr)[rev,,drop=FALSE]) + of_vat
  in_fc  <- colSums((cf*rr)[exp,,drop=FALSE]) + ov_vat
  zfc <- out_fc - in_fc
  # Basisjahr-Carry-in nur fuer die Seite, deren operativen Carry-in OPOS NICHT ersetzt (keine Doppelerfassung).
  zb_rev <- if (fo_off) numeric(ncol(base)) else colSums((base*rr)[rev,,drop=FALSE])
  zb_exp <- if (ve_off) numeric(ncol(base)) else colSums((base*rr)[exp,,drop=FALSE])
  zb  <- zb_rev - zb_exp
  zext <- c(zb, zfc)
  -vapply(1:hmax, function(m) zext[12 + m - lag], numeric(1))
}
# Ertragsteuer: GewSt (Messzahl x Hebesatz, Freibetrag nur EK/PersG) und KSt (+Soli, nur Kapitalges.,
# nicht gemeinnuetzig) aus dem hochgerechneten Jahres-EBT; als Quartals-Vorauszahlungen auf den
# gesetzlichen Terminen (KSt 3/6/9/12, GewSt 2/5/8/11), gemappt auf den Planmonat. Transparente
# Formen: keine KSt, stattdessen Steuerentnahme (Quote x Plangewinn) auf den ESt-Terminen.
build_ertragsteuer <- function(guv, tax, start_month, hmax, ebt_annual = NULL) {
  ebt_a <- if (!is.null(ebt_annual)) ebt_annual else sum(guv["EBT", ]) * 12 / hmax
  # INVARIANTE: Das annualisierte EBT ist endlich — ein NaN hier wuerde alle Vorauszahlungen stumm vergiften.
  if (!is.finite(ebt_a))
    stop(sprintf("build_ertragsteuer: INVARIANTE VERLETZT — Jahres-EBT ist %s, soll endliche Zahl.", format(ebt_a)))
  rf <- tax$rechtsform; gw <- tax$gewst_status
  gewst_a <- 0
  if (gw == "gewerblich") {
    fb <- if (rf != "kapital") tax$gewst_freibetrag else 0
    ge <- max(0, ebt_a - fb); gewst_a <- ge * tax$gewst_messzahl * (tax$gewst_hebesatz/100)
  }
  kst_a <- 0
  if (rf == "kapital" && gw != "gemeinnuetzig")
    kst_a <- max(0, ebt_a) * tax$kst_satz * (1 + tax$soli_satz)
  # INVARIANTE: Jahressteuern sind nie negativ — eine negative Steuer wuerde als stiller Cash-Zufluss
  # in die Vorauszahlungstermine laufen (vorzeichenkritisch).
  if (kst_a < 0 || gewst_a < 0)
    stop(sprintf("build_ertragsteuer: INVARIANTE VERLETZT — Jahressteuer negativ: KSt ist %.2f, GewSt ist %.2f, soll je >= 0.", kst_a, gewst_a))
  to_fc <- function(cm) { i <- ((cm - start_month) %% 12) + 1; if (i >= 1 && i <= hmax) i else NA }
  place <- function(q, cal) { v <- numeric(hmax); for (cm in cal) { i <- to_fc(cm); if (!is.na(i)) v[i] <- v[i] + q }; v }
  kst_vz <- place(kst_a/4, c(3,6,9,12)); gewst_vz <- place(gewst_a/4, c(2,5,8,11))
  entn <- numeric(hmax)
  if (rf != "kapital" && tax$entnahme_quote) entn <- place(max(0, ebt_a) * tax$entnahme_quote/4, c(3,6,9,12))
  list(expense=rep((kst_a + gewst_a)/12, hmax), kst_vz=kst_vz, gewst_vz=gewst_vz, entnahme=entn,
       kst_a=kst_a, gewst_a=gewst_a)
}

# Darlehensmodul (Phase: Finanzierung). Nur aktiv, wenn Blatt 09_Darlehen vorhanden.
# Je Kredit: Auszahlung im Startmonat (Neuvalutierung), danach Zins auf die fallende Restschuld
# und Tilgung nach Tilgungsart (annuitaet | linear | endfaellig | nur_zins). Konvention: die Bedienung
# beginnt im Monat nach der Auszahlung (start_m + 1); start_m <= 0 = bereits vor Forecast-Beginn
# valutiert (Restschuld = betrag, keine Auszahlung, Bedienung ab M1). disagio mindert nur die
# Auszahlung, der Zins laeuft auf den Nominalbetrag. Zins -> GuV/EBT (siehe run_model) und Cash.
build_darlehen <- function(loans, hmax) {
  zins <- numeric(hmax); tilg <- numeric(hmax); ausz <- numeric(hmax)
  if (is.null(loans) || !nrow(loans)) return(list(zins=zins, tilgung=tilg, auszahlung=ausz))
  col <- function(nm, d) if (nm %in% names(loans)) loans[[nm]] else rep(d, nrow(loans))
  betrag <- suppressWarnings(as.numeric(col("betrag", 0))); zsatz <- suppressWarnings(as.numeric(col("zinssatz", 0)))
  art  <- tolower(trimws(as.character(col("tilgungsart", "endfaellig"))))
  lauf <- suppressWarnings(as.integer(col("laufzeit_m", hmax))); strt <- suppressWarnings(as.integer(col("start_m", 1)))
  disag <- suppressWarnings(as.numeric(col("disagio", 0))); disag[is.na(disag)] <- 0
  for (i in seq_len(nrow(loans))) {
    P <- betrag[i]; r <- zsatz[i]/12; n <- lauf[i]; s <- strt[i]; a <- art[i]
    if (is.na(P) || P <= 0) next
    # INVARIANTE: Ein aktiver Kredit hat endlichen Zinssatz in [0, 1) (Dezimal p.a.), Laufzeit >= 1, Disagio in [0, 1]
    # und eine bekannte Tilgungsart — NA/unbekannte Art wuerde sonst still wie 'keine Tilgung' weiterrechnen.
    if (!is.finite(r) || r < 0 || zsatz[i] >= 1)
      stop(sprintf("build_darlehen: INVARIANTE VERLETZT — 09_Darlehen Zeile %d: zinssatz ist %s, soll Dezimal in [0, 1) (4.5%% = 0.045).", i, format(zsatz[i])))
    if (is.na(n) || n < 1)
      stop(sprintf("build_darlehen: INVARIANTE VERLETZT — 09_Darlehen Zeile %d: laufzeit_m ist %s, soll ganze Zahl >= 1.", i, format(lauf[i])))
    if (disag[i] < 0 || disag[i] > 1)
      stop(sprintf("build_darlehen: INVARIANTE VERLETZT — 09_Darlehen Zeile %d: disagio ist %s, soll Quote in [0, 1].", i, format(disag[i])))
    if (!a %in% c("annuitaet","linear","endfaellig","nur_zins"))
      stop(sprintf("build_darlehen: INVARIANTE VERLETZT — 09_Darlehen Zeile %d: tilgungsart '%s' unbekannt, soll annuitaet|linear|endfaellig|nur_zins.", i, a))
    if (!is.na(s) && s >= 1 && s <= hmax) ausz[s] <- ausz[s] + P * (1 - disag[i])   # Auszahlung (Neuvalutierung)
    first <- if (!is.na(s) && s >= 1) s + 1 else 1                                  # erste Bedienung
    if (first > hmax) next
    ann <- if (a == "annuitaet" && r > 0 && n > 0) P * r / (1 - (1 + r)^(-n)) else NA
    bal <- P; paid <- 0
    tilg_i <- 0   # (Instrumentierung) kumulierte Tilgung dieses Kredits fuer die Massengrenze unten
    for (m in first:hmax) {
      paid <- paid + 1; if (!is.na(n) && paid > n) break
      z <- bal * r
      t <- switch(a, annuitaet  = if (!is.na(ann)) ann - z else P / max(n, 1),
                     linear     = P / max(n, 1),
                     endfaellig = if (!is.na(n) && paid == n) bal else 0,
                     nur_zins   = 0, 0)
      t <- max(0, min(t, bal))
      zins[m] <- zins[m] + z; tilg[m] <- tilg[m] + t; bal <- bal - t
      tilg_i <- tilg_i + t
    }
    # INVARIANTE: Massengrenze der Tilgung — ueber die Laufzeit wird nie mehr getilgt als der Nominalbetrag,
    # und die Restschuld faellt nie unter 0. Bricht die min/max-Klammerung der Rate, feuert dieser Abgleich.
    if (tilg_i > P + 1e-6 * max(P, 1) || bal < -1e-6 * max(P, 1))
      stop(sprintf("build_darlehen: INVARIANTE VERLETZT — 09_Darlehen Zeile %d: kumulierte Tilgung ist %.2f, soll <= Betrag %.2f; Restschuld ist %.2f, soll >= 0.",
                   i, tilg_i, P, bal))
  }
  list(zins = zins, tilgung = tilg, auszahlung = ausz)
}

# OPOS-Modul (Phase: offene Posten). Nur aktiv, wenn Blatt 10_OPOS vorhanden.
# Bildet die expliziten offenen Forderungen/Verbindlichkeiten (Einzelrechnung oder Aging-Bucket)
# auf erwartete Zahlungsperioden ab. Forderung: Betrag x Eingangswahrscheinlichkeit, faellig am
# Zahlungsziel + Verzug. Verbindlichkeit: zahlbar am Faelligkeitstag (+ optional Stundung/Quote).
# period_fn(d) bildet ein Datum auf einen Periodenindex 1..n ab (Monat hier, Woche in Phase Wochen).
# Rueckgabe ersetzt im Cash-Build den statistischen Carry-in (keine Doppelerfassung).
# Robuster Faelligkeits-Parser (F-06/07): Excel-Serial, ISO (JJJJ-MM-TT), DE (TT.MM.JJJJ) und TT/MM/JJJJ.
# Die Trennzeichen (-, ., /) kollidieren nicht, daher ist die Reihenfolge der Formate unkritisch.
# Nicht aufloesbar -> NA (vom Aufrufer mit Zeilennummer gemeldet, nicht stumm gedroppt).
parse_due <- function(x) {
  if (is.na(x) || !nzchar(trimws(as.character(x)))) return(as.Date(NA))
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num) && num > 20000 && num < 80000) return(as.Date(num, origin = "1899-12-30"))  # Excel-Serial
  for (f in c("%Y-%m-%d","%d.%m.%Y","%d/%m/%Y")) { d <- as.Date(as.character(x), format = f); if (!is.na(d)) return(d) }
  as.Date(NA)
}
build_opos <- function(opos, start_date, n, period_fn) {
  fo <- numeric(n); ve <- numeric(n); fo_vat <- numeric(n); ve_vat <- numeric(n)
  meta <- list(n_fo=0, n_ve=0, sum_fo=0, sum_ve=0, beyond_fo=0, beyond_ve=0)
  if (is.null(opos) || !nrow(opos)) return(list(forderungen=fo, verbindlichkeiten=ve, fo_vat=fo_vat, ve_vat=ve_vat, fo_active=FALSE, ve_active=FALSE, meta=meta))
  col <- function(nm, d) if (nm %in% names(opos)) opos[[nm]] else rep(d, nrow(opos))
  art  <- tolower(trimws(as.character(col("art", "forderung"))))
  betr <- suppressWarnings(as.numeric(col("betrag", 0)))
  wahr <- suppressWarnings(as.numeric(col("wahrscheinlichkeit", 1))); wahr[is.na(wahr)] <- 1
  verz <- suppressWarnings(as.numeric(col("verzug_tage", 0))); verz[is.na(verz)] <- 0
  ustz <- suppressWarnings(as.numeric(col("ust_satz", 0))); ustz[is.na(ustz)] <- 0   # eingebetteter USt-Satz (brutto)
  faell <- as.character(col("faellig_am", NA)); ziel <- suppressWarnings(as.numeric(col("zahlungsziel_tage", NA)))
  bad <- integer(0)
  for (i in seq_len(nrow(opos))) {
    if (is.na(betr[i]) || betr[i] == 0) next
    # INVARIANTE: Eingangswahrscheinlichkeit ist eine Quote in [0, 1], der eingebettete USt-Satz ein Dezimalsatz
    # in [0, 1) — "80" statt "0.8" wuerde den erwarteten Zufluss um Faktor 80 aufblasen (groessenkritisch).
    if (wahr[i] < 0 || wahr[i] > 1)
      stop(sprintf("build_opos: INVARIANTE VERLETZT — 10_OPOS Zeile %d: wahrscheinlichkeit ist %s, soll in [0, 1].", i, format(wahr[i])))
    if (ustz[i] < 0 || ustz[i] >= 1)
      stop(sprintf("build_opos: INVARIANTE VERLETZT — 10_OPOS Zeile %d: ust_satz ist %s, soll in [0, 1) (19%% = 0.19).", i, format(ustz[i])))
    d <- parse_due(faell[i])
    if (is.na(d) && !is.na(ziel[i])) d <- start_date + ziel[i]
    if (is.na(d)) { bad <- c(bad, i); next }                     # weder Datum noch Zahlungsziel lesbar (F-06/07)
    amt <- betr[i] * wahr[i]; vat <- amt * ustz[i]/(1+ustz[i]); is_fo <- grepl("^f", art[i])
    p <- period_fn(d + verz[i])
    if (is.na(p)) {                                              # jenseits Horizont: nicht stumm fallen lassen (F-17)
      if (is_fo) meta$beyond_fo <- meta$beyond_fo + amt else meta$beyond_ve <- meta$beyond_ve + amt; next }
    if (is_fo) { fo[p] <- fo[p] + amt; fo_vat[p] <- fo_vat[p] + vat; meta$n_fo <- meta$n_fo + 1; meta$sum_fo <- meta$sum_fo + amt }
    else       { ve[p] <- ve[p] + amt; ve_vat[p] <- ve_vat[p] + vat; meta$n_ve <- meta$n_ve + 1; meta$sum_ve <- meta$sum_ve + amt }
  }
  if (length(bad)) stop(sprintf("10_OPOS: nicht parsbares Faelligkeitsdatum in Zeile(n) %s (faellig_am: ISO JJJJ-MM-TT, DE TT.MM.JJJJ oder Excel-Serial; alternativ zahlungsziel_tage setzen).", paste(bad, collapse=", ")))
  # INVARIANTE: OPOS-Massenerhaltung — platziert plus jenseits-Horizont entspricht je Seite exakt der Summe
  # Betrag x Wahrscheinlichkeit aller gueltigen Zeilen: kein Posten doppelt, keiner stumm verloren.
  amt_all <- betr * wahr; op_valid <- !is.na(betr) & betr != 0; op_isf <- grepl("^f", art)
  fo_soll <- sum(amt_all[op_valid & op_isf]); ve_soll <- sum(amt_all[op_valid & !op_isf])
  if (abs(sum(fo) + meta$beyond_fo - fo_soll) > 1e-6 * max(abs(fo_soll), 1) ||
      abs(sum(ve) + meta$beyond_ve - ve_soll) > 1e-6 * max(abs(ve_soll), 1))
    stop(sprintf("build_opos: INVARIANTE VERLETZT — OPOS-Massenerhaltung: Forderungen platziert+jenseits ist %.2f, soll %.2f; Verbindlichkeiten ist %.2f, soll %.2f.",
                 sum(fo) + meta$beyond_fo, fo_soll, sum(ve) + meta$beyond_ve, ve_soll))
  list(forderungen = fo, verbindlichkeiten = ve, fo_vat = fo_vat, ve_vat = ve_vat,
       fo_active = any(grepl("^f", art)), ve_active = any(grepl("^v", art)), meta = meta)
}
# Datum -> Monatsindex relativ zum Forecast-Beginn (1..n); ueberfaellig -> 1, jenseits Horizont -> NA.
period_month <- function(d, start_date, n) {
  mi <- (as.integer(format(d, "%Y")) - as.integer(format(start_date, "%Y"))) * 12 +
        (as.integer(format(d, "%m")) - as.integer(format(start_date, "%m"))) + 1L
  if (mi < 1) mi <- 1L
  if (mi > n) return(NA_integer_)
  mi
}

run_model <- function(mdl, assume = NULL, noise = NULL, tax_on = TRUE) {
  a <- if (is.null(assume)) mdl$assume else assume
  pr <- project(mdl$base, mdl$pos, a, mdl$HMAX, noise)
  guv  <- build_guv(pr$acc, mdl$pos, mdl$HMAX)
  # Darlehensmodul (optional, additiv): der Zins mindert EBT/Ergebnis VOR der Ertragsteuer.
  dl <- build_darlehen(mdl$loans, mdl$HMAX)
  dl_active <- !is.null(mdl$loans) && any(dl$zins != 0 | dl$tilgung != 0 | dl$auszahlung != 0)
  if (dl_active) { guv["EBT", ] <- guv["EBT", ] - dl$zins; guv["Ergebnis", ] <- guv["Ergebnis", ] - dl$zins }
  tax <- if (tax_on) mdl$tax else NULL
  active <- !is.null(tax)
  ust_pf <- active && identical(tax$ust_status, "pflichtig")
  ust_cash <- if (ust_pf && identical(tax$ust_methode, "soll")) build_ust(pr$acc, mdl$base, mdl$pos, mdl$ust, tax$ust_lag, mdl$HMAX) else NULL
  et <- NULL
  if (active) {
    # Steuer-Annualisierung saisonneutral aus voller 12-Monats-Projektion (F-08), nicht aus dem Planfenster.
    ebt_annual <- sum(build_guv(project(mdl$base, mdl$pos, a, 12)$acc, mdl$pos, 12)["EBT", ])
    if (dl_active) ebt_annual <- ebt_annual - sum(build_darlehen(mdl$loans, 12)$zins)  # Konsistenz Zins
    et <- build_ertragsteuer(guv, tax, mdl$start_month, mdl$HMAX, ebt_annual = ebt_annual)
  }
  if (active) guv["Ergebnis", ] <- guv["Ergebnis", ] - et$expense
  tax_cash <- NULL
  if (active) tax_cash <- list(ust = if (!is.null(ust_cash)) ust_cash else numeric(mdl$HMAX),
                               kst_vz = et$kst_vz, gewst_vz = et$gewst_vz, entnahme = et$entnahme)
  # OPOS (optional, additiv): offene Posten ersetzen den Carry-in; hier Monatsraster.
  op <- build_opos(mdl$opos, mdl$start_date, mdl$HMAX, function(d) period_month(d, mdl$start_date, mdl$HMAX))
  op_active <- !is.null(mdl$opos) && (op$fo_active || op$ve_active)
  cash <- build_cash(pr, mdl$base, mdl$pos, mdl$fin, mdl$HMAX, mdl$profiles, mdl$open_cash, mdl$kk_limit, mdl$kk_zins,
                     ust_rates = mdl$ust, ust_active = ust_pf, tax_cash = tax_cash,
                     darlehen = if (dl_active) dl else NULL, opos = if (op_active) op else NULL,
                     ust_methode = if (active) tax$ust_methode else "soll", ust_lag = if (active) tax$ust_lag else 1)
  end <- cash["Endbestand", ]
  # INVARIANTE: Die veroeffentlichten Kernreihen sind vollstaendig endlich — ein NaN, das alle Einzel-Guards
  # ueberlebt haette, stirbt hier benannt statt als KPI ausgeliefert zu werden.
  if (any(!is.finite(end)) || any(!is.finite(guv["Ergebnis", ]))) {
    rm_b <- which(!is.finite(end) | !is.finite(guv["Ergebnis", ]))[1]
    stop(sprintf("run_model: INVARIANTE VERLETZT — Monat %d: Endbestand ist %s, Ergebnis ist %s, soll je endliche Zahl.",
                 rm_b, format(end[rm_b]), format(guv["Ergebnis", rm_b]))) }
  kpis <- list(trough=min(end), trough_month=which.min(end), maxKK=max(cash["KK_Inanspruchnahme",]),
               months_below=sum(end < mdl$mindestreserve), result=sum(guv["Ergebnis",]), end=end[mdl$HMAX])
  list(guv=guv, cash=cash, kpis=kpis, et=et, darlehen=if (dl_active) dl else NULL, opos=if (op_active) op else NULL)
}

# Datum -> Wochenindex ab Forecast-Beginn (1..n); ueberfaellig -> 1, jenseits Horizont -> NA.
period_week <- function(d, start_date, n_weeks) {
  wi <- as.integer(floor(as.numeric(d - start_date) / 7)) + 1L
  if (is.na(wi)) return(NA_integer_)
  if (wi < 1) wi <- 1L
  if (wi > n_weeks) return(NA_integer_)
  wi
}

# Wochensicht (Phase Wochen): leitet die Liquiditaet woechentlich aus dem Monatslauf ab.
# Monatsbetraege werden auf Wochen verteilt -- glatt ueber die Tagesueberlappung, Personal als
# Lohn-Lump (Monatsende), Steuern auf die gesetzlichen Tage (USt/KSt/Entnahme 10., GewSt 15.),
# Darlehensdienst Monatsende, Auszahlung Monatsanfang; OPOS exakt auf ihren Termin. Summe je Monat
# bleibt erhalten -> rueckabgestimmt auf run_model. KK-Zins wochengenau (Tage/365). Voller Horizont.
run_model_weekly <- function(mdl, assume = NULL, tax_on = TRUE) {
  a <- if (is.null(assume)) mdl$assume else assume
  H <- mdl$HMAX; sd <- mdl$start_date
  pr <- project(mdl$base, mdl$pos, a, H); acc <- pr$acc; g <- pr$g
  guv <- build_guv(acc, mdl$pos, H)
  dl <- build_darlehen(mdl$loans, H)
  dl_active <- !is.null(mdl$loans) && any(dl$zins != 0 | dl$tilgung != 0 | dl$auszahlung != 0)
  if (dl_active) { guv["EBT",] <- guv["EBT",] - dl$zins; guv["Ergebnis",] <- guv["Ergebnis",] - dl$zins }
  tax <- if (tax_on) mdl$tax else NULL; active <- !is.null(tax)
  ust_pf <- active && identical(tax$ust_status, "pflichtig")
  ust_cash <- if (ust_pf && identical(tax$ust_methode, "soll")) build_ust(acc, mdl$base, mdl$pos, mdl$ust, tax$ust_lag, H) else numeric(H)
  et <- NULL
  if (active) {
    ebt_annual <- sum(build_guv(project(mdl$base, mdl$pos, a, 12)$acc, mdl$pos, 12)["EBT", ])
    if (dl_active) ebt_annual <- ebt_annual - sum(build_darlehen(mdl$loans, 12)$zins)
    et <- build_ertragsteuer(guv, tax, mdl$start_month, H, ebt_annual = ebt_annual)
  }

  # Forecast-Kalender je Monat
  ym0 <- as.integer(format(sd,"%Y"))*12L + as.integer(format(sd,"%m")) - 1L
  mo_first <- rep(as.Date(NA), H); mo_last <- rep(as.Date(NA), H); mo_dim <- integer(H); mo_year <- integer(H); mo_mon <- integer(H)
  for (m in 1:H) { yy <- (ym0+m-1L) %/% 12L; mm <- (ym0+m-1L) %% 12L + 1L
    f <- as.Date(sprintf("%04d-%02d-01", yy, mm)); l <- seq(f, by="month", length.out=2)[2] - 1
    mo_year[m]<-yy; mo_mon[m]<-mm; mo_first[m]<-f; mo_last[m]<-l; mo_dim[m]<-as.integer(l-f)+1L }
  horizon_end <- mo_last[H]
  n_weeks <- as.integer(ceiling(as.numeric(horizon_end - sd + 1) / 7))
  wk_start <- sd + (0:(n_weeks-1))*7L; wk_end <- pmin(wk_start + 6L, horizon_end); wk_days <- as.integer(wk_end - wk_start) + 1L

  O <- matrix(0, n_weeks, H)   # Tage von Woche w im Forecast-Monat m
  for (w in 1:n_weeks) for (m in 1:H) { lo <- max(wk_start[w], mo_first[m]); hi <- min(wk_end[w], mo_last[m])
    if (hi >= lo) O[w,m] <- as.integer(hi - lo) + 1L }
  # INVARIANTE: Die Wochen-Kacheln decken jeden Forecast-Monat luecken- und ueberlappungsfrei ab
  # (Summe der Wochen-Tage je Monat = Monatslaenge) — nur dann verteilt spread_even massenerhaltend.
  # Feuert insbesondere, wenn der Forecast-Beginn kein Monatserster ist (angeschnittener erster Monat).
  o_dev <- abs(colSums(O) - mo_dim)
  if (any(o_dev > 0.5)) { ob <- which(o_dev > 0.5)[1]
    stop(sprintf("run_model_weekly: INVARIANTE VERLETZT — Wochen-Kachelung: Monat %d hat %g Wochen-Tage, soll %d (Forecast-Beginn %s kein Monatserster oder Kalenderfehler).",
                 ob, colSums(O)[ob], mo_dim[ob], format(sd))) }
  spread_even <- function(v) { x <- numeric(n_weeks); for (m in 1:H) if (v[m] != 0) x <- x + O[,m]/mo_dim[m]*v[m]; x }
  lump_day <- function(v, day) { x <- numeric(n_weeks)
    for (m in 1:H) { if (v[m] == 0) next
      dd <- if (identical(day,"last")) mo_last[m] else as.Date(sprintf("%04d-%02d-%02d", mo_year[m], mo_mon[m], min(day, mo_dim[m])))
      wi <- period_week(dd, sd, n_weeks); if (!is.na(wi)) x[wi] <- x[wi] + v[m] }; x }

  # operative Monats-Cashflows je Position (wie build_cash; Carry-in je Seite ggf. aus)
  base <- mdl$base; if (ust_pf && !is.null(mdl$ust)) { acc <- acc * (1+mdl$ust); base <- base * (1+mdl$ust) }
  opm <- build_opos(mdl$opos, sd, H, function(d) period_month(d, sd, H))
  fo_off <- !is.null(mdl$opos) && isTRUE(opm$fo_active); ve_off <- !is.null(mdl$opos) && isTRUE(opm$ve_active)
  G <- mdl$pos$grp; inflow <- c("umsatz","betr_ertrag","zins_e"); outflow <- c("material","personal","sachkosten","zins_a","steuer")
  cf <- matrix(0, nrow(mdl$pos), H)
  for (i in seq_len(nrow(mdl$pos))) { ci <- TRUE
    if (fo_off && G[i] %in% c("umsatz","betr_ertrag")) ci <- FALSE
    if (ve_off && G[i] %in% c("material","sachkosten")) ci <- FALSE
    cf[i,] <- apply_profile(acc[i,], base[i,], mdl$pos$cashprofile[i], g[i], H, mdl$profiles, carryin = ci) }
  m_pers <- colSums(cf[G=="personal",,drop=FALSE]); m_in <- colSums(cf[G %in% inflow,,drop=FALSE])
  m_out <- colSums(cf[G %in% setdiff(outflow,"personal"),,drop=FALSE])
  if (ust_pf && identical(tax$ust_methode, "ist"))   # Ist: Zahllast aus dem Brutto-Cashflow (monatlich), dann auf den 10. kalendarisiert
    ust_cash <- ust_ist_remit(cf, base, mdl$pos, mdl$ust, opm$fo_vat, opm$ve_vat, tax$ust_lag, H, fo_off = fo_off, ve_off = ve_off)

  ein_w <- spread_even(m_in); aus_w <- spread_even(m_out) + lump_day(m_pers, "last")   # Personal Monatsende
  ow <- build_opos(mdl$opos, sd, n_weeks, function(d) period_week(d, sd, n_weeks))
  of_w <- ow$forderungen; ov_w <- ow$verbindlichkeiten; ein_w <- ein_w + of_w; aus_w <- aus_w + ov_w
  capex_w <- spread_even(mdl$fin$capex); foerder_w <- spread_even(mdl$fin$foerder); tilg_w <- spread_even(mdl$fin$tilg); aufn_w <- spread_even(mdl$fin$aufn)
  da_w <- lump_day(dl$auszahlung, 1); dz_w <- lump_day(dl$zins, "last"); dt_w <- lump_day(dl$tilgung, "last")
  ust_w <- numeric(n_weeks); kst_w <- numeric(n_weeks); gst_w <- numeric(n_weeks); ent_w <- numeric(n_weeks)
  if (active) { if (ust_pf) ust_w <- lump_day(ust_cash, 10)
    kst_w <- -lump_day(et$kst_vz, 10); gst_w <- -lump_day(et$gewst_vz, 15); ent_w <- -lump_day(et$entnahme, 10) }
  netcf_w <- ein_w - aus_w - capex_w + foerder_w - tilg_w + aufn_w + da_w - dz_w - dt_w + ust_w + kst_w + gst_w + ent_w
  # INVARIANTE: Rueckabstimmung Woche = Monat — die Summe des woechentlichen NettoCashflows entspricht der
  # Summe der monatlichen Komponenten. Verliert oder erfindet die Kalendarisierung (spread_even/lump_day,
  # inkl. Personal-, Steuer- und Darlehens-Lumps) Masse, feuert dieser Abgleich.
  netcf_w_soll <- sum(m_in) + sum(ow$forderungen) - sum(m_out) - sum(m_pers) - sum(ow$verbindlichkeiten) -
    sum(mdl$fin$capex) + sum(mdl$fin$foerder) - sum(mdl$fin$tilg) + sum(mdl$fin$aufn) +
    sum(dl$auszahlung) - sum(dl$zins) - sum(dl$tilgung) +
    (if (active && ust_pf) sum(ust_cash) else 0) +
    (if (active) -sum(et$kst_vz) - sum(et$gewst_vz) - sum(et$entnahme) else 0)
  w_dev <- abs(sum(netcf_w) - netcf_w_soll)
  if (w_dev > max(1e-6 * abs(netcf_w_soll), 1e-3))
    stop(sprintf("run_model_weekly: INVARIANTE VERLETZT — Rueckabstimmung: Summe Wochen-NettoCashflow ist %.2f, Summe Monats-Komponenten soll %.2f (Abweichung %.6g) — Kalendarisierung verliert oder erfindet Masse.",
                 sum(netcf_w), netcf_w_soll, w_dev))

  # INVARIANTE: KK-Zins-Vertragsformel Woche — kkz[w] = max(0, -Vorwochensaldo) * kk_zins * Tage/365
  # (wochengenau); Endbestand-Rekursion end[w] = Vorwochensaldo + netcf_w[w] - kkz[w]. Teleskop-Abgleich unten.
  end <- numeric(n_weeks); kkz <- numeric(n_weeks); prev <- mdl$open_cash
  for (w in 1:n_weeks) { kkz[w] <- max(0,-prev)*mdl$kk_zins*wk_days[w]/365; end[w] <- prev + netcf_w[w] - kkz[w]; prev <- end[w] }
  # INVARIANTE: KK-Zins nie negativ; letzter Wochen-Endbestand erfuellt die Teleskopsumme
  # Eroeffnung + Summe(netcf_w) - Summe(kkz).
  if (any(kkz < 0)) { wb <- which(kkz < 0)[1]
    stop(sprintf("run_model_weekly: INVARIANTE VERLETZT — KK_Zins in Woche %d ist %s, soll >= 0.", wb, format(kkz[wb]))) }
  endw_soll <- mdl$open_cash + sum(netcf_w) - sum(kkz)
  if (abs(end[n_weeks] - endw_soll) > 1e-6 * max(abs(endw_soll), 1))
    stop(sprintf("run_model_weekly: INVARIANTE VERLETZT — Endbestand-Teleskop: Endbestand[W%d] ist %.2f, soll %.2f (Abweichung %.6g).",
                 n_weeks, end[n_weeks], endw_soll, abs(end[n_weeks] - endw_soll)))

  rows <- rbind(Einzahlungen=ein_w, Auszahlungen=aus_w, Investitionen=capex_w, Foerdermittel=foerder_w, Tilgung=tilg_w, Darlehensaufnahme=aufn_w)
  if (!is.null(mdl$opos) && (ow$fo_active || ow$ve_active)) rows <- rbind(rows, OPOS_Forderungen=of_w, OPOS_Verbindlichkeiten=ov_w)
  if (dl_active) rows <- rbind(rows, Darlehen_Auszahlung=da_w, Darlehen_Zins=-dz_w, Darlehen_Tilgung=-dt_w)
  if (active) rows <- rbind(rows, USt_Zahllast=ust_w, KSt_Voraus=kst_w, GewSt_Voraus=gst_w, Steuerentnahme=ent_w)
  cash <- rbind(rows, NettoCashflow=netcf_w, KK_Zins=kkz, Endbestand=end,
                KK_Inanspruchnahme=pmax(0,-end), Freie_Kreditlinie=mdl$kk_limit-pmax(0,-end)+pmax(0,end))
  colnames(cash) <- paste0("W", 1:n_weeks)
  kpis <- list(trough=min(end), trough_week=which.min(end), maxKK=max(pmax(0,-end)),
               weeks_below=sum(end < mdl$mindestreserve), end=end[n_weeks], netcf_total=sum(netcf_w))
  list(cash=cash, kpis=kpis, n_weeks=n_weeks, wk_start=wk_start, wk_end=wk_end)
}

validate_history <- function(mdl) {
  a0 <- mdl$assume; for (k in names(a0)) a0[[k]] <- 0
  acc0 <- project(mdl$base, mdl$pos, a0, 12)$acc
  erg_eng <- sum(build_guv(acc0, mdl$pos, 12)["Ergebnis", ])
  inflow_g  <- c("umsatz","bestand","betr_ertrag","sopo","zins_e","neutral_e")
  outflow_g <- c("material","personal","sachkosten","afa","zins_a","neutral_a","steuer")
  erg_naiv <- sum(acc0[mdl$pos$grp %in% inflow_g,,drop=FALSE]) - sum(acc0[mdl$pos$grp %in% outflow_g,,drop=FALSE])
  # --- Cash-Massenerhaltung je Gruppe (F-11) ---
  # Nullannahmen => g=1 => stationaeres Basisjahr; ueber genau 12 Monate ist die Lag-Randmasse
  # (Carry-in vs. ueber den Horizont verlorener Tail) exakt null -> tighte Toleranz moeglich.
  # H=12 statt 36, weil quarterly und annual_jan einmalige Profile sind (wiederholen ueber
  # Mehrjahres-Horizonte bewusst nicht; bei H=36 erzeugten sie Schein-Verletzungen).
  # noncash-Positionen liefern per Deklaration keinen Cash -> aus dem Zielwert ausgeklammert,
  # zusaetzlich separat geprueft (Cash der noncash-Positionen muss exakt 0 sein).
  grps <- unique(mdl$pos$grp); is_nc <- mdl$pos$cashprofile == "noncash"
  cash_grp <- vapply(grps, function(grp) {
    idx <- which(mdl$pos$grp == grp); s <- numeric(12)
    for (i in idx) s <- s + apply_profile(acc0[i,], mdl$base[i,], mdl$pos$cashprofile[i], 1, 12, mdl$profiles)
    sum(s) }, numeric(1))
  cash_nc <- vapply(grps, function(grp) {
    idx <- which(mdl$pos$grp == grp & is_nc); if (!length(idx)) return(0)
    s <- numeric(12)
    for (i in idx) s <- s + apply_profile(acc0[i,], mdl$base[i,], mdl$pos$cashprofile[i], 1, 12, mdl$profiles)
    sum(s) }, numeric(1))
  acc_grp <- vapply(grps, function(grp) sum(acc0[mdl$pos$grp == grp & !is_nc, , drop = FALSE]), numeric(1))
  mass_res <- abs(cash_grp - acc_grp) / pmax(1, abs(acc_grp))
  mass_ok  <- all(mass_res < 0.02) && all(abs(cash_nc) < 1)
  list(erg_eng=erg_eng, erg_naiv=erg_naiv, ok=abs(erg_eng-erg_naiv) < 1,
       mass_ok=mass_ok, mass_res=setNames(mass_res, grps))
}

fmt <- function(x) format(round(x), big.mark=".", decimal.mark=",", scientific=FALSE)

# Implizite Zahlungslaufzeit (Tage) je Cashprofil -- fuer die DSO/DPO-Ausweisung (F-11).
# Ein Bucket k zahlt im k-ten Monat ab Leistung; Mitte des Monats angesetzt (mid = halber Monat).
profile_days <- function(profile, profiles) {
  mid <- 15.2
  if (profile %in% c("noncash","annual_jan")) return(NA_real_)   # kein Cash bzw. Einmalzahlung
  if (profile %in% c("inmonth","abschlag"))    return(mid)
  if (profile == "quarterly")                  return(30.44 + mid)
  pr <- profiles[[profile]]; if (is.null(pr) || !sum(pr)) return(NA_real_)
  sum(pr * ((seq_along(pr)-1)*30.44 + mid)) / sum(pr)
}
# DSO (Forderungen) bzw. DPO (Verbindlichkeiten) je Gruppe, mit der Basisjahres-Groesse gewichtet.
implied_dpo_dso <- function(mdl, grps) {
  base_sz <- rowSums(abs(mdl$base))
  vapply(grps, function(grp) {
    idx <- which(mdl$pos$grp == grp)
    d <- vapply(idx, function(i) profile_days(mdl$pos$cashprofile[i], mdl$profiles), numeric(1))
    w <- base_sz[idx]; keep <- !is.na(d) & w > 0
    if (!any(keep)) NA_real_ else sum(d[keep]*w[keep])/sum(w[keep])
  }, numeric(1))
}
