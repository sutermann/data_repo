#' ---
#' title: "R-Kurs, Uebung 65: ANOVA"
#' author: "F. von Lampe (nach Dormann 2013)"
#' date: "November 2021"
#' output: pdf_document
#' ---

## Arbeitsverzeichnis festlegen und Daten einladen
korm <- read.table("kormoran.txt", stringsAsFactors = T)

## Ueberblick der Datensatzstruktur
str(korm)
# Datentypen stimmen. Unterart und Jahreszeit sind jeweils Faktoren


## Teil A -------
## Boxplot der Tauchzeit in Abhaengigkeit der Unterart erstellen
par(mfrow = c(1, 1))
boxplot(Tauchzeit ~ Unterart, data = korm)
# Unterart C scheint laenger zu tauchen als S.


## ANOVA der Tauchzeit in Abhaengigkeit der Unterart
aov.korm <- aov(Tauchzeit ~ Unterart, data = korm)
# F-Statistik
summary(aov.korm)
# t-Statistik
summary.lm(aov.korm)
# R²adj. = 0.08811
# Unterart sinensis taucht ca. 3.3 s kuerzer als carbo
# Aufteilen in zwei Gruppen traegt signifikant zur Aufklaerung unerklaerter
# Varianz bei (p = 0.03523)

## Modelldiagnose
par(mfrow = c(2, 2))
plot(aov.korm)
# Die Residuen sind noch ok. Punkt 20 hat einen grossen Einfluss auf die
# Varianzinhomogenitaet

## Test auf Varianzhomogenitaet
library(car)
leveneTest(Tauchzeit ~ Unterart, data = korm)
# P = 0.33: Varianzen sind homogen

## Welch-T-Test der Tauchzeit in Abhaengigkeit der Unterart
t.test(Tauchzeit ~ Unterart, data = korm)
# P-Wert fast identisch im Welch-T-Test




## Teil B ------
## Boxplot der Tauchzeit in Abhaengigkeit der Jahreszeit
par(mfrow = c(1, 1))
boxplot(Tauchzeit ~ Jahreszeit, data = korm)

## ANOVA der Tauchzeit in Abhaengigkeit der Jahreszeit
aov.korm2 <- aov(Tauchzeit ~ Jahreszeit, data = korm)
# F-Statistik
summary(aov.korm2)
# t-Statistik
summary.lm(aov.korm2)
# R²adj. = 0.7708
# Fruehjahr: 11.9 s, Sommer: 15.1 s, Herbst: 19.2 s; Winter: 23.4 s
# diese Werte sind die mittleren Tauchzeiten fuer beide Unterarten
# Aufteilen in vier Gruppen traegt signifikant zur Aufklaerung unerklaerter
# Varianz bei (p < 0.001)

# Modelldiagnose
par(mfrow = c(2, 2))
plot(aov.korm2)
# Die Residuen sind gut. Punkt 20 uebt wieder grossen Einfluss aus

## Post-hoc Test: Tukey HSD durchfuehren
TukeyHSD(aov.korm2)
# Alle Faktorlevel unterscheiden sich signifikant voneinander


# Reihenfolge der Faktorlevels
# Faktorlevel werden alphabetisch aufsteigend geordnet. Bei Jahreszeiten unsinnig
levels(korm$Jahreszeit)
korm$Jahreszeit <- factor(korm$Jahreszeit, levels = c("F", "S", "H", "W"))


## Boxplot erstellen und signifikante Unterschiede symbolisieren
par(mfrow = c(1, 1))
plot(
  Tauchzeit ~ Jahreszeit,
  data = korm,
  ylim = c(10, 35),
  ylab = "Tauchzeit (s)",
  names = c("Frühjahr", "Sommer",
            "Herbst", "Winter")
)
text(1, 34, "A")
text(2, 34, "B")
text(3, 34, "C")
text(4, 34, "D")

## Boxplot mit ggplot2 erstellen
## Paket ggplot2 laden
library(ggplot2)

# Boxplot der Tauchzeit in Abhaengigkeit der Unterart mit Gruppenlabels erstellen
u <- ggplot(korm, aes(x = Unterart, y = Tauchzeit)) + geom_boxplot()
u + annotate(geom = "text",
             x = 1,
             y = 32,
             label = "A") +
  annotate(geom = "text",
           x = 2,
           y = 32,
           label = "B")

# Boxplot der Tauchzeit in Abhaengigkeit der Jahreszeit mit Gruppenlabels erstellen
j <- ggplot(korm, aes(x = Jahreszeit, y = Tauchzeit)) + geom_boxplot()
j + annotate(geom = "text",
             x = 1,
             y = 32,
             label = "A") +
  annotate(geom = "text",
           x = 2,
           y = 32,
           label = "B") +
  annotate(geom = "text",
           x = 3,
           y = 32,
           label = "C") +
  annotate(geom = "text",
           x = 4,
           y = 32,
           label = "D")