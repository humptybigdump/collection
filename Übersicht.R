# Variable zuordnen
Name <- 4

# Pakete laden
library(dplyr)
library(sjmisc)
library(ggplot2)
library(plotrix)
library(car)
library(DescTools)
library(correlation)
library(QuantPsyc)

# Rechnungen
4 + 4
4 - 4
4 * 4
4 / 4
4 ^ 4
7 %% 3

# class-Befehl
class(Name)

# Vektor
Vektor <- c(4, 7, 10)
names(Vektor) <- c("Piet", "Freddy", "Antje")
Vektor[1:2]
sum(Vektor)
mean(Vektor)

# Faktor
Faktor <- factor(Vektor, ordered = FALSE, levels = c(4,7,10))
summary(Faktor)
Faktor[1:2]

# Datensatz
data("trees")
head(trees)
tail(trees)
str(trees)
trees[1,3]
trees$Height
subset(trees, subset = Volume > 50)
Vektor2 <- c(5, 18, 17)
Vektor3 <- c(23, 55, -10)
Name <- c("Piet", "Freddy", "Antje")
Datensatz <- data.frame(Name, Vektor, Vektor2, Vektor3)
View(Datensatz)
Datensatz[Datensatz == -10] <- NA
anyNA(trees)
table(is.na(Datensatz))
Datensatz <- na.omit(Datensatz)
Datensatz
Datensatz %>%
  mutate(Vektor2 = Vektor2/100)
Datensatz %>%
  mutate(Vektor_standardisiert = scale(Vektor))
Datensatz %>%
  select(Vektor, Vektor3)
Datensatz %>%
  filter(Vektor3 > 40)
Datensatz %>%
  arrange(Vektor2)
Datensatz %>%
  arrange(desc(Vektor2))

# Deskriptive Statistik
data("starwars")
starwars %>%
  frq(homeworld, sort.frq = "desc")
median(trees$Height)
mean(trees$Height)
range(trees$Height)
range(trees$Height) %>% diff
min(trees$Height)
max(trees$Height)
quantile(trees$Height, probs = c(0, 0.25, 0.5, 0.75, 1))
IQR(trees$Height)
var(trees$Height)
sd(trees$Height)
scale(trees$Height)

# Grafische Darstellungen
starwars %>%
  ggplot(aes(x = gender)) +
  geom_bar()
starwars %>%
  ggplot(aes(x = birth_year)) +
  geom_histogram()
storms
storms %>%
  filter(name == "Amy") %>%
  ggplot(aes(x = day, y = wind)) +
  geom_point() +
  geom_line()
starwars %>%
  ggplot(aes(y = height)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot()
starwars %>%
  ggplot(aes(x = height, y = mass)) +
  geom_point()

# Inferenzstatistik
std.error(starwars$height)
trees %>%
  summarize(mean = mean(Height), sd = sd(Height), std.error = std.error(Height))
trees %>%
  summarize(mean = mean(Height), sd = sd(Height), std.error = std.error(Height), UG = mean-1.96*std.error, OG = mean+1.96*std.error)
shapiro.test(trees$Height)
ks.test(trees$Height, "pnorm")
leveneTest(trees$Height, trees$Volume)
t.test(trees$Height, mu = 70, alternative = "two.sided")
starwars_na <- na.omit(starwars)
t.test(starwars_na$height ~ starwars_na$gender, alternative = "two.sided")
Amy <- storms %>%
  filter(name == "Amy")
t.test(Amy$wind, Amy$pressure, alternative = "two.sided", paired = TRUE)
anova <- aov(height ~ sex, data = starwars_na)
summary(anova)
oneway.test(height ~ sex, data = starwars_na)
pairwise.t.test(starwars_na$height, starwars_na$sex, p.adjust.method = "bonferroni")

# Bivariate Analyse
Haarfarbe_x_Geschlecht <- table(starwars$hair_color, starwars$gender)
Haarfarbe_x_Geschlecht
margin.table(Haarfarbe_x_Geschlecht, 2)
margin.table(Haarfarbe_x_Geschlecht, 1)
prop.table(Haarfarbe_x_Geschlecht)
prop.table(Haarfarbe_x_Geschlecht, 2)
prop.table(Haarfarbe_x_Geschlecht, 1)
chisq.test(starwars$hair_color, starwars$gender, correct = FALSE)
fisher.test(starwars$hair_color, starwars$gender)
CramerV(x = starwars_na$hair_color, y = starwars_na$gender)
cov(starwars_na$height, starwars_na$mass)
cor(starwars_na$height, starwars_na$mass, method = "pearson")
cor.test(starwars_na$height, starwars_na$mass, method = "pearson")
cor(starwars_na$height, starwars_na$mass, method = "kendall")
cor.test(starwars_na$height, starwars_na$mass, method = "kendall")
cor(starwars_na$height, starwars_na$mass, method = "spearman")
cor.test(starwars_na$height, starwars_na$mass, method = "spearman")
starwars_na_klein <- starwars_na %>%
  select(mass, height)
cor(starwars_na_klein, method = "pearson")
summary(correlation(starwars_na_klein))

# Lineare Regression
Regression <- lm(mass ~ height, data = starwars_na)
Regression
summary(Regression)
Regression2 <- lm(mass ~ height + birth_year, data = starwars_na)
Regression2
avPlots(Regression2)
lm.beta(Regression2)
starwars_na <- starwars_na %>%
  mutate(mass_standard = scale(mass), height_standard = scale(height), birth_year_standard = scale(birth_year))
Regression3 <- lm(mass_standard ~ height_standard + birth_year_standard, data = starwars_na)
Regression3
anova(Regression, Regression2)
confint(Regression2)
residuals(Regression2)
scale(residuals(Regression2))
hatvalues(Regression2)
vif(Regression2)
durbinWatsonTest(Regression2)