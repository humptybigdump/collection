library(tidyverse)

biorxiv_data <- read_csv("biorxiv_small.csv")

biorxiv_data <- biorxiv_data %>% mutate(published = !((published == "NA")|is.na(published)))

biorxiv_data <- biorxiv_data %>% filter(category %in% c("neuroscience", "microbiology",
                                                        "bioinformatics", "genomics",
                                                        "evolutionary biology"))

ggplot(data = biorxiv_data, aes(category)) +
  geom_bar()

ggplot(data = biorxiv_data, aes(category)) +
  geom_bar() +
  labs(title = "Themenverteilung in bioRxiv",
       subtitle = "Fünf häufigste Kategorien",
       x = "Themenkategorie", y = "Anzahl Preprints")

ggplot(data = biorxiv_data, aes(category)) +
  geom_bar() +
  labs(title = "Themenverteilung in bioRxiv",
       subtitle = "Fünf häufigste Kategorien",
       x = "Themenkategorie", y = "Anzahl Preprints") +
  scale_x_discrete(labels = c("Bioinformatik", "Evolutionsbiologie",
                              "Genforschung", "Mikrobiologie",
                              "Neurowissenschaften"))

ggplot(data = biorxiv_data, aes(category)) +
  geom_bar(aes(fill = published), position = "dodge") +
  labs(title = "Themenverteilung in bioRxiv",
       subtitle = "Fünf häufigste Kategorien",
       x = "Themenkategorie", y = "Anzahl Preprints") +
  scale_x_discrete(labels = c("Bioinformatik", "Evolutionsbiologie",
                              "Genforschung", "Mikrobiologie",
                              "Neurowissenschaften")) +
  scale_fill_discrete(name = "Status des Preprints",
                      labels = c("Unveröffentlicht", "Veröffentlicht"))
