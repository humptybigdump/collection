library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

dorf_datensatz <- read_csv("dorf_befragungsdaten_long_reduziert.csv")
view(dorf_datensatz)

dorf_datensatz_knoten <- dorf_datensatz %>% select(name, sex, ageclass, villpart, state) %>% 
  distinct()

dorf_datensatz_kanten <- dorf_datensatz %>% filter(who %in% dorf_datensatz$name) %>% 
  select(name, who, privat, bse, informe, events) %>% 
  rename(from = name, to = who)

dorf_netzwerk <- as_tbl_graph(graph_from_data_frame(
  dorf_datensatz_kanten,
  vertices = dorf_datensatz_knoten))  

dorf_netzwerk <- dorf_netzwerk %>% activate(nodes) %>% 
  mutate(knotengrad = centrality_degree())

dorf_netzwerk <- dorf_netzwerk %>% activate(nodes) %>% 
  mutate(komponente = group_components())

layout_dorf_netzwerk <- create_layout(filter(dorf_netzwerk, komponente == 1),
                                      layout = "igraph", algorithm = "nicely")

ggraph(layout_dorf_netzwerk) +
  geom_node_point(aes(color = as.factor(villpart),
                      size = knotengrad)) +
  geom_edge_link() +
  scale_color_brewer(name = "Ortsteil", type = "qual", palette = "Paired",
                     labels = c("Colnrade", "Beckstedt", "Holtorf")) +
  scale_size_continuous(name = "Out-Degree")
