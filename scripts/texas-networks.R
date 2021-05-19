# install.packages("tidygraph")
# install.packages("ggraph")

library(tidyverse)
library(tidygraph)
library(ggraph)

letters <- read_csv("texas-correspondence-OpenRefine.csv")

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

destinations <- letters %>%
  distinct(target) %>%
  rename(label = target)

texas_nodes <- full_join(sources, destinations, by = "label")
texas_nodes

texas_nodes <- texas_nodes %>% rowid_to_column("id")

texas_nodes

interactions <- letters %>%  
  group_by(source, target) %>%
  summarise(weight = n()) %>% 
  ungroup()

texas_edges <- interactions %>% 
  left_join(texas_nodes, by = c("source" = "label")) %>% 
  rename(from = id)

texas_edges <- texas_edges %>% 
  left_join(texas_nodes, by = c("target" = "label")) %>% 
  rename(to = id)

texas_edges <- select(texas_edges, from, to, weight)

texas_edges
texas <- tbl_graph(nodes = texas_nodes, edges = texas_edges, directed = TRUE)

ggraph(texas)+ 
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

routes_tidy <- tbl_graph(nodes = texas_nodes, edges = texas_edges, directed = TRUE)

filtered_texas <- routes_tidy %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>%
  filter(degree > 1) %>%
  activate(edges) %>%
  filter(weight > 1) %>%
  activate(nodes) %>%
  filter(!node_is_isolated()) 

ggraph(filtered_texas) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()

texas %>% 
  mutate(centrality = centrality_authority()) %>%
  ggraph(layout = 'graphopt') + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()


pageranks <- as_tibble(texas %>% 
                         activate(nodes) %>% 
                         mutate(pgrnk = centrality_pagerank()))

# who has the best pagerank? what does that mean historically?
View(pageranks)

# which correspondence pair is most between?
bwness <- as_tibble(texas %>% 
                      activate(edges) %>% 
                      mutate(bw = centrality_edge_betweenness()))
# what might that mean historically?
