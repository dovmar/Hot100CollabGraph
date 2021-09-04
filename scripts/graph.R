library(tidygraph)
library(ggraph)
library(tidyverse)
library(gtools)
library(hrbrthemes)
library(lubridate)
library(igraph)
library(RColorBrewer)
library(here)

# Reading-in and cleaning data

x <- read_csv(here("data/input/billboardHot100_1999-2019.csv"))


y <- x %>%
  distinct(Artists, Name, .keep_all = TRUE) %>%
  filter(year(Week) > 2010) %>%
  filter(Artists != "Jack, Jack") %>%
  mutate(
    Artists = str_replace(Artists, "Tyler, The Creator", "Tyler The Creator"),
    Genre = str_replace(Genre, "Hip-Hop", "Rap"),
    Genre = str_replace(Genre, "R&;B", "R&B"),
    list_of_artists = str_split(Artists, ", "),
    list_of_genres = str_split(Genre, ",")
  ) %>%
  replace_na(list(Peak.position = -1)) %>%
  mutate(
    Peak.position = if_else(Peak.position == -1, Weekly.rank, Peak.position),
    weight = 100 - Peak.position
  )

write_csv(y,here("data/output/billboard_clean.csv"))

# Assigning a genre to an artist based on the genre of the songs they appear on
artists <- distinct(as_tibble(unlist(y$list_of_artists)))

genres <- unnest(y, col = list_of_genres) %>%
  count(list_of_genres) %>%
  arrange(desc(n))
genres <- genres$list_of_genres

artist_genre <- unnest_longer(y, col = list_of_genres) %>%
  unnest_longer(col = list_of_artists) %>%
  group_by(list_of_artists, list_of_genres) %>%
  count() %>%
  ungroup() %>%
  group_by(list_of_artists) %>%
  slice_max(n, n = 1) %>%
  arrange(factor(list_of_genres, levels = genres)) %>%
  slice_head(n = 1)

artist_genre$list_of_genres <- fct_lump_n(artist_genre$list_of_genres, 6)
artists <- left_join(artists, artist_genre, c("value" = "list_of_artists"))
artists <- artists[1:2]

# Constructing the graph
y <- y %>% filter(lengths(list_of_artists) > 1)
weights <- map2(y$weight, y$list_of_artists, ~ rep(.x, choose(length(.y), 2)))
name <- map2(y$Name, y$list_of_artists, ~ rep(.x, choose(length(.y), 2)))
genre <- map2(y$Genre, y$list_of_artists, ~ rep(.x, choose(length(.y), 2)))
edges <- map(y$list_of_artists, ~ combinations(length(.x), 2, .x))
edges <- reduce(edges, rbind)
edges <- as_tibble(edges)
edges$weights <- reduce(weights, c)
edges$Name <- reduce(name, c)
edges$Genre <- reduce(genre,c)

graph <- tbl_graph(nodes = artists, 
                   edges = edges, 
                   node_key = c("V1", "V2"), 
                   directed = F) 
graph <- discard(to_components(graph), ~ gorder(.x) < 5) %>%
  bind_graphs() %>% 
  activate(nodes) %>%
  mutate(
    degr = centrality_degree(),
    degr_weighted = centrality_degree(weights = weights)
  )

# Saving the graph 
saveRDS(graph,file=here("data/output/graph.RDS"))


# Graph analysis
as_tibble(graph) %>% slice_max(degr,n=5)
as_tibble(graph) %>% slice_max(degr_weighted, n=5)
