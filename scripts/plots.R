# Function to plot the graph
plot_graph <- function(graph, plot_subtitle) {
  ggraph(graph) +
    geom_edge_link0(aes(width = 100 - weights),
                    edge_colour = "grey66", 
                    show.legend = FALSE) +
    geom_node_point(aes(size = degr, 
                        color = list_of_genres)) +
    geom_node_text(aes(filter = (degr > 3), 
                       label = value),
                   family = "Roboto Condensed", repel = TRUE, size = 8.5
    ) +
    theme_graph(base_size = 30,
                title_size = 40, 
                subtitle_size = 36) +
    scale_size(range = c(10, 20)) +
    scale_edge_width_continuous(range = c(0.1, 2)) +
    scale_color_brewer(
      "Genre",
      palette = "Set2",
      limits = unique(artists$list_of_genres),
      guide = guide_legend(
        override.aes = list(size = 8),
        nrow = 1, title.position = "top"
      )
    ) +
    guides(size = "none") +
    theme(legend.position = "bottom") +
    labs(title = "Artist collaborations", 
         subtitle = plot_subtitle)
}


plot_graph(graph,"In songs of all genres")



# Filtering out only a single genre
subset_genre <- function(graph, genre_filter) {
  graph <- graph %>%
    activate(edges) %>%
    filter(str_detect(Genre, genre_filter))
  
  graph <- discard(to_components(graph), ~ gorder(.x) < 5) %>%
    bind_graphs()
  
  graph
}

# Pop songs
plot_graph(subset_genre(graph,"Pop"),"In pop songs only") 


# Rap songs
plot_graph(subset_genre(graph,"Hip-Hop|Rap|Trap"),"In rap songs only")


# Calculate the clusters
graph <- graph %>%
  mutate(clust = as_factor(tidygraph::group_louvain(weights = weights))) %>%
  morph(to_split, clust) %>%
  activate(nodes) %>%
  mutate(main = ifelse(centrality_degree() > max(centrality_degree())-1, value, "")) %>%
  unmorph()


myColors <- c(brewer.pal(13, "Paired"), "black")
names(myColors) <- levels(as_tibble(graph)$clust)


ggraph(graph, layout = "kk") +
  geom_edge_link0(aes(width = 100 - weights), edge_colour = "grey66") +
  geom_node_point(aes(size = degr, color = clust)) +
  geom_node_text(aes(label = main), family = "Roboto Condensed", repel = FALSE, size = 8.5) +
  theme_graph(base_size = 30, title_size = 40, subtitle_size = 36) +
  scale_size(range = c(10, 20)) +
  scale_edge_width_continuous(range = c(0.1, 1.5)) +
  scale_color_manual(values = myColors) +
  theme(legend.position = "none") +
  labs(title = "Artist collaboration", subtitle = "Billboard Hot 100 songs")


# Focus on specific clusters
plot_clusters <- function(graph, xlimit, ylimit, groups) {
  ggraph(graph, layout = "kk") +
    geom_edge_link0(aes(width = 100 - weights), edge_colour = "grey66") +
    geom_node_text(aes(filter = as.numeric(clust) %in% groups, label = value),
                   family = "Roboto Condensed", repel = TRUE, size = 8.5) +
    geom_node_point(aes(filter = as.numeric(clust) %in% groups, size = degr, color = clust)) +
    geom_node_point(aes(filter=!(as.numeric(clust) %in% groups),size=degr),
                    alpha=0.5,color="grey") +
    theme_graph(base_size = 30, title_size = 40, subtitle_size = 36) +
    scale_size(range = c(10, 20)) +
    scale_edge_width_continuous(range = c(0.1, 1.5)) +
    scale_color_manual(values = myColors) +
    theme(legend.position = "none") +
    labs(title = "Artist collaboration", subtitle = "Billboard Hot 100 songs") +
    xlim(xlimit) +
    ylim(ylimit)
}


plot_clusters(graph,c(-4,3),c(0,8),c(1:13))

plot_clusters(graph,c(0,6),c(-4,3),c(1:13))

plot_clusters(graph,c(-3,2),c(-5,0.5),c(1:13))
