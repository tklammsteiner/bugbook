library(tidyverse)
library(readxl)
library(reactable)
library(leaflet)
library(htmltools)
library(patchwork)

contributions <- read_excel("bugbook_data.xlsx", sheet = "contributions")
chapters <- read_excel("bugbook_data.xlsx", sheet = "chapters")

contributions %>% 
  distinct(Author) %>% 
  nrow()

contributions %>% 
  distinct(Country) %>% 
  nrow()

institution_freq <- contributions %>% 
  group_by(Institution) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

a <- ggplot(institution_freq %>% 
         filter(!is.na(Institution)) %>% 
         slice_max(n = 10, order_by = count), 
       aes(x = reorder(Institution, count), y = count)) +
  geom_bar(fill = "#2C5F5D", stat = 'identity', position = 'dodge') +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0, 21, 3), limits = c(0, 22)) +
  labs(x = NULL, y = NULL) +
  ggpubr::theme_pubclean() +
  theme(axis.text = element_text(size = 10)) +
  coord_flip()

authors_per_chapter <- contributions %>%
  group_by(Chapter) %>%
  summarise(UniqueAuthors = n_distinct(Author)) %>% 
  left_join(chapters %>% select(Chapter, First_Author)) %>% 
  mutate(Chapter = as.factor(Chapter),
         UniqueAuthors = as.numeric(UniqueAuthors),
         ID = paste0("(", Chapter, ") ", First_Author))
  

# Authors per chapter
b <- ggplot(authors_per_chapter, aes(x = reorder(ID, UniqueAuthors), y = UniqueAuthors)) +
  geom_bar(stat = "identity", fill = "#2C5F5D") +
  scale_y_continuous(breaks = seq(0, 22, 3)) +
  theme_minimal() +
  labs(x = NULL, 
       y = "Number of authors")  +
  ggpubr::theme_pubclean() +
  theme(axis.text = element_text(size = 10)) +
  coord_flip()


(a + theme(axis.text.x = element_blank(),
           axis.ticks.x = element_blank())) / b + 
  plot_layout(heights = c(0.45, 0.55)) +
  plot_annotation(tag_levels = c("A"))

# Top contributing authors
chapters_per_author <- contributions %>%
  group_by(Author) %>%
  summarise(ChaptersContributed = n_distinct(Chapter))

top_authors <- chapters_per_author %>%
  arrange(desc(ChaptersContributed)) %>%
  head(20)

ggplot(top_authors, aes(x = reorder(Author, ChaptersContributed), y = ChaptersContributed)) +
  geom_bar(stat = "identity", fill = "#2C5F5D") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Authors by Number of Chapters Contributed", x = "Author", y = "Chapters Contributed")

authors_by_country <- contributions %>%
  distinct(Author, Country) %>%
  count(Country)

# Authors by country
ggplot(authors_by_country, aes(x = "", y = n, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Authors by Country")



author_summary <- contributions %>%
  group_by(Author, Institution, City, Country, Lat, Long) %>%
  summarise(
    Chapters = paste(sort(unique(Chapter)), collapse = ", "),
    .groups = "drop"
  )

library(dplyr)
library(igraph)
library(visNetwork)

edges <- authors %>% 
  filter(Chapter != 5) %>% 
  dplyr::group_by(Chapter) %>%
  summarise(Authors = list(unique(Author)), .groups = "drop") %>%
  pull(Authors) %>%
  lapply(function(x) if(length(x) >= 2) t(combn(x, 2)) else NULL) %>%
  Filter(Negate(is.null), .) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  setNames(c("from", "to"))

# Create graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Create nodes/edges for visNetwork
nodes <- data.frame(
  id = V(g)$name, 
  label = V(g)$name,  # Show labels by default
  font.size = 20,      # Larger text
  color = "#2C5F5D"    # Node color
)

edges_vis <- as_data_frame(g, what = "edges")

# Create interactive plot with stabilization
visNetwork(nodes, edges_vis) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1),
    nodesIdSelection = TRUE
  ) %>%
  visPhysics(  # Stabilization settings
    stabilization = list(
      enabled = TRUE,
      iterations = 1000  # More iterations = more stable
    ),
    solver = "barnesHut",  # Better for larger networks
    barnesHut = list(
      gravitationalConstant = -2000,  # Adjust for node spacing
      springLength = 150
    )
  ) %>%
  visLayout(randomSeed = 123)  # Consistent initial layout
