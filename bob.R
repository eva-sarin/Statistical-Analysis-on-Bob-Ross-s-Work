getwd()
setwd("E:/bob2")
library(readr)
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")
bob_ross
library(dplyr)
library(tidyverse)
library(janitor)
bob_ross_gathered <- bob_ross %>%
  janitor::clean_names() %>%
  gather(element, present, -episode, -title) %>%
  filter(present == 1) %>%
  mutate(title = str_to_title(str_remove_all(title, '"')),
         element = str_to_title(str_replace(element, "_", " "))) %>%
  select(-present) %>%
  extract(episode, c("season", "episode_number"), "S(.*)E(.*)", convert = TRUE, remove = FALSE) %>%
  arrange(season, episode_number)

############################################## exploring

bob_ross_gathered %>%
  count(element, sort = TRUE) %>%
  head(25) %>%
  mutate(element = fct_reorder(element, n)) %>%
  ggplot(aes(element, n)) +
  geom_col() +
  coord_flip()
################################################
##finding the most crowded paintings (with most elements with them)
bob_ross_gathered %>%
  add_count(episode) %>%
  arrange(desc(n))
###########################################
##how the paintings changed overtime
by_season_element <- bob_ross_gathered %>%
  filter(!element %in% c("Tree", "Trees")) %>%
  group_by(season) %>%
  mutate(number_episodes = n_distinct(episode)) %>%
  count(season, element, number_episodes, sort = TRUE) %>%
  mutate(percent_included = n / number_episodes) %>%
  group_by(element) %>%
  mutate(element_total = sum(n)) %>%
  ungroup()
by_season_element %>%
  filter(element_total >= 50) %>%
  ggplot(aes(season, percent_included, color = element)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  expand_limits(y = 0) +
  facet_wrap(~ element)
##############################################
##clustering
#(what things tend to appear together)

library(widyr)
correlations <- bob_ross_gathered %>%
  add_count(element) %>%
  filter(n >= 5) %>%
  pairwise_cor(element, episode, sort = TRUE)
correlations %>%
  filter(item1 == "River") %>%
  mutate(item2 = fct_reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_col() +
  coord_flip() +
  labs(title = "What tends to appear with a river?",
       subtitle = "Among elements that appeared in at least 10 paintings")
correlations %>%
  filter(item1 == "Snow") %>%
  mutate(item2 = fct_reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_col() +
  coord_flip() +
  labs(title = "What tends to appear with snow?",
       subtitle = "Among elements that appeared in at least 10 paintings")
#############################################################3
library(ggraph)
library(igraph)
set.seed(353)
correlations %>%
  head(100) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
##################################################
## Principal Component Analysis
#(finding what dimensions drive a lot of the variation among paintings)
library(reshape2)
library(broom)
library(tidytext)
binary_matrix <- bob_ross_gathered %>%
  acast(title ~ element)
##centering the columns
centered_matrix <- t(t(binary_matrix) - colMeans(binary_matrix))
svd_result <- svd(centered_matrix)
element_weights <- tidy(svd_result, matrix = "v") %>%
  mutate(element = colnames(binary_matrix)[column])
element_weights %>%
  filter(PC <= 4) %>%
  group_by(PC) %>%
  top_n(16, abs(value)) %>%
  ungroup() %>%
  mutate(element = reorder_within(element, value, PC)) %>%
  ggplot(aes(element, value, fill = factor(PC))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ PC, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "First four principal components of elements in Bob Ross paintings")
#Ans.
#1. Mountains/Conifer vs Ocean/Beach and deciduous trees
#2. Trees, especially deciduous, vs Ocean
#3. Spring/Summer vs Winter
#4. Lake vs River

painting_weights <- broom::tidy(svd_result, matrix = "u") %>%
  mutate(painting = rownames(binary_matrix)[row])
painting_weights %>%
  filter(PC == 1) %>%
  arrange((value))
bob_ross_gathered %>%
  filter(title == "Frozen Solitude")

painting_weights %>%
  filter(PC <= 4) %>%
  group_by(PC) %>%
  top_n(20, abs(value)) %>%
  ungroup() %>%
  mutate(painting = reorder_within(painting, value, PC)) %>%
  ggplot(aes(painting, value, fill = factor(PC))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ PC, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "First four principal components of Bob Ross paintings")

broom::tidy(svd_result, matrix = "d") %>%
  ggplot(aes(PC, percent)) +
  geom_point()
