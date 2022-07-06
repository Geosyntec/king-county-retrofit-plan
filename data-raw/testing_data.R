source("~/kingCoDecision/R/fct_helpers.R")
source("~/kingCoDecision/R/promethee.R")


library(readxl)
example_mcda_exercise <- read_excel("data-raw/example_mcda_exercise.xlsx",sheet = "Sheet3", skip = 2, n_max = 6)
usethis::use_data(example_mcda_exercise,overwrite = TRUE)


load("~/kingCoDecision/data/test_data.rda")




pf_test <- promethee_2(test_data %>% dplyr::select(c(6:12)) %>% sample_n(15))
adj_mat_numeric <- pf_test[[2]]

g <-
  igraph::graph_from_adjacency_matrix(
    adj_mat_numeric,
    diag = FALSE,
    mode = "directed"
  )


  pf2<- pf_test[[1]] %>% rownames_to_column('id')

  name_col <- 'id'

basins <- pf2[name_col]
g.vis <- visNetwork::visIgraph(g)
nodes <- g.vis$x$nodes
edges <- g.vis$x$edges

from_rank <- left_join(edges, pf2, by = c("from" = name_col))
to_rank <- left_join(edges, pf2, by = c("to" = name_col))

ranked_edges <- data.frame(edges,
                           from_rank = from_rank$rank,
                           to_rank = to_rank$rank
)

cleaned_edges <- ranked_edges %>%
  group_by(from) %>%
  slice(which.min(to_rank))


lone_nodes_from <- basins[which(!basins %in% cleaned_edges$from)]

lone_nodes_to <- ranked_edges %>%
  filter(!to %in% cleaned_edges$to) %>%
  group_by(to) %>%
  filter(from_rank == max(from_rank))

add_in_nodes <- ranked_edges %>%
  add_row(lone_nodes_to)

cleaned_edges <- cleaned_edges %>% rbind(lone_nodes_to)

coords <- data.frame(
  x = pf2$phi_plus,
  y = pf2$phi_minus
) %>% as.matrix()

# Rotate 45 degrees
phi <- pi / 4

rotation_matrix <- cbind(
  c(cos(phi), sin(phi)),
  c(-sin(phi), cos(phi))
)

rotated_coords <- coords %*% rotation_matrix

nodes <- data.frame(id = basins,
                    label = basins,
                    rotated_coords,


                    title = paste0(
                      "phi-: ",



                      pf2$phi_minus,"<br>",
                      "phi+: ",
                      pf2$phi_plus, "<br>",
                      "phi: ", pf2$score
                    ))

usethis::use_data(pf_test,overwrite = TRUE)
usethis::use_data(cleaned_edges,overwrite = TRUE)
usethis::use_data(nodes,overwrite = TRUE)
usethis::use_data(rotated_coords,overwrite = TRUE)

plot_pref_flows(pf2 = pf_test[[1]],adj_mat_numeric = pf_test[[2]])
usethis::use_data(pf_test,overwrite = TRUE)


library(readr)
cars_example <- read_csv("data-raw/cars_example.csv") %>% column_to_rownames("Name")
usethis::use_data(cars_example,overwrite = TRUE)


library(readxl)
example_mcda_exercise <- read_excel("data-raw/example_mcda_exercise.xlsx",sheet = "Sheet3", skip = 2, n_max = 6)
usethis::use_data(example_mcda_exercise,overwrite = TRUE)
