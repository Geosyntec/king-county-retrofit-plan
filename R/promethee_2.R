library(dplyr)
library(MCDA)
library(igraph)
library(relations)


#function takes a dataframe
#returns a list containing a dataframe and a visnetwork htmlwidget


promethee_2 <-  function(dataset=NULL,
                         #,
                         name_col = "watershed.name",
                         weighting = NULL,
                         minmax = NULL,
                         IndT = NULL,
                         PreT = NULL,
                         PreF = NULL,
                         gaussP = NULL
)

{



  # dataset with criteria

  if(is.null(dataset)){
    dataset =
      psau_watersheds_data[50:58, ] %>%
      as.data.frame() %>%
      dplyr::select(c(
        watershed.name, clustered_trees,
        traffic)) %>% unique()
  }

  # qualitative parameters


  # get numeric data; convert add in rownames
  dataset <- dataset %>%
    remove_rownames() %>%
    column_to_rownames(name_col) %>%
    select_if(is.numeric)







  # dataset <- demo_eval_table
  # name_col <- "Evaluations"

  basins <- rownames(dataset)
  criteria <- colnames(dataset)

  n <- length(basins)
  c <- length(criteria)


  # Weighting ---------------------------------------------------------------
  if (is.null(weighting)) {
    weighting <- runif(c,min=1,max=10)
  }
  # scale the weighting values to sum to 1
  weighting <- scale(weighting,
                     center = FALSE,
                     scale = sum(weighting)) %>%
    as.vector()




  # Min max -----------------------------------------------------------------


  if (is.null(minmax)) {
    minmax <- rep("min", c)
  }


  # Thresholds, indifference ------------------------------------------------
  if (is.null(IndT)){
    IndT <- runif(c, 0, 0)
    }

  if (is.null(PreT)){
    PreT <- runif(c, 0, 0)
    }

  if (is.null(PreF)){
    PreF <- rep("Usual", c)
    }

  if (is.null(gaussP)){
    gaussP <- runif(c, 0, 0)
    }


  res <- PROMETHEEOutrankingFlows(
    performanceTable = dataset %>% # dplyr::select(-watershed.name) %>%
      as.matrix(),
    preferenceFunction = PreF, PreT, IndT, criteriaWeights = weighting,
    criteriaMinMax = minmax, gaussParameter = gaussP
  )
  out_flows <- data.frame(
    phi_plus = res[["outrankingFlowsPos"]],
    phi_minus = res[["outrankingFlowsNeg"]]
  ) %>% round(2)



  # get partial ranking


  adj_mat <- matrix(rep("U", n * n), n, n)

  for (i in 1:n) {#row
    for (j in 1:n) {#column
      a_pos <- out_flows[i, "phi_plus"]
      b_pos <- out_flows[j, "phi_plus"]
      a_neg <- out_flows[i, "phi_minus"]
      b_neg <- out_flows[j, "phi_minus"]

      # a is preferred to b when aP+b and aP-b,
      if (a_pos > b_pos && a_neg < b_neg) {
        adj_mat[i, j] <- "P1"
      # or aPb when  aI+b and aI-b
      } else if (a_pos == b_pos && a_neg  < b_neg) {#< b_neg) {
        adj_mat[i, j] <- "RP2"
        # or aPb when  aP+b and aI-b
      } else if (a_pos > b_pos && a_neg == b_neg) {
        adj_mat[i, j] <- "RP3"
        #aIb if aI+b and aI-b
      } else if (a_pos == b_pos && a_neg == b_neg) {
        adj_mat[i, j] <- "I1"
        #incomparable otherwise
      } else if (a_pos > b_pos && a_neg > b_neg) {
        adj_mat[i, j] <- "R1"
      } else if (a_pos < b_pos && a_neg < b_neg) {
        adj_mat[i, j] <- "R2"
      } else {
        adj_mat[i, j] <- "R3"
      }
    }
  }




  # Numeric adjacency matrix ------------------------------------------------


  adj_mat_numeric <- matrix(rep(0, n * n), n, n)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        adj_mat_numeric[i, j] <- 0
      }
      adj_mat_numeric[i, j] <- ifelse(adj_mat[i, j] %in% c("P1","P2","P3"),
                                      1, 0
      )
    }
  }

  row.names(adj_mat_numeric) <- basins
  colnames(adj_mat_numeric) <- basins

  row.names(adj_mat) <- basins
  colnames(adj_mat) <- basins

  # Get the scored table with pos and neg flows -----------------------------

  pf2 <- out_flows %>%
    mutate(score = (phi_plus - phi_minus) %>%
             round(digits = 2)) %>%
    rownames_to_column(var = name_col)

  # add rankings -----------------------------

  # make a ranking dataframe
  pf2$rank <- min_rank(-pf2$score)


  # build graph object from adjacency matrix
  # g <-
  #   igraph::graph_from_adjacency_matrix(
  #     adj_mat_numeric,
  #     diag = FALSE,
  #     mode = "directed"
  #   )

  g0 <- graph_from_adjacency_matrix(adj_mat_numeric,diag = FALSE, mode = "directed")
  df <- get.data.frame(g0)
  r <- endorelation(
    domain = as.list(unique(unlist(df[c("from", "to")]))),
    graph = df[c("from", "to")]
  )
  mat <- relation_incidence(transitive_reduction(r))
  mattr <- adj_mat_numeric[row.names(mat), colnames(mat)] * mat
  g <- graph_from_adjacency_matrix(mattr, mode = "directed", weighted = TRUE)



  g.vis <- visNetwork::visIgraph(g)
  nodes <- g.vis$x$nodes
  edges <- g.vis$x$edges

  from_rank <- left_join(edges, pf2, by = c("from" = name_col))
  to_rank <- left_join(edges, pf2, by = c("to" = name_col))

  ranked_edges <- data.frame(edges,
                             from_rank = from_rank$rank,
                             to_rank = to_rank$rank
  )

  cleaned_edges <- ranked_edges #%>%
    #group_by(from) %>%
    #slice(which.min(to_rank))


  # lone_nodes_from <- basins[which(!basins %in% cleaned_edges$from)]
  #
  # lone_nodes_to <- ranked_edges %>%
  #   filter(!to %in% cleaned_edges$to) %>%
  #   group_by(to) %>%
  #   filter(from_rank == max(from_rank))
  #
  # add_in_nodes <- ranked_edges %>%
  #   add_row(lone_nodes_to)
  #
  # cleaned_edges <- cleaned_edges %>% rbind(lone_nodes_to)

  coords <- data.frame(
    x = out_flows$phi_plus,
    y = out_flows$phi_minus
  ) %>% as.matrix()

  # Rotate 45 degrees
  phi <- pi / 4

  rotation_matrix <- cbind(
    c(cos(phi), sin(phi)),
    c(-sin(phi), cos(phi))
  )

  rotated_coords <- coords %*% rotation_matrix

  nodes <- data.frame(id = basins,
                      label = basins %>% abbreviate(minlength = 16),
                      rotated_coords,


                      title = paste0("<b>",basins,"</b><br>",
                                     "Upside: ",
                                     pf2$phi_plus,"<br>",
                                     "Downside: ",
                                     pf2$phi_minus, "<br>",
                                     "Net Score: ", pf2$score
                      ))


  plot.graph <-
    visNetwork::visNetwork(
      nodes=nodes,
      edges=edges,
      main = "A really simple example",
      submain = list(text = "Custom subtitle",
                     style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;"),
      footer = "Fig.1 minimal example",
      width = "100%"
      ) %>% #cleaned_edges) %>%

    visNetwork::visEdges(arrows = "to") %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visIgraphLayout( # layout = "layout_nicely",
      layout = "layout.norm", smooth = TRUE, physics = TRUE,
      layoutMatrix = rotated_coords
    ) %>%
    visNetwork::visNodes(
      shape = "box",
      fixed = list(y=TRUE),

      shadow = TRUE,

      `shapeProperties` = list(borderRadius = 5)
    ) %>% visNetwork::visPhysics(repulsion =  list(
      damping = 0.95))
  #
  return(list(pf2,res, plot.graph,adj_mat,adj_mat_numeric))
}



