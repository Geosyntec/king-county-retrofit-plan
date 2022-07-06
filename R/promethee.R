library(dplyr)
library(MCDA)
library(tibble)
promethee_2 <-  function(dataset=NULL,
                         #,
                         #name_col = "watershed.name",
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

  }

  # qualitative parameters


  # get numeric data; convert add in rownames
  # dataset <- dataset %>%
  #   remove_rownames() %>%
  #   column_to_rownames(name_col) %>%
  #   select_if(is.numeric)







  # dataset <- demo_eval_table
  # name_col <- "Evaluations"

  basins <- rownames(dataset)
  criteria <- colnames(dataset)

  n <- length(basins)
  c <- length(criteria)


  # Weighting ---------------------------------------------------------------

  if (is.null(weighting)) {
    weighting <- rep(1, c)
  }




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
    PreF <- rep("Level", c)
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


  out_flows <- data.frame(row.names = basins,
    phi_plus = res[["outrankingFlowsPos"]],
    phi_minus = res[["outrankingFlowsNeg"]]
  ) %>% round(2)



  # get partial ranking


  adj_mat <- matrix(rep("U", n * n), n, n)

  for (i in 1:n) {
    for (j in 1:n) {
      a_pos <- out_flows[i, "phi_plus"]
      b_pos <- out_flows[j, "phi_plus"]
      a_neg <- out_flows[i, "phi_minus"]
      b_neg <- out_flows[j, "phi_minus"]


      if (a_pos > b_pos && a_neg < b_neg) {
        adj_mat[i, j] <- "P"
      } else if (a_pos == b_pos && a_neg < b_neg) {
        adj_mat[i, j] <- "P"
      } else if (a_pos > b_pos && a_neg == b_neg) {
        adj_mat[i, j] <- "P"
      } else if (a_pos == b_pos && a_neg == b_neg) {
        adj_mat[i, j] <- "I1"
      } else if (a_pos > b_pos && a_neg > b_neg) {
        adj_mat[i, j] <- "R"
      } else if (a_pos < b_pos && a_neg < b_neg) {
        adj_mat[i, j] <- "R"
      } else {
        adj_mat[i, j] <- "X"
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
      adj_mat_numeric[i, j] <- ifelse(adj_mat[i, j] == "P",
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
             round(digits = 2)) #%>%
    #rownames_to_column(var = name_col)

  # add rankings -----------------------------

  # make a ranking dataframe
  pf2$rank <- min_rank(-pf2$score)



  #
  return(list(pf2,adj_mat_numeric))
}



