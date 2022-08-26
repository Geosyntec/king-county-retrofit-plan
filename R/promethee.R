

# turns a vector into a repeating matrix
make.matrix <- function(vector.in,n) {
  matrix(rep(vector.in,each=n),nrow=n)
}



#' promethee_2
#'
#' Revised 8-26-2022 to use the PROMETHEE Package, which returns unicriterion
#' outflows
#'
#' @param dataset dataframe with rownames of the actions
#' @param weighting character
#' @param minmax character
#' @param IndT character
#' @param PreT character
#' @param PreF character
#' @param gaussP character
#'
#'
#' @return
#' @export
#'
#' @examples
promethee_2 <- function(dataset = NULL,
                        # ,
                        # name_col = "watershed.name",
                        weighting = NULL,
                        minmax = NULL,
                        IndT = NULL,
                        PreT = NULL,
                        PreF = NULL,
                        gaussP = NULL) {
  basins <- rownames(dataset)
  criteria <- colnames(dataset)

  n <- length(basins)
  c <- length(criteria)

  # Weighting ---------------------------------------------------------------

  if (is.null(weighting)) {
    weighting <- rep(1, c)
  }

  # handle nas: replaces column with zeros

  dataset[which(is.na(dataset %>% colSums()))] <- 0

  weighting <- weighting %>% make.matrix(n)

  # Min max -----------------------------------------------------------------


  if (is.null(minmax)) {
    minmax <- rep("min", c)
  }
  minmax <- minmax %>% make.matrix(n)

  # Thresholds, indifference ------------------------------------------------
  if (is.null(IndT)) {
    IndT <- runif(c, 0, 0)
  }

  IndT <- IndT %>% make.matrix(n)

  if (is.null(PreT)) {
    PreT <- runif(c, 0, 0)
  }
  PreT <- PreT %>% make.matrix(n)

  if (is.null(PreF)) {
    PreF <- rep("V-shape", c)
  }

  PreF <- PreF %>% make.matrix(n)

  if (is.null(gaussP)) {
    gaussP <- runif(c, 0, 0)
  }
  gaussP <- gaussP %>% make.matrix(n)

  # handle nas: replaces column with zeros
  na_cols <- (is.na(dataset %>% colSums()))
  dataset[which(is.na(dataset %>% colSums()))] <- 0

  res <- PROMETHEE(dataset = dataset,
                   PreferenceF = PreF,
                   PreferenceT = PreT,
                   S_Gauss = gaussP,
                   IndifferenceT = IndT,
                   Weights = weighting,
                   Min_Max = minmax)

  # res <- PROMETHEEOutrankingFlows(
  #   performanceTable = dataset %>% # dplyr::select(-watershed.name) %>%
  #     as.matrix(),
  #   preferenceFunction = PreF,
  #   PreT,
  #   IndT,
  #   criteriaWeights = weighting,
  #   criteriaMinMax = minmax,
  #   gaussParameter = gaussP
  # )


  # out_flows <- data.frame(
  #   row.names = basins,
  #   phi_plus = res[["outrankingFlowsPos"]],
  #   phi_minus = res[["outrankingFlowsNeg"]]
  # ) %>%
  #   round(2) %>%
  #   mutate(score = (phi_plus - phi_minus) %>%
  #     round(digits = 2)) %>%
  #   mutate(score_rank = min_rank(-score))

  # if(!is.null(limit)){
  #   out_flows <- slice_min(score_rank,limit)
  # }

  # get partial ranking




  # Get the scored table with pos and neg flows -----------------------------

 # pf2 <- out_flows
  # %>%
  #   mutate(score = (phi_plus - phi_minus) %>%
  #            round(digits = 2)) #%>%
  # rownames_to_column(var = name_col)

  # add rankings -----------------------------

  # make a ranking dataframe
  # pf2$rank <- min_rank(-pf2$score)



  #
  return(res)
}


adjacency_matrix <- function(n, out_flows, i, j, basins) {
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
}
