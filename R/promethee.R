

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
                        gaussP = NULL,
                        limit = NULL) {
  basins <- rownames(dataset)
  criteria <- colnames(dataset)

  n <- length(basins)
  c <- length(criteria)

  # Weighting ---------------------------------------------------------------

  if (is.null(weighting)) {
    weighting <- rep(1, c)
  }

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

  res <- PROMETHEE(dataset = dataset,
                   PreferenceF = PreF,
                   PreferenceT = PreT,
                   S_Gauss = gaussP,
                   IndifferenceT = IndT,
                   Weights = weighting,
                   Min_Max = minmax)


  #make row and column names



  out_flows <- data.frame(
    row.names = basins,
    phi_plus = res[["PROMETHEE1"]][,1],
    phi_minus = res[["PROMETHEE1"]][,2],
    score = res[["PROMETHEE2"]]
  ) %>%
    round(2) %>%
    mutate(score_rank = min_rank(-score)) %>%
    arrange(desc(score))

  ucflows <- res$UnicriterionNetFlows %>%
    as.data.frame() %>%
    `colnames<-`(criteria) %>%
    `rownames<-`(basins) %>%
  merge(out_flows %>% dplyr::select(c(score_rank, score)),by=0) %>%
    arrange(desc(score)) %>%
    column_to_rownames("Row.names")


  if(!is.null(limit)){
    out_flows <- out_flows %>%
      head(limit)
    ucflows <- subset(ucflows, rownames(ucflows) %in% rownames(out_flows))
  }
res$out_flows <- out_flows
res$UnicriterionNetFlows <- ucflows
  #
  return(res)
}


adjacency_matrix <- function(out_flows, basins) {
  n <- nrow(out_flows)
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
        adj_mat[i, j] <- "I"
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

  return(adj_mat_numeric)
}




