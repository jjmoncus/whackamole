
#' The number of moles on and around (i, j)
#'
local_sum <- function(board, i, j) {

  board[i ,j] + board[i,j+1] + board[i,j-1] + board[i+1,j] + board[i-1,j]
}

#' Weight: how many moles are near you
density_global <- function(board) {

  board <- make_big(board)
  n <- dim(board)[1]
  out <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {

      # zero weight by default if we cant whack the board there (i.e. == 0)
      out[i, j] <- ifelse(board[i,j] == 0, 0, local_sum(board, i, j))
    }
  }

  out <-  strip_edges(out) %>%
    {. / sum(.)}
  return( out )
}

#' Weight: are there moles in a star-pattern around you?
density_local <- function(board, i, j, degree = 1) {

  board <- make_big(board)
  n <- dim(board)[1]
  # only define subspace where i,j = 1,
  # we dont need to know the subspace for places we cant whack

  # zero weight nearly everywhere, except at the four points of degree `degree` away from the reference [i,j] (i.e. the previous hit)
  out <- matrix(0, nrow = n, ncol = n)
  # fill in the subspaces
  for (z in list(c(i-degree, j),
                 c(i+degree, j),
                 c(i, j-degree),
                 c(i, j+degree))) {
    # if it's on the edge or below/above, skip it
    if (z[1] < 2) next
    if (z[1] > n+1) next
    if (z[2] < 2) next
    if (z[2] > n+1) next

    out[z[1],z[2]] <- ifelse(board[z[1],z[2]] == 0, 0, local_sum(board, z[1], z[2]))

  }

  # turn back to original dimensions and normalize
  # if sum is zero, need to divide by 1 instead

  out <- strip_edges(out)
  denom <- sum(out)
  out <- ifelse(denom == 0, out, out / denom)

  return( out )
}

#' Weight: how far you are from the top-left corner?
radial_top_left <- function(board, n = dim(board)[1]) {

  out <- matrix(0, nrow = n, ncol = n)

  # divide every column by its index
  # then divide every row by its index
  # zeros remain zeros
  for (i in 1:n) out[i,] <- (board[i,] / i)
  for (j in 1:n) out[,j] <- (board[, j] / j)

  # normalize
  out <- out / sum(out)
  return( out )
}


#' Weight: how far are you from the left-most column?
linear_left <- function(board, n = dim(board)[1]) {

  # each column of the matrix is divided by its index
  # so columns further to the left get more weight than those to the right
  # zeros remain zero
  out <- matrix(0, n, n)
  for (j in 1:n) out[,j] <- (board[, j] / j)

  # normalize
  out <- out / sum(out)
  return( out )
}


#' Weight: how close to you to a certain point
close_to <- function(board,
                     i_star = ceiling(dim(board)[1]/2),
                     j_star = ceiling(dim(board)[1]/2)) {

  n <- dim(board)[1]

  i_dists <- seq(1, n) - i_star
  j_dists <- seq(1, n) - j_star

  X <- outer(rep(1, n), i_dists, FUN = "*")
  Y <- outer(j_dists, rep(1, n), FUN = "*")

  out <- 1 / (sqrt(X^2 + Y^2) + 1)

  # zero out weights where there are no moles
  return( board * out )
}


#' Weight: what's the net mole consequence of whacking you?
greedy <- function(board) {

  board <- make_big(board)
  n <- dim(board)[1]
  density <- matrix(0, nrow = n, ncol = n)

  for (i in 2:(n-1)) {
    for (j in 2:(n-1)) {
      density[i, j] <- local_sum(board, i, j)
    }
  }

  out <- density * board

  out <-  strip_edges(out)
  return( out )
}

#' Weight: are you farther away from somewhere?
far_from <- function(board, i_star, j_star) {

  n <- dim(board)[1]

  i_dists <- seq(1, n) - i_star
  j_dists <- seq(1, n) - j_star

  X <- outer(rep(1, n), i_dists, FUN = "*")
  Y <- outer(j_dists, rep(1, n), FUN = "*")

  out <- (X^2 + Y^2) * board

  # zero out weights where there are no moles
  return( out / sum(out) )
}
