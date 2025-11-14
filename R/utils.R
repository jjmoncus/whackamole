
#' Add padding around a board
#'
#'
make_big <- function(board) {

  big_board <- board %>%

    # add padder rows above and below
    {rbind(rep(0, times = ncol(.)),
           .,
           rep(0, times = ncol(.)))} %>%

    # add padder columns in first and last to the new thing
    {cbind(rep(0, times = nrow(.)),
           .,
           rep(0, times = nrow(.)))}

  return( big_board )
}

#' Remove edges around a board
#'
#' remove the edges of a board and return it's sub-board
#' (useful for turning a board back to its original after making it big)
#'
strip_edges <- function(board) {

  n <- dim(board)[1]
  return( board[2:(n-1), 2:(n-1)] )
}


#' Is a board solved?
#'
is_solved <- function(board) (board == 0) %>% all()


#' The number of moles on and around (i, j)
#'
local_sum <- function(board, i, j) {

  board[i ,j] + board[i,j+1] + board[i,j-1] + board[i+1,j] + board[i-1,j]
}

#' The density of moles across a whole matrix
density_global <- function(board) {

  board <- make_big(board)
  n <- dim(board)[1]
  out <- matrix(0, nrow = n, ncol = n)

  # fill in the subspaces
  for (i in 1:n) { # first row is special
    for (j in 1:n) {

      # zero weight by default if we cant whack the board there (i.e. == 0)
      out[i, j] <- ifelse(board[i,j] == 0, 0, local_sum(board, i, j))
    }
  }

  out <-  strip_edges(out) %>%
    {. / sum(.)}
  return( out )
}

#' The density of moles across a matrix, with strong local preference
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

#' Weights relative to the position of a cell in a board
radial_top_left <- function(board, n = dim(board)[1]) {

  out <- matrix(0, nrow = n, ncol = n)
  # fill in the subspaces
  for (i in 1:n) { # first row is special
    for (j in 1:n) {

      # zero weight where we can't whack
      out[i,j] <- ifelse(board[i,j] == 0, 0, 1/(i*j))
    }
  }

  # normalize
  out <- out / sum(out)
  return( out )
}




#' Flip something from 0 to 1, and vice versa
#'
flip <- function(x) 1 - x

#' Whack a mole at a particular location
whack <- function(board, i, j) {

  board <- make_big(board)
  # new location with reference to new big board
  i <- i+1
  j <- j+1

  board[i, j] <- flip(board[i, j])

  board[i-1, j] <- flip(board[i-1, j])
  board[i, j+1] <- flip(board[i, j+1])
  board[i+1, j] <- flip(board[i+1, j])
  board[i, j-1] <- flip(board[i, j-1])

  # back to old coordinates
  i <- i-1
  j <- j-1
  # remove boundary - this accounts for the possibility of having a cascade of moles on the boundary that aren't real
  board <- strip_edges(board) %>%
    constructor_board() %>%
    structure("last_whack" = c(i, j),
              "adjacents" = list(c(i-1, j),
                                 c(i, j+1),
                                 c(i+1, j),
                                 c(i, j-1))
              )


  size <- sum(board)
  cat(glue("# ------ Whack @ ({i}, {j})----- #"), "\n")
  print(board)
  cat(glue("# ------ # moles left: {size} ------ #"), "\n")
  return( board )
}


#' Get the locations of all moles
mole_locations <- function(board) {

  mole_locs <- which(board == 1, arr.ind = TRUE)
  out <- apply(mole_locs, 1, list)

  return( unlist(out, recursive = FALSE) )
}
