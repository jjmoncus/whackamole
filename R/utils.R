
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

#' Find the locations of elements in a grid
find_in_grid <- function(x, look_for) {

  locs <- which(x == look_for, arr.ind = TRUE)
  out <- apply(locs, 1, list)

  return( unlist(out, recursive = FALSE) )
}

#' Get the locations of all moles
mole_locations <- function(board) find_in_grid(board, look_for = 1)

#' For each cell, is there a mole below?
fellow_mole_below <- function(board) {

  n <- dim(board)[1]
  out <- matrix(0, n, n)
  for (i in 1:n-1) {

    out[i,] <- board[i,] + board[i+1,]
  }

  find_in_grid(out, look_for = 2)
}

#' For each mole, is there a mole to the right?
fellow_mole_right <- function(board) {

  n <- dim(board)[1]
  out <- matrix(0, n, n)
  for (j in 1:n-1) {

    out[,j] <- board[,j] + board[,j+1]
  }

  find_in_grid(out, look_for = 2)
}




#' Build an action matrix
#'
action <- function(n) {

  I <- diag(n)
  T_matrix <- diag(n) # The main diagonal (self-flip)
  T_matrix[row(T_matrix) == col(T_matrix) - 1] <- 1 # Upper diagonal (neighbor)
  T_matrix[row(T_matrix) == col(T_matrix) + 1] <- 1 # Lower diagonal (neighbor)

  A_kronecker <- kronecker(T_matrix, I) + kronecker(I, T_matrix) - kronecker(I, I)

  # Note: All operations are effectively modulo 2, but in R, we rely on the
  # final check to ensure all entries are either 0 or 1.
  A_kronecker %% 2 # Perform modulo 2 to ensure all entries are 0 or 1
}


