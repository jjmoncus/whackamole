

#' Play a game of Whack-A-Mole
#'
#' Initiates a random board of moles, then iterates over an algorithm to whack moles,
#' with the goal of whacking all moles within a certain number of whacks
#'
#'
#' @export

play <- function(n = 3, theta = .8) {

  cat("#########################################", "\n")
  cat("# ------ Let's play Whack-A-Mole! ----- #", "\n")
  cat("#########################################", "\n")
  Sys.sleep(.3)

  # initialize a random matrix 4x4 matrix of zeros and 1s
  board <- sample(c(0, 1), n*n, replace = TRUE, prob = c(1 - theta, theta)) %>%
    matrix(nrow = n, ncol = n)

  # adding padding of rows and columns around `board`
  # so we can flexibly get subspace numbers
  big_board <- rbind(rep(0, times = ncol(board)),
                     board,
                     rep(0, times = ncol(board)))
  big_board <- cbind(rep(0, times = nrow(big_board)),
                     big_board,
                     rep(0, times = nrow(big_board)))
  moves <- 0

  cat("# ------ Initial big board (i.e. [n+2, n+2]----- #", "\n")
  print(big_board)
  size <- sum(board)
  cat(glue("# ------ # moles left: {size} ------ #"), "\n")
  # helpful utilities
  is_solved <- function(board) (board == 0) %>% all()
  # trivially see if it's solved, if not continue
  board_is_solved <- is_solved(board)
  Sys.sleep(.3)

  local_sum <- function(board, i, j) {

    board[i ,j] + board[i,j+1] + board[i,j-1] + board[i+1,j] + board[i-1,j]
  }
  density_global <- function(board) {

    # only define subspace where i,j = 1,
    # we dont need to know the subspace for places we cant whack

    out <- matrix(0, nrow = nrow(board), ncol = ncol(board))
    # fill in the subspaces
    for (i in 2:(n+1)) { # first row is special
      for (j in 2:(n+1)) {

        # zero weight by default if we cant whack the board there (i.e. == 0)
        out[i, j] <- ifelse(board[i,j] == 0, 0, local_sum(board, i, j))
      }
    }

    return( out )
  }
  density_local <- function(board, i, j, degree = 1) {

    # only define subspace where i,j = 1,
    # we dont need to know the subspace for places we cant whack

    # zero weight nearly everywhere, except at the four points of degree `degree` away from the reference [i,j] (i.e. the previous hit)
    out <- matrix(0, nrow = nrow(board), ncol = ncol(board))
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

    return( out )
  }
  radial_top_left <- function(board) {

    out <- matrix(0, nrow = nrow(board), ncol = ncol(board))
    # fill in the subspaces
    for (i in 2:(n+1)) { # first row is special
      for (j in 2:(n+1)) {

        # zero weight where we can't whack
        out[i,j] <- ifelse(board[i,j] == 0, 0, 1/(i*j))


      }
    }
    return( out )
  }

  flip <- function(x) 1 - x
  whack <- function(board, i, j) {

    board[i, j] <- flip(board[i, j])

    board[i-1, j] <- flip(board[i-1, j])
    board[i, j+1] <- flip(board[i, j+1])
    board[i+1, j] <- flip(board[i+1, j])
    board[i, j-1] <- flip(board[i, j-1])

    # coerce anything on the boundary to 0, in case we hit near the edge
    board[1, ] <- 0
    board[n+2, ] <- 0
    board[, 1] <- 0
    board[, n+2] <- 0

    size <- sum(board)
    cat(glue("# ------ Whack @ ({i}, {j})----- #"), "\n")
    print(board)
    cat(glue("# ------ # moles left: {size} ------ #"), "\n")
    Sys.sleep(.3)
    return( board )
  }


  # Initially, whack with strong preference for radial top left
  radial <- radial_top_left(big_board)
  global <- density_global(big_board)
  weight <- .9 * radial + .1 * global
  hit <- weight %>% which.max() %>% arrayInd(c(n+2, n+2)) # get location of highest weighted mole
  big_board <- whack(big_board, hit[1], hit[2]) # whack the mole
  board_is_solved <- is_solved(big_board) # check if we've solved
  moves <- moves + 1 # add a move

  while(!board_is_solved) {

    # after t = 1, radial.+ global + local preference

    radial <- radial_top_left(big_board)
    local_2 <- density_local(big_board, i = hit[1], j = hit[2], degree = 2)
    global <- density_global(big_board)
    weight <- .8 * radial + .1 * local_2 + .1 * global
    hit <- weight %>% which.max() %>% arrayInd(c(n+2, n+2)) # get location of first, largest subspace
    big_board <- whack(big_board, hit[1], hit[2])
    board_is_solved <- is_solved(big_board)
    moves <- moves + 1
  }

  return(
    cat("The board is solved! Great game!", "\n", glue("Game was won in {moves} whacks"), "\n")
  )
}

