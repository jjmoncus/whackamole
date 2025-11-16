

#' Play a game of Whack-A-Mole
#'
#' Initiates a random board of moles, then iterates over an algorithm to whack moles,
#' with the goal of whacking all moles within a certain number of whacks
#'
#' @importFrom glue glue
#' @importFrom gt gt
#'
#' @param n The dimension of the grid of moles (n x n)
#' @param theta Number from 0 to 1, determining how densely the initial grid is packed with moles
#' @param turn_time How long you'd like to wait in between turns (in seconds)
#'
#' @export

play <- function(n = 3, theta = .8, turn_time = 1) {

  cat("#########################################", "\n")
  cat("# ------ Let's play Whack-A-Mole! ----- #", "\n")
  cat("#########################################", "\n")
  Sys.sleep(turn_time)

  # make sure n is an integer
  if (!is.integer(n)) {
    new_n <- as.integer(n)
    cat(glue("# ---- `n` squashed from {n} to {new_n}"), "\n")
    # just needed `new_n` for messaging, can go back to normal n name now
    n <- new_n
  }

  # initialize a random matrix 4x4 matrix of zeros and 1s
  board <- new_board(n = n, theta = theta)
  moves <- 0

  cat("# ------ Initial board ----- #", "\n")
  print(board)
  size <- sum(board)
  cat(glue("# ------ # moles left: {size} ------ #"), "\n")
  # helpful utilities

  # trivially see if it's solved, if not continue
  board_is_solved <- is_solved(board)
  if (board_is_solved) {
    return(
      cat("The board is trivially solved! Great game!", "\n")
    )
  }
  Sys.sleep(turn_time)

  # Initially, whack with strong preference for:
  # ---- 1) centrality
  # ---- 2) mole density
  central <- close_to(board, i_star = ceiling(n/2), j_star = ceiling(n/2))
  global <- density_global(board)
  # leftward <- linear_left(board)
  # weight <- .8 * global
  weight <- greedy(board) * central
  # .2 * central +
  hit <- weight %>% which.max() %>% arrayInd(c(n, n)) # get location of highest weighted mole
  board <- whack(board, hit[1], hit[2]) # whack the mole
  Sys.sleep(turn_time)
  board_is_solved <- is_solved(board) # check if we've solved
  moves <- moves + 1 # add a move
  sparsity <- attr(board, "num_moles") / (n*n)

  while(!board_is_solved) {

    central <- close_to(board, i_star <- ceiling(n/2), j_star = ceiling(n/2))
    near_last_hit <- close_to(board, i_star <- hit[1], j_star = hit[2])
    global <- density_global(board)

    # as the matrix gets sparser, have more of a preference to jump away from the last hit
    lambda <- 1 - sparsity

    weight <- .5*greedy(board) * central + lambda * far_from(board, hit[1], hit[2])
    # + .05 * central + .05 * near_last_hit
    hit <- weight %>% which.max() %>% arrayInd(c(n, n)) # get location of first, largest subspace
    board <- whack(board, hit[1], hit[2])
    Sys.sleep(turn_time)
    board_is_solved <- is_solved(board)
    moves <- moves + 1
    sparsity <- attr(board, "num_moles") / (n*n)
  }

  return(
    cat("The board is solved! Great game!", "\n", glue("Game was won in {moves} whacks"), "\n")
  )
}
