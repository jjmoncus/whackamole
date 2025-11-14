


#' Constructor for our board class
#'
#'
#'
constructor_board <- function(x) {

  class(x) <- c("board", class(x))

  num_moles <- sum(x)
  return(
    structure(x,
              num_moles = num_moles)
    )
}



#' Print method for 'board' class
#'
#' Implements the custom print behavior for whack-a-mole boards
#'
#' @param board An object of class 'board'.
#' @param ... Additional arguments (can ignore for now)
#'
#' @return The input object 'board' (invisibly)
#'
#' @export
#'
#' @method print board
#'
#' @importFrom purrr keep map reduce
#' @importFrom gt tab_header tab_options cols_align pct tab_style cell_fill cell_text cells_body tab_source_note
#'
#'
print.board <- function(board, ...) {

  n <- dim(board)[1]
  last_whack <- attr(board, "last_whack")
  num_moles <- attr(board, "num_moles")

  mole_locs <- mole_locations(board)
  # mole_loc_params <- map(
  #   mole_locs,
  #   function(z) {
  #
  #     list(z)
  #     })
  apply_mole_loc_borders <- function(g, args) {
    g %>%
      tab_style(
        style = cell_borders(sides = "all",
                             color = "grey",
                             weight = px(1)),
        locations = cells_body(rows = args[["row"]],
                               columns = args[["col"]]))
  }


  # generic output
  out <- board %>%
    # noquote() %>% # at this point, it's a matrix
    as.data.frame() %>%
    gt() %>%
    tab_header(title = "Whack-A-Mole") %>%
    tab_options(column_labels.hidden = TRUE,
                table.width = pct(60)) %>%
    cols_align(align = "center") %>%
    tab_source_note(glue("Number of moles left: {num_moles}"))

  if (is.null(last_whack)) {

    # if no last whack, then we're looking at the initial board, and dont need to style cells
    # do nothing
  } else {

    # the board has been whacked, we know there's an `adjacents` attributes for neighboring holes
    adj_locs <- attr(board, "adjacents") %>%
    # some of these adjacents might be outside the bounds of the grid, i.e. if the last whack was on an edge
    # we should remove these
    keep( function(z) (z[1] > 0 & z[1] <=n) & (z[2] > 0 & z[2] <= n) )


    adj_params <- map(
      adj_locs,
      function(z) {

        list(row = z[1],
             column = z[2],
             text_color = ifelse(board[z[1], z[2]] == 0, "darkgreen", "#800000"),
             bg_color = ifelse(board[z[1], z[2]] == 0, "lightgreen", "#fadadd")
             )
      })

    apply_adj_style <- function(g, args) {
      g %>%
        tab_style(
          style = list(
            cell_fill(color = args$bg_color),
            cell_text(color = args$text_color)
            ),
          locations = cells_body(rows = args$row,
                                 columns = args$column))
    }

    out <- out %>%
      tab_style(
        style = list(
          # cell_fill(color = "#fadadd"),
          cell_text(weight = "bold"),
          cell_fill(color = "lightgrey")
        ),
        locations = cells_body(rows = last_whack[1],
                               columns = last_whack[2])
      ) %>%
      {reduce(adj_params, apply_adj_style, .init = .)}

  }


  # finally, add boarders around mole locations
  out %>%
    {reduce(mole_locs, apply_mole_loc_borders, .init = .)} %>%
    print()

  # Return the input object invisibly (standard R convention)
  invisible(board)
}
