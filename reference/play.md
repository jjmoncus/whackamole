# Play a game of Whack-A-Mole

Initiates a random board of moles, then iterates over an algorithm to
whack moles, with the goal of whacking all moles within a certain number
of whacks

## Usage

``` r
play(
  n = 3,
  theta = 0.8,
  turn_time = 1,
  pref_radial = 0.05,
  pref_left = 0.05,
  pref_local = (1 - (pref_radial + pref_left)/2),
  pref_global = (1 - (pref_radial + pref_left)/2),
  lambda = 0.1
)
```

## Arguments

- n:

  The dimension of the grid of moles (n x n)

- theta:

  Number from 0 to 1, determining how densely the initial grid is packed
  with moles

- turn_time:

  How long you'd like to wait in between turns (in seconds)
