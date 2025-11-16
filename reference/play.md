# Play a game of Whack-A-Mole

Initiates a random board of moles, then iterates over an algorithm to
whack moles, with the goal of whacking all moles within a certain number
of whacks

## Usage

``` r
play(n = 3, theta = 0.8, turn_time = 1)
```

## Arguments

- n:

  The dimension of the grid of moles (n x n)

- theta:

  Number from 0 to 1, determining how densely the initial grid is packed
  with moles

- turn_time:

  How long you'd like to wait in between turns (in seconds)
