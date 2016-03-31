## Advance Challenges 

This R code is designed to solve `AB - CD = EF` and `EF - GH = PPP` with base from 10 to 36 (can be larger therotically, if a larger set of character code is provided)

#### General idea:

1. Find all permutations of `e` and `f`.

2. Find `p` using `trunc((e + g)/ base)` and `trunc((e + g + 1)/ base)`.

3. Omit any `(e, g, p)` s.t. `e + g > pp` or `e + g < pp - 1`.

4. Find `(f, h)` s.t. `ef + gh = ppp`.

5. Find `(b, d)` s.t. `b - d - f` is dividable by 10.

6. Find `(a, c)` s.t. `a - c = e` or `a - c = (e - 1)`.


#### Possible improvement in the future

* Make use of the properties that `ef + gh = gh + ef`



