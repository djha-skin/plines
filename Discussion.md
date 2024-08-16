
2024-07-16T19:12:23-0600

The goal is to make a persistent collections library that:

* Respects the CL standard and implements, where possible, a drop-in replacement
  for functions and semantics everyone already knows and so that old code is not
  broken but new code or is easily ported.
* Is in pure common lisp
* Is kinda fast
* But still totally persistent/functional, thus getting the benefits of that
  genre of data structures.
* Would be nice if the implementation was as simple as possible, to help people
  comprehend it and devs to debug it.

Non-goals:

* Needless acrobatics
* Impractical but impressive asymptotic bounds ("Constant time", but what's the
  constant?)
* Your insensibiliities.

Things that mass destructively modify:

Functions that modify the original and therefore make no sense to actually
implement:

  - `delete`, delete-if, delete-if-not
  - nsubstitute-if, nsubstitute-if-not
  - fill
  - map-into
  - replace
  - nsubstitute

Functions that don't care what the sequence is, they just need one (and
therefore traversal needs to be kinda fast, hopefully somewhat
locality-respecting).

  - map
  - reduce
  - COUNT-IF, COUNT-IF-NOT
  - find-if, find-if-not (??? Again, maybe could be fast?)
  - position-if, position-if-not (ditto)
  - substitute-if, substitute-if-not
  - REMOVE-IF, REMOVE-IF-NOT,
  - `search`
  - `sort`, `stable-sort`

Functions that can be implemented persistently and therefore need to be "kinda
fast":

  - `elt`
  - `copy-seq`
  - `make-sequence`
  - `subseq`
  - `count`
  - `length`
  - `reverse`
  - `find` (check if sorted, check the sorted flag)
  - `position` (check if sorted)
  - `mismatch` (check if sorted)
  - `substitute` (
  - `concatenate`
  - `merge`
  - `remove`
  - `remove-duplicates` (tree's left arm is `<=`, multiset)

This just in, CL has stuff for [subst](http://clhs.lisp.se/Body/f_substc.htm)
and [`tree-equal`](http://clhs.lisp.se/Body/f_tree_e.htm) that we should totally
just plug into. That way, we won't have to replace the standard, we can just
plug into it.



`tree-equal` is interesting but the trees _must be the same shape_ so that could
be prickly, just watch it

`copy-tree` but I thought we were trying to avoid doing this very thing

`sublis` for making substitutions, but it's a bigger subst.

Nevermind, there's not much here useful.

And now back to your regularly scheduled program.


| Function            | IHTree | RBTree     | FinTree   | RandAccSkew |
|---------------------|--------|------------|-----------|-------------|
| `elt`               |        | `O(lg n)`  | `O(lg n)` |             |
| `copy-seq`          |        | `O(1)`     | `O(1)`    |             |
| `make-sequence`     |        | `O(1)`     | `O(1)`    |             |
| `subseq`            |        | `O(1)`     | `O(lg n)` |             |
| `length`            |        | `O(1)`     | `O(1)`    |             |
| `reverse`           |        | `O(1)`     | `O(1)`    |             |
| `find`              |        | `O(lg n)`  | `O(n)`    |             |
| `position`          |        | `O(lg n)`  | `O(n)`    |             |
| `substitute`        |        | `O(lg n)`  | `O(n)`    |             |
| `remove`            |        | `O(lg n)`  | `O(lg n)` |             |
| `concatenate`       |        | `O(n)` :(  | `O(lg n)` |             |
| `merge`             |        | `O(n)` :(  | `O(lg n)` |             |
| `count`             |        | `O(1)`*    | `O(1)`    |             |
| `sort`              |        | `O(1)`**   | `O(n)`    |             |
| `search (tries?)`   |        | `O(1)`*    | `O(n)`    |             |
| `mismatch`          |        | `O(lg n)`* | `O(n)`    |             |
| `remove-duplicates` |        | N/A        | `O(n)`    |             |
| `tree-equal`        | Uh-uh  | ...?       | ...?      |             |

I don't know. RB trees do a pretty good job.

But there's supposed to be stuff you can do when inserting things that blah blah
blah finger trees are awesome.

* = It's a set, not a sequence, they're fast, but they're also cheating.
** = Can't "re-sort" this, but _can_ leave it sorted.

near as I can tell that's pretty good.
