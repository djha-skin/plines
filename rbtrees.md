# Red Black Finger Search Trees

## Why use

I wanted to create persistent collections in Common Lisp. I wanted to implement
the "Sequences" Common Lisp API (or at least a useful subset of it). I thought a
natural choice would be [Finger Trees](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html),
due to Ralf Hinze and Ross Paterson.

However, that data structure involves polymorphic recursion. Polymorphic
recursion is annoying in Common Lisp. The CLOS is well designed, but can be
slow, especially in the "hot path" in which data structure operations often find
themselves. Without using the CLOS, we are left with the choice of using
`defstruct` and `typecase`, which could be less than ideal.

It would be ideal if the datastructure were homogeneous -- it looked the same
throughout, like a red/black tree node.

Herein is presented a homogeneous data structure with good running time on most
operations, and sublinear upper bounds on running time for merge and split:


(still sublinear) upper bounds

## Merging!

Say I have two finger search red black trees. The content in one tree sorts
strictly greater than the content in the other tree. We need to merge these two
trees. How fast can we do it?

_Why would you want to merge two trees with disparate content_? You ask. That
seems like a very specific thing to want to do and doesn't seem like it could be
useful. However, we present several operations previously unheard of with red
black trees that now become possible.

Well, since the content is disjoint, we can reuse much of the structure of both
trees.

Let us consider a few cases.

Suppose we have two trees of black height $h$ Call these the _left_ and _right_
trees. The contents of one sort before the contents of the other. WLOG, the
content of the left tree sorts strictly _before_ all the elements of the right
tree.

We would like to "glue them together" somehow -- one tree on one side as one
subtree and one on the other. All we need is a _pivot_, an element that can
serve as a root node for both.

To do this, we may try to delete the minimum node from the right tree, or the
maximum node from the left, and use this element as our pivot for our new root
node. However, to do so could potentially decrease the black height of the tree
in question. Since their black heights would then be unequal, we would violate
the first invariant of red-black trees: that black height must be the same
across all paths in a tree. No matter what we color the new root node -- red or
black -- the black height wouldn't be the same between the new subtrees, so the
trees wouldn't be useful.

To pull this trick off, we must commit to a third invariant in the first place
when creating the tree:

_The minimum node of any tree must be red._

By adopting this variant and "keeping it true" throughout the life of our red
black trees, we can delete the minimum from the right tree of any two trees
which we wish to merge without affecting the black height of that tree. Then we
can use it as a pivot element in a new root node. If this node is the actual
root of the tree, we color it black to ensure no red node has red children (the
new root node might have red children).

Can we do this?

<do some research and prove that we can here>

## Merging trees of unequal height

What about merging two trees of non-overlapping intervals which do are not of
the same height?

There are two cases.

Suppose the left tree has a larger black height than the right. The two sets are
of non-overlapping intervals, we merge the right tree with some subtree along
the right spine of the left tree. All paths have the same black height, so if
the black height of the right tree is $h$, there will be a subtree along the
right spine of the left tree which is the same height as the right tree. We
merge the right tree with this subtree, and make the new pivot node _red_
instead of black (since we're not at the root). We are left with a "balanced
tree" that might have violated the no-two-reds-in-a-row-rule with the parent of
the subtree, so we work up the tree and rebalance, just like we would during
element insertion.

The other case is a mirror of the first, in which the left tree is smaller than
the right. We then merge left tree with the subtree from along the left spine of the
right tree which is of equal black height, make the pivot red, and rebalance.

## Deletion by merge

Rebalancing after deletion in red black trees is notoriously difficult, wherein
one must deal with white, double-black, and red nodes. We here present a cleaner
algorithm for doing such.

To delete a node from a tree:

1. Find the subtree of which the deleted node is an immediate child. There are
   two cases: either the node to be deleted is a left child, or a right.

2. Split the child from the parent.

3. Insert the element from the "old parent" into the sibling tree. If the
   deleted node is the left, insert the old parent into the right, and vice
   versa. The old parent is either larger than all the elements in the sibling
   tree, or smaller. Therefore, it would be an insertion into the end or
   beginning of the tree. Insertion into a finger search binary tree is an O(1)
   time operation, and rebalancing afterwards is O(1) amortized, so this is a
   constant time operation.

We are left with two trees





of the We cannot do this in general with red black trees.




I started noodling

```lisp

(defstruct
   (value
    size
    left
    right
    farleft
    farright))

```

```

(defstruct b
  (value
   size
   lefts
   rights))

```


```
                 +-------+--------+
              +--+ Value | Size   +-+
              |  +---+---+---+----+ |
              |      |       |      |
              |     /         \     |
              |    /           \    |
              |   /             \   |
              |  /               \  |
              +-+                 +-+

 Normal list: 50% waste

 +-------+---+   +-------+---+   +-------+---+   +-------+---+   +-------+---+
 | value | +-+-->| value | +-+-->| value | +-+-->| value | +-+-->| value | x |
 +-------+---+   +-------+---+   +-------+---+   +-------+---+   +-------+---+

 Binary finger search tree: color - value - size - l - r - fl - fr
 potentially 6/7 waste :(

 size, l, r, fl, and fr are only interesting if I'm not the leaf.

 How had the king talked him into leaving the sanctuary?

## Delete-by-merge
