# catch the bunny

[![Actions Status](https://github.com/mbarbin/catch-the-bunny/workflows/ci/badge.svg)](https://github.com/mbarbin/catch-the-bunny/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/catch-the-bunny/badge.svg?branch=main&service=github)](https://coveralls.io/github/mbarbin/catch-the-bunny?branch=main)

The is a toy project that implements a solver for a fun little logic puzzle.

## What's the puzzle ?

The goal is to catch a bunny that hides in a box within a row of boxes.

To catch the bunny, you have to open the actual box where the bunny is hiding.
When boxes are closed, you can't see what's in them.
```
+---+---+---+---+
| 1 | 2 | 3 | 4 | Where is the bunny ?
+---+---+---+---+
```
When the game starts, the bunny is located in one of the boxes, but you don't
know which one. At each step, you choose a box and you open it. If the bunny is
there, the game stops and you've won. If it isn't there, you close the box and
then the bunny moves to a different box. Then the game repeats : you choose
another box, if you haven't caught the bunny, it moves again and the game goes
on.

## How does the bunny move exactly ?

The bunny moves to the box that is either to the left or to the right of where
it is currently.

For example, if the bunny starts in box number 2:
```
+---+---+---+---+
|   | X |   |   | The 'X' is the bunny - it hides in box 2.
+---+---+---+---+
```
after it moves it will either be located in box number 1:
```
+---+---+---+---+
| X |   |   |   | The bunny moved to the left.
+---+---+---+---+
```
or in box number 3:
```
+---+---+---+---+
|   |   | X |   | The bunny moved to the right.
+---+---+---+---
```
If the bunny is on the side, it may only move away from that side.

For example, if the bunny is in box number 1:
```
+---+---+---+---+
| X |   |   |   | There's nowhere to go to the left !
+---+---+---+---+
```
after it moves it will necessarily be in box number 2:
```
+---+---+---+---+
|   | X |   |   | The bunny had to move to the right.
+---+---+---+---+
```
Symmetrically, if the bunny is in the last box, it has to move to the left.

## What's an actual solution to the problem ?

It isn't sufficient to establish good odds of catching the bunny. You shall
rather devise a strategy that will make sure that you catch the bunny in a
maximum number of steps.

### Examples : size 2

If the row is of size 2, then by opening the box 1 twice in a row, you'll make
sure to catch the bunny in at most 2 steps.

If the bunny is initially in 1, you'll catch it at step 1:
```
+---+---+
| X |   | You open box 1 and catch the bunny
+---+---+
```
If it is initially in 2, at first you won't see it in box 1:

Step 1
```
+---+---+
|   | X | You open box 1 - the bunny isn't there
+---+---+
```
The bunny moves. It is now necessarily in 1.

Step 2:
```
+---+---+
| X |   | You open box 1 and catch the bunny
+---+---+
```
### Actual size

Can you solve this puzzle for a row of 6 or 8 boxes ?

# Where does the puzzle comes from ?

It comes from an online compute science challenge for kids that is
hosted there:

https://concours.castor-informatique.fr/

That specific puzzle was one of the training exercises of the year
2021.

## Motivations

While the puzzle isn't meant to be solved using a computer program, it's a fun
little project to implement regardless.

## Code documentation

The code documentation of the latest release is built with `odoc` and published
to `GitHub` pages [here](https://mbarbin.github.io/catch-the-bunny).

The code is executed through test files located in the [test/](test/)
subdirectory.
