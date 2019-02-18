aoc2018

These are my solutions to <https://adventofcode.com/>.  I make no claim as to
code quality, organization, etc.

"*.lisp" files are Common Lisp.  I am using SBCL.  I haven't tested with
anything else.




After around December 19th, I gave up.  There was one puzzle where I had a
small bug I couldn't find, which was discouraging.  (This was the falling water
puzzle, I think.)  The puzzles became persnickety.  This, coupled with the
demands of the holidays, meant that I prioritized other things over finishing
this out.

I got bored of trying to implement arbitrarily complicated rules for toy
problems.

This is the most Lisp I have written to date.  (We used some Lisp in college
for 15-212, the 3rd class in the canonical CS sequence at CMU at the time.)  As
much as I like Lisp, I found it a little frustrating.  I don't like the
defclass syntax.  I haven't figured out a good pattern for introducing a local
in the middle of a block, something I do all the time in C-like languages.
Quicklisp is wonderful, but still not as good as Go for packaging, or even
Maven (at least as a consumer).

If I revisit this, I need to find better ways to visualize complex grids or
frames of data stored in arrays.  It was hard to do some of this in a terminal,
and I was jealous of @visnup's ObservableHQ views, which were gorgeous.
