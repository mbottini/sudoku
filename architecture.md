# ExactCover.hs

We solve the more abstract problem first before turning to Sudoku.

>     data Row a = Row {label :: a, vals :: [Bool]} deriving (Show)

As always, we define our types first. A `Row` contains a `label` and `vals`.

This particular syntax is called "record syntax," which defines two functions
for interacting with row types.

```hs
ghci> :t label
label :: Row a -> a
ghci> :t vals
vals :: Row a -> [Bool]
```

This is Haskell's way of doing field access on structs. `label r` would be
rendered as `r.label` in some other language.

Crucially, the `label` is generic. For debugging purposes, I frequently
make the type parameter a `String` (so that I can name my sets human-readable
names). When I solve Sudoku, I will end up making the `label` a custom type
containing the information of a particular Mark (row, column, and number).

The `deriving` keyword is **not** inheritance, which you'd see in Java. Instead,
it is telling the Haskell compiler to generate a default interface implementation
(which will error out if it's not possible). In this case, I'm asking the compiler
to make `Row` an instance of type `Show`, which allows me to call `print` on it.

```hs
ghci> :t print
print :: Show a => a -> IO ()
ghci> print (Row "blarg" [True, False, True])
Row {label = "blarg", vals = [True,False,True]}
```

> `enumerate`

The fact that this is a one-liner does not make it any less annoying that I
have to roll it myself in Haskell.

> `createBoolList`

Given a list like `[1, 3, 6]`, I want to produce the list of Booleans
`[True, False, True, False, False, True]`. I have an additional parameter
`limit` because our matrix's rows will need to contain the same number
of elements.

> `createBitfield`

Concatenate all of the elements together and find its maximum element. That
will be the `limit` that gets passed to calling `createBoolList` on each of the
lists.

> `chooseColumn`

You'll see `go` used a lot in this code. You should think of this as a helper
function that is not available to anything else, which is why I name it the
same thing in a lot of functions. In Haskell we tend to call it `go`. In
OCaml we call it `aux` ("auxillary").

Note the Very Silly composition involved. `go` is the composition of

* getting the values of a list of `Row` objects (producing `[[Bool]]`)
* transposing them (`[[Bool]]`)
* counting the `True` elements (filtering on the identity function, then finding the length of the lists)
* enumerating to associate each column index with its respective count. So if column 5 has 10 elements in
it, its tuple will be `(5, 10)`.

I'll pause here. Tuples of type `(a, b)` are instances of type `Ord`, which allows me to sort them and
find the minimum. Unfortunately, for our purposes, it looks at the first element first, and then compares
the second element if there's a tie with the first element. This is backwards; we want to compare by
the count of `True` elements, so we swap them. Anyway:

* swap all of the elements
* call `minimum` to find the element with the smallest count

Lastly, we examine the count to see if it's 0. If it has, the computation has
failed and we return `Nothing` (which is distinct from 0. Nonexistence is a
very powerful concept!). Otherwise, we return the column index (inside the
other branch of the `Maybe` type`.

If it really is that important to preserve the original structure, I can also
use `minimumBy`, which contains a custom `Ordering` function. This is
additional visual noise and is unnecessary here.

> `findRows`

`!!` is an infix operator:

```hs
xs !! n
```

is the same as

```
xs[n]
```

in other languages.

> `commonElement`

We `zipWith` the Boolean `&&` operator on the lists of elements, and we call
Boolean OR on the resulting list to find if any of them are true.

> `dropCols`

Part of Algorithm X is to remove columns from other lists, so if an element
of `v1` is `True`, we remove that corresponding element from the list of `v2`.

> `algorithmX`

Note the type that we're returning. We're returning the *labels*, since the actual
content of the rows is "eroded" over time as we remove rows and columns.

* We call `chooseColumn` on the remaining rows.
* If it gets a column, (meaning that there is a non-empty column but with a minimum of `True`s)
  * Select all rows that have a `True` in that column
  * For each row `r`, recurse by calling `pareRow r` on the rows, and cons `r` with the resulting list of `Row`s.
* At the end, we just want the `label`s.

That recursive step is very nasty and requires some squinting to conceptualize.
Contorting your brain to figure out that kind of recursion is hard and is one
of the biggest reasons why functional programming languages are not more popular.

I personally really like the fact that this function works by induction.

* Successful base case
* Failing degenerate case
* Recursive case

Assuming that you've defined your types correctly, this is actually kinda hard to screw up
the logic for; there are only so many ways that you can apply these functions in ways that
line up with the types.

# Sudoku.hs

>     data Mark = Mark {row :: Int, col :: Int, number :: Int} deriving (Show, Eq, Ord)

As mentioned before, this is going to be our `label` type for our `Row`s.

I'm indulging in some silliness by having it derive `Ord` (which requires me to
derive `Eq` as well. You can't have comparison without equality). By default,
the generated implementation of `Ord` treats it as a tuple `(row, col, number)`. If
I have a completed puzzle of `Mark`s, sorting the `Mark`s this way produces all of the
1st row, followed by all of the 2nd row, and so on, and I can thus write the puzzle
by chunking 9-element lists from the list of `Mark`s.

> `genConstraint` functions

Each of these sections of constraints are generated separately so that I can test
them. I then concatenate them together when I actually generate a `Row` for each
`Mark`.

> `parseMark`

I need to generate existing `Mark`s when given an initial puzzle.

I forgot that I did some Haskell Monad Madness in here. In this
context, `return` is turning a single `Char` into a `[Char]` (a string)
which can then be parsed by `readMaybe` into a `Maybe Int`. Then
we call `Mark r c` on that `Maybe Int`.

So suppose that we call `parseMark 3 5 ' '` (a space). It goes

    ' ' -> " " -> Nothing

since we can't parse a number from a `" "`. Then attempting to map `Mark 3 5` onto
that `Nothing` returns `Nothing`.

Suppose that we call `parseMark 3 5 '9'. It goes

    '9' -> "9" -> Just 9 -> Just (Mark {row=3, col=5, number=9})

Note that there is a method to the madness of the weird infix operators like
`<$>`. They attempt to duplicate the same composition syntax that you've seen,
but inside the context of the functor / applicative / monad type.

> `parseMarks`

All of the `zipWith` stuff is breaking up each string into characters and zipping
them with the indices so that the top left character is called with `parseMark 1 1`,
the second character in the first string is called with `parseMark 1 2`, and so on.

`mapMaybe` removes all of the `Nothing`s and extracts all of the `Just (Mark)`s into
a list of `Mark`s.

> `applyMark`

This pre-does a step of Algorithm X on the `Mark`s that we know are in the set
(since they come with the puzzle), returning a smaller list of `Row`s.

> `applyMarks`

The folding operation here calls `applyMark` over and over again on an
*accumulator* (the grid of `Row`s) and a list of `Mark`s, one after the
other.

> `solvePuzzle`

* Parse the strings into a list of `Mark`s.
* Generate the initial grid of `Row`s, and apply those initial `Mark`s to it.
* Solve the puzzle with `algorithmX`.
* For each solution, concatenate with the initial marks (since `algorithmX`
starts with those marks already applied and doesn't track them) and sort them.

# Main.hs

The biggest goal of Haskell is for **side effects to be separate from logic**.

All of the Sudoku logic should be totally separate from the stuff that actually
reads files and prints stuff to the screen. Haskell enforces this by requiring
all IO to be performed inside the context of the `IO` monad. This makes
everyone Very Mad.

> `unifyFileInput`

Unix utilities that take a filepath as input should also do the following:

* If no filepath is provided, read from stdin.
* If a dash `-` is provided, read from stdin.
* Otherwise, open the file and read from the file descriptor.

Haskell's structural pattern matching makes this very easy.

> `getPath`

This accesses `getArgs`, which is Haskell's version of `argv`.

> `getHandle`

This is our first exposure to the monadic bind operator `>>=`.

* We call `getPath` on our path, producing an `IO (Maybe String)`.
* `unifyFileInput` takes a `Maybe String` and, itself, returns an `IO Handle`.
* The monadic bind operator extracts that `Maybe String` from the context of
  the `IO` monad and calls `unifyFileInput` on that value, which puts it back
  into the context of the `IO` monad.

It's worth noting that `Maybe` is also a monad. In more complicated cases that
combine `IO` and `Maybe`, it is common to use a *monad transformer*, which provides
more convenient syntax for nesting the two monads.

> `main`

The `do` syntax is syntactic sugar for calling the `>>=` operator on a lambda
function. Ewwwww

```hs
do
    handle <- getHandle
    solutions <- solvePuzzle . lines <$> hGetContents handle
    putStrLn (show . length $ solutions)
    mapM_ (putStrLn . showSolvedPuzzle) solutions
    hCloseHandle
```

is equivalent to

```hs
getHandle >>= \handle ->
    solvePuzzle . lines <$> hGetContents handle >>= \solutions ->
        putStrLn (show . length $ solutions) >>
        mapM_ (putStrLn . showSolvedPuzzle) solutions >>
        hCloseHandle
```

In this context, you might be able to see why the bind operator is called
"bind." It is doing variable assignment!

The `>>` operator is called "then." It evaluates the left side, discards it,
and returns the right side. It's a way of "sequentially combining" monadic
values - in this case, printing the number of solutions, printing
the solved puzzles, and then closing the input file.

`do` syntax is very useful for implementing imperative logic for stuff like IO.
It makes people Very Mad when you use it in non-imperative contexts to obfuscate
the use of the "bind" and "then" operators. For example, it turns out that
lists are also monads, which lets you do

```hs
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    return (x, y)
```

which is syntactic sugar for

```hs
cartesianProduct xs ys = 
    xs >>= \x ->
        ys >>= \y ->
            return (x, y)
```

This is Very Legal and Very Cool, but will make your friends and enemies unhappy.
