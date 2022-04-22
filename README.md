# Overview

This is a Haskell library for computing obstructions to symplectic embeddings using a certain combinatorial criterion developed by Michael Hutchings in his paper 
["Beyond ECH Capacities"](https://arxiv.org/abs/1409.1352). These obstructions are derived from embedded contact homology (ECH) and are stronger than the 
obstructions obtained from ECH capacities. There are several known cases where symplectic folding can be used to show that the obstructions produced by Hutchings' 
criterion are optimal.

I wrote this code in 2015 as part of a Research Experience for Undergraduates (REU) program at Columbia University. This research ultimately resulted in a [paper]
(https://arxiv.org/abs/1610.00566) (co-authored with Jo Nelson, and published under my former legal name) that extends some of Hutchings' results in "Beyond ECH 
Capacities."
[Note: this research was partially supported by NSF grants DMS-1206667, DMS-0970108, and a Graduate Research Fellowship.]

This version of the library is written in Haskell. Some of the combinatorics involved are very computationally intensive; rather than optimizing the Haskell code, I 
chose to rewrite it in C++, which resulted in a huge performance improvement. If you're interested in doing computations for research with this library, I recommend 
checking out the [C++ version](https://github.com/michristianson/ech-capacities-cpp/). However, if you just want to try out some of these combinatorial computations 
and see how they work, the Haskell version is a good choice, because it's easy to do computations interactively using GHCi.



# Installation

## Broad Strokes

Since this library was initially just intended for personal use, it was never
packaged for distribution in any nice way. Even so, it isn't too difficult
to set up. The library only has a couple of dependencies, all of which can be
installed using cabal-install. They are:

- cereal
- decimal
- combinat
- multiset-comb

Once these dependencies are installed, you should be able to import the modules
from this library and use their functions however you like. For most use cases,
importing the Obstruct module should be enough, as this module exports all
objects and functions that the library intends to provide. If you want to extend
the library's functionality or otherwise work under the hood, you may also need
to import LessThans, which has a couple internal functions that are not exported
by Obstruct.

If you want to compile this library, I highly recommend using the -O2 flag for
GHC. The compile time is still on the order of seconds, but the performance
increase is quite noticeable.

For a description of the all functions offered by this library, see the "Core
Modules" section below.

## Step-by-Step

Here are step-by-step instructions on how to get this library working.

1. Install GHC, the standard Haskell compiler, and cabal-install, the standard Haskell package manager. Both are available on almost all operating systems using a 
   (mostly) automated installation process called GHCup; see [this page](https://www.haskell.org/ghcup/install/) for details.

2. Download (or clone) this git repository to some directory on your computer.
   On the main page of the repository on Github, this can be done by clicking the green
   "Code" button at the top and selecting "Download Zip"; then, unzip the
   downloaded file and put it wherever you'd like to store this library. Alternately, if
   you have git installed, you can run the following command on a command line:

   git clone https://github.com/michristianson/ech-capacities-haskell target/directory/on/my/computer

3. Install dependencies for the library by running the following command on your
   system's command-line interface:

   cabal install cereal decimal combinat multiset-comb

## An Example Computation

After the above steps, the library should be set up for you to use. To do
anything sophisticated with it, you will probably want to learn the basics of 
Haskell (perhaps by following [this tutorial](http://learnyouahaskell.com/introduction) or any of the others that may be found online).
If you don't want to learn any Haskell, the following steps will walk you through an example computation.

1. Open your system's command-line interface and navigate to the directory where
   you downloaded this library in Step 2 above. (If you don't use the
   command line much, the command for switching directories is:
   "cd directory/name".)

2. Type "ghci" and hit enter. You should get a prompt that looks like this:

   Prelude>

   Type ":l Obstruct" and hit enter. You should get some messages that say
   "Compiling Internals," "Compiling IO," etc. When it's done, the prompt should
   change to:

   \*Obstruct>
   
   If you get any errors, make sure you've completed all of the installation instructions correctly,
   then try again.

3. Now all of the functions in the library should be available for interactive
   use on this prompt. First, enter the following command:

   \*Obstruct> cg = makeCGen (pathFromEdges [(1,0), (3,-3)]) [E, E]

   This command creates a convex generator and stores it in the variable cg.
   The list after pathFromEdges should be a list of edges in the convex
   generator, and the list at the end of the command should be a list of labels
   for each edge (either "E" or "H").

   To confirm what convex generator we made, we can use cg as a command by itself:

   \*Obstruct> cg<br>
   E(1,0)3E(1,-1)

   The line below the prompt is the output that you should get. This line is the
   library's way of writing the convex generator
   e<sub>1,0</sub>e<sub>1,1</sub><sup>3</sup>
   (we avoid using subscripts and superscripts, because they're not so easy on
   a command line, and we use capital "e"s or "h"s because it's easier due to
   some details about how Haskell code works).

4. Next, let's make some convex toric domains and store them in variables. This
   works just like with cg above, except that the commands to make a convex
   toric domain are a bit different. Try this:

   \*Obstruct> ctd1 = Polydisk 2.43 1<br>
   \*Obstruct> ctd2 = Ellipsoid 3.215 3.215

   This makes ctd1 the polydisk P(2.43, 1) and ctd2 the ball E(3.215, 3.215).

5. We can start by finding all the convex generators that are less than or
   equal to cg with respect to ctd1 and ctd2. To do this, enter:

   \*Obstruct> lessthans = generateLessThan defaultHeuristics ctd1 ctd2 cg

   Note that "defaultHeuristics" is an object defined by the library that
   specifies certain heuristics for the less than computation, and ctd1, ctd2,
   and cg specify the convex toric domains and the convex generator we're using.
   These 4 objects are all parameters of the generateLessThan function, and
   their order does matter!

   The variable lessthans now holds a list of all the convex generators less
   than or equal to cg. To view this list:

   \*Obstruct> lessthans<br>
   [2E(1,0)E(8,-1),3E(1,0)E(6,-1)]

   This list is a bit hard to read as is (especially for convex generators that
   have a lot of less thans). To print out one list item per line,
   we can use the following command instead:

   \*Obstruct> mapM_ (putStrLn . show) lessthans<br>
   2E(1,0)E(8,-1)<br>
   3E(1,0)E(6,-1)

   (A brief explanation of this command: the mapM_ function means "do something
   to every element of a list," the (putStrLn . show) means "write an object
   on its own line," and lessthans is the list.)

   Alternately, if we just want the number of less thans, we can use the length
   function:

   \*Obstruct> length lessthans<br>
   2

6. If we don't just want to do "less than" computations but instead want to look
   for an obstruction to a symplectic embedding, we can use the functions thm119
   or thm119'. (These are named after Theorem 1.19, which is the theorem giving
   the combinatorial criterion in Hutchings' paper "Beyond ECH Capacities.")
   For instance:

   \*Obstruct> thm119 ctd1 ctd2 cg<br>
   Just ([E(1,0)3E(1,-1)],[2E(1,0)E(8,-1)])

   This means that we did not find an obstruction, because valid convex
   generators as in Theorem 1.19 of Hutchings' paper do exist. Specifically, those
   convex generators are given in two separate lists after the word "Just": they
   are E(1,0)3E(1,-1) and 2E(1,0)E(8,-1). In general, these two lists
   may have many elements, because Theorem 1.19 refers to factorizations of
   convex generators into other convex generators; in this case, however,
   the factorization of cg is just cg itself, so we only get one convex
   generator in each list.

   Alternately, we can use the thm119' function to get a list of all possible factorizations that satisfy
   Theorem 1.19. For instance:

   \*Obstruct> decomps = thm119' ctd1 ctd2 cg
   
   Entering decomps would now give a whole list of possible factorizations, but
   they might be a bit difficult to read. To make them a bit more readable, we
   can use the same mapM_ trick that we used with lessthans in Step 5:

   mapM_ (putStrLn . show) decomps<br>
   ([2E(1,-1),E(1,0)E(1,-1)],[E(4,-1)E(3,-1)])<br>
   ([2E(1,-1),E(1,0)E(1,-1)],[E(4,-1)4E(1,0)])<br>
   ([E(1,0)3E(1,-1)],[3E(1,0)E(6,-1)])<br>
   ([E(1,0)3E(1,-1)],[2E(1,0)E(8,-1)])<br>

   (This is still not so easy to read, but cleaning it up more would require
   a more complicated Haskell command; if you don't know Haskell, it's probably
   easier to just copy-paste into a text editor at this point and then reformat
   to your own liking.)

   The above output show 4 different possible factorizations that satisfy
   Theorem 1.19. The first 2 factorize cg as

   E(1,0)3E(1,-1) = 2E(1,-1) * E(1,0)E(1,-1),

   and the second 2 factorize cg as just itself. In all cases, the first list
   is the factorization of cg (which, in the terminology of Hutchings' theorem,
   would be the factorization of lambda'), and the second list contains one "less
   than" for each element of the first list, subject to certain other combinatorial
   restrictions (in the terminology of Hutchings' theorem, this is the
   factorization of lambda).

7. In this case, we didn't find any obstruction. If we make ctd2 "smaller,"
   however, we can expect that ctd1 won't be able to embed in ctd2, and we will
   get an obstruction. For instance, try changing ctd2 by entering

   \*Obstruct> ctd2 = Ellipsoid 2.8 2.8

   Now, if we try to generate our obstructions, we'll get:

   \*Obstruct> thm119 ctd1 ctd2 cg<br>
   Nothing

   The fact that we got "Nothing" here instead of "Just . . ." means that no
   convex generators satisfying Theorem 1.19 were found, so the theorem
   implies that ctd1 cannot be symplectically embedded into ctd2. Alternately,
   if we use thm119' In this case, we should get an empty list (again,
   indicating no valid decompositions:)

   \*Obstruct> decomps = thm119' ctd1 ctd2 cg<br>
   \*Obstruct> decomps<br>
   []

   (Note: in this case, the mapM_ command would actually give no output at all,
   because the list is empty. Another option would be to use the command
   "length decomps" and check that the output is 0.)



# Modules

Here is a description of all the modules currently available in this library. 

## Automation Modules

These modules function more or less as "scripts," i.e. main points of entry to automate certain tasks.

1. Benchmark: tweak numbers in Benchmark.hs and run to benchmark main computations for the program.

2. RunScans: tweak numbers in RunScans.hs and run to "scan" for obstructions to a specific problem. Currently set up to scan for obstructions to embedding a certain
   polydisk into a ball.

3. Sandbox: temporary script for trying out new calculations. Currently set up to output a bunch of "less than" computations to a file as a test of less than 
   generation.

## Core Modules

These modules contain the main functionality of the library, i.e. all the objects and computations necessary for Hutchings' combinatorial criterion. Note that 
everything intended for use by any user of this library is exported by the Obstruct module, so using "import Obstruct" should give access to everything described in 
this section.

### IO
Defines commands to output the results of certain functions to the command line and/or to files. The only functions intended for external use here are 
related to the output of the scanning functions (see the Obstruct module below):
- removeOffsets: can be used to replace offsets of the doubles that construct
  ctd2 with the actual values of those doubles.
- scanToStd: prints scan results on the command line.
- scanToFile: prints scan results to a file. The first parameter is the file
  name; the second is True to overwrite the file and False to append to it.
- scanToFileAndStd: works just like scanToFile, except that output is also printed on the command line. 

### Internals
Defines the main data types used in the project and provides basic operations on these types. These types are:
 
1. Vertex and Edge: pairs of integers representing a vertex or an edge of a path.
2. CIP: short for convex integral path. Can be constructed from lists of
   vertices or edges using the functions pathFromEdges or pathFromVerts.
3. CGen: short for convex generator. Can be constructed in 4 ways:

   1. makeAllEGen path: makes a convex generator with CIP path and all edges labelled "E".
   2. makeCGen path labels: makes a convex generator with CIP path and whose edges are labelled with the elements of the list labels (in order from left 
       to right).
   3. readCGen str: makes a convex generator whose string representation is str. For instance, try: readCGen "E(1,0)2E(1,-1)3E(0,-1)".
   4. cg1 <> cg2: creates the convex generator given by multiplying cg1 and cg2 together.

   Some useful functions to compute various properties of a CGen:
   - getX cg: returns x(cg).
   - getY cg: returns y(cg).
   - getM cg: returns m(cg).
   - getL cg: returns L(cg).
   - getH cg: returns the number of edges of cg labelled "h."
   - index cg: returns I(cg).

4. CTD: short for convex toric domain. Can be constructed in 3 ways:

   1. Polydisk a b: constructs the polydisk P(a,b).
   2. Ellipsoid a b: constructs the ellipsoid E(a,b).
   3. CTD path: constructs the CTD bounded by the CIP path.

   Some useful functions related to CTD's:
   - action ctd cg: computes the action of the CGen cg with respect to the CTD ctd.
   - generateMinimal ctd i: generates all minimal CGen's for the CTD ctd that
     have index equal to i.
 
 5. Decomposition: a pair of the form ([CGen],[CGen]), i.e. a pair of lists of
    CGens. These are meant to represent the factorizations that arise from
    Hutchings' Theorem 1.19, with the first list in the pair representing the
    factorization of lambda' in the theorem, and the second list representing
    the factorization of lambda.

Internals also has several miscellaneous functions for general use:
- isLessThan ctd1 ctd2 cg1 cg2: returns True if cg1 is less than or equal to
  cg2 with respect to ctd1 and ctd2 and False if not.
- noSharedEOrbits cg1 cg2: returns True if cg1 and cg2 have no elliptical
  orbits in common and False otherwise.
- bullet2 decomp: returns True if bullet 2 in Hutchings' Theorem 1.19 is
  satisfied by decomp and false otherwise.
- bullet3 decomp: returns True if bullet 3 in Hutchings' Theorem 1.19 is
  satisfied by decomp and false otherwise.
- validDecomposition ctd1 ctd2 decomp: returns True if decomp satisfies all
  the conditions in Hutchings' Theorem 1.19 and False otherwise.
- generateI i: generates a list of all CGen's that have index i and have all
  edges labelled E. Currently not in use in any other part of the library.

### LessThans
Provides the function generateLessThan, which returns a list of all convex generators less than or equal to a given convex generator. Because
this is the most computationally expensive part of the library, the module also provides Heuristics objects that can be used to try to speed it up. 
The only heuristic used in the current version of the code is an upper bound on the value of y(lt) for any
lt <= cg. (Other heuristics existed in other versions; see the section "Memoizer Versions" below for details.)

The module provides the following ways to set the upper bound on y(lt):
- defaultHeuristics: a Heuristics object that uses no upper bound on y(lt).
- thm114Heuristics: a Heuristics object providing a relatively good upper
  bound on y(lt) for polydisk-into-ball embedding problems. More
  specifically, thm114Heuristics should only be used in the command

  generateLessThan thm114Heuristics ctd1 ctd2 cg

  assuming that
  1. ctd1 = Polydisk a 1 for some choice of a,
  2. ctd2 = Ellipsoid c c for some choice of c, 
  3. cg = dE(1,-1) for some integer d, and
  4. a >= 1 and c <= 2+a/2.
- useMaxY: the command "useMaxY m defaultHeuristics" returns a Heuristics
  object whose upper bound on y(lt) is the number m (assuming m >= 0).
- useMaxYFn: given a function fn of type (CGen,CTD) -> Int, the command

  generateLessThan (useMaxYFn fn defaultHeursitics) ctd1 ctd2 cg

  will use as the output of fn(cg,ctd1) as the upper bound on y(lt).
- noMaxY: given a Heuristics object h, the output of (noMaxY h) is a
  Heuristics object that uses no upper bound on y(lt).

### Obstruct
Contains the main functions that can be used to compute obstructions to symplectic embeddings. These come in a few varieties.
1. Auxiliary functions
   - factorizations cg: computes all factorizations of the CGen cg. Mainly
     intended for use by other functions in this module.
   - decomposition ctd1 ctd2 cg: computes all decompositions that satisfy the 
     first bullet of Hutchings' Theorem 1.19. Mainly intended for use by
     other functions in this module.
2. Basic obstruction
   - obstruct and obstruct': compute whether an obstruction is obtained by a specific choice of CGen. obstruct returns Just ([factorization1],[factorization2]) 
     if no obstruction is found and Nothing if an obstruction is found; obstruct' returns a list of all possible pairs ([factorization1],[factorization2]), which 
     will be an empty list if an obstruction is found. Note that the pair ([factorization1],[factorization2]) is an example of a Decomposition as defined in 
     Internals. In particular, using the terminology of Hutchings' original theorem, factorization1 is the factorization of lambda', 
     and factorization 2 is the factorization of lambda.
   - thm119 and thm119': the same as obstruct and obstruct', but they use default heuristics (instead of a user-specified Heuristics object from the LessThans 
     module). In other words, calling them119 or thm119' is the same as calling obstruct or obstruct' with the defaultHeuristics object.
   - thm114 and thm114': the same as obstruct and obstruct', but specific to the question of embedding certain polydisks into balls. (Note: the polydisk-into-
     ball problem is Theorem 1.4 in Hutchings' original paper, so these functions probably should have been named thm14 and thm14'.) 
     More specifically, the command

     thm114 a c cg

     looks for obstructions to the embedding of Polydisk a 1 into Ellipsoid c c using the convex generator cg and the Heuristics object thm114Heuristics.<br>
     **Warning:** not all values of a, c, and cg will produce valid results here! See the description of thm114Heuristics above for restrictions.

3. Scans
   - normalScan: attempts to find obstructions for a range of possible CTD's using a range of possible CGens. To accomplish this, it takes in functions of 
     type Double -> CTD and then lists of doubles to feed into these functions. These functions are bundled into an object called a ScanDomain. 
     See RunScans.hs for an example (with comments in the code) of how to construct a ScanDomain and use it in a scan. (RunScans.hs uses squeezeScan, but
     the usage of normalScan is identical; the only difference is in how the functions conduct the scan.)
   - squeezeScan: like normalScan, but it skips certain computations by assuming that larger doubles produce "larger" CTD's. This assumption is always true for
     the polydisk-into-ball problem, and these "scan" functions were based on that problem: in the code, the doubles that construct ctd are named "a" 
     (reminiscent of the polydisk P(a,1)) and the doubles that construct ctd' are named "c" (reminiscent of the ball E(c,c)).

4. Bounding: some experimental code at the end of Obstruct.hs (currently commented out and probably non-functional) defines functions boundNormalOver and 
   boundSqueezeOver. These are meant to behave similarly to normalScan and squeezeScan, except that, given a choice of ctd, they aim to compute bounds on the 
   parameters defining some ctd' such that we can obstruct embeddings of ctd into ctd'. In the polydisk-into-ball problem, this would mean, given a polydisk like 
   P(2.51,1), putting some bound on the number c such that we can obstruct an embedding of P(2.51, 1) into the ball E(c,c).



# Other Files

## Memoizer Versions

The "Memoizer Versions" folder contains versions of the modules IO, LessThans,
and Obstruct that allowed for another heuristic for the less than search:
namely, a memoizer that could record and look up the results of calls to
generateLessThan. This greatly increases the performance of
obstruction-searching functions like obstruct and thm119: the most expensive
part of those functions is the call to generateLessThan, so we never want to
have to do that call more than once for the same parameters.

Unfortunately, I chose to use a MySQl database to store these memos, with an
ODBC driver to communicate with the Haskell code via the Haskell packages HDBC
and HDBC-odbc. This turned out to be overkill for this use case, and it also
turned out to be not very portable. So I removed all memoization from the
current version of the library.

If you want to restore this memoization functionality, I think you could do so
with a little work. Here's what you would need to do:

1. Replace IO.hs, LessThans.hs, and Obstruct.hs with the versions of these files
   in the "Memoizer Versions" folder.

2. Run "cabal install HDBC HDBC-odbc" to install dependencies for the
   memoization.

3. Set up a relational database with an ODBC driver and no tables in it.
   The code will request a connection with DSN name "CIPConn", then it will
   automatically create a table named "lessthans" and store all of its
   memoization data in it.

Alternately, it might be easier to scrap the database memoization and write
your own memoization using flat files (this is what the C++ version of the
library does). For this, you won't need the memoizer version of IO.hs (which just
adds a bunch of database I/O commands), but you might want to use the memoizer
versions of LessThans.hs and Obstruct.hs. Specifically, the additions to the
memoizer versions of these files are:

- LessThans.hs has Heuristics objects set up with LTMemoizer objects and has
  appropriate combinators for changing the different heuristics.
- LessThans.hs has a getLTs function, which takes care of all lookup and storage
  of memos using some helper functions. This function should be used instead of
  generateLessThan when memoization is desired.
- Obstruct.hs uses getLTs instead of generateLessThan for obstruction searches,
  and exports some of the stuff from LessThans.

If you make use of the memoizer versions of these files, you'll likely only need
to change the definition of an LTMemoizer object in LessThans.hs, plus some of
the functions that manipulate these objects in the same file (e.g.
memoizeLessThans, updateLTMemoizer, initializeLTMem, etc.)

## Scan Results

The "Scans" folder contains text files with the output of various scans that I ran in the past (run using the scanning functions in the Obstruct module). These 
scans were all performed for obstructing the embedding of P(a,1) into E(c,c) by using a convex generator of the form dE(1,-1) for some integer d (in Hutchings' 
original notation, this would be <img src="https://render.githubusercontent.com/render/math?math=e_{1,1}^d">). Most of the data gathered by these scans is subsumed 
by the results in the paper I co-authored, which can be found [here](https://arxiv.org/abs/1610.00566). In the paper, we demonstrate that Hutchings' criterion 
produces optimal obstructions for <img src="https://render.githubusercontent.com/render/math?math=2 \leq a < \frac{\sqrt{7} - 1}{\sqrt{7} - 2} \approx 2.549">. 
However, some of the scans in here do obtain (non-optimal, but still close to optimal) obstructions for larger values of a.

The Scan output files are structured as follows. Each file is named according to the values of d used in the scane. For instance, Scan1-14.txt contains the results 
of the scan for every value of d from 1 to 14 (inclusive). The content of the files is arranged like tables, with 1 scan result on each line and 4 columns 
(separated by whitespace) to describe the scan and its result. (Some of the files also have "titles" for these columns as the first line, which are: CGen, a, c, and 
Result.) The data in these 4 columns, from left to right, is as follows.

1. The CGen used to search for an obstruction. In other words, this
column specifies the value of d.
2. The value of a used to define the polydisk P(a,1).
3. The value of c used to define the ball E(c,c), or the offset from c = 2+a/2 used (e.g. -0.01 means c = 2+a/2-0.01).
4. The result of calling obstruct with these parameters. If this result is None, then an obstruction is found; if it is a list of CGens, or has type Just CGen, then 
   no obstruction was found.

In some scan files, not every line is displayed. Instead, these files have, for each value of c, and smallest value of a such that an obstruction was *not* found. 
The implication is that an obstruction *was* found for every smaller value of a. For instance, the last line of Scan28.txt has a = 2.48, c = -0.01, and then a Just 
CGen object. This means that an obstruction was not found for c = 2+a/2-0.01 when a = 2.48, but for every value of a that is less than 2.48, an obstruction was 
found for c = 2+a/2 - 0.01. Note that these tests were run with steps of 0.01 in both a and c, so the values of a that are less than 2.48 were 2.47, 2.46, 2.45, 
etc.

The file "Scan Results.txt" contains a summary of all these findings. For each value of d, this file gives two numbers (again arranged as columns in a table):

1. The "Optimal" column contains the largest value of a for which an obstruction was found for c = 2+a/2 - 0.01. Note that this generally means obstructions will be 
   found for any smaller value of a and any c <= 2+a/2 - 0.01.

2. The "Any" column contains the largest value of a for which an obstruction was found for c = 2+a/2 - 0.05. Since c = 2+a/2 - 0.05 was the smallest value of
   c considered in these scans, this means that for larger a values, no obstructions will be found at all (or at least, none within 0.05 of 2+a/2, which is 
   what I was interested in).

## Documentation

This readme serves as the most complete form of documentation for this library. In particular, the "Core Modules" section above describes all of the functions 
provided by the library. This is probably the best place to start. If you don't know how to use a specific function, you can always enter ":type fn" in GHCI to get 
its type signature. If you know how Hutchings' combinatorial criterion is supposed to work, the type signature plus the description in "Core Modules" above is 
likely enough to understand how everything works. Alternately, most functions in the library also have comments that describe what the function is supposed to do. 
If you don't mind looking at Haskell code, this is another good way to find out what functions exist, what they do, and what their type signatures are.

HTML documentation for some of the code has been generated from the comments using Haddock. This documentation is very old, and everything in it is at least 
partially covered in the "Core Modules" section above. However, it is still largely accurate, and it is cleaner than both the code itself and this readme.

This old HTML documentation exists in the "Old Documentation" folder. If you want to view it, make sure the entire folder is downloaded locally, and then open 
index.html. Please note the following corrections to this old version of the documentatoin:

- The CIP module no longer exists; it has been split into Internals, LessThans and Obstruct. However, all of the objects and functions described in the 
  documentation for this module do still exist, and all of them are exported by the Obstruct module. In other words, anything in the CIP module documentation should 
  still be usable as stated, provided that you import Obstruct.

- The Scans module no longer exists. Currently, scanning functions reside in Obstruct; for the most part, these are the same as the ones in the old Scans module, 
  except that the functions and their arguments have different names. If you want to understand these scanning functions, it could be helpful to compare this 
  documentation again the type signatures of the functions normalScan and squeezeScan in Obstruct.hs. It might be even more helpful, however, to just look at 
  RunScans.hs for an example (with comments in the code) of how to use squeezeScan. (normalScan's usage is the same; the only difference is in how the functions 
  conduct the scan.)



# Licensing

This library is provided under the MIT license. See LICENSE.txt for details. The gist of it is: feel free to do anything you want with this code, provided you 
retain the information in LICENSE.txt whenever you make copies of it.
