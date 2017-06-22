This functional animation library (FAL) is courtesy of Paul Hudak and others.

It comes from the book's website:

	http://www.cs.yale.edu/homes/hudak/SOE/

The examples from the lecture are all found in Fal.hs. To run them, you need 
to install the GLFW library in ghc, like this:

	cabal install GLFW

Apparently, this does not work well with Mac OS X (see the "software" section on the
website above).
						--- cxl, 17.06.2014

With (K)Ubuntu 14.10, GLFW can also be installed as a package (libghc-glfw-def and libghc-glfw-prof).

From within ghci, load Fal.hs and use 
> run "Windowname" b
to run FAL animations b of type Behavior Picture.

						--- cxl, 21.04.2016

