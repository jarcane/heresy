Heresy 
======
[![Build Status](https://travis-ci.com/jarcane/heresy.svg?branch=master)](https://app.travis-ci.com/jarcane/heresy)

Heresy is a BASIC-inspired functional dialect of Lisp, currently implemented as a Racket language.

Heresy aims to provide a simple, semi-pure functional Lisp language that is nevertheless familiar to programmers of BASIC, by providing a combination of familiar control structures and features with purely functional execution.

The official documentation and reference can be found here:  http://pkg-build.racket-lang.org/doc/heresy/index.html

Heresy is chiefly written by myself, with considerable contributions from Alex Knauth, and several others in the Racket community. The code Copyright 2014 by Annaia Berry, and is licensed via the LGPL v3.

Installation
------------

To install:

``raco pkg install heresy``

To use, append this to the start of your file in DrRacket or your favorite text-editor:

``#lang heresy``

Contributing
------------

Pull requests should be made against the `develop` branch. `master` is reserved for the current release, as it is the source for the raco package for Heresy.
