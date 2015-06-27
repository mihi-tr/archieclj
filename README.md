# archieclj

A Clojure parser for [archieml](http://archieml.org).

Archieml is a markup language designed to be written by humans for
computers. It is very forgiving and highly welcoming. Developed at the New
York Times its main audience are Journalists who do require to write
structured data but do not want to bother with JSON or yaml. 

This is a Clojure parser for Archieml. 

## Usage

In leinigen add

[![Clojars
Project](http://clojars.org/archieclj/latest-version.svg)](http://clojars.org/archieclj)

Then use

```clojure
(require '[archieclj.core :as archieclj])

archieclj/parse("key: value")
```

## Contribute

Contributions are highly welcome! You can contribute by

* Using the library and providing feedback in github issues
* Writing better Documentation
* Fixing bugs and Issues 
* Discussing Issues and solving them (some might not need code)

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
