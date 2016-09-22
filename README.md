SPECifIC Tool
====

Graphical user interface to explore and integrate functional change management,
verification and NLP techniques from the SPECifIC project

------------

How to build:
----

For the change management we need several prerequesites:

1. [clang](http://clang.llvm.org/)
2. [SystemC](http://accellera.org/downloads/standards/systemc) 2.3.1 - has to be compiled with clang and the debug symbols enabled:
  * `tar zxf systemc-2.3.1.tgz`
  * `cd systemc-2.3.1`
  * `env CC=clang CXX=clang++ CPP="clang -E -g" .configure`
  * `make`
  * `make install`
  * `cd ..`
  * `sudo mv systemc-2.3.1 /usr/local/`

In addition you need [sbt](http://www.scala-sbt.org/download.html) installed for the build process (any version will do).

*Configuration:*
* Please configure the paths to clang, systemc, and dwarfdump in `/server/conf/application.conf` (use `/server/conf/reference.conf` as guideline)

To start the tool, follow these simple steps:

3. `cd` to this project directory (where `build.sbt` lies)
4. start server with `sbt run`
5. open browser and navigate to `localhost:9000`

------------

May 2015 - Martin Ring, DFKI Bremen <martin.ring@dfki.de>