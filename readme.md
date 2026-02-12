A-Team: Animator of Asynchronous Team Automata
=================

This tool accompanies a paper entitled _"Asynchronous Team Automata"_, accepted at [FM 2026 symposium](https://conf.researchr.org/home/fm-2026). This tool is web-based, compiled to JavaScript, and can be executed by opening a provided HTML file that loads a companion JavaScript file. The tool is developed in Scala and uses our CAOS libraries (https://github.com/arcalab/CAOS).

The A-Team tool can load an input program, describing a network of communicating processes, present graphical step-by-step animations, perform traversals of the full state-space, and perform some static analysis (e.g., type checking and search for undesirable states).

This repository contains the latest version the source code, and a snapshot of the tool can be immediately loaded by opening the associated GitHub pages.

 - https://fm-dcc.github.io/a-team

A more permanent version with the source code and a snapshot of the tool can be found in https://doi.org/10.5281/zenodo.18601857.


----------
# Overview

This readme describes two ways to run/recompile/change the tool:

- Running the precompiled version
- Compiling the source code


------------------------
# How to run A-Team


1. Open the link https://fm-dcc.github.io/a-team; this will open the file at `docs/index.html`. Alternatively download the repository and open `docs/index.html`.

   The left panels provide the input interface. It consists of an input form to write the A-Team program (widget "Input program") and buttons to load existing examples (widget "Examples"). By default, the first example "coffee-sync" is loaded. The right panels provide the output interface.

2. Initially three right widgets are expanded:
   - "Well-behaved?" describes analyses over the full state-space (e.g., search for deadlocks, orphan messages, and simple unbounded loops).
   - "Run Semantics" describes the initial state of the system on the right side, and includes buttons on the left side - one for each possible step. Pressing any of these buttons makes the system evolve by using the corresponding step. The "undo" button reverts the previous step.
   - "Local Components" draws a state machine describing the behaviour of each of the processes being executed concurrently.

3. The other two widgets are collapsed and can be expanded by clicking on the title (not the whole box; exactly the title). This will expand the corresponding widget and reload it.
   - "Build LTS" depicts the full state space (up to a fixed bound). States have no names and labels describe atomic steps: either a synchronisation step involving multiple agents, a sending or receiving of a message, or an internal action.
   - "Number of stats and edges" count the total number of states and transitions (up to a fixed bound).

4. Small extras can be found, e.g., a "View pretty data" widget at the bottom left part that re-prints the internal structure of the program, and a button "Hide comm-props" that modifies the behaviour of the "Well-behaved?" widget to hide some of the analyses.



------------------------------------
# How to compile and run using the source code

You will first need to compile the JavaScript file `lib/caos/tool/js/gen/main.js` and then open the HTML file `lib/caos/tool/js/gen/index.html`, which loads the compiled JavaScript.


## Getting CAOS

This project includes a git submodule at `lib/caos`. When cloning the project for the first time you need to load the submodule with CAOS by typing

> git submodule update --init

Alternatively, you can download the latest version of CAOS from `https://github.com/arcalab/CAOS` and place it inside the folder `lib/caos`.


## Compiling the tool

To (re)compile the tool, there are two approaches:

* Using JRE+SBT: 

    1. Install [SBT](https://www.scala-sbt.org) (Scala Building Tools) and [JRE](https://www.java.com/en/download) (Java Runtime Environment - tested with Openjdk version 17.0.17).
    
    2. Compile the code by opening a terminal in `ateam-src` and executing:

        ```
        sbt fastLinkJS
        ```

    3. Run the (re)compiled program by opening in a browser the file `ateam-src/lib/caos/tool/index.html`.

* Using Docker:

    1. Install [Docker](https://www.docker.com/get-started/).

    2. Build the Docker image and run the container by opening a terminal in `ateam-src` and executing:

        ```
        docker build -t tool .
        docker run -d -p 80:80 tool
        ```
    
    3. Run the (re)compiled tool by opening `localhost:80` in a browser (instead of `ateam-src/lib/caos/tool/index.html`).

    4. Stop the docker container by executing `docker stop <container-id>`, using the number produced by running `docker run (...)` above.
  
  **Note:** If the source code is changed, the image is rebuilt, and the container is re-run, some browsers require the cache to be cleared to properly show the new version of the tool.



## Changing the tool

To get a better feeling of how to modify the source code, we will explain how to make a simple change to the source code below. The explanations refer to lines in the source code from the commit [7226c72](https://github.com/FM-DCC/a-team/tree/7226c723c689576dbe5753850b284791027db765), and more recent versions may not be fully aligned.

The semantics of a system in A-Team is specified in the file `src/main/scala/ateam/backend/Semantics.scala`. We will explain how to edit this file to customise the semantics of A-Team. More concretely, we will modify the semantics of local components to add an extra rule: `a.P --(a*)--> a.P`. I.e., a process that can perform an action "a" and continue to behave like "P", will now also be able to perform a new (internal) action "a*" and return to the same state. The motivation is to make visible the local actions from each component, even if they are not ready to communicate.

  1. Open the file `src/main/scala/ateam/backend/Semantics.scala`.
  2. Go to line 292, inside the definition of `nextProc` (specifying how to calculate the next step of a local process in a given state).
  3. This line reads `case Prefix(act,p2) => Set(act -> p2)`, describing how to evolve a process that is a Prefix with action `act` and continuation `p2`: it can only perform the action `act` evolve to `p2`.
  4. Update this line to produce one more action:
     `case Prefix(act,p2) => Set(act -> p2, Act.IO(act+"*",Set(),Set()) -> p)`
     This will add the possibility of performing an internal action called `act` followed by `*` (the two empty sets denote the possible participants involved), and evolves to process `p` (which is the original process, introduced in line 289).
  5. Recompile the code as explained above in _"Compiling the tool"_.
  5. Open (or reload) the compiled A-Team.
  6. The first example `coffee-sync` will be loaded and you will see that the local components have new loops around the states. Note that some states have no loops, since the processes `Usr` and `coin!.tau.coffee?.Usr` (its definition) are syntactically different, causing the illusion of being different states.
  7. Press the "Build LTS" widget and you will also see the new actions with `*`, exposing the possible local actions at each state.

As a smaller second example of a modification, you can add new widgets by modifying the "widgets" variable in `src/main/scala/ateam/frontend/CaosConfig.scala`. Go to line 71 and introduce a new line above with the following code:

  `"Program.toString" -> view(prog => prog.toString, Text),`

After recompiling and reloading the tool as before, you will see a new widget on top of the right column called "Program.toString". Clicking on the title will reveal the name of the classes used to represent the internal object after parsing the given program, illustrating how the program is represented internally.


---
__Notes on the compilation process:__

  - This was not thoroughly experimented with different JVM versions, and our last version was tested with OpenJDK 17.0.7 (macOS and Fedora); we expect this to work with any JVM >= 8, but this is often a source of problems.

  - If you want to modify part of the code just to confirm that the compiled version is different, we recommend modifying the file `src/main/scala/ateam/frontend/CaosConfig.scala`. This manages the core layout of the web-frontend. You can modify, for example:
    + the `name` variable to change the title,
    + the `languageName` variable to change the header of the input widget,
    + the `widgets` variable to change one of the headers of a widget, or creating a new widget based on an example.

  - Internet will be likely required, due to how SBT works:
    + a precise SBT version will be fetched and used (the one mentioned in `project/build.properties`)
    + a precise pre-compiler of Scala will be fetched and used (following the Scala version mentioned in `build.sbt`)
    + a precise ScalaJS compiler for JavaScript will be fetched and used (from a Maven repository, following the details in `project/plugins.sbt`)
    + the concrete dependencies of the project will be downloaded from a Maven repository (following the `libraryDependencies` in the `build.sbt` file - e.g., a parsing library `org.typelevel/cats-parse/0.3.4`.)

 - the very first time compilation should take some time, to fetch all needed dependencies, which will be cached for future runs.

 - If you installed SBT only to compile this artefact, we recommend removing all the cached files at the end to save disk space: in our case this means removing the directory `$HOME/.sbt` (in macOS and Fedora).
