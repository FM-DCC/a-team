# Animator of A-Teams

Animator of an experimental semantics for asynchronous Team Automata, using the Caos framework.
This project contains all the source code, and a pre-compiled version can be executed from the static web-page stored in the `docs` folder, by accessing the link:

 - https://fm-dcc.github.io/a-team/

# Caos

This project uses and the Caos's framework, placed at `lib/caos`. More information on it can be found online:

 - Caos' GitHub page: https://github.com/arcalab/CAOS
 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 

When cloning the project for the first time you need to load the submodule with CAOS by typing

> git submodule update --init
 

## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to compile this project using the ScalaJS plug-in, following the steps below.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `sbt fastLinkJS`
2. open the file `lib/tool/index.html`
