# Fellowship Prover

These are the sources for the Fellowship prover written by Florent Kirchner and Claudio Sacerdoti Coen. 
The prover is an implementation of the $\bar{lambda}\mu\tilde{\mu}$-calculus [due to Pierre-Louis Curien and Hugo Herbelin](http://pauillac.inria.fr/~herbelin/publis/icfp-CuHer00-duality+errata.pdf).
The system and the theory behind it is described extensively in Florent Kirchner's [PhD-thesis]([)](https://pastel.hal.science/pastel-00003192v1/document).

## Setup

Clone the directory to a destination of your choosing.
Navigate to one of the available versions in the tags subdirectory, e.g. 'fellowship-0.1.0'.
Use 'make' to build (requires OCaml).
As of late 2024, the system still works on Linux (tested on WSL) and MacOS

## Usage

Use ./fsp to run the prover. Use 'help.' to get help. For further questions refer to Florent Kirchner's [PhD-thesis]([)](https://pastel.hal.science/pastel-00003192v1/document), especially chapter 7.
The tests folder contains some examples to run.
