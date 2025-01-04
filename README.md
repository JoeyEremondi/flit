This is the "flit" package. It it is based off of the `plait` language for Racket, and to a lesser degree `plai-typed` and `plai`. It is meant to accompany the book ["Programming Languages: Application and Interpretation"](https://www.plai.org/) by Shriram Krishnamurthi, specifically the second or third editions.

## Name 

The name stands for Functional Languages, Interpreters, and Types, the name of the course notes that use this language.

## Usage

To write a program in Flit, start your Racket file with the line:

```
 #lang flit
```

## Documentation

See the documentation [here](https://eremondi.com/flit/flit.html). There are also [detailed course notes on functional programming in Flit](https://eremondi.com/cs350/notes.html) (WIP).

## Installation

The package is meant to be installed with the Racket package manager:

 * In recent versions of DrRacket, choose "Install Package.."
   from the "File" menu, and enter

```
       flit
```

   in the dialog.

 * From the comamnd line:

     raco pkg install flit

