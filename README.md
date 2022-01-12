# ProjectA
An Interpreter for [automata](https://en.wikipedia.org/wiki/Abstract_machine) that comes with a powerful domain-specific language describing the behaviours of automata of various types 

The project is fully written in Haskell.


# Features
Main purpose of this project is to help learning/teaching automata theory by

1. Emulating several types of automata.
2. Help debugging automata by providing step-by-step execution 
3. Using automata as computational black-boxes with support for both recognition and computation modes, e.g., testing a string for membership in the language induce by the automata is only one command.

### Accepted Machines

1. DFA: Deterministic Finite Automata
2. NFA: Non-deterministic Finite Automata
3. λ-NFA: NFA with λ-edges
4. PDA: Pushdown Automata
5. Queue Automata
6. Turing Machine (deterministic/non-deterministic) (single/multi-tape)
7. Your own custom automata

# Parsing
Parsing takes place in `AutomatonParser.hs`, making use of `Parsec` library, making fully use of `Alternative Applicatives`

# Running the Project
1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repo
```
git clone https://github.com/zyasserd/ProjectA
```
2. Compile 
```
stack ghc AutomatonInterpreter.hs
```
3. Run
```
AutomatonInterpreter.exe filename.aut
```

# The DSL
This section describes the syntax of the domain specific language used to define the Automata. You'll find plenty of examples inside the folder `myAutomata`

Refer to `AutomatonParser.hs` Lines `15-103` for a complementary but a more technical and raw description of the DSL.

## Header

### Automata Type
In the first (non-comment) line of the file you define the type of the automata by listing the data structures (DS) needed (`stack`/`s`, `queue`/`q`, `tape`/`t`) separated by whitespace. Also, you need to specify at most one input, and output DS by appending `:I` or `:O` to the end.

Note, if you didn't specify any input DS, a 


- DFA: [EMPTY LINE]
- PDA: `stack`
- Turing Machine with one input, one helper, one output tapes: `t:I t t:O`  

### Start Vertices
Define the start vertex is by adding the line
```
start <- vertexName
```

### Accepted Vertices
Define the accepted vertex is by adding the line.
```
accept <- q1 [, q2]
```
Note: you can add as many vertices as you want separated by commas

### Global Variables
Can be only defined in the header. Global Vars have to start with an asterisk `*` followed by alphanumerical characters.
All global vars describle a list of characters, and defined as follows:
```
*var <- a, b, c
```
Now, we can you `*var` to match any char of (`a`, `b`, `c`)

### The Special Global Variables
`*alpha`, `*num` are pre-defined as the following
```
*alpha <- abc...xyzABC...XYZ
*num   <- 0123456789
```
`*` is a global variable intended to match the input alphabet, imitating a wildcard but initalized to include only the characters in `*alpha` and `*num`. You SHOULD redefine it if you input alphabet is different that the one stated before.

### Comments
Like python comments, all character after `#` till the end of the line will be ignored.

### Lambda Character
Use `_` (underscore) to represent an empty cell (ε, λ), no popping the stack/queue

## Body
The body represents the transition function of the automaton enumerated in entries separated by an empty line.

### Entries
Every entry follows the standard format of the transition function (δ).
It consist of an input part followed by `:` followed by the output part,
where every part consists of comma separated chars/strings.

### Advanced features
Refer to `AutomatonParser.hs` Lines `15-103`

# ToDo
1. Display errors as printed strings not exceptions

2. Add support for history tree in the navigation mode and history actions (skip, next, prev, move to end).

3. Create a web interface to ProjectA preferably using _Haskell_. Use `diagrams` library to draw the transition diagram of the Automaton.

4. Add Support for non-standard models of computation as _Probabilistic Turing Machine_ and _Quantum Turing Machine_

