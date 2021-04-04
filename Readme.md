### Introduction

This is a [PureScript](https://www.purescript.org/) port of the great functional programming course [fp-course from system-f](https://github.com/system-f/fp-course). It's a hands-on introduction to functional programming concepts and category theory. 
Prevous knowledge of the PureScript language and mathematical jargon is not required, references to learning the language and category theory concepts are provided throughout the source of this course. However, learning new things doesn't come for free, and dedication and persistence will greatly impact what you will end up taking away from this course.

### Prerequisites

You need to have [purescript and spago](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md) installed.
Clone this repository 
```git clone https://github.com/maurobalbi/from-mempty-to-monad```.

### Using this repository
Run ```spago repl``` to start the [PureScript repl](https://github.com/purescript/documentation/blob/master/guides/PSCi.md).

Import the module you want to test with ```import Test.ListTest```. This will import the tests and the modules with your implemenation. The naming convention is `Test.${name of srcfile}Test`. All the available modules are listed in the Course Order paragraph. 

Test your implementation with ```test listTest```. You find the exact names of the available tests in the corresponding files in the test folder.
Enter ```:r``` shorthand for ```:reload``` in the repl to recompile your solution and rerun the tests until they pass.
The compiler can assist you if you prefix a variable in your implementation with ?. This is called [typed hole](https://github.com/JordanMartinez/purescript-jordans-reference/blob/0de8de5cfc0993fe56b00d617d9da80d474c282f/11-Syntax/01-Basic-Syntax/src/03-Special-Compiler-Features/01-Typed-Holes.purs) and on compilation the compiler will error with the type it expects in the place of the variable.


### Course Order
The recommended order is:


If you're already familiar with certain concepts, feel free to skip ahead to later parts of the course.

### Trivia
From (**zero** of sum-type) to (**hero** of functional programming).
