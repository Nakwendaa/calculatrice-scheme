# calculatrice-scheme

The application is a small calculator with unlimited accuracy. The calculator supports numbers of arbitrary length.  The expressions are in **postfix** form.

## Installation

You may need to install [gambit](http://dynamo.iro.umontreal.ca/wiki/index.php/Main_Page) to compile or interpret ``calculatrice-scheme.scm``

## Usage

Each expression is on a line and the calculator waits for the input by printing ``>`` as an incentive. 
After the end of the line the program prints the result on a new line. The expressions are in **postfix** form.

It is possible to assign variables using the syntax:
``⟨expression⟩ =⟨variable⟩``

After the assignment the variable can be used in expressions. Valid variable names have only one character. It is a mistake to use a variable that has not yet received a value. 

Negative numbers are not allowed in expressions, but the result of a calculation can be negative.

## Example

```
> 1 1 + 1 - 48 *
48
> 1000 1000 * =a
1000000
> 1 a -
-999999
> a 1 + =a
1000001
> a 1 * =b
1000001
> 1001 =b
1001
> b 100 + =b
1101 
> b b b b * * * 
1469431264401
> 1000 =b 1000 * =c 
1000000
```

## License
[MIT](https://raw.githubusercontent.com/Nakwendaa/calculatrice-scheme/master/LICENSE)
