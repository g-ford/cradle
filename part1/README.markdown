#Let's build a compiler (in Haskell): Part 1 - Introduction
 
When looking for information on getting started with learning how to build an interpreter or compiler, inevitable two resources come up.  First, the canonical compiler resource, the [Dragon Book](http://en.wikipedia.org/wiki/Dragon_Book_(computer_science)), and second, the noob guide, [Let's Build a Compiler](http://compilers.iecc.com/crenshaw/) (LBaC) by Jack W. Crenshaw.
 
So rather than spending time and money diving into the deep end, I decided to start with LBaC.  I don't have a CompSci degree so I need to start at the very beginning and LBaC seems to fit the bill.

However there was one problem with LBaC:

> This is a "learn-by-doing" series. In the course of the series I will be performing experiments on a computer. You will be expected to follow along, repeating the experiments that I do, and performing some on your own. I will be using **Turbo Pascal 4.0** on a PC clone. 

Yep, this series was written way back in the 90's when [Turbo Pascal](http://en.wikipedia.org/wiki/Turbo_Pascal) was cool.  Thankfully the theory and basic practices are still relevant. 

##So why Haskell?

First and formost because it is my [Language of the Year](http://alephnullplex.appspot.com/blog/view/2009/06/04/one-language-a-year), and considering I have done nothing with it in the last 6 months, I owe it to myself to actually start a project in Haskell.

Secondly there is extensive tools for more advanced compiler writing such as parsers, lexers and LLVM bindings etc. 

# Following along

I will be writing these articles as I work my way through LBaC in my spare time as and when I can find it, so they may be a little few and far between.

All code, and these articles, will be available on Github. You will also need [LBaC](http://compilers.iecc.com/crenshaw/) ([PDF Version](http://www.stack.nl/~marcov/compiler.pdf)) and the [Haskell Platform](http://hackage.haskell.org/platform/).

[Part 2 - The Cradle]()