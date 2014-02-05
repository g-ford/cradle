# Let's Build a Compiler (in Haskell)

[Read the articles here](http://alephnullplex.github.io/cradle/)

When looking for information on getting started with learning how to build an interpreter or compiler, inevitable two resources come up.  First, the canonical compiler resource, the [Dragon Book](http://en.wikipedia.org/wiki/Dragon_Book_\(computer_science\)), and second, [Let's Build a Compiler](http://compilers.iecc.com/crenshaw/) (LBaC) by Jack W. Crenshaw.
 
So rather than spending time and money diving into the deep end, I decided to start with LBaC.  Crenshaws approach is a much more hands on, top down approach that is easier to see immediate results and applications.  It skimps a lot on the theory, models and alternatives and I will attempt to fill in the gaps where necessary.

However there was one problem with LBaC:

> This is a "learn-by-doing" series. In the course of the series I will be performing experiments on a computer. You will be expected to follow along, repeating the experiments that I do, and performing some on your own. I will be using **Turbo Pascal 4.0** on a PC clone. 

This series was written way back in the 90's when [Turbo Pascal](http://en.wikipedia.org/wiki/Turbo_Pascal) was cool.  Thankfully the theory and basic practices are still relevant. 

##So why Haskell?

First and formost because it was my [Language of the Year](http://alephnullplex.appspot.com/blog/view/2009/06/04/one-language-a-year) at the time of starting this project.  Secondly there is extensive tools for more advanced compiler writing such as parsers, lexers and LLVM bindings etc. for going beyond the basics.

And lastly because Haskell is fundamentally different to my day-to-day languages.  Using something completely different helps to keep things a little more exciting. 

## Following along

I will be [writing articles](http://alephnullplex.github.io/cradle/) as I work my way through LBaC in my spare time (as and when I can find some) so they may be a little few and far between.

All code, and these articles, will be available on [Github](http://github.com/alephnullplex/cradle). You will also need [LBaC](http://compilers.iecc.com/crenshaw/) ([PDF Version](http://www.stack.nl/~marcov/compiler.pdf)) and the [Haskell Platform](http://hackage.haskell.org/platform/).
