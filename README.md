## How to access the functions in the R shell

- Go to top level directory.
- Run R in your console.
- Run `install.packages("devtools")`
- Run `devtools::load_all()`

Now you have loaded all functions into the R shell. They can be found under the 
`regrtest` namespace. 

## Finding the correct functions during the test

By tabcompletion on `regrtest::` you can see all available functions. However,
the function name alone is not sufficient as a lot of functions
calculate multiple important results. As with all R packages, you can read a
functions documentation by using the `?` operator. For example: 
`?regrtest::Sxx`. Doing this one by one is however too slow during a test. **So
we have also created a `regrtest::search()` function.** This searches for a search
pattern trough all the documentation and displays it. For example 
`regrtest::search("predictor centered")` will display the docs for `regrtest::Sxx()`

### Adding documentation

In order for the search function to work, all functions should have a proper
roxygen documentation. See [search()](https://github.com/nstylo/Rbomination/blob/83d3a1d4a131713e4fd740a6930dba66430a4df3/R/search.R#L1-L11) for an example.
Not all functions are properly documented yet, **Help is greatly appreciated**.
Please fill the docs with as many relevant keywords as possible. Tags like 
`@param` are not required. _After you made changes to the docs, don't 
forget to run_ `devtools::document()`!.
