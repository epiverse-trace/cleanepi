# linelist (development version)

## Breaking changes

* Deprecated functions and arguments have been completely removed. The following
  operations are no longer possible:
  
  * List of tags now needs to be spliced in `make_linelist()`:
    ```r
    my_tags <- list(
      id = "case_ID",
      date_onset = "date_of_prodrome",
      age = "age",
      gender = "gender"
    )

    # No longer possible
    make_linelist(obj, my_tags)

    # Instead slice list in dynamic dots
    make_linelist(obj, !!!my_tags)
    ```
    
  * `select_tags()`:
  ```r
  # No longer possible
  x %>%
    select_tags("age")

  # Instead use
  x %>%
    select(has_tag("age")) %>%
    tags_df()
  ```

  * `select.linelist()`. This change should be invisible to users since the 
    parent method `select.data.frame()` will be used with the same effect.
  
  * `lost_tags_action()` as part of a pipeline is no longer possible:
  ```
  # No longer possible
  make_linelist(cars, date_onset = "dist", date_outcome = "speed") |>
    lost_tags_action("none") |>
    dplyr::select(-dist)
  
  # Instead do
  lost_tags_action("none") 
  make_linelist(cars, date_onset = "dist", date_outcome = "speed") |>
    dplyr::select(-dist)
  ```

# linelist 1.1.4

## Internal change

* Promotion of testthat warnings to errors on CI is restored, thanks to a report and suggestion by @krlmlr. This was accidentally removed during #123  (#128).

## New features

* Default tag loss condition can now permanently be set via an environment variable `LINELIST_LOST_ACTION` (@Bisaloo, #126). The environment variable needs to be set before the R session is started due to the way R environment variables work. This allow users to set a stricter default behaviour for tag loss if they want to ensure tag loss never go unnoticed (#104).
* `validate_linelist()` now returns invisibly, with a message, for valid linelist object (@Bisaloo, #146, based on a report from @avallecam in #143).

# linelist 1.1.3

## Minor change

* Convert `meta$Date` in CITATION to `Date` before attempting to run `format()` (@Bisaloo, #125)

# linelist 1.1.2

## Minor change

* Fix URL in CITATION (@Bisaloo, #123)

# linelist 1.1.1

## Minor change

* Tests no longer error on warnings on CRAN, to avoid false positives on a 
platform where we have limited control (@Bisaloo, #123).

# linelist 1.1.0

## Breaking changes

* `make_linelist()` and `set_tags()` no longer accept a named list of characters
as input. Instead, `make_linelist()`, `set_tags()` and `tags_types()` now use
[rlang's dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html), which 
means that you can splice list arguments. This implementation is more robust,
better tested, and makes it explicit that users want to splice the list 
(@Bisaloo, #96).

  * Before:
  ```r
  my_tags <- list(
    id = "case_ID",
    date_onset = "date_of_prodrome",
    age = "age",
    gender = "gender"
  )
  make_linelist(obj, my_tags)
  # OR
  make_linelist(
    obj,
    id = "case_ID",
    date_onset = "date_of_prodrome",
    age = "age",
    gender = "gender"
  )
  ```
  * Now:
  ```r
  my_tags <- list(
    id = "case_ID",
    date_onset = "date_of_prodrome",
    age = "age",
    gender = "gender"
  )
  make_linelist(obj, !!!my_tags)
  # OR
  make_linelist(
    obj,
    id = "case_ID",
    date_onset = "date_of_prodrome",
    age = "age",
    gender = "gender"
  )
  ```

## New features

* linelist warnings and errors in the case of a tag loss now have a custom 
class (@Bisaloo, #109), which means it is easier to silence them specifically, 
or to catch them programmatically for advanced error handling. One example of a
new advanced condition handling that was before not possible is:

  ``` r
  warning_counter <- 0
  
  withCallingHandlers({
    x <- linelist::make_linelist(cars, date_onset = "dist", age = "speed")
    x <- x[, -1]
    x <- x[, -1]
    warning("This is not a linelist warning", call. = FALSE)
  }, linelist_warning = function(w) {
    warning_counter <<- warning_counter + 1
  })
  #> Warning: The following tags have lost their variable:
  #>  age:speed
  #> Warning: The following tags have lost their variable:
  #>  date_onset:dist
  #> Warning: This is not a linelist warning
  
  warning("This pipeline generated ", warning_counter, " linelist warnings.")
  #> Warning: This pipeline generated 2 linelist warnings.
  ```

* linelist objects now have a new custom `$<-.linelist()` to prevent tag loss
when subsetting a linelist object (@Bisaloo, #86). This completes the
functionality already provided by the `[<-.linelist()` and `[[<-.linelist()`
methods.

  ```r
  x$tagged_column <- NULL
  #> Warning in prune_tags(out, lost_action): The following tags have lost their variable:
  #>  tag:tagged_column
  ```

* Validation failures in `validate_types()` now integrate a delayed error 
mechanism (@Bisaloo, #106). This ensures that the error message will return all
the invalid tag types at once rather than having to go through multiple trials
and runs.

  * Before: only the first invalid tag type is returned.
  ```r
  # No warning about age, even though it also has an invalid type
  x <- make_linelist(cars, age = "speed", gender = "dist")
  validate_types(x, ref_types = tags_types(age = "factor"))
  #> Error in validate_types(x, ref_types = tags_types(age = "factor")) : 
  #>   Issue when checking class of tag `gender`:
  #> Must inherit from class 'character'/'factor', but has class 'numeric'
  ```
  * Now: the error message returns all the invalid tag types at once.
  ```r
  x <- make_linelist(cars, age = "speed", gender = "dist")
  validate_types(x, ref_types = tags_types(age = "factor"))
  #> Some tags have the wrong class:
  #>   - gender: Must inherit from class 'character'/'factor', but has class 'numeric'
  #>   - age: Must inherit from class 'factor', but has class 'numeric'
  ```

## Internal changes

* Internal duplication in the specification of the tags supported by linelist
by default has been removed. This makes it easier to add or remove tags in the
future, and reduces the risk of inconsistencies between the different parts of
the package (@Bisaloo, #111).
* The internal `tag_variable()` function has been replace by a vectorized
alternative `tag_variable`, thus improving performance in `make_linelist()` and
`set_tags()` about twofold. The error message when tags are specified by 
position with a number larger than the number of columns in the dataset to tag
has also been clarified (@Bisaloo, #110).

## Documentation

* linelist now provides a design vignette for future contributors or maintainers
(@Bisaloo, #112).

# linelist 1.0.0

## New features

* Increased compatibility with dplyr is now documented and ensured through
tests of all dplyr verbs on linelist objects as part of our testing & continuous
integration system, as well as a new vignette: 
<https://epiverse-trace.github.io/linelist/articles/compat-dplyr.html> 
(@Bisaloo, #53)

* A new selection helper is provided for tidyverse users, based on the existing
selectors provided by the tidyselect package: `has_tag()` (@Bisaloo, #61). By 
feeding it a character vector of tags to operate on, you can work with dplyr
verbs on specific tagged columns without having to explicitly use the column
names:

  ```r
  x %>%
    dplyr::select(has_tag(c("id", "date_of_onset")))
  ```

## Breaking changes

* It is no longer possible to use `lost_tags_action()` within a pipeline. It 
must now be set as a separate step. This makes the internal code more robust and
clarifies what is part of the pipeline versus a global option (@Bisaloo, #79).

* The `select_tags()` function is now deprecated to ensure we provide just one
clear way to address a given issue and that our "happy path" is clearly
signposted (@Bisaloo, #61). If you were using this function, we now recommend 
using the more explicit two-steps process:

  ```r
  # Deprecated
  x %>%
    select_tags("age")

  # Instead use
  x %>%
    tags_df() %>%
    select(age)
  
  # Or
  x %>%
    select(has_tag("age")) %>%
    tags_df()
  ```

* The custom `select.linelist()` method has been deprecated as providing a 
custom `[.linelist()` is sufficient to ensure compatibility with
`dplyr::select()` default methods, including triggering `lost_tags_action()`
on tag removal (@Bisaloo, #61).
A full deletion of this method is not possible at the moment because we want to
provide a smooth transition for users that relied on the custom `tags` argument
of the `select.linelist()` method. It is now recommend instead to use the new 
`has_tag()` selection helper:

  ```r
  x %>%
    dplyr::select(has_tag(c("id", "date_of_onset")))
    
  # Instead of
  x %>%
    select(tags = c("id", "date_of_onset"))
  ```

* The custom `rename.linelist()` method has been removed as providing a custom
`names<-().linelist` method is sufficient to ensure compatibility with 
`dplyr::rename()`, including appropriate modification of the tags. (@Bisaloo, 
#60)

## Documentation

* added a hex logo thanks to @dgmascarina contribution

* added short lay description to README thanks to Emma Marty's contribution

* fixed some typos in package documentation (@pitmonticone, #30)

## Bug fixes

* linelist is now explicitly marked as incompatible with data.table. 
In practice, `make_linelist(x)` now errors if `x` inherits from `data.table` 
(#55, @Bisaloo, based on discussions with @TimTaylor).
* `[.linelist()` now works to subset by column when including just one argument 
(#54, @Bisaloo). E.g., `x[1]`. As an indirect effect, this also improves
compatibility with dplyr verbs that rely on this method (#51).
* subsetting a linelist with extra tags (e.g., created via 
`make_linelist(allow_extra = TRUE)`) no longer causes an error (#65, @Bisaloo; 
reported by @TimTaylor in #63)

## Internal changes

* testthat tests now run in parallel (#77, @Bisaloo)
* testthat tests now warn on partial matching (#77, @Bisaloo)

# linelist 0.0.1

This is the first proof-of-concept release on CRAN.

## New features

* the `linelist` class implements a tagging system to keep track of key
  epidemiological variables in a `data.frame`
  
* validation routines for tagged variables

* accessors to retrieve tagged variables

* an API for easy extension to additional tagged variables

* dedicated S3 methods providing safeguards for operations which could lose
  tagged variables

* full documentation of all functions

* a vignette

* 100% test coverage


