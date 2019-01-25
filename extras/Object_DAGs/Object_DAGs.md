Traversing Object DAGs in R
================

``` r
mk_obj <- function(source = list()) {
  r <- list(source = source)
  class(r) <- "node"
  r
}

o4 <- mk_obj()
dag <- mk_obj(list(mk_obj(list(o4)), mk_obj(list(o4))))
rm(list = "o4")
str(dag)
```

    ## List of 1
    ##  $ source:List of 2
    ##   ..$ :List of 1
    ##   .. ..$ source:List of 1
    ##   .. .. ..$ :List of 1
    ##   .. .. .. ..$ source: list()
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..- attr(*, "class")= chr "node"
    ##   ..$ :List of 1
    ##   .. ..$ source:List of 1
    ##   .. .. ..$ :List of 1
    ##   .. .. .. ..$ source: list()
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..- attr(*, "class")= chr "node"
    ##  - attr(*, "class")= chr "node"

``` r
r_visitor_1 <- function(obj) {
  obj$mutable_annotation_space <- new.env(parent = emptyenv())
  for(ii in obj$source) {
    r_visitor_1(ii)
  }
}
r_visitor_1(dag)
str(dag)
```

    ## List of 1
    ##  $ source:List of 2
    ##   ..$ :List of 1
    ##   .. ..$ source:List of 1
    ##   .. .. ..$ :List of 1
    ##   .. .. .. ..$ source: list()
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..- attr(*, "class")= chr "node"
    ##   ..$ :List of 1
    ##   .. ..$ source:List of 1
    ##   .. .. ..$ :List of 1
    ##   .. .. .. ..$ source: list()
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..- attr(*, "class")= chr "node"
    ##  - attr(*, "class")= chr "node"

``` r
# oops our alterations are not saved

r_visitor_2 <- function(obj) {
  obj$mutable_annotation_space <- new.env(parent = emptyenv())
  obj$source <- lapply(obj$source, r_visitor_2)
  obj
}
dag2 <- r_visitor_2(dag)
str(dag2)
```

    ## List of 2
    ##  $ source                  :List of 2
    ##   ..$ :List of 2
    ##   .. ..$ source                  :List of 1
    ##   .. .. ..$ :List of 2
    ##   .. .. .. ..$ source                  : list()
    ##   .. .. .. ..$ mutable_annotation_space:<environment: 0x7fa610279430> 
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..$ mutable_annotation_space:<environment: 0x7fa610276518> 
    ##   .. ..- attr(*, "class")= chr "node"
    ##   ..$ :List of 2
    ##   .. ..$ source                  :List of 1
    ##   .. .. ..$ :List of 2
    ##   .. .. .. ..$ source                  : list()
    ##   .. .. .. ..$ mutable_annotation_space:<environment: 0x7fa610281a58> 
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..$ mutable_annotation_space:<environment: 0x7fa61027c540> 
    ##   .. ..- attr(*, "class")= chr "node"
    ##  $ mutable_annotation_space:<environment: 0x7fa60ef7a730> 
    ##  - attr(*, "class")= chr "node"

``` r
# oops each visit to o4 creates a different copy with a different
# mutable_annotation_space.
dag2$source[[1]]$source[[1]]$mutable_annotation_space
```

    ## <environment: 0x7fa610279430>

``` r
dag2$source[[2]]$source[[1]]$mutable_annotation_space
```

    ## <environment: 0x7fa610281a58>

``` r
mk_obj <- function(source = list()) {
  r <- list(
    source = source,
    mutable_annotation_space = new.env(parent = emptyenv()))
  class(r) <- "node"
  r
}

o4 <- mk_obj()
dag <- mk_obj(list(mk_obj(list(o4)), mk_obj(list(o4))))
rm(list = "o4")
str(dag)
```

    ## List of 2
    ##  $ source                  :List of 2
    ##   ..$ :List of 2
    ##   .. ..$ source                  :List of 1
    ##   .. .. ..$ :List of 2
    ##   .. .. .. ..$ source                  : list()
    ##   .. .. .. ..$ mutable_annotation_space:<environment: 0x7fa60e3a80b0> 
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..$ mutable_annotation_space:<environment: 0x7fa610200638> 
    ##   .. ..- attr(*, "class")= chr "node"
    ##   ..$ :List of 2
    ##   .. ..$ source                  :List of 1
    ##   .. .. ..$ :List of 2
    ##   .. .. .. ..$ source                  : list()
    ##   .. .. .. ..$ mutable_annotation_space:<environment: 0x7fa60e3a80b0> 
    ##   .. .. .. ..- attr(*, "class")= chr "node"
    ##   .. ..$ mutable_annotation_space:<environment: 0x7fa610200088> 
    ##   .. ..- attr(*, "class")= chr "node"
    ##  $ mutable_annotation_space:<environment: 0x7fa6101ffc28> 
    ##  - attr(*, "class")= chr "node"

``` r
# success bottom of dag recieves the same environment both times

label_nodes <- function(obj, label_key = "node_id") {
  force(label_key)
  clear_node_labels <- function(obj) {
    assign(label_key, NULL, envir = obj$mutable_annotation_space)
    lapply(obj$source, clear_node_labels)
    NULL
  }
  clear_node_labels(obj)
  counter <- new.env(parent = emptyenv())
  assign(label_key, 1, envir = counter)
  set_node_labels <- function(obj) {
    see <- get(label_key, envir = obj$mutable_annotation_space)
    if(is.null(see)) {
      id <- get(label_key, envir = counter)
      assign(label_key, id + 1, envir = counter)
      assign(label_key, id, envir = obj$mutable_annotation_space)
      lapply(obj$source, set_node_labels)
    }
    NULL
  }
  set_node_labels(obj)
  get(label_key, envir = counter) - 1
}

label_nodes(dag)
```

    ## [1] 4

``` r
dag$mutable_annotation_space$node_id
```

    ## [1] 1

``` r
dag$source[[1]]$mutable_annotation_space$node_id
```

    ## [1] 2

``` r
dag$source[[1]]$source[[1]]$mutable_annotation_space$node_id
```

    ## [1] 3

``` r
dag$source[[2]]$mutable_annotation_space$node_id
```

    ## [1] 4

``` r
dag$source[[2]]$source[[1]]$mutable_annotation_space$node_id
```

    ## [1] 3

``` r
#works!
```
