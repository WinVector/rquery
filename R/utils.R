


#' Produce a temp name generator with a given prefix.
#'
#' Returns a function f where: f() returns a new temporary name,
#' f(remove=vector) removes names in vector and returns what was removed,
#' f(dumpList=TRUE) returns the list of names generated and clears the list,
#' f(peek=TRUE) returns the list without altering anything.
#'
#' @param prefix character, string to prefix temp names with.
#' @return name generator function.
#'
#' @examples
#'
#' f <- mkTempNameGenerator('EX')
#' print(f())
#' nm2 <- f()
#' print(nm2)
#' f(remove=nm2)
#' print(f(dumpList=TRUE))
#'
#' @noRd
#'
mkTempNameGenerator <- function(prefix) {
  force(prefix)
  if((length(prefix)!=1)||(!is.character(prefix))) {
    stop("rquery::mkTempNameGenerator prefix must be a string")
  }
  count <- 0
  nameList <- list()
  function(..., peek=FALSE, dumpList=FALSE, remove=NULL) {
    if(length(list(...))>0) {
      stop("rquery::mkTempNameGenerator tempname generate unexpected argument")
    }
    if(peek) {
      return(names(nameList))
    }
    if(dumpList) {
      v <- names(nameList)
      nameList <<- list()
      return(v)
    }
    if(!is.null(remove)) {
      victims <- intersect(remove, names(nameList))
      # this removes from lists
      nameList[victims] <<- NULL
      return(victims)
    }
    nm <- paste(prefix, sprintf('%04d',count), sep='_')
    nameList[[nm]] <<- 1
    count <<- count + 1
    nm
  }
}


check_have_cols <- function(have, requested, note) {
  if(length(have)!=length(unique(have))) {
    stop(paste(note,"duplicate declared columns",
               paste(diff, collapse = ", ")))
  }
  requested <- unique(requested)
  diff <- setdiff(requested, have)
  if(length(diff)>0) {
    stop(paste(note,"unknown columns",
               paste(diff, collapse = ", ")))
  }
  TRUE
}


unpack_assignments <- function(source, parsed,
                               have = column_names(source)) {
  n <- length(parsed)
  if(n<=0) {
    stop("must generate at least 1 column")
  }
  nms <-  character(n)
  assignments <- character(n)
  uses <- vector(n, mode='list')
  for(i in 1:n) {
    si <- parsed[[i]]
    if(length(si$symbols_produced)!=1) {
      stop("each assignment must be of the form name := expr")
    }
    nms[[i]] <- si$symbols_produced
    assignments[[i]] <- si$parsed
    uses[[i]] <- si$symbols_used
  }
  names(assignments) <- nms
  if(n!=length(unique(names(assignments)))) {
    stop("generated column names must be unique")
  }
  check_have_cols(have, unlist(uses), "used")
  assignments
}

parse_se <- function(source, assignments, env,
                     have = column_names(source)) {
  n <- length(assignments)
  if(n<=0) {
    stop("must generate at least 1 expression")
  }
  if(n!=length(unique(names(assignments)))) {
    stop("generated column names must be unique")
  }
  parsed <- vector(n, mode = 'list')
  for(i in 1:n) {
    ni <- names(assignments)[[i]]
    ai <- assignments[[ni]]
    ei <- paste(parse(text = paste(ni, ":=", ai)), collapse = "\n")
    parsed[[i]] <- prepForSQL(ei,
                              colnames = have,
                              node = source,
                              env = env)
    have <- unique(c(have, parsed[[i]]$symbols_produced))
  }
  parsed
}


parse_nse <- function(source, exprs, env,
                      have = column_names(source)) {
  n <- length(exprs)
  if(n<=0) {
    stop("must have at least 1 assigment")
  }
  parsed <- vector(n, mode = 'list')
  for(i in 1:n) {
    parsed[[i]] <- prepForSQL(exprs[[i]],
                              colnames = have,
                              node = source,
                              env = env)
    have <- unique(c(have, parsed[[i]]$symbols_produced))
  }
  parsed
}



# get field by name from list
merge_fld <- function(reslist, field) {
  if(length(reslist)<=0) {
    return(NULL)
  }
  got <- lapply(reslist,
                function(ri) {
                  ri[[field]]
                })
  unique(unlist(got))
}

