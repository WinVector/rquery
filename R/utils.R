


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
  db_inf <- db_info(source)
  parsed <- vector(n, mode = 'list')
  for(i in 1:n) {
    ni <- names(assignments)[[i]]
    ai <- assignments[[ni]]
    ei <- parse(text = paste(ni, ":=", ai))[[1]]
    pi <- parse_for_SQL(ei,
                        colnames = have,
                        env = env)
    pi$parsed <- to_query(pi$parsed_toks,
                          db_info = db_inf)
    have <- unique(c(have, pi$symbols_produced))
    parsed[[i]] <- pi
  }
  parsed
}


parse_nse <- function(source, exprs, env,
                      have = column_names(source)) {
  n <- length(exprs)
  if(n<=0) {
    stop("must have at least 1 assigment")
  }
  db_inf <- db_info(source)
  parsed <- vector(n, mode = 'list')
  for(i in 1:n) {
    ei <- exprs[[i]]
    pi <- parse_for_SQL(ei,
                        colnames = have,
                        env = env)
    pi$parsed <- to_query(pi$parsed_toks,
                          db_info = db_inf)
    have <- unique(c(have, pi$symbols_produced))
    parsed[[i]] <- pi
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


# mege named maps of column vectors
merge_columns_used <- function(cu1, cu2) {
  nms <- sort(unique(names(cu1), names(cu2)))
  cu <- lapply(nms,
               function(ni) {
                 sort(unique(cu1[[ni]], cu2[[ni]]))
               })
  names(cu) <- nms
  cu
}

