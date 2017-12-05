
check_have_cols <- function(have, requested, note) {
  if(length(requested)!=length(unique(requested))) {
    stop(paste(note,"duplicate columns"))
  }
  diff <- setdiff(requested, have)
  if(length(diff)>0) {
    stop(paste(note,"unknown columns",
               paste(diff, collapse = ", ")))
  }
  TRUE
}


unpack_assignments <- function(source, parsed) {
  have <- column_names(source)
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

parse_se <- function(source, assignments, env) {
  n <- length(assignments)
  if(n<=0) {
    stop("must generate at least 1 expression")
  }
  if(n!=length(unique(names(assignments)))) {
    stop("generated column names must be unique")
  }
  have <- column_names(source)
  db <- dbi_connection(source)
  parsed <- vector(n, mode = 'list')
  for(i in 1:n) {
    ni <- names(assignments)[[i]]
    ai <- assignments[[ni]]
    ei <- parse(text = paste(ni, ":=", ai))[[1]]
    parsed[[i]] <- prepForSQL(ei,
                              colnames = have,
                              db = db,
                              env = env)
  }
  parsed
}


parse_nse <- function(source, exprs, env) {
  n <- length(exprs)
  if(n<=0) {
    stop("must have at least 1 assigment")
  }
  have <- column_names(source)
  db <- dbi_connection(source)
  parsed <- lapply(exprs,
                   function(ei) {
                     prepForSQL(ei,
                                colnames = have,
                                db = db,
                                env = env)
                   })
  parsed
}

