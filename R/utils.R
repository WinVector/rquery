




check_have_cols <- function(have, requested, note) {
  if(length(have)!=length(unique(have))) {
    dups <- table(have)
    dups <- names(dups[dups>1])
    stop(paste(note,"duplicate declared columns",
               paste(dups, collapse = ", ")))
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
                               ...,
                               have = column_names(source),
                               check_is_assignment = TRUE) {
  if(length(list(...))>0) {
    stop("rquery::unpack_assignments unexpected argument")
  }
  n <- length(parsed)
  if(n<=0) {
    stop("must generate at least 1 column")
  }
  nms <- character(n)
  assignments <- character(n)
  uses <- vector(n, mode='list')
  for(i in seq_len(n)) {
    si <- parsed[[i]]
    if(length(si$symbols_produced)>1) {
      stop("more than one symbol produced")
    }
    if(check_is_assignment) {
      if(length(si$symbols_produced)!=1) {
        stop("each assignment must be of the form name := expr")
      }
    }
    if(length(si$symbols_produced)==1) {
      nms[[i]] <- si$symbols_produced
    }
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
  # R-like db-info for presentation
  db_inf <- rquery_db_info(indentifier_quote_char = '`',
                           string_quote_char = '"')
  parsed <- vector(n, mode = 'list')
  for(i in seq_len(n)) {
    ni <- names(assignments)[[i]]
    ai <- assignments[[i]]
    ei <- parse(text = ai)[[1]]
    pi <- tokenize_for_SQL(ei,
                        colnames = have,
                        env = env)
    pi$symbols_produced <- unique(c(pi$symbols_produced, ni))
    pi$parsed <- to_query(pi$parsed_toks,
                          db_info = db_inf)
    if((!is.null(ni)) && (nchar(as.character(ni))>0)) {
      pi$presentation <- paste(ni, ":=", pi$presentation)
    }
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
  # R-like db-info for presentation
  db_inf <- rquery_db_info(indentifier_quote_char = '`',
                           string_quote_char = '"')
  parsed <- vector(n, mode = 'list')
  for(i in seq_len(n)) {
    ni <- names(exprs)[[i]]
    ei <- exprs[[i]]
    pi <- tokenize_for_SQL(ei,
                        colnames = have,
                        env = env)
    pi$symbols_produced <- unique(c(pi$symbols_produced, ni))
    if((!is.null(ni)) && (nchar(as.character(ni))>0)) {
      pi$presentation <- paste(ni, ":=", pi$presentation)
    }
    pi$parsed <- to_query(pi$parsed_toks,
                          db_info = db_inf)
    have <- unique(c(have, pi$symbols_produced))
    parsed[[i]] <- pi
  }
  parsed
}


redo_parse_quoting <- function(parsed, db_info) {
  n <- length(parsed)
  for(i in seq_len(n)) {
    pi <- parsed[[i]]
    pi$parsed <- to_query(pi$parsed_toks,
                          db_info = db_info)
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

