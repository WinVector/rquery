
equiv_res <- function(a, b) {
  if(nrow(a)!=nrow(b)) {
    return(FALSE)
  }
  a <- a[order(a$subjectID), ]
  b <- b[order(b$subjectID), ]
  if(!all(a$subjectID == b$subjectID)) {
    return(FALSE)
  }
  if(!all(a$diagnosis == b$diagnosis)) {
    return(FALSE)
  }
  if(max(abs(a$probability - b$probability))>1.0e-5) {
    return(FALSE)
  }
  return(TRUE)
}


mkData <- function(nrep) {
  dLocal <- data.frame(
    subjectID = sort(rep(paste0('s', 1:nrep),2)),
    surveyCategory = c(
      'withdrawal behavior',
      'positive re-framing'
    ),
    stringsAsFactors = FALSE)
  dLocal$assessmentTotal =
    sample.int(10, nrow(dLocal), replace=TRUE)-1
  dLocal
}


# base-R function
# could also try base::split() or base:table()
base_r_calculate_tabular <- function(d) {
  d <- d[order(d$subjectID, d$surveyCategory), , drop=FALSE]
  # compute un-normalized probability
  d$probability <- exp(d$assessmentTotal * scale)
  # set up of for selection
  dmax <- stats::aggregate(d$probability,
                           by = list(subjectID = d$subjectID),
                           FUN = max)
  maxv <- dmax$x
  names(maxv) <- dmax$subjectID
  # set up for normalization
  dsum <- stats::aggregate(d$probability,
                           by = list(subjectID = d$subjectID),
                           FUN = sum)
  sumv <- dsum$x
  names(sumv) <- dsum$subjectID
  # start selection
  d$maxv <- maxv[d$subjectID]
  d <- d[d$probability >= d$maxv,
         ,
         drop=FALSE]
  # de-dup
  d$rownum <- seq_len(nrow(d))
  drow <-  stats::aggregate(d$rownum,
                            by = list(subjectID = d$subjectID),
                            FUN = min)
  minv <- drow$x
  names(minv) <- drow$subjectID
  d$rmin <- minv[d$subjectID]
  d <- d[d$rownum <= d$rmin, , drop=FALSE]
  # renormalize
  d$probability <- d$probability/sumv[d$subjectID]
  d <- d[, c("subjectID", "surveyCategory", "probability")]
  colnames(d)[[2]] <- "diagnosis"
  d
}

# base-R function sequencing accross categories
base_r_calculate_sequenced <- function(d) {
  cats <- base::sort(base::unique(d$surveyCategory))
  res <- NULL
  for(ci in cats) {
    di <- d[d$surveyCategory == ci, , drop=FALSE]
    di <- di[base::order(di$subjectID), , drop=FALSE]
    di$probability <- exp(di$assessmentTotal * scale)
    if(length(base::unique(di$subjectID))!=nrow(di)) {
      stop("base_r_calculate repeated subjectID")
    }
    if(is.null(res)) {
      res <- data.frame(subjectID = di$subjectID,
                        totalProb = di$probability,
                        bestScore = di$probability,
                        diagnosis = ci,
                        stringsAsFactors = FALSE)
    } else {
      if((nrow(di)!=nrow(res)) ||
         (!all(di$subjectID == res$subjectID))) {
        stop("base_r_calculate saw irregular data")
      }
      change <- di$probability > res$bestScore
      res$diagnosis[change] <- ci
      res$bestScore <- base::pmax(res$bestScore,
                                  di$probability)
      res$totalProb <- res$totalProb + di$probability
    }
  }
  res$probability <- res$bestScore/res$totalProb
  res <- res[, c("subjectID",
                 "diagnosis",
                 "probability")]
  res
}


# base-R function sequencing accross categories
base_r_calculate_cframe <- function(d) {
  d$probability <- exp(d$assessmentTotal * scale)
  cframe <- build_cframe(d$subjectID, d$surveyCategory)
  selections <- grouped_arg_max(cframe, d$probability)
  totals <- grouped_sum(cframe, d$probability)
  res <- data.frame(subjectID = rownames(cframe),
                    diagnosis = d$surveyCategory[selections],
                    probability = d$probability[selections]/totals,
                    stringsAsFactors = FALSE)
  res
}



# base-R function
# could also try base::split() or base:table()
base_r_calculate_rows <- function(d) {
  d <- d[base::order(d$subjectID, d$surveyCategory), , drop=FALSE]
  d$probability <- exp(d$assessmentTotal * scale)
  ids <- sort(unique(d$subjectID))
  totals <- numeric(length(ids))
  names(totals) <- ids
  n <- nrow(d)
  choices <- logical(n)
  sum <- 0
  maxID <- 1
  for(i in seq_len(n)) {
    id <- d$subjectID[[i]]
    probi <- d$probability[[i]]
    sum <- sum + probi
    if(probi>d$probability[[maxID]]) {
      maxID <- i
    }
    end_of_group <- (i>=n) || (d$subjectID[[i+1]]!=id)
    if(end_of_group) {
      choices[[maxID]] <- TRUE
      totals[id] <- sum
      sum <- 0
      maxID <- i+1
    }
  }
  d <- d[choices, , drop=FALSE]
  d$probability <- d$probability/totals[d$subjectID]
  d <- d[, c("subjectID",
             "surveyCategory",
             "probability")]
  colnames(d)[[2]] <- "diagnosis"
  d
}

# this is a function,
# so body not evaluated until used
# # TODO: need to reverse survyCategory sorting
rquery_pipeline <- function(.) {
  extend_nse(.,
             probability :=
               exp(assessmentTotal * scale)/
               sum(exp(assessmentTotal * scale)),
             count := count(1),
             partitionby = 'subjectID') %.>%
    extend_nse(.,
               rank := rank(),
               partitionby = 'subjectID',
               orderby = c('probability', 'surveyCategory'))  %.>%
    rename_columns(., 'diagnosis' := 'surveyCategory') %.>%
    select_rows_nse(., rank == count) %.>%
    select_columns(., c('subjectID',
                        'diagnosis',
                        'probability')) %.>%
    orderby(., 'subjectID')
}


# this is a function,
# so body not evaluated until used
dplyr_pipeline <- . %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, desc(surveyCategory)) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)

# this is a function,
# so body not evaluated until used
# pipeline re-factored to have filter outside
# mutate
# work around: https://github.com/tidyverse/dplyr/issues/3294
dplyr_pipeline2 <- . %>%
  group_by(subjectID) %>%
  mutate(probability =
           exp(assessmentTotal * scale)/
           sum(exp(assessmentTotal * scale), na.rm = TRUE)) %>%
  arrange(probability, desc(surveyCategory)) %>%
  mutate(count = n(), rank = row_number()) %>%
  ungroup() %>%
  filter(count == rank) %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)

.datatable.aware <- TRUE
# library(data.table) # can't load into namespace as it overides :=

# improved code from:
# http://www.win-vector.com/blog/2018/01/base-r-can-be-fast/#comment-66746
data.table_local <- function() {
  dDT <- data.table::data.table(dLocal)
  dDT <- dDT[,list(diagnosis = surveyCategory,
                   probability = exp (assessmentTotal * scale ) /
                     sum ( exp ( assessmentTotal * scale ) ))
             ,subjectID ]
  setorder(dDT, subjectID, probability, -diagnosis)
  dDT <- dDT[,.SD[.N],subjectID]
  setorder(dDT, subjectID)
}
