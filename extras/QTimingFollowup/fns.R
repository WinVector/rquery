

mkData <- function(nrep) {
  dLocal <- data.frame(
    subjectID = c(1,
                  1,
                  2,
                  2),
    surveyCategory = c(
      'withdrawal behavior',
      'positive re-framing',
      'withdrawal behavior',
      'positive re-framing'
    ),
    assessmentTotal = c(5,
                        2,
                        3,
                        4),
    stringsAsFactors = FALSE)
  norig <- nrow(dLocal)
  dLocal <- dLocal[rep(seq_len(norig), nrep), , drop=FALSE]
  dLocal$subjectID <- paste((seq_len(nrow(dLocal)) -1)%/% norig,
                            dLocal$subjectID,
                            sep = "_")
  rownames(dLocal) <- NULL

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
                    probability = d$probability/totals,
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
rquery_pipeline <- . := {
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
  arrange(probability, surveyCategory) %>%
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
  arrange(probability, surveyCategory) %>%
  mutate(count = n(), rank = row_number()) %>%
  ungroup() %>%
  filter(count == rank) %>%
  rename(diagnosis = surveyCategory) %>%
  select(subjectID, diagnosis, probability) %>%
  arrange(subjectID)

.datatable.aware <- TRUE

data.table_local <- function() {
  dDT <- data.table::data.table(dLocal)
  dDT[
    , one := 1 ][
      , probability := exp ( assessmentTotal * scale ) /
        sum ( exp ( assessmentTotal * scale ) ) ,subjectID ][
          , count := sum ( one ) ,subjectID ][
            , rank := rank ( probability ) ,subjectID ][
              rank == count ][
                , diagnosis := surveyCategory ][
                  , c('subjectID', 'diagnosis', 'probability') ][
                    order(subjectID) ]
}
