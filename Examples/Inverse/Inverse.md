Inverse
================

``` r
library(wrapr)
library(cdata)

df <- wrapr::build_frame(
  "val_loss"  , "val_acc", "loss" , "acc" , "epoch" |
    -0.377    , 0.8722   , -0.5067, 0.7852, 1       |
    -0.2997   , 0.8895   , -0.3002, 0.904 , 2       |
    -0.2964   , 0.8822   , -0.2166, 0.9303, 3       |
    -0.2779   , 0.8899   , -0.1739, 0.9428, 4       |
    -0.2843   , 0.8861   , -0.1411, 0.9545, 5       |
    -0.312    , 0.8817   , -0.1136, 0.9656, 6       )

print(df)
```

    ##   val_loss val_acc    loss    acc epoch
    ## 1  -0.3770  0.8722 -0.5067 0.7852     1
    ## 2  -0.2997  0.8895 -0.3002 0.9040     2
    ## 3  -0.2964  0.8822 -0.2166 0.9303     3
    ## 4  -0.2779  0.8899 -0.1739 0.9428     4
    ## 5  -0.2843  0.8861 -0.1411 0.9545     5
    ## 6  -0.3120  0.8817 -0.1136 0.9656     6

``` r
controlTable <- wrapr::build_frame(
  "measure"                     , "training", "validation" |
    "minus binary cross entropy", "loss"    , "val_loss"   |
    "accuracy"                  , "acc"     , "val_acc"    )

xform <- rowrecs_to_blocks_spec(
  controlTable = controlTable,
  recordKeys = 'epoch') 

print(xform)
```

    ## {
    ##  row_record <- wrapr::qchar_frame(
    ##    "epoch"  , "loss", "acc", "val_loss", "val_acc" |
    ##      .      , loss  , acc  , val_loss  , val_acc   )
    ##  row_keys <- c('epoch')
    ## 
    ##  # becomes
    ## 
    ##  block_record <- wrapr::qchar_frame(
    ##    "epoch"  , "measure"                   , "training", "validation" |
    ##      .      , "minus binary cross entropy", loss      , val_loss     |
    ##      .      , "accuracy"                  , acc       , val_acc      )
    ##  block_keys <- c('epoch', 'measure')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = FALSE, strict = FALSE, allow_rqdatatable = TRUE)
    ## }

``` r
res <- df %.>% xform

print(res)
```

    ##    epoch                    measure training validation
    ## 1      1 minus binary cross entropy  -0.5067    -0.3770
    ## 2      1                   accuracy   0.7852     0.8722
    ## 3      2 minus binary cross entropy  -0.3002    -0.2997
    ## 4      2                   accuracy   0.9040     0.8895
    ## 5      3 minus binary cross entropy  -0.2166    -0.2964
    ## 6      3                   accuracy   0.9303     0.8822
    ## 7      4 minus binary cross entropy  -0.1739    -0.2779
    ## 8      4                   accuracy   0.9428     0.8899
    ## 9      5 minus binary cross entropy  -0.1411    -0.2843
    ## 10     5                   accuracy   0.9545     0.8861
    ## 11     6 minus binary cross entropy  -0.1136    -0.3120
    ## 12     6                   accuracy   0.9656     0.8817

``` r
inverse <- t(xform)

print(inverse)
```

    ## {
    ##  block_record <- wrapr::qchar_frame(
    ##    "epoch"  , "measure"                   , "training", "validation" |
    ##      .      , "minus binary cross entropy", loss      , val_loss     |
    ##      .      , "accuracy"                  , acc       , val_acc      )
    ##  block_keys <- c('epoch', 'measure')
    ## 
    ##  # becomes
    ## 
    ##  row_record <- wrapr::qchar_frame(
    ##    "epoch"  , "loss", "acc", "val_loss", "val_acc" |
    ##      .      , loss  , acc  , val_loss  , val_acc   )
    ##  row_keys <- c('epoch')
    ## 
    ##  # args: c(checkNames = TRUE, checkKeys = FALSE, strict = FALSE, allow_rqdatatable = TRUE)
    ## }

``` r
res %.>% inverse
```

    ##   epoch    loss    acc val_loss val_acc
    ## 1     1 -0.5067 0.7852  -0.3770  0.8722
    ## 2     2 -0.3002 0.9040  -0.2997  0.8895
    ## 3     3 -0.2166 0.9303  -0.2964  0.8822
    ## 4     4 -0.1739 0.9428  -0.2779  0.8899
    ## 5     5 -0.1411 0.9545  -0.2843  0.8861
    ## 6     6 -0.1136 0.9656  -0.3120  0.8817
