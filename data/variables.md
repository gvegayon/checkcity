
# Data pre-proc

This document describes the steps I followed to pre-process the data for
building a predictive model of fraud. I will use the `data.table`
package for this.

## Loading the data

``` r
library(data.table)
```

``` r
# Processing kpis
kpis <- fread("data-raw/kpis file.csv")
vars <- fread("data-raw/variables file.csv")
```

## Cleaning KPIS main variables (return and isbad)

``` r
# Removing all double quotes
kpis <- kpis[, lapply(.SD, \(x) {
  gsub("^[\"]{2,}(.+)[\"]{2,}$", "\\1", x, perl = TRUE)
})]

# Checking IsBad
kpis[, table(IsBad)]
```

    IsBad
         0      1   NULL 
    198528  16148  14597 

``` r
kpis[, IsBad := as.numeric(IsBad)]
```

    Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Checking GRODI180
kpis[grepl("[[:alpha:]]", GRODI180, perl = TRUE), ][, table(GRODI180)] # only NULL
```

    GRODI180
     NULL 
    14597 

``` r
kpis[, GRODI180 := as.numeric(GRODI180)] 
```

    Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

## Creating an indicator variable for fraud

``` r
# Generating the variable
kpis[, is_fraud := as.numeric((IsBad == 1) & (GRODI180 == 0))]

# Cross tabulating it with the other two
kpis[GRODI180 == 0, table(is_fraud, IsBad, useNA = "always")]
```

            IsBad
    is_fraud    0    1 <NA>
        0      75    0    0
        1       0 7105    0
        <NA>    0    0    0

``` r
kpis[GRODI180 != 0, table(is_fraud, IsBad, useNA = "always")]
```

            IsBad
    is_fraud      0      1   <NA>
        0    198453   9043      0
        <NA>      0      0      0

## Characteristics of the application

Amount

``` r
kpis[, FundedLoanAmount := as.numeric(FundedLoanAmount)]
```

Loan types

``` r
# Removing NULL
kpis <- kpis[LoanType != "NULL"]

kpis[, length(unique(LoanType))] # 59 (we can tabulate)
```

    [1] 58

``` r
loan_types <- kpis[, as.data.table(prop.table(table(LoanType)))][order(-N)]
loan_types[, Ncum := sprintf("%.4f", cumsum(N))]

# Many of these could predict fraud exactly
kpis[, as.data.table(
  table(LoanType, is_fraud, useNA = "always")
  )][order(-N)]
```

         LoanType is_fraud     N
      1:      D89        0 78636
      2:      PAB        0 43018
      3:      CA1        0 17776
      4:      D80        0 12542
      5:      IAB        0 11487
     ---                        
    173:      WI2     <NA>     0
    174:      WI3     <NA>     0
    175:      WI4     <NA>     0
    176:      WY1     <NA>     0
    177:     <NA>     <NA>     0

``` r
# Generating a correlation between loan type and fraud
kpis[, perfect := mean(is_fraud), by = LoanType]
kpis[perfect >.90] # 2 records
```

       InquiryDate        UniqueID    PayPeriod LeadPurchasedIndicator
    1:  2021-06-18 L-IDO-5206388T-    BI_WEEKLY                      0
    2:  2020-11-23 L-MOO-5087860T- SEMI_MONTHLY                      0
       FundedIndicator FundedLoanAmount PaymentAmount Term     APR FirstDueDate
    1:               1              500         97.09    7 465.87%   2021-07-01
    2:               1             1500        294.91    6 408.08%   2020-12-07
       SecondDueDate ThirdDueDate ReturningCustomerIndicator NumberOfTradelines
    1:    2021-07-15   2021-07-29                          0                  1
    2:    2020-12-22   2021-01-07                          0                  1
       FirstPaymentDefault LoanType PaidAmount FeesInterestCollected NumberPayments
    1:                   1      ID4       0.00                  0.00              0
    2:                   1      MO5       0.00                  0.00              0
       NumberBounces CollectionIndicator RecoveryIndicator AmountRecovered IsBad
    1:             0                   1                 0            0.00     1
    2:             0                   1                 0            0.00     1
       GRODI180 is_fraud perfect
    1:        0        1       1
    2:        0        1       1

``` r
# Removing those records and cleaning the variable
kpis <- kpis[perfect <= .90][, perfect := NULL]

# Generating an indicator for loan types, we will only analyze
# Those for which we have at least 100 obs
loan_types2 <- kpis[, as.data.table(table(LoanType))][order(-N)]
loan_types2 <- loan_types2[N >= 100]

kpis <- kpis[LoanType %in% loan_types2$LoanType]

for (lt in head(loan_types2$LoanType, -1)) # Excluding the last one (indicator)
  kpis[, paste0("lt_", lt) := as.numeric(LoanType == lt)]
```

Term

``` r
kpis[, length(unique(Term))] # 23
```

    [1] 22

``` r
kpis[, as.data.table(table(Term))][order(-N)]
```

        Term      N
     1:    1 116012
     2:    6  44324
     3:    7  20606
     4:    4  16900
     5:    5   6577
     6:    2   6264
     7:   19   1323
     8:   13    696
     9:    3    573
    10:   18    529
    11:    8    184
    12:   16     13
    13:   20     10
    14:   14      8
    15:   17      5
    16:   10      4
    17:   25      4
    18: NULL      4
    19:   15      2
    20:   12      1
    21:   23      1
    22:   24      1
        Term      N

``` r
# Turning into numeric
kpis[, Term := as.numeric(Term)]
```

    Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Generating a correlation between term and fraud
kpis[, perfect := mean(is_fraud), by = Term]
kpis[perfect >.90] # 0 records
```

    Empty data.table (0 rows and 61 cols): InquiryDate,UniqueID,PayPeriod,LeadPurchasedIndicator,FundedIndicator,FundedLoanAmount...

``` r
kpis[, perfect := NULL] # Removing
```

Pay period

``` r
kpis[, length(unique(PayPeriod))] # 5
```

    [1] 5

``` r
kpis[, as.data.table(table(PayPeriod))][order(-N)]
```

          PayPeriod      N
    1:    BI_WEEKLY 105959
    2:      MONTHLY  38404
    3: SEMI_MONTHLY  38332
    4:       WEEKLY  28330
    5:         NULL   3016

``` r
# Removing NULL obs (we can't use them)
kpis <- kpis[PayPeriod != "NULL"]

# Turning into categorical (indicator variable)
kpis[, pays_weekly := as.numeric(PayPeriod == "WEEKLY")]
kpis[, pays_biweekly := as.numeric(PayPeriod == "BI_WEEKLY")]
kpis[, pays_semimonth := as.numeric(PayPeriod == "SEMI_MONTHLY")]
kpis[, pays_monthly := as.numeric(PayPeriod == "MONTHLY")]

# Tabulating
# kpis[, table(PayPeriod, pays_weekly)] Reference
kpis[, table(PayPeriod, pays_biweekly)]
```

                  pays_biweekly
    PayPeriod           0      1
      BI_WEEKLY         0 105959
      MONTHLY       38404      0
      SEMI_MONTHLY  38332      0
      WEEKLY        28330      0

``` r
kpis[, table(PayPeriod, pays_semimonth)]
```

                  pays_semimonth
    PayPeriod           0      1
      BI_WEEKLY    105959      0
      MONTHLY       38404      0
      SEMI_MONTHLY      0  38332
      WEEKLY        28330      0

``` r
kpis[, table(PayPeriod, pays_monthly)]
```

                  pays_monthly
    PayPeriod           0      1
      BI_WEEKLY    105959      0
      MONTHLY           0  38404
      SEMI_MONTHLY  38332      0
      WEEKLY        28330      0

At this point, I would make sure I have two types of observations for
the model:

1.  Fraud observations, and
2.  Loans that were approved.

We can’t know if an unapproved loan could have been fraud!

``` r
kpis <- kpis[(FundedIndicator == "1") | is_fraud]
kpis[, table(FundedIndicator, is_fraud)]
```

                   is_fraud
    FundedIndicator      0      1
                  1 204189   6836

I would leave it there. I could continue parsing variables in this
fashion. We will only keep the numeric variables

``` r
kpis2 <- kpis[, .SD, .SDcols = c(
  "is_fraud", "Term", "pays_biweekly", "pays_semimonth", 
  "pays_monthly", "UniqueID", "FundedLoanAmount",
  colnames(kpis)[grep("lt_", colnames(kpis))])
  ]
```

# Processing variables

Because of the number of variables, I would inspect the overall types of
all. If all are numeric, I would then continue. Otherwise, I would have
to analyze in detail.

``` r
vars_types <- vars[, unlist(lapply(.SD, class))]
vars_types <- data.table(
  var = names(vars_types),
  type = vars_types
)

vars_types[, table(type)]
```

    type
    character      Date     IDate   integer   logical   numeric 
          402        27        27       398         9       273 

Since a sizable number are ‘character’; I’ll assess if we can turn them
into numeric. For now, I’ll just check whether NULL/NA values can be
found, and after that, I’ll check if they can be turned into numeric

``` r
vars_char <- vars[, lapply(.SD, function(x) {
  fifelse((x == "NULL") | (x == "NA"), NA_character_, x)
}), .SDcols = vars_types[type == "character", var]]

# Reassessing if have any character
vars_char_all_num <- vars_char[, lapply(.SD, \(x) !any(grepl("[[:alpha:]]+", x)))] |> unlist()

# Clunky, but works
for (v in names(vars_char_all_num)[which(vars_char_all_num)])
  vars[, c(v) := as.numeric(.SD[[1]]), .SDcols = v]
```

For now, I will skip the rest of the variables (I only have about 20
minutes left for the ShinyApp!) Preserving only numeric types (plus the
ID)

``` r
vars_types <- vars[, unlist(lapply(.SD, class))]
vars_types <- data.table(
  var = names(vars_types),
  type = vars_types
)

vars_types[, table(type)]
```

    type
    character      Date     IDate   integer   logical   numeric 
           15        27        27       398         9       660 

``` r
tokeep <- vars_types[type %in% c("integer", "numeric")]$var |>
  c("lenderuniqueid") 

vars <- vars[, ..tokeep]
```

## Processing the id variable

We start by inspecting whether the ids can be merged:

``` r
str(kpis[,1:10])
```

    Classes 'data.table' and 'data.frame':  211025 obs. of  10 variables:
     $ InquiryDate           : chr  "2020-07-06" "2020-07-13" "2019-10-01" "2020-12-03" ...
     $ UniqueID              : chr  "L-U270-4996692T-" "L-U270-5000285T-" "L-U180-4804816T-" "L-U170-5095061T-" ...
     $ PayPeriod             : chr  "BI_WEEKLY" "BI_WEEKLY" "SEMI_MONTHLY" "MONTHLY" ...
     $ LeadPurchasedIndicator: chr  "0" "0" "0" "0" ...
     $ FundedIndicator       : chr  "1" "1" "1" "1" ...
     $ FundedLoanAmount      : num  1370 200 2500 1101 500 ...
     $ PaymentAmount         : chr  "237.76" "245.77" "2945.00" "437.40" ...
     $ Term                  : num  6 1 1 7 1 1 1 1 1 1 ...
     $ APR                   : chr  "387.92%" "181.59%" "382.18%" "382.05%" ...
     $ FirstDueDate          : chr  "2020-07-17" "2020-07-31" "2019-10-15" "2021-01-04" ...
     - attr(*, ".internal.selfref")=<externalptr> 

``` r
str(vars[,1:10])
```

    Classes 'data.table' and 'data.frame':  222161 obs. of  10 variables:
     $ setting_inq_app_id   : int  90678647 90667142 90664925 90678698 90663946 90676028 90670218 90668585 90678793 90663969 ...
     $ bqbankstabilityindex : int  2 5 3 5 7 1 9 1 5 1 ...
     $ bqattr1              : int  1 1 1 2 1 1 1 1 2 1 ...
     $ bqattr2              : int  1505 374 2643 249 1607 3 3658 3 697 3 ...
     $ bqattr3              : int  0 0 0 0 0 0 0 0 0 0 ...
     $ bqattr4              : int  0 1 0 1 1 0 1 0 1 0 ...
     $ bqattr5              : int  0 0 0 0 0 0 0 0 0 0 ...
     $ bqattr6              : int  NA NA NA NA NA NA NA NA NA NA ...
     $ generic_bvp_score    : int  276 290 294 211 287 208 268 233 288 208 ...
     $ bvp_rac_hastrans_rule: int  0 0 0 0 0 0 0 0 0 0 ...
     - attr(*, ".internal.selfref")=<externalptr> 

Looking at the first few, it seems that the `lenderuniqueid` may provide
extra information (so it doesn’t fully match with vars.):

``` r
merge(kpis, vars, by.x = "UniqueID", by.y = "lenderuniqueid") # is empty
```

    Empty data.table (0 rows and 1122 cols): UniqueID,InquiryDate,PayPeriod,LeadPurchasedIndicator,FundedIndicator,FundedLoanAmount...

For now, I will assume that the `UniqueID` is the same as
`lenderuniqueid`, and the extra information in lenderuniqueid is not
relevant. After that, we can merge both datasets

``` r
vars[, UniqueID := gsub("_[0-9]+$", "", lenderuniqueid, perl = TRUE)]
vars <- merge(vars, kpis, by = "UniqueID") # This merges
```

## Finally, we will only keep variables for which we have enough information

``` r
avg_missing <- vars[, unlist(lapply(.SD, \(x) mean(is.na(x))))]
avg_missing <- data.table(
  var = names(avg_missing),
  avg_missing = avg_missing
)

# Sorting
setorder(avg_missing, avg_missing)

# We will tolerate 10% missing
avg_missing <- avg_missing[avg_missing <= .1]

vars <- subset(vars, select = c(
  avg_missing$var, "lenderuniqueid", "is_fraud", "UniqueID"
  ))
```

# Saving the data

To make things easy, only the complete cases

``` r
idx <- complete.cases(vars) |> which()
length(idx)
```

    [1] 154444

``` r
idx <- vars[idx]
fwrite(vars, "data/variables.csv.gz")
```

We will also save a smaller version for the app to run faster

``` r
set.seed(3312)
idx <- sample.int(nrow(vars), 20000)
fwrite(vars[idx], "data/variables20000.csv.gz")
```
