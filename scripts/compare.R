

mhsurv0 <- readRDS("data/2-final/past/scored_results.rds")
mhsum0 <- readRDS("data/2-final/past/summarized_results.rds")

all.equal(
  mhsurv$q4_1,
  mhsurv0$q4.1$q4.1
)

all.equal(
  mhsurv$q4_2,
  mhsurv0$q4.2$q4.2
)

all.equal(
  mhsurv$q4_3,
  mhsurv0$q4.3$q4.3
)

all.equal(
  mhsurv$q4_4,
  mhsurv0$q4.4$q4.4
)

all.equal(
  mhsurv$q5,
  mhsurv0$q5$q5
)

all.equal(
  mhsurv$q6,
  mhsurv0$q6$q6
)

all.equal(
  mhsurv$q7,
  mhsurv0$q7$q7
)

# 8
mhsurv |>
  group_by(q8_recode) |>
  count()

mhsurv0$q8 |>
  group_by(q8) |>
  count()

# 9
all.equal(
  mhsurv$q9,
  mhsurv0$q9$q9
)

# 10
all.equal(
  mhsurv$q10,
  mhsurv0$q10$q10
)

# 11
df0 <- as.data.frame(
  lapply(
    mhsurv0[grepl("^q11", names(mhsurv0))],
    \(x) x[, grepl("^q11", colnames(x))]
  )
)

df0 <- cbind(id = mhsurv0$q11.01$id, df0)

df <- mhsurv |>
  filter(!is.na(q11_01)) |>
  select(id, matches("^q11_\\d")) |>
  as.data.frame()

all.equal(df0, df)

# 12
all.equal(
  mhsurv$q12,
  mhsurv0$q12$q12
)

# 13
df0 <- mhsurv0$q13 |>
  select(id, q13)

df <- mhsurv |>
  select(id, q13_binned) |>
  filter(!is.na(q13_binned))

all.equal(df0, df)

# 14
df0 <- mhsurv0$q14 |>
  select(id, q14_raw, q14)

df <- mhsurv |>
  select(id, q14_scored, q14_binned) |>
  filter(!is.na(q14_binned))

all.equal(df0, df)

# 15
df0 <- mhsurv0$q15 |>
  select(id, q15_raw, q15)

df <- mhsurv |>
  select(id, q15, q15_scored)

all.equal(df0, df)

# 16
df0 <- mhsurv0$q16 |>
  select(id, hse, q16)

df <- mhsurv |>
  select(id, hse_pct, hse_scored) |>
  filter(!is.na(hse_scored))

all.equal(df0, df)

# 17
all.equal(
  mhsurv$q17,
  mhsurv0$q17$q17
)

