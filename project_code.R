# ECE 710 Project
# Fraser Walker (001219429)

setwd(paste(
  "C:/Users/frase/Documents/School Work/MSc/Winter 2018/ECE 710/Project R",
  sep = "/"))

pckgs <- c("dplyr", "ggplot2", "gurobi")
lapply(pckgs, library, character.only = TRUE)
rm(pckgs)

# function definitions ----

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# load data ----

df <- read.csv("dksalaries.csv", header = TRUE, sep = ",") %>%
  select(Name, AvgPointsPerGame, Salary, Position)

df2 <- read.csv("proj.csv", header = TRUE, sep = ",") %>%
  select(Player, FFPts)

df_name <- read.csv("dfs_name.csv", header = TRUE, sep = ",")

colnames(df) <- c("name","points","salary","position")
colnames(df2) <- c("name", "proj")

# replace unofficial names with official names ----
for (i in 1:nrow(df_name)){
  df$name <- gsub(df_name$name[i], df_name$official[i], df$name)
}
for (i in 1:nrow(df_name)){
  df2$name <- gsub(df_name$name[i], df_name$official[i], df2$name)
}

# creating final df ----
df3 <- merge(df, df2, by.x = "name", by.y = "name", all.x = TRUE, all.y = TRUE) %>%
  completeFun(., "salary") %>%
  mutate(
    qb = 1*(position == 'QB'),
    rb = 1*(position == 'RB'),
    wr = 1*(position == 'WR'),
    te = 1*(position == 'TE'),
    flex = 1*(position == 'RB' | position == 'WR'),
    dst = 1*(position == 'DST')
  )

for (i in 1:nrow(df3)){
  if(is.na(df3$proj[i]) == TRUE){
    df3$proj[i] <- df3$points[i]
  }
}

# remove players who aren't playing
name <- df3$name
del <- grep("Ezekiel Elliott|Carson Wentz|Carson Palmer|Allen Hurns", name)
df3 <- df3[-del,]
name <- df3$name

df3 <- select(df3,-starts_with("points"), -starts_with("name"), -starts_with("position")) %>%
  select(proj, everything())

df.mat <- as.matrix(df3) %>%
  t() # Gurobi wants the players to be 'columns' (players are binary vars)

# Gurobi model building ----

A <- df.mat[-1,]
obj <- df.mat[1,]

model <- list()

model$A <- A
model$obj <- obj
model$modelsense <- 'max'
model$rhs        <- c(50000,1,3,4,1,6,1)
model$sense      <- c('<', '=', '<', '<', '=', '=', '=')
model$vtype      <- 'B'

result <- gurobi(model)

print('Solution:')
print(result$objval)
ind <- which(result$x > c(0.9))
print(name[ind])

# alternative formulation: TE at flex ----

df <- read.csv("dksalaries.csv", header = TRUE, sep = ",") %>%
  select(Name, AvgPointsPerGame, Salary, Position)

colnames(df) <- c("name","points","salary","position")

df <- df %>%
  mutate(
    qb = 1*(position == 'QB'),
    rb = 1*(position == 'RB'),
    r2 = 1*(position == 'RB'),
    wr = 1*(position == 'WR'),
    wr2 = 1*(position == 'WR'),
    te = 1*(position == 'TE'),
    te2 = 1*(position == 'TE'),
    flex = 1*(position == 'RB' | position == 'WR' | position == 'TE'),
    dst = 1*(position == 'DST')
  )
name <- df$name
#df$points[27] <- 0 # Zeke was suspended for this game, comment out as required
df <- select(df,-starts_with("position"), -starts_with("name"))

df.mat <- as.matrix(df) %>%
  t() # Gurobi wants the players to be 'columns' (players are binary vars)

A <- df.mat[-1,]
obj <- df.mat[1,]

model <- list()

model$A <- A
model$obj <- obj
model$modelsense <- 'max'
model$rhs        <- c(50000,1,2,3,3,4,1,2,7,1)
model$sense      <- c('<', '=', '>', '<', '>', '<', '>','<','=','=')
model$vtype      <- 'B'

result <- gurobi(model)

print('Solution:')
print(result$objval)
ind <- which(result$x %in% c(1))
print(name[ind])
