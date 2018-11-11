## DFS Optimizer Code
## by Fraser Walker

rm(list = ls())
setwd(paste(
  "C:/Users/frase/Documents/DraftKings DFS",
  sep = "/"))
source("functions.R")
pckgs <- c("dplyr", "ggplot2", "gurobi")
lapply(pckgs, library, character.only = TRUE)
rm(pckgs)

# Load Data ----

# salaries
dk <- read.csv(file.choose(), 
               header = TRUE, 
               sep = ",",
               stringsAsFactors = FALSE) %>%
  select(ID, Name, AvgPointsPerGame, Salary, Position, TeamAbbrev)

# projections
proj <- read.csv(file.choose(), header = TRUE, sep = ",",
                 stringsAsFactors = FALSE) %>%
  select(PID, Player, Pos, Team, FFPts)

# key value pairs
key <- read.csv("dk_4for4_key.csv", header = TRUE, sep = ",",
               stringsAsFactors = FALSE)

colnames(dk) <- c("id","name","points","salary","position", "team")
colnames(proj) <- c("id", "name", "position", "team", "proj") # 4for4

# Specify the week! ----

week <- c("id.dk.w10")

# Update id sheet ----

# specify the week
key <- dkUpdate(dk, key, week) %>%
  { projUpdate(proj, .) }
write.csv(key, "dk_4for4_key.csv", row.names = FALSE)

# join projections based on key value pairs from dk_4for4_key csv

df.comb <- dfsMerge(dk, proj, key, week) %>%
  completeFun(., "proj") # remove players without projections

# optional filter remove teams not on slate
teams <- c()
players <- c("Alvin Kamara")

df.comb <- dfsFilter(df.comb, key = "teams", teams)

name <- df.comb$name # save names for later

# select final columns
df.comb <- select(df.comb, name, proj, salary, qb, rb, wr, te, flex, dst)

# optional filter require certain players
players <- c("Alvin Kamara")
df.comb <- dfsFilter(df.comb, key = "players", players)

# Gurobi wants the players to be 'columns' (players are binary vars)
df.mat <- as.matrix(df.comb[,-1]) %>%
  t() 

# Gurobi model building ----

result <- gurobiModel(df.mat, players)

print('Solution:')
print(result$objval)
ind <- which(result$x > c(0.9))
print(name[ind])