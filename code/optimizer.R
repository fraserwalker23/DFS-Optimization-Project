## DFS Optimizer Code
## by Fraser Walker

rm(list = ls())
setwd(paste(
  "C:/Users/frase/Documents/DraftKings DFS",
  sep = "/"))
source("functions.R")
pckgs <- c("data.table", "tidyr", "stringdist", "gurobi")
lapply(pckgs, library, character.only = TRUE)
rm(pckgs)

# Load Data ----

# salaries
dk <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE) %>%
  select(dkId = ID, 
         dkName = Name, 
         dkPosition = Position, 
         dkTeam = TeamAbbrev, 
         dkPpg = AvgPointsPerGame,
         dkSalary = Salary)

# projections
proj <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Matching ----

mat <- stringdistmatrix(dk$Name, proj$Player)
l <- apply(mat, 1, function(x)  which(x <= 5))

# code empty values to NA
for (i in 1:length(l)){
  if (length(l[[i]]) == 0){
    l[[i]] <- NA
  }
}

lt <- transpose(l)

df <- as.data.frame(lt, 
                    col.names = paste0("match", c(1:length(lt)))) %>%
  cbind(., dk) %>%
  gather(., key = number, value = index, -starts_with("dk")) %>%
  filter(!is.na(index) | 
           (is.na(index) & number == "match1")) %>%
  mutate(fourName = proj$Player[index],
         fourPosition = proj$Pos[index],
         fourTeam = proj$Team[index],
         fourId = proj$PID[index],
         fourProj = proj$FFPts[index],
         stringDist = stringdist(dkName, fourName),
         similarity = stringsim(dkName, fourName),
         qualityScore = 1*(stringDist <= 4) + 
           1*(dkPosition == fourPosition) + 
           1*(dkTeam == fourTeam)
  ) %>%
  arrange(dkId) %>%
  filter(qualityScore >= 2) %>%
  group_by(fourId) %>%
  top_n(1, qualityScore) %>%
  top_n(1, similarity) %>%
  group_by(dkId) %>%
  top_n(1, qualityScore) %>%
  top_n(1, similarity)

# Formatting ----

final <- bind_rows(df, filter(dk, dkPosition == "DST")) %>%
  mutate(
    qb = 1*(dkPosition == 'QB'),
    rb = 1*(dkPosition == 'RB'),
    wr = 1*(dkPosition == 'WR'),
    te = 1*(dkPosition == 'TE'),
    flex = 1*(dkPosition == 'RB' | dkPosition == 'WR'),
    dst = 1*(dkPosition == 'DST'),
    proj = if_else(is.na(fourProj) & dst == 1, dkPpg, fourProj)
  ) %>%
  ungroup() %>%
  select(dkName, proj, dkSalary, qb, rb, wr, te, flex, dst)

name <- final$dkName # save for later

# optional filter require certain players
players <- c("")
final <- dfsFilter(final, key = "players", players)

# Gurobi wants the players to be 'columns' (players are binary vars)
df.mat <- as.matrix(df.comb[,-1]) %>%
  t() 

# Gurobi model building ----

result <- gurobiModel(df.mat, players)

print('Solution:')
print(result$objval)
ind <- which(result$x > c(0.9))
print(name[ind])