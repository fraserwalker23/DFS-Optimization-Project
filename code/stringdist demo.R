library(stringdist)
library(tidyr)
library(data.table)

dk <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE) %>%
  select(dkId = ID, 
         dkName = Name, 
         dkPosition = Position, 
         dkTeam = TeamAbbrev, 
         dkPpg = AvgPointsPerGame,
         dkSalary = Salary)
proj <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)

# examples ----

# example 1: amatch with Todd Gurley
amatch("Todd Gurley II", proj$Player, maxDist = 5)

# example 2: stringdist with Todd Gurley
stringdist("Todd Gurley II", c("Aaron Rodgers", "Tyrod Taylor", "Todd Gurley"))
sort(stringdist("Todd Gurley II", proj$Player))

# example 3: stringdist with Jacoby Brissett (not in lookup table)
sort(stringdist("Jacoby Brissett", proj$Player))

# example 4: phonetic with Todd Gurley, Todd Gurley II
phonetic(c("Todd Gurley", "Todd Gurley II"))

# example 5: similarity scores
stringsim(c("Todd Gurley", "Odell Beckham", "TJ Jones", "LeVeon Bell"), 
          c("Todd Gurley II", "Odell Beckham Jr.", "T.J. Jones", "Le'Veon Bell"))

# example 6: stringdistance Matrix
stringdistmatrix(c("Todd Gurley", "Odell Beckham", "TJ Jones", "LeVeon Bell"), 
                 c("Todd Gurley II", "Odell Beckham Jr.", "T.J. Jones", "Le'Veon Bell"))

# example 7: problematic cases
stringdistmatrix(c("Benjamin Watson", "Michael Thomas"), 
                 c("Ben Watson", "Mike Thomas", "Deshaun Watson", "Michael Thomas"))

# matching ----

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

# formatting ----

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

# diagnostics ----

# multiple <- df %>%
#   group_by(fourName) %>%
#   filter(n()>1)

# low <- filter(df, similarity <= 2/3)

# unmatched <- filter(proj, !(Player %in% df$fourName))

# noProj <- final[!(complete.cases(final$proj)), ]