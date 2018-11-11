# Supplementary Functions for DFS Optimizer


# completeFun() ----
# remove any columns that take on an NA value for NA

completeFun <- function(data, desiredCols) {
  require(dplyr)
  completeVec <- complete.cases(data[, desiredCols])
  data <- filter(data, completeVec)
}

# dkUpdate() ----
# update the id file with new draftkings salary info

dkUpdate <- function(dk, key, week){
  require(dplyr)
  test <- full_join(dk,
                key,
                by = c("name" = "name", 
                       "position" = "position", 
                       "team" = "team")) %>%
    select(-starts_with("points"), 
           -starts_with("salary"))
x <- colnames(test)
colnames(test) <- c(week, x[2:length(x)])
return(test) 
}

# projUpdate() ----
# update the id file with new projections info

projUpdate <- function(proj, key){
  require(dplyr)
  
  # look for new names not in the key sheet
  new<- filter(proj, !(id %in% key$id.4for4 
               & position %in% key$position
               & team %in% key$team)
               & position != "K")
  
  test <- full_join(key,
                    new,
                    by = c("name" = "name",
                           "position" = "position",
                           "team" = "team")) %>%
    filter(name %in% key$name) %>%
    mutate(id.4for4 = if_else(is.na(id.4for4) == TRUE, 
                              id, 
                              id.4for4)) %>%
    select(-ends_with("id"), -starts_with("proj"))
  
  return(test)
  
}

# dfsMerge() ----
# merge the prices with the projections via the key value pairs

dfsMerge <- function(dk, pr, key, week){
  require(dplyr)
  comb <- left_join(dk,
                    select(key, id.4for4, week),
                    by = c("id" = week)) %>%
    left_join(.,
          select(pr, id, proj),
          by = c("id.4for4" = "id")) %>%
    mutate(
      qb = 1*(position == 'QB'),
      rb = 1*(position == 'RB'),
      wr = 1*(position == 'WR'),
      te = 1*(position == 'TE'),
      flex = 1*(position == 'RB' | position == 'WR'),
      dst = 1*(position == 'DST')
    ) %>%
    select(id.4for4, name, position, team, points, salary, proj, qb, rb, wr, te, flex, dst) %>%
    mutate(proj = if_else(is.na(proj) == TRUE & dst == 1, points, proj))

  return(comb)
}

# dfsFilter() ----
# optional filter to remove certain teams, or require certain players

dfsFilter <- function(df, key, value){
  
  if (key == "teams"){
    df <- filter(df, !(team %in% value)) 
  } else if (key == "players"){
    df <- mutate(df,
                 must = 1*(name %in% value))
  }
  return(df)
}

# gurobiModel() ----
# build the optimizer, takes an optional argument if any players were specified

gurobiModel <- function(mat, players){
  
  A <- mat[-1,]
  obj <- mat[1,]
  
  model <- list()
  
  model$A <- A
  model$obj <- obj
  model$modelsense <- 'max'
  
  if (length(players) > 0){
    model$rhs        <- c(50000,1,3,4,1,6,1
                          , length(players) # 'must' optional for specific player(s)
    )
    model$sense      <- c('<', '=', '<', '<', '=', '=', '='
                          , '=' # 'must' optional for specific player(s)
    )
  } else{
    model$rhs        <- c(50000,1,3,4,1,6,1)
    model$sense      <- c('<', '=', '<', '<', '=', '=', '=')
  }
  model$vtype      <- 'B'
  
  result <- gurobi(model)
  return(result)
}