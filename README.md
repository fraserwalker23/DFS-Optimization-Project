# DFS-Optimization-Project
final project for CSE 710. Optimizing an NFL DFS lineup

This repository contains the code used in the final project for CSE 710: Engineering Optimization taught by Dr. Tim Davidson at McMaster University, completed in the Winter 2018 school term. The optimizer is powered by Gurobi (http://www.gurobi.com/index) and the academic license is currently valid until May 2019. I would like to continue working with this code when the NFL season begins in September 2018 and I hope to make updates during the season. Presently too many elements of the code are too ad-hoc for my liking. I would also like to practive developing a user interface for the code (vs. running a script in the R command line for example).

# Update: 11/11/2018 -----

changed a lot of the code from when I first uploaded it. Code is now broken down into two files: optimizer.R and functions.R, I tried to hide any behind the scenes/heavy lifting in the functions.R file in order to keep the main optimizer.R script as readable as possible. Also changed the sample data given here: both the projections and salary .csv sheets are from Week 1 of the 2018 season. And for anyone who is curious, I also have an excel workbook which is keeping track of my results.
