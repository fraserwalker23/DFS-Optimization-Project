# DFS-Optimization-Project
final project for CSE 710. Optimizing an NFL DFS lineup

This repository contains the code used in the final project for CSE 710: Engineering Optimization taught by Dr. Tim Davidson at McMaster University, completed in the Winter 2018 school term. The optimizer is powered by Gurobi (http://www.gurobi.com/index) and the academic license is currently valid until May 2019. I would like to continue working with this code when the NFL season begins in September 2018 and I hope to make updates during the season. Presently too many elements of the code are too ad-hoc for my liking. I would also like to practive developing a user interface for the code (vs. running a script in the R command line for example).

# Update: 11/11/2018

Changed a lot of the code from when I first uploaded it. Code is now broken down into two files: optimizer.R and functions.R, I tried to hide any behind the scenes/heavy lifting in the functions.R file in order to keep the main optimizer.R script as readable as possible. Also changed the sample data given here: both the projections and salary .csv sheets are from Week 1 of the 2018 season. And for anyone who is curious, I also have an excel workbook which is keeping track of my results.

This code currently uses DraftKings salaries (http://draftkings.com) and projections from 4for4 (https://www.4for4.com/). Please keep in mind that 4for4 has a paid subscription to access their projections. 

# Update: 1/4/2019

Major changes to the optimizer code. This was prompted when I realized I hadn't been correctly linking Amari Cooper ever since he got traded to Dallas Lol. The old process had a lot of problems at the merging stage when trying to link DraftKings data to 4for4 data. I completely got rid of the dk_4for4_key.csv file and the merging uses fuzzy string matching techniques and is FAR more robust than before.

Beyond that, I finally drafted up a web application version of the optimizer using R Shiny (https://shiny.rstudio.com/). The application is admittedly bare bones but it has all the functionality of the code and presents the results cleanly (vs. running the code from a script and reading the results in the console). NOTE: this application will not run without the Gurobi package.

Finally, I've reorganized the repository to include folders and make it nicer to sift through. The results for this season have been middling but this isn't entirely surprising considering 1) issues RE: merging DK and 4for4 sources and 2) NFL DFS is generally pretty tough to do well in anyway.

I'm also thinking about writing up a little report/article on fuzzy string matching and how it related to this DFS project. I think this would be illustrative since I doubt these problems are only unique to me and would give me good practice using markdown.
