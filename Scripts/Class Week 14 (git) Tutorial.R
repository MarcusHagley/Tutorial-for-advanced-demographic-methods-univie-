# Install necessary packages if you haven't yet
# install.packages(c("usethis", "gitcreds"))

library(usethis)
library(gitcreds)

cat("The 'usethis' package helps you create a Personal Access Token (PAT). 
A PAT is like a special password created on GitHub, allowing Git to identify you securely. 
You'll specify the token's access permissions on the GitHub webpage.\n\n
Once stored, Git uses your PAT automatically for authentication instead of your password.\n\n")

# Opens browser to create your PAT on GitHub
create_github_token()

cat("\n\nNow, copy the PAT from GitHub, and store it securely using gitcreds.\n\n")

# Securely store your PAT so you don't need to type it repeatedly
gitcreds_set()

cat("\nAdditional useful Git commands:\n
  git pull                               # fetch and merge latest changes from GitHub\n
  git add .                              # stage all changes for commit\n
  git branch                             # list all branches and show current branch\n
  git checkout -b branchname             # create and switch to a new branch\n
  git log                                # show commit history\n
  git clone URL                          # download repository from GitHub\n
  git merge branchname                   # merge changes from another branch\n
")

