setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")
library("usethis")
library("gert")
library("gitcreds")

# Change these to match your setup exactly
my_repo_url <- "https://github.com/punkmoose11/andyhulme.git"
my_folder   <- "C:/Users/andyh/Documents/OMAHASERIES/"

# Fix SSL issues (Use Windows Secure Channel)
sys::exec_wait("git", c("config", "--global", "http.sslBackend", "schannel"))

# Ensure we are pointing to the right GitHub address
try(git_remote_add(my_repo_url, name = "origin"), silent = TRUE)
git_remote_set_url(my_repo_url, remote = "origin")

message("Staging files...")
git_add(".") # This stages everything in the folder

# Check if there are actually changes to commit
status <- git_status()
if (nrow(status) == 0) {
  message("No changes detected. Nothing to upload!")
} else {
  message("Committing changes...")
  git_commit(paste("Auto-update:", Sys.time()))
  
  # Ensure branch is named 'main' (Fixes the 'master' vs 'main' error)
  try(git_branch_move(branch = "master", new_branch = "main"), silent = TRUE)
  
  message("Pushing to GitHub...")
  # We use the explicit refspec to avoid 'refspec' errors
  git_push(
    remote = "origin", 
    refspec = "refs/heads/main:refs/heads/main",
    set_upstream = TRUE
  )
  message("SUCCESS! Your files are live.")
}