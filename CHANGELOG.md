## 1.0.0
* Improve startup behavior by refreshing local status without fetching from remotes
* Restore terminal state safely during single-key input
* Execute built-in Git actions without shell-interpolating repository paths, branch names, or commit messages
* Store RepoConductor tags by repository path and preserve tags outside the current scan
* Show exact checked-out Git tags instead of plain HEAD in detached repositories
* Use a faster status refresh based on `git status --porcelain=v2 --branch`
* Refactor repository status and filters into named records
* Add complete product documentation

## 0.2.0
* Discover git repositories recursively from the current working directory instead of reading configured paths
* Remove the TOML config dependency for repository targeting

## 0.1.1
* "Dirty" filter works as a toggle from now on

## 0.1.0

* The very first release. Made in Haskell <3
