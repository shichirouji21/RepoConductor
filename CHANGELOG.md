## 1.3.1
* Fix tag persistence so adding, removing, or clearing tags from one subfolder no longer deletes the on-disk tags of repositories discovered from a different subfolder

## 1.3.0
* Add a Nix flake at the repository root: `nix run github:shichirouji21/RepoConductor`, `nix profile install github:shichirouji21/RepoConductor`, `nix shell github:shichirouji21/RepoConductor`
* Install bash, zsh, and fish completion files via the flake's `postInstall`
* Provide a Haskell development shell via `nix develop` with `cabal-install`, `haskell-language-server`, and `git`

## 1.2.0
* Add command-line flags: `--version`, `--path`, `--no-recurse`, `--jobs`, `--no-startup-refresh`, `--debug`
* Ship shell completion scripts for bash, zsh, and fish in `completions/`
* Allow scanning a directory other than the current working directory via `--path`
* Allow non-recursive discovery of immediate child repositories via `--no-recurse`
* Allow overriding the concurrent job cap via `--jobs`
* Allow skipping the initial status refresh via `--no-startup-refresh`
* Print full captured failure output when `--debug` is set; otherwise truncate to 200 characters

## 1.1.0
* Refresh repository status concurrently with a CPU-relative job cap
* Run built-in actions (fetch, pull, switch, commit & push, manual) concurrently across filtered repositories
* Show per-repo job state in a dedicated `Job` column (`wait`, `run`, `ok`, `err`) with a live-updating dashboard
* Print captured failure output for each failed repository after a run completes
* Build the executable threaded with `-N` so concurrency uses available CPU cores

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
