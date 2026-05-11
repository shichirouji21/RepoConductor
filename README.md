# RepoConductor

[![Version](https://img.shields.io/badge/version-1.3.1-blue)](https://github.com/shichirouji21/RepoConductor/releases/tag/v1.3.1)
[![AUR](https://img.shields.io/badge/AUR-repoconductor--bin-1793d1)](https://aur.archlinux.org/packages/repoconductor-bin)
[![License](https://img.shields.io/badge/license-BSD--2--Clause-green)](LICENSE)

RepoConductor is a terminal dashboard for managing many Git repositories at once.

Run it from a directory such as `~/repos`, and it recursively discovers Git repositories below the current working directory. From there, you can inspect status, filter repositories, run common Git actions, open `lazygit`, and organize repositories with local tags.

```text
[RepoConductor] (v1.3.1)

Name                    Branch          <-   ->   M    Job   Tag

RepoConductor           master          ok                ok
my-service              feature/auth              2        run    work|api
old-release             v1.2.0          ok                ok

(F)ilter, (A)ction, (U)pdate, (T)ag, (Q)uit
```

## Why

If you keep many projects locally, small maintenance tasks become repetitive:

```bash
cd repo-a && git fetch
cd repo-b && git status
cd repo-c && git pull
```

RepoConductor turns that pile of repositories into one focused terminal view. It is built for quickly answering questions like:

```text
Which repositories are dirty?
Which repositories are behind upstream?
Which repos are on this branch?
Which work repos need attention today?
```

## Features

| Feature | Description |
|---|---|
| Recursive discovery | Finds Git repositories under the current working directory. No repository path config is required. |
| Multi-repo status | Shows branch or checked-out tag, behind count, ahead count, and modified file count. |
| Concurrent operations | Refreshes status and runs built-in actions concurrently across filtered repositories with a CPU-relative job cap. |
| Live progress | A `Job` column shows per-repo state (`wait`, `run`, `ok`, `err`) and the dashboard redraws while work is in flight. |
| Fast refresh | Uses `git status --porcelain=v2 --branch` for compact status collection. |
| Filters | Filter by dirty state, repository name, branch, or RepoConductor tag. |
| Actions | Run `fetch`, `pull`, `switch`, commit and push, `lazygit`, or a manual command across filtered repositories. |
| Failure reporting | Captures stderr from failed Git actions and prints it after the run completes. |
| Tags | Add, remove, and clear local tags for groups of repositories. |
| Safer built-ins | Built-in Git actions use argument-list process execution instead of shell string interpolation. |
| Terminal-safe input | Terminal mode is restored even if input handling is interrupted. |

## Installation

### Arch Linux

```bash
paru -S repoconductor-bin
```

or with another AUR helper:

```bash
yay -S repoconductor-bin
```

### GitHub Release

Download the latest `repoconductor` binary from the releases page:

```text
https://github.com/shichirouji21/RepoConductor/releases
```

Then place it somewhere on your `PATH`, for example:

```bash
chmod +x repoconductor
mkdir -p ~/.local/bin
mv repoconductor ~/.local/bin/
```

### Nix

RepoConductor ships a flake. With Nix installed and flakes enabled:

```bash
# Run without installing
nix run github:shichirouji21/RepoConductor

# Install into your user profile
nix profile install github:shichirouji21/RepoConductor

# Drop into an ephemeral shell that has it on PATH
nix shell github:shichirouji21/RepoConductor
```

The flake installs the binary plus bash, zsh, and fish completion files. It works on `x86_64-linux`, `aarch64-linux`, `x86_64-darwin`, and `aarch64-darwin`.

### From Source

Requirements:

```text
GHC
Cabal
Git
```

Build locally:

```bash
cabal build
```

Run the built executable:

```bash
cabal run repoconductor
```

Install with Cabal:

```bash
cabal install exe:repoconductor
```

## Usage

Open RepoConductor from the directory that contains your repositories:

```bash
cd ~/repos
repoconductor
```

RepoConductor scans recursively from the current directory. If `~/repos` contains `RepoConductor`, `my-api`, `sandbox/tool`, and `archive/project`, they are all included.

When a repository itself is found, RepoConductor treats that directory as the repository root and does not keep walking inside it.

## Command-Line Flags

| Flag | Description |
|---|---|
| `-V`, `--version` | Show version and exit. |
| `-h`, `--help` | Show built-in help and exit. |
| `-p DIR`, `--path DIR` | Scan `DIR` instead of the current working directory. |
| `--no-recurse` | Only treat immediate children of the path as candidates. |
| `-j N`, `--jobs N` | Maximum concurrent git jobs. Default: `max(4, capabilities * 2)`. |
| `--no-startup-refresh` | Skip the initial status refresh; rows stay empty until `U` is pressed. |
| `--debug` | Verbose failure output (full captured stderr per failure). |

Examples:

```bash
repoconductor --version
repoconductor --path ~/work --no-recurse
repoconductor --jobs 16 --debug
```

## Shell Completion

RepoConductor ships completion for bash, zsh, and fish. The Arch package installs them automatically. For manual installation:

```bash
# bash
sudo install -Dm644 completions/repoconductor.bash /usr/share/bash-completion/completions/repoconductor

# zsh
sudo install -Dm644 completions/_repoconductor /usr/share/zsh/site-functions/_repoconductor

# fish
sudo install -Dm644 completions/repoconductor.fish /usr/share/fish/vendor_completions.d/repoconductor.fish
```

## Dashboard

| Column | Meaning |
|---|---|
| `Name` | Repository directory name. |
| `Branch` | Current branch, checked-out tag, or `HEAD` when detached without an exact tag. |
| `<-` | Commits behind upstream. |
| `->` | Commits ahead of upstream. |
| `M` | Modified, added, deleted, or otherwise changed files. |
| `Job` | Per-repo state of the most recent operation: `wait` (queued), `run` (in progress), `ok` (succeeded), `err` (failed). Empty when idle. |
| `Tag` | Local RepoConductor tags assigned to the repository. |

If a repository has a tag checked out, RepoConductor shows the tag name instead of plain `HEAD`.

## Controls

### Main Menu

| Key | Action |
|---|---|
| `F` | Open filters. |
| `A` | Open actions. |
| `U` | Refresh status without fetching. |
| `T` | Add, remove, or clear RepoConductor tags. |
| `Q` | Quit. |

### Filters

| Key | Filter |
|---|---|
| `A` | Clear all filters. |
| `D` | Toggle dirty-only repositories. |
| `N` | Filter by repository name. |
| `B` | Filter by branch or checked-out tag text. |
| `T` | Filter by RepoConductor tag. |

Filters affect both the displayed repositories and subsequent actions.

### Actions

| Key | Action |
|---|---|
| `F` | Run `git fetch`. |
| `P` | Run `git pull`. |
| `S` | Prompt for a branch and run `git switch <branch>`. |
| `C` | Prompt for a commit message, then run `git add .`, `git commit -m <message>`, and `git push`. |
| `L` | Open `lazygit` for each filtered repository, one at a time. |
| `M` | Prompt for a manual shell command and run it in each filtered repository. |

Built-in actions are executed with the repository as the process working directory. Manual commands intentionally use the shell so you can run complex commands when you need to.

## Tags

RepoConductor tags are local labels for grouping repositories. They are separate from Git tags.

Examples:

```text
work
haskell
client-a
archive
```

Use tags to filter and act on meaningful groups of repositories, such as all work projects or all Haskell projects.

Tag data is stored at:

```text
~/.config/repoconductor/tags.shi
```

Tags are keyed by absolute repository path, so repositories with the same directory name do not collide.

## Notes

RepoConductor does not use a repository list config file. The current working directory is the source of truth.

RepoConductor does not fetch on startup. Startup performs a local status refresh only. Use `A` then `F` when you want to fetch from remotes.

`lazygit` support is optional. If `lazygit` is not installed, the action simply fails for that command.

## Development

Build:

```bash
cabal build
```

Run:

```bash
cabal run repoconductor
```

Clean build output:

```bash
cabal clean
```

With Nix, drop into a development shell that pins `cabal-install`, `haskell-language-server`, and `git`:

```bash
nix develop
```

## Release Checklist

For a binary release consumed by `repoconductor-bin` on AUR:

```text
1. Update version metadata and changelog.
2. Build the release binary.
3. Create a GitHub tag and release.
4. Upload the binary as the release asset named repoconductor.
5. Update AUR PKGBUILD pkgver and sha256sums.
6. Regenerate .SRCINFO with makepkg --printsrcinfo > .SRCINFO.
7. Verify with makepkg --verifysource.
8. Commit and push the AUR package repository.
```

## License

RepoConductor is licensed under the BSD-2-Clause license. See [LICENSE](LICENSE).
