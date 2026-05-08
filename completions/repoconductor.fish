# fish completion for repoconductor
# Install: copy to /usr/share/fish/vendor_completions.d/repoconductor.fish
# or to ~/.config/fish/completions/repoconductor.fish

complete -c repoconductor -s h -l help        -d 'Show this help text'
complete -c repoconductor -s V -l version     -d 'Show version and exit'
complete -c repoconductor -s p -l path        -d 'Scan DIR instead of the current working directory' -r -F -a '(__fish_complete_directories)'
complete -c repoconductor      -l no-recurse  -d 'Do not recurse: only treat immediate children of the path as candidates'
complete -c repoconductor -s j -l jobs        -d 'Maximum concurrent git jobs' -x -a '1 2 4 8 16'
complete -c repoconductor      -l no-startup-refresh -d 'Skip the initial status refresh'
complete -c repoconductor      -l debug       -d 'Verbose failure output (full captured stderr per failure)'
