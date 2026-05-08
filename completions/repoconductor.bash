# bash completion for repoconductor
# Install: copy to /usr/share/bash-completion/completions/repoconductor
# or source from your ~/.bashrc

_repoconductor()
{
    local cur prev
    if declare -F _init_completion >/dev/null 2>&1; then
        _init_completion || return
    else
        cur="${COMP_WORDS[COMP_CWORD]}"
        prev="${COMP_WORDS[COMP_CWORD-1]}"
    fi

    local opts="--help -h --version -V --path -p --no-recurse --jobs -j --no-startup-refresh --debug"

    case "$prev" in
        --path|-p)
            if declare -F _filedir >/dev/null 2>&1; then
                _filedir -d
            else
                COMPREPLY=( $(compgen -d -- "$cur") )
            fi
            return 0
            ;;
        --jobs|-j)
            COMPREPLY=( $(compgen -W "1 2 4 8 16" -- "$cur") )
            return 0
            ;;
    esac

    COMPREPLY=( $(compgen -W "$opts" -- "$cur") )
}
complete -F _repoconductor repoconductor
