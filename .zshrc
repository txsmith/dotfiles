# Path to your oh-my-zsh installation.
  export ZSH=/home/thomas/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'


# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git gitfast dirhistory mvn npm sudo web-search wd stack)

# User configuration

# NPM global package directory
  NPM_PACKAGES="${HOME}/.npm-packages"
  # Unset manpath so we can inherit from /etc/manpath via the `manpath` command
  unset MANPATH # delete if you already modified MANPATH elsewhere in your config
  export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"

  export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/thomas/.local/bin"
  export PATH="/home/thomas/.npm-global/bin:$PATH"
  export PATH="$HOME/.cargo/bin:$PATH"
  export PATH="$NPM_PACKAGES/bin:$PATH"

# GPG agent config
  GPG_TTY=$(tty)
  SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
  export GPG_TTY SSH_AUTH_SOCK

  export PASSWORD_STORE_CLIP_TIME=10

  # export _JAVA_AWT_WM_NONREPARENTING=1

source $ZSH/oh-my-zsh.sh

zstyle ':completion:*' matcher-list '' \
  'm:{a-z\-}={A-Z\_}' \
  'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
  'r:[[:ascii:]]||[[:ascii:]]=** r:|=* m:{a-z\-}={A-Z\_}'

# Alt-up keybind
cdParentKey() {
  pushd .. > /dev/null
  zle      reset-prompt
  echo
  ls
  echo
}

zle -N                 cdParentKey
bindkey '^[[1;3A'      cdParentKey

# redefine prompt_context for hiding user@hostname
prompt_context () { }

# Git alisaes
alias gpu='git push --set-upstream origin $(git_current_branch)'
alias glo='git log'
alias gr='git reset'
alias grh='git reset --hard'
alias grho='git reset --hard origin/$(git_current_branch)'
# Other aliases
alias xo='xdg-open'
alias dist-upgrade='sudo apt-get update && sudo apt-get dist-upgrade'
alias lock='gnome-screensaver-command -l'
alias change-terminal-theme='wget -O gogh https://git.io/vQgMr && chmod +x gogh && ./gogh && rm gogh'

function ghc-flamegraph-render() {
  cat "$1" | ghc-prof-flamegraph | flamegraph.pl > $1.svg
}
function gi() {
  curl -L -s https://www.gitignore.io/api/$@ ;
}
