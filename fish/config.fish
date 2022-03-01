set fish_greeting
set -gx EDITOR emacs
alias clearAgents="gpg-connect-agent reloadagent /bye && ssh-add -D"
alias syncTime="sudo ntpdate -u 0.europe.pool.ntp.org"
alias reloadConfig="source ~/.config/fish/config.fish"
bind \u00E6 forward-word
bind \u00E2 backward-word
bind \u00E8 backward-kill-word
bind \u00E4 kill-word
bind \a cancel
bind \eh backward-kill-word
