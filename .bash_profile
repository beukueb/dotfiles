if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
if [ -f ~/.bash_env ]; then . ~/.bash_env; fi

# to prevent escaping $ to \$ tab expanding dir with env variables
shopt -s direxpand

# MacPorts Installer addition on 2015-12-02_at_09:58:56: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.
