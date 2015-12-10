if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# MacPorts Installer addition on 2015-12-02_at_09:58:56: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# Adding the texlive dir
export PATH="/Library/TeX/texbin:$PATH"