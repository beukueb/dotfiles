# Windows specific aliases
alias docker="/cygdrive/c/Program\ Files/Docker/Docker/Resources/bin/docker.exe"

# Windows tweaks
export LC_ALL=
export LC_CTYPE=en_US.UTF-8

function ipadress () {
  /cygdrive/c/Windows/system32/ipconfig |
  grep -i 'IPv4 Address' |
  cut -d':' -f2 | tr -d "[:blank:]" |
  head -n1
}
