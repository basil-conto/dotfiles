# ~/.bash_logout: executed by bash(1) when login shell exits.

# When leaving the console clear the screen to increase privacy
[ "${SHLVL}" -eq 1 ]                \
  && [ -x /usr/bin/clear_console ]  \
  && /usr/bin/clear_console -q      \
  || true
