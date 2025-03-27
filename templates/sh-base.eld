sh-base-mode

(dir "# shellcheck disable=SC2039" n>
     "DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"")
