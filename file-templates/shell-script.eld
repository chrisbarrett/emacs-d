(nil
 "#!/usr/bin/env " (pcase (file-name-extension (buffer-file-name))
                     ("zsh" "zsh")
                     (_ "bash"))
 "
set -eu -o pipefail

")
