library(tidyverse)
library(fs)

# path in the terminal 'echo $PATH' runing zsh
term <- "/opt/homebrew/Cellar/gdal/3.9.0/bin:/opt/homebrew/Cellar/gdal/3.9.0/lib:/opt/homebrew/opt/gdal/3.9.0/lib:/opt/homebrew/bin/proj:/opt/homebrew/opt/jpeg/bin:/Users/juanrocha/.juliaup/bin:/opt/homebrew/opt/gdal/3.9.0/lib:/opt/homebrew/opt/proj/bin:/opt/homebrew/opt/jpeg/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/Library/Frameworks/Python.framework/Versions/3.12/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/opt/homebrew/opt/gdal/3.9.0/lib:/opt/homebrew/opt/proj/bin:/opt/homebrew/opt/jpeg/bin"

rstudio_console <- "/opt/homebrew/Cellar/gdal/3.9.0/bin:/opt/homebrew/Cellar/gdal/3.9.0/lib:/opt/homebrew/opt/gdal/3.9.0/lib:/opt/homebrew/bin/proj:/opt/homebrew/opt/jpeg/bin:/Users/juanrocha/.juliaup/bin:/opt/homebrew/opt/gdal/3.9.0/lib:/opt/homebrew/opt/proj/bin:/opt/homebrew/opt/jpeg/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/Library/Frameworks/Python.framework/Versions/3.12/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/opt/homebrew/opt/gdal/3.9.0/lib:/opt/homebrew/opt/proj/bin:/opt/homebrew/opt/jpeg/bin:/opt/homebrew/Cellar/gdal/3.9.0/bin:/opt/homebrew/Cellar/gdal/3.9.0/lib:/opt/homebrew/bin/proj:/Users/juanrocha/.juliaup/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/Library/Frameworks/Python.framework/Versions/3.12/bin:/opt/homebrew/Cellar/gdal-3.8.5/lib/bin:/var/folders/jm/d3760lfd491gkrb4_gll_n_w0000gn/T/RtmpyZAL9t/rstudio/terminal:/var/folders/jm/d3760lfd491gkrb4_gll_n_w0000gn/T/RtmpUPvm3b/rstudio/terminal:/var/folders/jm/d3760lfd491gkrb4_gll_n_w0000gn/T/Rtmpp5lRkj/rstudio/terminal:/Users/juanrocha/Applications/quarto/bin:/Library/TeX/texbin:/usr/texbin:/Applications/RStudio.app/Contents/Resources/app/quarto/bin:/Applications/RStudio.app/Contents/Resources/app/bin/postback"

naked_R <- "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"

term <- str_split(term, ":") |> unlist()
rstudio_console <- str_split(rstudio_console, ":") |> unlist()
naked_R <- str_split(naked_R, ":") |> unlist()

term %in% rstudio_console |> all() # TRUE

## add teh gdal paths to the terminal, manually solved in .zshrc by adding
# options added manually for gdal
# export PATH="/opt/homebrew/opt/jpeg/bin:$PATH"
# export PATH="/opt/homebrew/bin/proj:$PATH"
# export PATH="/opt/homebrew/opt/gdal/3.9.0/lib:$PATH"
# export PATH="/opt/homebrew/Cellar/gdal/3.9.0/lib:$PATH"
# export PATH="/opt/homebrew/Cellar/gdal/3.9.0/bin:$PATH"
rstudio_console[!rstudio_console %in% term ]

term

## ideal path without repetitions:
term <- "/opt/homebrew/lib:/opt/homebrew/opt/proj/lib:/opt/homebrew/opt/jpeg/lib:/Users/juanrocha/.juliaup/bin:/opt/homebrew/sbin:/Library/Frameworks/Python.framework/Versions/3.12/bin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin"

## I pasted manually on zshrc
## I used on termina on the directorly opt/homebrew/lib the following:
## ln -s ../Cellar/gdal/3.9.0/lib/libgdal.35.3.9.0.dylib libgdal.dylib
## it fixes the symbolic link between the lib directory and the original file
