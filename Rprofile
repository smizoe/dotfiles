if(interactive()){
    options(vimcom.verbose = 1)
    # Load the libraries required by vim-r-plugin:
    library(colorout)
    library(setwidth)
    library(vimcom)

    # load frequently used packages
    library(dplyr)
    library(ggplot2)
    library(data.table)
    library(tidyr)
    library(caret)
    library(readr)
    library(reshape2)

    # Use the text based web browser w3m to navigate through R docs
    # in Linux Console after help.start():
    if(Sys.getenv("TMUX") != "" && Sys.getenv("DISPLAY") == "")
    options(browser = function(u) system(paste0("tmux new-window 'w3m ", u, "'")))
}
