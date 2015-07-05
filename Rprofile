if(interactive()){
    options(vimcom.verbose = 1)
    # Load the required libraries:
    library(colorout)
    library(setwidth)
    library(vimcom)

    # Use the text based web browser w3m to navigate through R docs
    # in Linux Console after help.start():
    if(Sys.getenv("TMUX") != "" && Sys.getenv("DISPLAY") == "")
    options(browser = function(u) system(paste0("tmux new-window 'w3m ", u, "'")))
}
