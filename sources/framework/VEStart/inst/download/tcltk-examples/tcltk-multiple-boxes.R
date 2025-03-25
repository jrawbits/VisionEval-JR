# Initial filter criteria (can be changed in dialog)
selected.repos <- tclVar("")
selected.release <- tclVar("")
selected.installer <- tclVar("")

require("tcltk",quietly=TRUE) # Should be there as part of base distribution

tt <- tktoplevel()
tkwm.title(tt, "Select Installer")

lb.repos <- tklistbox(tt, selectmode = "single", height = length(repos.list))
for (item in repos.list) {
  tkinsert(lb.repos, "end", item)
}
tkgrid(lb.repos, row = 0, column = 0, sticky = "nsew", padx=5, pady=5)

lb.release <- tklistbox(tt, selectmode = "single", height = 2) # fixed height for clarity
tkgrid(lb.release, row = 0, column = 1, sticky = "nsew", padx=5, pady=5)

update_release <- function() {
  selection <- as.integer(tcl(lb.repos, "curselection")) + 1
  tclvalue(selected.repos) <- selection
  message("Repos ",selected.repos)
  if (length(selection) > 0) {
    repos.name <- repos.list[selection]
    release_items <- as.character(sapply(all.releases[[repos.name]], function(rel) rel$release))
    tkdelete(lb.release, 0, "end")
    if (!is.null(release_items)) {
      for (item in release_items) {
        tkinsert(lb.release, "end", item)
      }
    }
  } else {
    tkdelete(lb.release, 0, "end") # clear releases if no repository selected
  }
}
tkbind(lb.repos, "<<ListboxSelect>>", update_release)

lb.installer <- tklistbox(tt, selectmode = "single", height = 2) # fixed height for clarity
tkgrid(lb.installer, row = 0, column = 2, sticky = "nsew", padx=5, pady=5)

update_installer <- function() {
  message("Updating installer list")
  selection <- as.integer(tcl(lb.release, "curselection")) + 1
  tclvalue(selected.release) <- selection
  if (length(selection) > 0) {
    installer_items <- sapply(all.releases[[tclvalue(selected.repos)]][[selection]]$assets, function(asset) asset$file)
    message("repos ",tclvalue(selected.repos)," release ",selection)
    print(installer_items)
    tkdelete(lb.installer, 0, "end")
    if (!is.null(installer_items)) {
      for (item in installer_items) {
        tkinsert(lb.installer, "end", item)
      }
    }
  } else {
    message("No selection")
    tkdelete(lb.installer, 0, "end") # clear installers if no release selected
  }
}
tkbind(lb.release, "<<ListboxSelect>>", update_installer)

select_installer <- function() {
  tclvalue(selected.installer) <- as.integer(tcl(lb.release,"curselection")) + 1
}
tkbind(lb.installer, "<<ListboxSelect>>", select_installer)

onOK <- function() {
  repository <- as.integer(tclvalue(selected.repos)
    release    <- as.integer(tclvalue(selected.release)
      installer  <- as.integer(tclvalue(selected.installer)
        message("repository: ",repository)
        message("release: ",release)
        message("installer: ",installer)
        print(c(repository=repository,release=release,installer=installer))
        if (length(repository) > 0) {
          selected.repos <- repository
        }
        if (length(release) > 0) {
          selected.release <- release
        }
        if (length(installer) > 0) {
          selected.installer <- installer
        }
        tkdestroy(tt)
      }

onCancel <- function() {
  tkdestroy(tt)
}

ok_button <- tkbutton(tt, text = "OK", command = onOK)
cancel_button <- tkbutton(tt, text = "Cancel", command = onCancel)

tkgrid(ok_button, row = 1, column = 0, sticky = "e", padx = 5, pady = 5)
tkgrid(cancel_button, row = 1, column = 2, sticky = "w", padx = 5, pady = 5)

tkgrid.columnconfigure(tt, 0, weight = 1)
tkgrid.columnconfigure(tt, 1, weight = 1)
tkgrid.columnconfigure(tt, 2, weight = 1)
tkgrid.rowconfigure(tt, 0, weight = 1)

tkwait.window(tt)

installer <- list(
  repos     = selected.repos,
  release   = selected.release,
  installer = selected.installer
)
print(installer)
return(all.releases[[installer$repos]][[installer$release]]$assets[[installer$installer]])
