# Load the tcltk package
library(tcltk)

# TODO: get the various approaches to be more generic (a la the get_input / name function, where the
# destination variable is a parameter)

# Then start figuring out what needs to be in each of the dialogs...q

setup.dialog <- function(all.releases,max_width=800) {
  # Dialog values to update
  runtime <- tclVar("Runtime")                                  # Checkbox selector from "Runtime" or "Build"
  repository <- tclVar(names(all.releases)[1])                  # List box selector from names(all.releases)
  release <- tclVar(names(all.releases[[1]])[1])                # List box selector from names(all.releases[[repository]])
  installer <- tclVar(names(all.releases[[1]][[1]][["assets"]])[1])  # List box selector from names(all.releases[[repository]][[release]]$assets)
  doit <- tclVar("No")                                          # Change this if user chooses "Install"

  getAssetType <- function() {
    if ( tclvalue(runtime)=="Runtime" ) "assets" else "zipball"
  }

  # Installation Type
  select_runtime <- function() {
    pick <- tkmessageBox(title = "Install Type", message = "Build Installation?", icon = "question", type = "yesno")
    if (as.character(pick) == "yes") {
      pick <- "Build"
    } else {
      pick <- "Runtime"
    }
    tclvalue(runtime) <- pick
    tclvalue(installer) <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]])[1]
    print(names(all.releases[[tclvalue(repository)]][[tclvalue(release)]]))
    print(all.releases[[tclvalue(repository)]][[tclvalue(release)]])
    message("Asset Type: ",getAssetType())
    message("New installer: ",tclvalue(installer))
  }

  # Our own listbox function to pick from a list
  select_from_list <- function(parent_window, dest_var, items, title="Make a Selection") {
    tt <- tktoplevel(parent = parent_window)
    tkwm.title(tt, title)

    original_var <- tclVar(tclvalue(dest_var))

    Instructions <- tklabel(tt,text=title,justify="center")
    tkgrid(Instructions, row=0, column=0, sticky="ew",pady=5)

    lb.frame <- tkframe(tt,borderwidth=2,relief="solid")
    lb1 <- tklistbox(lb.frame, selectmode = "single", height=0, width=0)

    tkgrid(lb1,row=0,column=0,padx=5,pady=5,sticky="ew")
    tkgrid.columnconfigure(lb.frame,0,weight=1)
    message(length(items)," items to insert.")
    for (item in items) {
      message("inserting ",item)
      tkinsert(lb1, "end", item)
    }
    tkselection.set(lb1,0)
    tkgrid(lb.frame, row = 1, column = 0, sticky = "ew", padx=5, pady=5)

    onOK <- function() {
      selection <- as.integer(tcl(lb1, "curselection")) + 1
      if (length(selection) > 0) {
        tclvalue(dest_var) <- items[selection]
      }
      tkdestroy(tt)
    }

    onCancel <- function() { # leave dest_var unchanged
      tclvalue(dest_var) <- tclvalue(original_var)
      tkdestroy(tt)
    }

    button.frame <- tkframe(tt)
    ok_button <- tkbutton(button.frame, text = "OK", command = onOK)
    cancel_button <- tkbutton(button.frame, text = "Cancel", command = onCancel)

    tkgrid(ok_button, row = 0, column = 0, padx = 5, pady = 5,sticky="e")
    tkgrid(cancel_button, row = 0, column = 1, padx = 5, pady = 5,sticky="w")
    tkgrid(button.frame,row=2,column=0)

    tkgrid.columnconfigure(tt, 0, weight = 1)
    tkgrid.rowconfigure(tt, 0, weight = 1)
  }

  # Example 6: A simple GUI with buttons to trigger the dialogs
  create_gui <- function(max_width=400) {
    tt <- tktoplevel()
    tkwm.title(tt, "Tcl/Tk Sub-Dialog Examples")
    tkwm.maxsize(tt, max_width, 10000) # we don't expect to expand vertically

    # Actions to gather information
    runtime_button <- tkbutton(tt, text = "Installation Type", command = select_runtime)
    repos_button <- tkbutton(tt, text = "Repository", command = function() {
      repos.list <- names(all.releases)
      if ( length(repos.list) < 2 ) return()
      select_from_list(tt,repository,repos.list) # will update repository variable
      release_list <- all.releases[[tclvalue(repository)]]
      tclvalue(release) <- names(release_list)[1] # reset to first release
      inst_list <- release_list[[tclvalue(release)]][[getAssetType()]]
      tclvalue(installer) <- names(inst_list)[1]
    })

    release_button <- tkbutton(tt, text = "Release", state="normal", command = function() {
      release_list <- names(all.releases[[tclvalue(repository)]])
      if ( length(release_list) < 2 ) return()
      select_from_list(tt,release,release_list) # will update repository variable
      inst_list <- all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType]]
      tclvalue(installer) <- names(inst_list)[1]
    })

    installer_button <- tkbutton(tt, text = "Installer", state="normal", command = function() {
      installer_list <- names(all.releases[[tclvalue(repository)]][[tclvalue(release)]][[getAssetType()]])
      if ( length(installer_list) < 2 ) return()
      select_from_list(tt,installer,installer_list) # will update repository variable
    })

    # Display the buttons
    tkgrid(runtime_button, column = 0, row = 0, sticky = "e", padx = 5, pady = 5)
    tkgrid(repos_button, column = 0, row = 1, sticky = "e", padx = 5, pady = 5)
    tkgrid(release_button, column = 0, row = 2, sticky = "e", padx = 5, pady = 5)
    tkgrid(installer_button, column = 0, row = 3, sticky = "e", padx = 5, pady = 5)

    # Display the values set by the buttons in label widgets  
    runtime_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    runtime_label <- tklabel(runtime_frame,textvariable=runtime, justify="left")
    tkpack(runtime_label,anchor="w",padx=5,pady=5)
    tkgrid(runtime_frame, column = 1, row = 0, sticky="ew", padx = 5, pady = 5)

    repos_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    repos_label <- tklabel(repos_frame,textvariable=repository, justify="left")
    tkpack(repos_label,anchor="w",padx=5,pady=5)
    tkgrid(repos_frame, column = 1, row = 1, sticky="ew", padx = 5, pady = 5)

    release_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    release_label <- tklabel(release_frame,textvariable=release, justify="left")
    tkpack(release_label,anchor="w",padx=5,pady=5)
    tkgrid(release_frame, column = 1, row = 2, sticky="ew", padx = 5, pady = 5)

    installer_frame <- tkframe(tt, borderwidth = 2, relief = "groove")
    installer_label <- tklabel(installer_frame,textvariable=installer, justify="left")
    tkpack(installer_label,anchor="w",padx=5,pady=5)
    tkgrid(installer_frame, column = 1, row = 3, sticky="ew", padx = 5, pady = 5)

    # OK and Cancel buttons
    onOK <- function() {
      tclvalue(doit) <- "Install"
      tkdestroy(tt)
    }

    onCancel <- function() {
      tclvalue(doit) <- "Cancel"
      tkdestroy(tt)
    }

    ok_button <- tkbutton(tt, text = "Install", command = onOK)
    cancel_button <- tkbutton(tt, text = "Cancel", command = onCancel)

    tkgrid(ok_button, column = 0, row = 5, pady = 10)
    tkgrid(cancel_button, column = 1, sticky="w", row = 5, pady = 10)
    tkgrid.columnconfigure(tt, 1, weight = 1) #Make the second column expandable.

    tkwait.window(tt)
    return(
      list(
        Runtime=tclvalue(runtime),
        Repos=tclvalue(repository),
        Release=tclvalue(release),
        Installer=tclvalue(installer),
        DoIt=tclvalue(doit)
      )
    )
  }

  # Run the GUI
  return( create_gui(max_width=max_width) )
}
