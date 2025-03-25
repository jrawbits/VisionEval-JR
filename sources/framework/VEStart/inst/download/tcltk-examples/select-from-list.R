require("tcltk")

select_list <- function(items, title) {
  tt <- tktoplevel()
  tkwm.title(tt, title)

  Instructions <- tklabel(tt,text=title,justify="center")
  tkgrid(Instructions, row=0, column=0, sticky="ew",pady=5)

  lb.frame <- tkframe(tt,borderwidth=2,relief="solid")
  lb1 <- tklistbox(lb.frame, selectmode = "single", height=0, width=0)

  tkgrid(lb1,row=0,column=0,padx=5,pady=5,sticky="ew")
  tkgrid.columnconfigure(lb.frame,0,weight=1)
  for (item in items) {
    tkinsert(lb1, "end", item)
  }
  tkselection.set(lb1,0)
  tkgrid(lb.frame, row = 1, column = 0, sticky = "ew", padx=5, pady=5)

  selected_item <- tclVar(items[1])

  onOK <- function() {
    selection <- as.integer(tcl(lb1, "curselection")) + 1
    if (length(selection) > 0) {
      tclvalue(selected_item) <- items[selection]
    }
    tkdestroy(tt)
  }

  onCancel <- function() {
    tclvalue(selected_item) <- ""
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

  tkwait.window(tt)

  return(tclvalue(selected_item))
}

items <- c("Fruits", "Vegetables", "Grains")

selected <- select_list(items, "Select Items")

if (selected != "") {
  cat("Selected:", selected, "\n")
} else {
  cat("No item selected.\n")
}
