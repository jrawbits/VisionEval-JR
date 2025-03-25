require("tcltk")

select_two_lists <- function(list1_items, list2_data, title) {
  tt <- tktoplevel()
  tkwm.title(tt, title)

  list1_var <- tclVar(list1_items[1])
  list2_var <- tclVar("")

  lb1 <- tklistbox(tt, selectmode = "single", height = length(list1_items))
  for (item in list1_items) {
    tkinsert(lb1, "end", item)
  }
  tkgrid(lb1, row = 0, column = 0, sticky = "nsew")

  lb2 <- tklistbox(tt, selectmode = "single", height = 5) # fixed height for clarity
  tkgrid(lb2, row = 0, column = 1, sticky = "nsew")

  update_list2 <- function() {
    selection <- as.integer(tcl(lb1, "curselection")) + 1
    if (length(selection) > 0) {
      selected_item1 <- list1_items[selection]
      list2_items <- list2_data[[selected_item1]]
      tkdelete(lb2, 0, "end")
      if (!is.null(list2_items)) {
        for (item in list2_items) {
          tkinsert(lb2, "end", item)
        }
      }
    } else {
      tkdelete(lb2, 0, "end") # clear list 2 if no selection in list 1
    }
  }

  tkbind(lb1, "<<ListboxSelect>>", update_list2)

  selected_item1 <- tclVar("")
  selected_item2 <- tclVar("")

  onOK <- function() {
    selection1 <- as.integer(tcl(lb1, "curselection")) + 1
    selection2 <- as.integer(tcl(lb2, "curselection")) + 1
    if (length(selection1) > 0) {
      tclvalue(selected_item1) <- list1_items[selection1]
    }
    if (length(selection2) > 0) {
      tclvalue(selected_item2) <- list2_data[[tclvalue(selected_item1)]][selection2]
    }
    tkdestroy(tt)
  }

  onCancel <- function() {
    tkdestroy(tt)
  }

  ok_button <- tkbutton(tt, text = "OK", command = onOK)
  cancel_button <- tkbutton(tt, text = "Cancel", command = onCancel)

  tkgrid(ok_button, row = 1, column = 0, sticky = "w", padx = 5, pady = 5)
  tkgrid(cancel_button, row = 1, column = 1, sticky = "e", padx = 5, pady = 5)

  tkgrid.columnconfigure(tt, 0, weight = 1)
  tkgrid.columnconfigure(tt, 1, weight = 1)
  tkgrid.rowconfigure(tt, 0, weight = 1)

  tkwait.window(tt)

  return(list(list1 = tclvalue(selected_item1), list2 = tclvalue(selected_item2)))
}

list1_items <- c("Fruits", "Vegetables", "Grains")
list2_data <- list(
  Fruits = c("Apple", "Banana", "Cherry"),
  Vegetables = c("Carrot", "Broccoli", "Spinach"),
  Grains = c("Rice", "Wheat", "Oats")
)

selected_items <- select_two_lists(list1_items, list2_data, "Select Items")

if (selected_items$list1 != "") {
  cat("Selected List 1:", selected_items$list1, "\n")
} else {
  cat("No item selected from List 1.\n")
}
if (selected_items$list2 != "") {
  cat("Selected List 2:", selected_items$list2, "\n")
} else {
  cat("No item selected from List 2.\n")
}
