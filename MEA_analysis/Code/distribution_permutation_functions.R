# distribution_permutation_functions.R
# Diana Hall
# May 13, 2015
# purpose: to add user imput functionality to distribution_Permutations.R

suppressMessages(library(tcltk))

# clean the user input 
clean.user.input<-function(trt, color.choices=c("red", "blue", "green", "black", "grey") ){
  
  #user input
  if (!exists('np') | is.null(np) | is.na(np) | np==""){
    np<<-1000
    tkmessageBox(message=paste(
      "np parameter unassigned ",
      paste("assigning np= 10000"), 
      sep="\n" ) , icon = "error", type="ok"  )
  } else if (np<1 | np>10^8 ){
    np<<-1000
  }
  #type catch
  if( exists('type')  ){ type<-trim.trailing ( type ) }
  if (!exists('type') | is.na(type) | type=="" ){
    tkmessageBox(message=paste(
      paste("No trt type 1"),
      paste("assigning trt type 1="),
      trt[1], 
      sep="\n" ) , icon = "error", type="ok"    )
    
    type<<-trt[1]
  } else if ( !any(is.element( type, trt ) )  ){
    tkmessageBox(message=paste(
      "trt type 1 chosen: ",
            type,
      paste("is not available in distribution file chosen "),
      paste("assigning trt type 1="), 
      trt[1], 
      sep="\n" ), icon = "error", type="ok"   )
    
    type<<-trt[1] 
  }
  
  #typecol catch
  if( exists('typecol')  ){ typecol<-trim.trailing ( tolower(typecol) ) }
  if (!exists('typecol') | is.na(typecol)){
    
    tkmessageBox(message=paste(
      paste("No color chosen for trt type 1"),
      paste("assigning trt 1 color="), 
      "blue", 
      sep="\n" ) , icon = "error", type="ok"    )

    typecol<<-"blue"
  } else if ( !any(is.element( typecol, color.choices ) )  ){
    tkmessageBox(message=paste(
      paste("trt type 1 color chosen"),
      typecol,
      "is not available",
      paste("assigning trt 1 color= blue"), 
      sep="\n" ) , icon = "error", type="ok"    )
    
      typecol<<-"blue" 
  }
  
  #kotype catch
  if( exists('kotype')  ){ kotype<-trim.trailing ( kotype ) }
  if (!exists('kotype') | is.na( kotype) | kotype=="" ){
    tkmessageBox(message=paste(
      paste("No trt type 2 chosen"),
      paste("assigning trt type 2="),
      trt[2], 
      sep="\n" ) , icon = "error", type="ok"    )
    
    kotype<<-trt[2]
  } else if ( !any(is.element( type, trt ) )  ){
    tkmessageBox(message=paste(
      "trt type 2 chosen: ",
      kotype,
      paste("is not  available in distribution file chosen "),
      paste("assigning trt type 2="), 
      trt[2], 
      sep="\n" ), icon = "error", type="ok"   )
    
    kotype<<-trt[2] 
  }
  # check that choices are different
  if (type==kotype){
    kotype<<-setdiff(trt, type)[1]
  }
  
  #kotypecol catch
  if( exists('kotypecol')  ){ kotypecol<-trim.trailing( tolower(kotypecol) ) }
  if (!exists('kotypecol') | is.na(kotypecol)){
    tkmessageBox(message=paste(
      paste("No color chosen for trt type 2"),
      paste("assigning trt 2 color= red"), 
      sep="\n" ) , icon = "error", type="ok"    )
    kotypecol<<-"red"
  } else if ( !any(is.element( kotypecol, color.choices ) )  ){
    tkmessageBox(message=paste(
      paste("trt type 2 color",kotypecol,"is not available"),
      paste("assigning trt 2 color= red"), 
      sep="\n" ) , icon = "error", type="ok"    )
      kotypecol<<-"red" 
  }  
  #ensure colors not same
  if (kotypecol==typecol){
    typecol<<-"blue"
    kotypecol<<-"red"
  }

}

# Trim trailing spaces
trim.trailing <- function (x) sub("\\s+$", "", x)

get.trt<-function(distFiles ){ 
  trt.t<-c()
  for ( cur.file in 1:length(distFiles)){
    trt.t<-c(trt.t,
             read.table(distFiles[cur.file], sep=",", header=F, stringsAsFactors=F )[,1] )
  }
  trt<-unique(trt.t)
  
  #error catch
  if (length(trt)<2){
    # make a pop up that informs user that there's insufficient treatments in the distribution file
    # loop and get data again
    tkmessageBox(message=paste(
      paste("Distribution file contains treatment indicators:                          ", ""),
      paste(trt, collapse=" "),
      paste(" ", sep=""),
      paste(length(trt)," treatment indicators is insufficient                    ", ""),
      paste(" "),
      paste( "Please choose a different file", ""),  
      sep="\n" )   )
    
    return("Insufficent")
  }
  return(trim.trailing(trt))
}


getDistributionToolParams<-function(trt, color.choices=c("red", "blue", "green", "black", "grey") ){
  
  
  # load needed packages
  suppressMessages(library("tcltk") )
  suppressMessages(library("tkWidgets"))
  tclRequire("BWidget")
  
  done<-tclVar(0) #initialize the variable for tkwait
  
  w<-tktoplevel() #this is a window: class(w) tkwin
  tktitle(w)<-"   Permutation tool     " #add title
  
  is.tclObj(w) # this is a window which is a synonym for widget in a way
  
  # make a function to create a tcl list from an R list
  as.TclList <- function(object,...) UseMethod("as.TclList")
  
  as.TclList.list <- function(stringList){
    result <-"{"
    for (i in (1:length(stringList)))
      result <- paste(result,"{",stringList[[i]],"} ",sep="")
    result <- paste(result,"}",sep="")
    result
  }
  
  ###-------------np
  comboBox0 <- .Tk.subwin(w) # make the first subwidget 
  
  np.list.R<-list( 100,1000, 10000, 20000, 30000, 40000, 50000 )
  np.list<-as.TclList.list( np.list.R )
  
  np.init<-tclVar(init=unlist(np.list.R[[3]]) )
  .Tcl(paste("ComboBox",.Tk.ID(comboBox0),"-editable false -values", np.list,
             "-textvariable", np.init ))
  
  
  ###-------------trt.type1
  comboBox1 <- .Tk.subwin(w) # make the first subwidget 
  
  trt.types.R<-as.list(trt)
  trt.types<-as.TclList.list( trt.types.R)
  
  trt.type1.init<-tclVar(init=trt.types.R[[1]] )
  .Tcl(paste("ComboBox",.Tk.ID(comboBox1),"-editable false -values",trt.types,
             "-textvariable", trt.type1.init ))
  
  
  
  ###-------------col.type1
  comboBox1.col <- .Tk.subwin(w)
  
  col.types.R<-color.choices
  col.types<-as.TclList.list( col.types.R )
  
  col.type1.init<-tclVar(init=col.types.R[[1]])
  .Tcl(paste("ComboBox",.Tk.ID(comboBox1.col),"-editable false -values",
             col.types , "-textvariable", col.type1.init ))
  
  
  
  ###-------------trt.type2
  comboBox2 <- .Tk.subwin(w)
  
  trt.type2.init<-tclVar(init=trt.types.R[[2]])
  .Tcl(paste("ComboBox",.Tk.ID(comboBox2),"-editable false -values",trt.types, 
             "-textvariable", trt.type2.init ))
  
  
  ###-------------col.type2
  comboBox2.col <- .Tk.subwin(w)
  
  col.type2.init<-tclVar(init=col.types.R[[2]])
  .Tcl(paste("ComboBox",.Tk.ID(comboBox2.col),
             "-editable false -values", col.types , 
             "-textvariable", col.type2.init ))
  
  
  
  OnOK <- function(){
    np<<-as.numeric( tclvalue(np.init) )
    type<<-tclvalue( trt.type1.init ) 
    typecol<<-tclvalue( col.type1.init )
    kotype<<-tclvalue( trt.type2.init )
    kotypecol<<-tclvalue( col.type2.init )
    
    np<<-as.numeric( np.list.R[[ sum(1, as.numeric( 
      tclvalue( tcl(comboBox0 , "getvalue") ) ), na.rm=T ) ]] )
    type<<-trt.types.R[[ sum(1, as.numeric( 
      tclvalue( tcl(comboBox1 , "getvalue") ) ), na.rm=T ) ]]
    typecol<<-col.types.R[[ sum(1, as.numeric( 
      tclvalue( tcl(comboBox1.col , "getvalue") ) ), na.rm=T ) ]]
    kotype<<-trt.types.R[[ sum(1, as.numeric( 
      tclvalue( tcl(comboBox2 , "getvalue") ) ),na.rm=T) ]]
    kotypecol<<-col.types.R[[ sum(1, as.numeric( 
      tclvalue( tcl(comboBox2.col , "getvalue") ) ),na.rm=T) ]]
    
    tclvalue(done)<<-1 # change the 'done' variable to destroy the box
    
    tkdestroy(w)
  }
  OK.but <-tkbutton(w, text="   Make plot   ", command=OnOK )
  
  
  
  label.box.np<-tklabel(w, text="Choose # permutations: " )
  label.box1<-tklabel(w, text="Choose treatment type 1: " )
  label.box1.col<-tklabel(w, text="Choose trt type1 col: " )
  label.box2<-tklabel(w, text="Choose treatment type 2: ")
  label.box2.col<-tklabel(w, text="Choose trt type2 col: " )
  
  tkgrid(label.box.np)
  tkgrid(comboBox0)
  tkgrid(label.box1)
  tkgrid(comboBox1)
  tkgrid(label.box1.col)
  tkgrid(comboBox1.col)
  tkgrid(label.box2)
  tkgrid(comboBox2)
  tkgrid(label.box2.col)
  tkgrid(comboBox2.col)
  
  tkgrid(OK.but)
  
  tkfocus(w)
  tkwait.variable(done)
  #doneVal<<- as.integer(tclvalue(done))  
  
}













