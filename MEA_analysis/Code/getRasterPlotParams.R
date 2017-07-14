# dialoge box for getting parameters for Raster Plot
# Diana Hall
# 12-28-2015
getRasterPlotParams <- function(s=NULL){
  
  # load needed packages
  library("tcltk") 
  library("tkWidgets")
  tclRequire("BWidget")
  
  w<-tktoplevel() #this is a window: class(w) tkwin
  tktitle(w)<-" Raster " #add title
  
  done<-tclVar(0) #initialize the variable for tkwait
  
  
  # make a function to create a tcl list from an R list
  as.TclList <- function(object,...) UseMethod("as.TclList")
  
  as.TclList.list <- function(stringList){
    result <-"{"
    for (i in (1:length(stringList)))
      result <- paste(result,"{",stringList[[i]],"} ",sep="")
    result <- paste(result,"}",sep="")
    result
  }
  
  # input check
  if ( is.null(s) ){
    stop("no data supplied")
  }
  
  #get available wells
  if ( is.element( "channels", names(s[[1]]))  ){
    wells.present<-unique(substring(s[[1]]$channels,1,2))
  } else if ( is.element( "spikes", names(s[[1]])) ){
    wells.present<-unique(substring(names(s[[1]]$spikes),1,2))
  } 
  
  if ( is.element( "rec.time", names(s[[1]]))  ){
    time.present<-s[[1]]$rec.time
  } else{ time.present<-c(0, 100) }
  
  
  TF.list.R<-as.list(c(T,F) )
  TF.list<-as.TclList.list( TF.list.R )
  
  ###-------------well dropdown box
  if(!exists("well.for.raster") ){
    well.for.raster<<-wells.present[1]
  } else if (!is.element(well.for.raster,wells.present) ){
    well.for.raster<<-wells.present[1]
  }
  comboBox.well.for.raster<- .Tk.subwin(w) # make the first subwidget 
  
  well.list.R<-as.list(wells.present)
  well.list<-as.TclList.list( well.list.R )
  
  well.for.raster.init<-tclVar(init=well.for.raster  )
  well.for.raster<<-tclvalue(well.for.raster.init)
  .Tcl(paste("ComboBox",.Tk.ID(comboBox.well.for.raster),"-editable false -values", well.list,
             "-textvariable", well.for.raster.init ))
  
  
  ###-------------show.bursts radio button
  rb.show.bursts.1 <- tkradiobutton(w)
  rb.show.bursts.2 <- tkradiobutton(w)
  show.bursts.init<- tclVar(as.character( as.logical(show.bursts) ))
  tkconfigure(rb.show.bursts.1,variable=show.bursts.init, value="TRUE")
  tkconfigure(rb.show.bursts.2,variable=show.bursts.init, value="FALSE")
  
  
  ###-------------show.burst.number
  rb.show.burst.number.1 <- tkradiobutton(w)
  rb.show.burst.number.2 <- tkradiobutton(w)
  show.burst.number.init<- tclVar(as.character( as.logical(show.burst.number) ) )
  tkconfigure(rb.show.burst.number.1,variable=show.burst.number.init, value="TRUE")
  tkconfigure(rb.show.burst.number.2,variable=show.burst.number.init, value="FALSE")       
  
  ###-------------show.networkspikes
  rb.show.networkspikes.1 <- tkradiobutton(w)
  rb.show.networkspikes.2 <- tkradiobutton(w)
  show.networkspikes.init<- tclVar(as.character( as.logical(show.networkspikes) ))
  tkconfigure(rb.show.networkspikes.1,variable=show.networkspikes.init, value="TRUE")
  tkconfigure(rb.show.networkspikes.2,variable=show.networkspikes.init, value="FALSE")
  
  ###-------------show.ns.number
  rb.show.ns.number.1 <- tkradiobutton(w)
  rb.show.ns.number.2 <- tkradiobutton(w)
  show.ns.number.init<- tclVar(as.character( as.logical(show.ns.number  )))
  tkconfigure(rb.show.ns.number.1,variable=show.ns.number.init, value="TRUE")
  tkconfigure(rb.show.ns.number.2,variable=show.ns.number.init, value="FALSE")  
  
  
  ###-------------interval.for.raster lower  
  times.list.R<-as.list(time.present)
  times.list<-as.TclList.list( times.list.R )
  
  range.low<-tclVar( init=floor( time.present[1] ) ) 
  range.high<-tclVar( init=floor( time.present[2] ) ) 
  
  if( !exists("interval.for.raster") ){
    start.i<-tclVar( init=floor( time.present[1] ) ) 
    stop.i<-tclVar( init=floor(time.present[2]) )
    interval.for.raster<<-c(as.numeric(tclvalue(start.i)),
                            as.numeric(tclvalue(stop.i)) )
  } else{
    
    start.i<-tclVar( init=floor( interval.for.raster[1] ) ) 
    stop.i<-tclVar( init=floor(interval.for.raster[2]) )
  }
  
  spinbox.interval.for.raster.l <- .Tk.subwin(w)
  
  interval.for.raster.l.init<-tclVar(init=tclvalue(start.i) )
  .Tcl(paste("spinbox", .Tk.ID(spinbox.interval.for.raster.l),
             "-from", tclvalue(range.low), "-to", tclvalue(range.high), "-width 8",
             "-textvariable", start.i ) )
  
  
  ###-------------interval.for.raster upper
  spinbox.interval.for.raster.u <- .Tk.subwin(w)  
  interval.for.raster.u.init<-tclVar(init=tclvalue(stop.i)) 
  .Tcl(paste("spinbox", .Tk.ID(spinbox.interval.for.raster.u),
             "-from", tclvalue(range.low), "-to", tclvalue(range.high), "-width 8",
             "-textvariable", stop.i ) )
  
  
  OnOK <- function(){
    well.for.raster<<-tclvalue(well.for.raster.init)
    show.bursts<<-as.logical(as.character(tclvalue(show.bursts.init)))
    show.burst.number<<-as.logical(as.character(tclvalue(show.burst.number.init)))
    show.networkspikes <<-as.logical(as.character(tclvalue(show.networkspikes.init)))
    show.ns.number <<-as.logical(as.character(tclvalue(show.ns.number.init)))
    interval.for.raster<<-c(as.numeric( tclvalue(interval.for.raster.l.init) ),
                            as.numeric( tclvalue(interval.for.raster.u.init) ) )
    
    well.for.raster<<-wells.present[ sum(1, as.numeric( 
      tclvalue( tcl(comboBox.well.for.raster , "getvalue") ) ), na.rm=T ) ]  
    
    show.bursts<<-as.logical(as.character(tclvalue(show.bursts.init)))
    
    show.burst.number<<-as.logical(as.character(tclvalue(show.burst.number.init)))
    
    show.networkspikes<<-as.logical(as.character(tclvalue(show.networkspikes.init)))
    
    show.ns.number<<-as.logical(as.character(tclvalue(show.ns.number.init)))
    
    interval.for.raster[1]<<-as.numeric( 
      tclvalue( tcl(spinbox.interval.for.raster.l , "get") ) )
    print( as.numeric( tclvalue( tcl(spinbox.interval.for.raster.l , "get") ) ) )
    
    interval.for.raster[2]<<- as.numeric( 
      tclvalue( tcl(spinbox.interval.for.raster.u , "get") ) ) 
    
    print( as.numeric( tclvalue( tcl(spinbox.interval.for.raster.u , "get") ) ) )
    
    tclvalue(done)<<-1 
    tkdestroy(w)
  }
  OK.but <-tkbutton(w, text="   Make plot   ", command=OnOK )
  
  
  exit.but <- tkbutton(w, text = "Exit", command = function(){
    tclvalue(done)<<-2 # change the 'done' to 2 for tkwait to change
    want.new.raster<<-F
    #tkdestroy(tt) 
  } )
  
  
  label.box.well<-tklabel(w, text="well : " )
  label.box.interval.for.raster.l<-tklabel(w, text="start time : " )
  label.box.interval.for.raster.u<-tklabel(w, text="end time : " )
  
  tkgrid(label.box.well)
  tkgrid(comboBox.well.for.raster)
  
  tkfocus(w)
  
  tkgrid(tklabel(w,text="Show bursts: "))
  tkgrid(tklabel(w, text="TRUE "),rb.show.bursts.1)
  tkgrid(tklabel(w, text="FALSE "),rb.show.bursts.2)
  
  tkgrid(tklabel(w,text="Show bursts #: "))
  tkgrid(tklabel(w, text="TRUE "),rb.show.burst.number.1)
  tkgrid(tklabel(w, text="FALSE "),rb.show.burst.number.2)
  
  tkgrid(tklabel(w,text="Show network spikes: "))
  tkgrid(tklabel(w, text="TRUE "),rb.show.networkspikes.1)
  tkgrid(tklabel(w, text="FALSE "),rb.show.networkspikes.2)
  
  tkgrid(tklabel(w,text="Show ns #: "))
  tkgrid(tklabel(w, text="TRUE "),rb.show.ns.number.1)
  tkgrid(tklabel(w, text="FALSE "),rb.show.ns.number.2)
  
  
  tkgrid(label.box.interval.for.raster.l)
  tkgrid(spinbox.interval.for.raster.l)
  
  tkgrid(label.box.interval.for.raster.u)
  tkgrid( spinbox.interval.for.raster.u )
  
  tkgrid(OK.but,  exit.but,  pady= 10, padx= 10)
  
  tkwait.variable(done)
  doneVal<<- as.integer(tclvalue(done))   # Get and coerce content of a Tcl variable
  tkdestroy(w)
  
  
  return( well.for.raster, interval.for.raster,
          show.bursts, show.burst.number, show.networkspikes,
          show.ns.number )
  
} #end of function
