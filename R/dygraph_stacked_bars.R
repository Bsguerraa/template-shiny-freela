library(xts)
library(dygraphs)

dy_position<-function(data_final, plot_title, bar_color, y2_names=NULL, y1_label, y2_label, y1_step=F, y2_step=F, stacked=T){
  
  if (!is.null(bar_color) && length(bar_color) != ncol(data_final)) {
    stop("Length of bar_color must match the number of columns in data_final.")
  }
  data_final<-reorder_xts(data_final, y2_names) #reorder necessary so that all y2 are at the right end of the xts. Needed for the multibar plot
  
  dyg <- dygraphs::dygraph(data_final, main=plot_title)
  dyg <- dygraphs::dyAxis(dyg, "x", rangePad=20)
  dyg <- dygraphs::dyAxis(dyg, "y", label = y1_label,
                          axisLabelWidth = 90)
  y1_names<-colnames(data_final)[!(colnames(data_final) %in%y2_names)]
  
  if (length(y1_names)==1){
    stacked<-T #in this case only stacking works
  }
  
  if (stacked){
    dyg <- dygraphs::dyOptions(dyg,stepPlot=y1_step,stackedGraph = T)
    for(i in seq_along(y1_names)) {
      dyg <- dygraphs::dySeries(dyg, y1_names[i], axis = "y", strokeWidth = 1.5, color = bar_color[i],
      stepPlot = y1_step, plotter="  function barChartPlotter(e) {
                            var ctx = e.drawingContext;
                            var points = e.points;
                            var y_bottom = e.dygraph.toDomYCoord(0);

                            ctx.fillStyle = e.color;

                            // Find the minimum separation between x-values.
                            // This determines the bar width.
                            var min_sep = Infinity;
                            for (var i = 1; i < points.length; i++) {
                            var sep = points[i].canvasx - points[i - 1].canvasx;
                            if (sep < min_sep) min_sep = sep;
                            }
                            var bar_width = Math.floor(8.0 / 3 * min_sep);

                            // Do the actual plotting.
                            for (var i = 0; i < points.length; i++) {
                            var p = points[i];
                            var center_x = p.canvasx;

                            ctx.fillRect(center_x - bar_width / 2, p.canvasy,
                            bar_width, y_bottom - p.canvasy);

                            ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
                            bar_width, y_bottom - p.canvasy);
                            }
}")
    }
  } else {
    dyg <- dygraphs::dyOptions(dyg,stepPlot=y1_step)
    for(i in seq_along(y1_names)) {
      
      #plotter in function
      dyg <- dygraphs::dySeries(dyg, y1_names[i], axis = "y", strokeWidth   = 1.5, stepPlot = y1_step, plotter =multibar_combi_plotter(length(y2_names)))
    }
  }
  
  # put stuff on y2 axis
  dyg <- dygraphs::dyAxis(dyg, "y2", label = y2_label, independentTicks = T)
  for(i in seq_along(y2_names)) {
    dyg <- dygraphs::dySeries(dyg, y2_names[i], axis = "y2", strokeWidth = 1.5, stepPlot = y2_step)
  }
  
  return(dyg)
}

#we need to take into account all values and then leave out the ones we do not like
multibar_combi_plotter<-function(num_values){
  #plotter function
  plotter_text<-"function multiColumnBarPlotter(e) {
  // We need to handle all the series simultaneously.
  if (e.seriesIndex !== 0) return;

  var g = e.dygraph;
  var ctx = e.drawingContext;
  var sets = e.allSeriesPoints;
  var y_bottom = e.dygraph.toDomYCoord(0);

  // Find the minimum separation between x-values.
  // This determines the bar width.
  var min_sep = Infinity;
   for (var j = 0; j < sets.length-%s; j++) {
    var points = sets[j];
    for (var i = 1; i < points.length; i++) {
     var sep = points[i].canvasx - points[i - 1].canvasx;
     if (sep < min_sep) min_sep = sep;
  }
  }
  var bar_width = Math.floor(8.0 / 3 * min_sep);

  var fillColors = [];
  var strokeColors = g.getColors();
  for (var i = 0; i < strokeColors.length; i++) {
  fillColors.push(strokeColors[i]);
  }

  for (var j = 0; j < sets.length-%s; j++) {
  ctx.fillStyle = fillColors[j];
  ctx.strokeStyle = strokeColors[j];
  for (var i = 0; i < sets[j].length; i++) {
    var p = sets[j][i];
    var center_x = p.canvasx;
    var x_left = center_x - (bar_width / 2) * (1 - j/(sets.length-%s-1));

   ctx.fillRect(x_left, p.canvasy,
   bar_width/sets.length, y_bottom - p.canvasy);

  ctx.strokeRect(x_left, p.canvasy,
  bar_width/sets.length, y_bottom - p.canvasy);
 }
 }
   }"
  
  custom_plotter <- sprintf(plotter_text, num_values, num_values, num_values)
  return(custom_plotter)
}


reorder_xts<-function(xts_series,line_names){
  bar_names<-colnames(xts_series)[!(colnames(xts_series)%in%line_names)]
  xts_series<-xts_series[,c(bar_names,line_names)]
  return(xts_series)
}

