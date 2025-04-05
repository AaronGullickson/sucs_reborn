function addPlotlyLabels(el, x) {
   console.log('Binding plotly_relayout and plotly_restyle events...');
   var plot = document.getElementById(el.id);

   let throttleTimeout = null;

   function updateAnnotations(eventData) {
     console.log('Updating annotations...', eventData);

     var xaxisMin = eventData['xaxis.range[0]'];
     var xaxisMax = eventData['xaxis.range[1]'];
     var yaxisMin = eventData['yaxis.range[0]'];
     var yaxisMax = eventData['yaxis.range[1]'];

     if (throttleTimeout) return;
     throttleTimeout = requestAnimationFrame(function() {
       if (xaxisMin !== undefined && xaxisMax !== undefined && yaxisMin !== undefined && yaxisMax !== undefined) {
         var zoomLevelX = Math.abs(xaxisMax - xaxisMin);
         var zoomLevelY = Math.abs(yaxisMax - yaxisMin);
         var zoomLevel = Math.max(zoomLevelX, zoomLevelY);

         var zoomThreshold = 300;
         var annotations = [];

         // Only add annotations if zoom level is below threshold
         if (zoomLevel < zoomThreshold) {
           console.log('Zoomed in: Showing labels');

           for (var i = 0; i < x.data.length; i++) {
             var trace = x.data[i];

             // We no longer check visibility here; just add all relevant annotations
             for (var j = 0; j < trace.x.length; j++) {
               if (trace.x[j] >= xaxisMin && trace.x[j] <= xaxisMax && trace.y[j] >= yaxisMin && trace.y[j] <= yaxisMax) {
                 
                 // Constrain label positions to ensure they don't overflow
                 let labelX = trace.x[j];
                 let labelY = trace.y[j];

                 // Check if the label is too far to the right, adjust position if necessary
                 let labelXAnchor = 'left';
                 if (labelX > xaxisMax - 10) {  // Label too far right
                   labelXAnchor = 'right';  // Change anchor to the right
                 }

                 // Check if the label is too far at the bottom, adjust position if necessary
                 let labelYAnchor = 'bottom';
                 if (labelY > yaxisMax - 10) {  // Label too far up
                   labelYAnchor = 'top';  // Change anchor to the top
                 }
                 
                 annotations.push({
                   x: trace.x[j],
                   y: trace.y[j],
                   text: trace.customdata[j],  // Using customdata for label text
                   showarrow: false,
                   xanchor: labelXAnchor,
                   yanchor: labelYAnchor,
                   font: { 
                     color: '#f2f2f2',
                     size: 8
                   }
                 });
               }
             }
           }
         } else {
           console.log('Zoomed out: Hiding labels');
         }

         // After zoom/pan adjustments, update annotations
         Plotly.relayout(el, { annotations: annotations });
       }
       throttleTimeout = null;
     });
   }

   // Listen for zoom/pan events and update annotations
   el.on('plotly_relayout', updateAnnotations);

   // This ensures the labels persist even after plot updates (like legend visibility change)
   el.on('plotly_restyle', function(eventData) {
     console.log('Plotly restyle event triggered: Updating labels');
     // Force reapplication of annotations after restyle
     updateAnnotations(eventData);
   });

   // Initial annotations setup
   updateAnnotations({
     'xaxis.range[0]': plot.layout.xaxis.range[0],
     'xaxis.range[1]': plot.layout.xaxis.range[1],
     'yaxis.range[0]': plot.layout.yaxis.range[0],
     'yaxis.range[1]': plot.layout.yaxis.range[1]
   });
}
