// This function will add dynamic labels to the plotly plot that only
// appear at certain zoom levels

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

         if (zoomLevel < zoomThreshold) {
           console.log('Zoomed in: Showing labels');

           for (var i = 0; i < x.data.length; i++) {
             var trace = x.data[i];

             // Check if the trace is visible (trace.visible is 'legendonly' when hidden)
             if (trace.visible === true || trace.visible === undefined) {
               for (var j = 0; j < trace.x.length; j++) {
                 if (trace.x[j] >= xaxisMin && trace.x[j] <= xaxisMax && trace.y[j] >= yaxisMin && trace.y[j] <= yaxisMax) {
                   annotations.push({
                     x: trace.x[j],
                     y: trace.y[j],
                     text: trace.customdata[j],
                     showarrow: false,
                     xanchor: 'left',
                     yanchor: 'bottom',
                     font: { 
                       color: '#f2f2f2',
                       size: 8
                     }
                   });
                 }
               }
             }
           }
         } else {
           console.log('Zoomed out: Hiding labels');
         }

         Plotly.relayout(el, { annotations: annotations });
       }
       throttleTimeout = null;
     });
   }

   // Listen for zoom/pan events
   el.on('plotly_relayout', updateAnnotations);

   // Listen for legend clicks (trace visibility changes)
   el.on('plotly_restyle', function() {
     console.log('Legend clicked: Updating labels');
     Plotly.relayout(el, { annotations: [] }); // Clear annotations
   });
}
