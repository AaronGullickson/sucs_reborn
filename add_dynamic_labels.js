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

            // Log the trace to help debug
            console.log('Trace:', trace);

            // Check if trace is valid
            if (!trace || !trace.x || !trace.y) {
              console.log('Skipping trace due to missing x, y, or customdata:', trace);
              continue;  // Skip this trace if essential data is missing
            }

            // Check if it's a polygon
            var isPolygon = trace.type === 'scatter' && trace.fill === 'toself';
            
            // Handle scatter plots or polygons differently
            if (isPolygon) {
              console.log('Handling polygon trace:', trace);

              // Handle polygon case (example: no labels needed or different behavior)
              // You can skip the label creation for polygons or customize as needed

            } else {
              console.log('Handling scatter plot trace:', trace);

              // If customdata exists, proceed with label creation for scatter plot
              var hasCustomData = trace.customdata && trace.customdata.length === trace.x.length;

              for (var j = 0; j < trace.x.length; j++) {
                if (trace.x[j] >= xaxisMin && trace.x[j] <= xaxisMax && trace.y[j] >= yaxisMin && trace.y[j] <= yaxisMax) {
                  
                  // Constrain label positions
                  let labelX = trace.x[j];
                  let labelY = trace.y[j];

                  // Label position adjustments for overflow
                  let labelXAnchor = 'left';
                  if (labelX > xaxisMax - 10) labelXAnchor = 'right';

                  let labelYAnchor = 'bottom';
                  if (labelY > yaxisMax - 10) labelYAnchor = 'top';

                  // If customdata exists, use it for the label text
                  let labelText = hasCustomData ? trace.customdata[j] : 'No Data';

                  annotations.push({
                    x: labelX,
                    y: labelY,
                    text: labelText,
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
