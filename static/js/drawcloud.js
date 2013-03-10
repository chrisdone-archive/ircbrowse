var drawCloud = (function(){
  // taken from the d3 example
  var fill = d3.scale.category20();
  function drawGraph(spec) {
    d3.layout.cloud().size([spec.width, spec.height])
      .words(spec.words)
      .rotate(function() { return ~~(Math.random() * 2) * 90; })
      .font("Helvetica")
      .fontSize(function(d) { return d.size; })
      .on("end", function(words){ draw(spec,words); })
      .start();
  }
  function draw(spec,words) {
    d3.select(spec.parent).append("svg")
      .attr("width", spec.width)
      .attr("height", spec.height)
      .append("g")
      .attr("transform", "translate(" + (spec.width/2) +"," + (spec.height/2) + ")")
      .selectAll("text")
      .data(words)
      .enter().append("text")
      .style("font-size", function(d) { return d.size + "px"; })
      .style("font-family", "Helvetica")
      .style("fill", function(d, i) { return fill(i); })
      .attr("text-anchor", "middle")
      .attr("transform", function(d) {
        return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
      })
      .text(function(d) { return d.text; });
  }
  return drawGraph;
})();
