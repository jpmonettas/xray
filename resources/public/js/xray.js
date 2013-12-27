function drawGraph(graphDotStr){

    var g = graphlibDot.parse(graphDotStr);

    var renderer = new dagreD3.Renderer();
    var layout = renderer.run(g, d3.select("svg g"));

    // d3.select("svg")
    //     .attr("width", layout.graph().width + 40)
    //     .attr("height", layout[.graph().height + 40);
}


$(document).ready(function(){
        $("#btn-update").click(function(){
        $.getJSON("/graph",function(data){
            drawGraph(data.graph);
        });
    });
});
