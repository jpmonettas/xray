function drawGraph(graphDotStr){

    var g = graphlibDot.parse(graphDotStr);

    var renderer = new dagreD3.Renderer();

    var svg = d3.select("svg");

    // function transition(selection) {
    //     return selection.transition().duration(500);
    // }



    var layout = renderer.run(g, svg.select("g"));


    // transition(d3.select("svg"))
    //     .attr("width", layout.graph().width + 40)
    //     .attr("height", layout.graph().height + 40);


    // d3.select("svg")
    //     .call(d3.behavior.zoom().on("zoom", function() {
    //         var ev = d3.event;
    //         svg.select("g")
    //             .attr("transform", "translate(" + ev.translate + ") scale(" + ev.scale + ")");
    //     }));

}


$(document).ready(function(){
    $("#btn-update").click(function(){
        $.getJSON("/graph",function(data){
            drawGraph(data.graph);
        });
    });
});
