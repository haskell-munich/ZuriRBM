

var buttons =
    [ { "id"  : "Click Me",
        "func": function (d) { alert("Hey, you clicked!"); } },

      { "id"  : "Button 2",
        "func": function (d) { alert("Hey, you clicked button 2!"); } } ]



function mkButtons (svg) {

    var spacing = 20;
    var width=120;
    var height=30;

    svg.selectAll("rect.button")
	.data(buttons)
	.enter()
	.append("rect")
	.attr("class", "button")
	.attr("id", function (d) { return d.id; })
	.attr("x", function (d, i) { return 20 + i*(spacing + width); })
	.attr("y", 20)
	.attr("width", width)
	.attr("height", height)
	.attr("rx", "10")
	.on("click", function (d) { d.func(); })

    svg.selectAll("text.button")
	.data(buttons)
	.enter()
	.append("text")
	.attr("class", "button")
	.attr("x", function (d, i) { return 40 + i*(spacing+width); })
	.attr("y", 43)
	.text(function (d) { return d.id; })
	.on("click", function (d) { d.func(); })

}

function app() {

    var width = 800;
    var height = 300


    var svg = d3.select('.content')
	.append('svg')
	.attr('width', width)
	.attr('height', height)
	.attr('class', 'thesvg');

    svg.append("rect")
	.attr("class", "outer-rect")
	.attr("x", 0)
	.attr("y", 0)
	.attr("width", width)
	.attr("height", height);

    mkButtons(svg);
}

app();
