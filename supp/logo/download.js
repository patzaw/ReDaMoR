"use strict";

function saveToPng(svgId, scale, fileName){

    if(!scale) scale = 1;
    if(!fileName) fileName = "image.png";

    var target = d3.select("#"+svgId)
        .attr("version", 1.1)
        .attr("xmlns", "http://www.w3.org/2000/svg");

    var w = target.attr("width")*scale;
    var h = target.attr("height")*scale;

    target = target[0][0].outerHTML;

    //var toSave = target.replace(/</g, "\n<");
    //var imgsrc = 'data:image/svg+xml;base64,'+ btoa(target);

    var imgsrc = 'data:image/svg+xml;charset=utf8,'+ target;

    var cont = d3.select("#techContainer");

    var canvas = cont.append("canvas")
        .attr("width", w)
        .attr("height", h);
    var png = cont.append("div");

    var context = canvas[0][0].getContext("2d");
    var img = new Image();
    img.onload = function() {
        context.drawImage(img, 0, 0, w, h);
        var canvasdata = canvas[0][0].toDataURL();
        // var canvasdata = canvas[0][0].toDataURL("image/png");
        var a = cont.append("a")
            .attr("download", fileName)
            .attr("href", canvasdata);
        a[0][0].click();
    };
    img.src = imgsrc;

}

function saveToSvg(svgId, fileName){

    if(!fileName) fileName = "image.svg";

    var target = d3.select("#"+svgId)
        .attr("version", 1.1)
        .attr("xmlns", "http://www.w3.org/2000/svg");

    target = target[0][0].outerHTML;

    //var toSave = target.replace(/</g, "\n<");
    //var imgsrc = 'data:image/svg+xml;base64,'+ btoa(target);

    var imgsrc = 'data:image/svg+xml;charset=utf8,'+
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
        target;

    var cont = d3.select("#techContainer");
    var a = cont.append("a")
        .attr("download", fileName)
        .attr("href", imgsrc);
    a[0][0].click();


}
