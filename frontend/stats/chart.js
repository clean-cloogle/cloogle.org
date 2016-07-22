// Adapted from http://weblogs.asp.net/dwahlin/creating-a-line-chart-using-the-html-5-canvas
var Chart = function () {
	var ctx;
	var margin = { top: 40, left: 75, right: 0, bottom: 75 };
	var chartHeight, chartWidth, yMax, xMax, data;
	var maxYValue = 0;
	var ratio = 0;

	var render = function(canvasId, dataObj) {
		data = dataObj;
		getMaxDataYValue();
		var canvas = document.getElementById(canvasId);
		chartHeight = canvas.getAttribute('height');
		chartWidth = canvas.getAttribute('width');
		xMax = chartWidth - (margin.left + margin.right);
		yMax = chartHeight - (margin.top + margin.bottom);
		ratio = yMax / maxYValue;
		ctx = canvas.getContext("2d");
		renderChart();
	};

	var renderChart = function () {
		renderText();
		renderLinesAndLabels();
		renderData();
	};

	var getMaxDataYValue = function () {
		maxYValue = 0;
		if (typeof data.minMaxY != 'undefined')
			maxYValue = data.minMaxY;
		for (var i = 0; i < data.dataPoints.length; i++) {
			var y = data.dataPoints[i].y;
			if (y > maxYValue)
				maxYValue = y + (5 - y % 5);
		}
	};

	var renderText = function() {
		var labelFont = (data.labelFont != null) ? data.labelFont : '12pt Arial';
		ctx.font = labelFont;
		ctx.textAlign = 'center';

		//Title
		if (typeof data.title != 'undefined') {
			var txtSize = ctx.measureText(data.title);
			ctx.fillText(data.title, (chartWidth / 2), (margin.top / 2));
		}

		//X-axis text
		if (typeof data.xLabel != 'undefined') {
			txtSize = ctx.measureText(data.xLabel);
			ctx.fillText(data.xLabel, margin.left + (xMax / 2) - (txtSize.width / 2), yMax + (margin.bottom / 1.2));
		}

		//Y-axis text
		if (typeof data.yLabel != 'undefined') {
			ctx.save();
			ctx.rotate(-Math.PI / 2);
			ctx.font = labelFont;
			ctx.fillText(data.yLabel, 0 - (margin.top + yMax / 2), margin.left / 4);
			ctx.restore();
		}
	};

	var renderLinesAndLabels = function () {
		//Vertical guide lines
		var yInc = yMax / data.dataPoints.length;
		var xInc = getXInc();
		var xPos = margin.left;
		for (var i = 0; i <= data.yLines; i++) {
			var y = margin.top + (yMax * i / data.yLines);
		
			// Draw horizontal lines
			drawLine(margin.left, y, xMax, y, '#E8E8E8');
			
			//y axis labels
			ctx.font = (data.dataPointFont != null) ? data.dataPointFont : '10pt Calibri';
			var txt = Math.round(maxYValue - (y - margin.top) / ratio);
			var txtSize = ctx.measureText(txt);
			ctx.fillText(txt, margin.left - ((txtSize.width >= 14) ? txtSize.width : 10) - 7, y + 4);
		}

		var yPos = 0;
		for (var i = 0; i < data.dataPoints.length; i++) {
			yPos += (i == 0) ? margin.top : yInc;

			//x axis labels
			txt = data.dataPoints[i].x;
			txtSize = ctx.measureText(txt);
			ctx.fillText(txt, xPos, yMax + margin.top + (margin.bottom / 3));
			xPos += xInc;
		}

		//Vertical line
		drawLine(margin.left, margin.top, margin.left, yMax + margin.top, 'black');

		//Horizontal Line
		drawLine(margin.left, yMax + margin.top, xMax, yMax + margin.top, 'black');
	};

	var renderData = function() {
		var xInc = getXInc();
		var prevX = 0, 
			prevY = 0;

		for (var i = 0; i < data.dataPoints.length; i++) {
			var pt = data.dataPoints[i];
			var ptY = (maxYValue - pt.y) * ratio;
			ptY += margin.top;
			var ptX = (i * xInc) + margin.left;

			if (i > 0) {
				//Draw connecting lines
				drawLine(ptX, ptY, prevX, prevY, 'black', 2);
			}

			prevX = ptX;
			prevY = ptY;
		}
	};

	var getXInc = function() {
		return Math.round(xMax / data.dataPoints.length) - 1;
	};

	var drawLine = function(startX, startY, endX, endY, strokeStyle, lineWidth) {
		if (strokeStyle != null) ctx.strokeStyle = strokeStyle;
		if (lineWidth != null) ctx.lineWidth = lineWidth;
		ctx.beginPath();
		ctx.moveTo(startX, startY);
		ctx.lineTo(endX, endY);
		ctx.stroke();
		ctx.closePath();
	};

	return {
		render: render
	};
} ();
