function drawGrid(id, x, y, w, h, d, col, which) {
  var c = $("#canvas")[0].getContext('2d');

  c.lineWidth = 1;
  c.strokeStyle = col;

  c.beginPath();
  for (var i = 0; i <= d; i++) {
    if (which == "tl") {
  	  c.moveTo(x, y + i * h / d);
	  c.lineTo(x + w - i * w / d, y);
    } else if (which == "tr") {
	  c.moveTo(x + w, y + i * h / d);
	  c.lineTo(x + i * w / d, y + 0);
    } else if (which == "bl") {
	  c.moveTo(x, y + i * h / d);
	  c.lineTo(x + i * w / d, y + h);
    } else if (which == "br") {
	  c.moveTo(x + w, y + i * h / d);
	  c.lineTo(x + w - i * w / d, y + h);
    }
  }
  c.stroke();
}
