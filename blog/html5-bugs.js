jQuery(function($) {
  var c = $("#canvas")[0].getContext('2d');
  c.lineWidth = 1;
  c.fillStyle = "black";

  var width = $("#canvas").width();
  var height = $("#canvas").height();
  
  var dotCount = 10;
  var dots = new Array();

  function roll(n) {
    return Math.floor(Math.random() * n);
  }
  
  function randomHex() {
    return roll(256).toString(16);
  }
  
  function randomColor() {
    return "#" + randomHex() + randomHex() + randomHex();
  }
  
  function resetBugs() {
    for (var i = 0; i < dotCount; i++) {
      dots[i] = new Array(
        Math.floor(roll(width)),
        Math.floor(roll(height)),
        randomColor()
      );
    }
    c.clearRect(0, 0, width, height);
  }
  $("#resetBugs").click(resetBugs);
  resetBugs();
  
  function update() {
    for (var i = 0; i < dotCount; i++) {
      c.fillStyle = dots[i][2];
      c.fillRect(dots[i][0], dots[i][1], 1, 1);
      dots[i][0] = (dots[i][0] + Math.floor(Math.random() * 3) - 1 + width) % width;
      dots[i][1] = (dots[i][1] + Math.floor(Math.random() * 3) - 1 + height) % height;
    }
    setTimeout(update, 10);
  }
  update();
  
  function doSave() { 
    window.open(jQuery("#canvas")[0].toDataURL());
  }
  $("#saveBugs").click(doSave);
});
