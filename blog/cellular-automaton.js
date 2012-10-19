jQuery(function($) {
  function div(a, b) {
    return Math.floor(a / b);
  }

  function drawRule(rule, initial, scale) {
    var c = $("#canvas")[0].getContext('2d');
    c.lineWidth = 1;
    c.fillStyle = "black";

    var width = $("#canvas").width();
    var height = $("#canvas").height();
    
    var scaleWidth = div(width, scale);
    var scaleHeight = div(height, scale);

    c.clearRect(0, 0, width, height);
  
    var setTo = [0, 0, 0, 0, 0, 0, 0, 0];
    for (var i = 0; i < 8; i++)
      setTo[i] = (rule >> i) & 1;

    var oldRow = new Array(scaleWidth);
    var newRow = new Array(scaleWidth);
    
    for (var i = 0; i < scaleWidth; i++) {
      if (initial == "random")
        oldRow[i] = (Math.random() > 0.5 ? 1 : 0);
      else if (initial == "5050")
        oldRow[i] = (i < scaleWidth / 2 ? 0 : 1);
      else if (initial == "point")
        oldRow[i] = (i == div(scaleWidth, 2) ? 1 : 0);
      else if (initial == "black")
        oldRow[i] = 1;
      else if (initial == "white")
        oldRow[i] = 0;
      else
        oldRow[i] = 0;
      
      newRow[i] = 0;
    }

    var index = 0;
    for (var row = 0; row < scaleHeight; row++) {
      for (var col = 0; col < scaleWidth; col++) {
        if (oldRow[col] == 1)
          c.fillRect(col * scale, row * scale, scale, scale);

        index = oldRow[col == 0 ? 0 : col - 1] * 4 + oldRow[col] * 2 + oldRow[col == scaleWidth - 1 ? scaleWidth - 1 : col + 1];
        newRow[col] = setTo[index];
      }

      for (var col = 0; col < scaleWidth; col++) {
        oldRow[col] = newRow[col];
      }
    }
  }
  
  $(".warning").css("color", "red");

  function doUpdate() {
    $(".warning").empty();
    
    var rule = parseInt($("#inputRule").val());
    if (isNaN(rule) || rule < 0 || rule > 255) {
      $("#inputRuleWarning").html('Rule must be an integer in the range [0, 255]');
      return;
    }
    
    var scale = parseInt($("#inputScale").val());
    if (isNaN(scale) || scale < 1 || scale > 10) {
      $("#inputScaleWarning").html('Rule must be an integer in the range [1, 10]');
      return;
    }
    
    var initial = $("#inputInitial").val();
    drawRule(rule, initial, scale);
  };
  
  $(".update").change(doUpdate);
  $("#drawButton").click(doUpdate);
  
  $("#inputInteresting").change(function() {
    if ($("#inputInteresting").val() != "") {
      $("#inputRule").val($("#inputInteresting").val());
      $("#inputInitial").val("point");
      $("#inputInteresting").val("");
      doUpdate();
    }
  });
  
  doUpdate();
  
  function doSave() { 
    window.open(jQuery("#canvas")[0].toDataURL());
  }
  $("#saveButton").click(doSave);
});
