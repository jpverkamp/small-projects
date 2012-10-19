jQuery(function($) {
  var userOverall = 0;
  var compOverall = 0;
  var fastMode = false;

  $(".dice").css("display", "inline-block");  
  $(".dice").css("width", "32px");
  $(".dice").css("height", "32px");
  $(".dice").css("border", "2px solid black");
  $(".dice").css("padding", "8px");
  $(".dice").css("text-align", "center");
  $(".dice").css("font", "24px/32px monospace");
  $(".dice").css("color", "white");

  $("#red").css("background-color", "red");
  $("#green").css("background-color", "green");
  $("#blue").css("background-color", "blue");

  var resultDiv = $("#results");
  resultDiv.css("height", "250px");
  resultDiv.css("overflow", "scroll");

  var redRolls = new Array(2, 4, 9);
  var greenRolls = new Array(1, 6, 8);
  var blueRolls = new Array(3, 5, 7);

  function roll(die) {
    if (die == "red")
      return redRolls[Math.floor(Math.random() * 3)];
    else if (die == "green")
      return greenRolls[Math.floor(Math.random() * 3)];
    else
      return blueRolls[Math.floor(Math.random() * 3)];
  }

  function animateDice(userDie, compDie, userScore, compScore, round, frame) {
    if (frame == 1)
      resultDiv.append("<b>Round " + round + "</b><br />");

    if ((fastMode && frame >= 2) || frame >= 20) {
      var userRoll = parseInt($("#" + userDie).text());
      var compRoll = parseInt($("#" + compDie).text());

      if (userRoll > compRoll) userScore += 1;
      else compScore += 1

      resultDiv.append(
        "You rolled <span style='color: " + userDie + ";'>" + userRoll + "</span> and " + 
        "I rolled <span style='color: " + compDie + ";'>" + compRoll + "</span>.<br />" + 
        "The current score is " + userScore + " to " + compScore + "; " + (userScore >= compScore ? "you're" : "I'm") + " winning.<br /><br />"
      );
      resultDiv.scrollTop(resultDiv[0].scrollHeight);

      if (round == 20) {
        if (userScore >= compScore) {
          resultDiv.append("<b>You win!</b><br /><br />");
          userOverall += 1;
        } else { 
          resultDiv.append("<b>I win!</b><br /><br />");
          compOverall += 1;
        }
        resultDiv.append(
          "<b>So far, you've won " + userOverall + " game" + (userOverall == 1 ? "" : "s") + " and " + 
          "I've won " + compOverall + ".</b><br />"
        );
        resultDiv.scrollTop(resultDiv[0].scrollHeight);
        $(".dice").click(playGame);
      } else {
        fastMode = $("#fastMode").is(':checked');
        setTimeout(function() { animateDice(userDie, compDie, userScore, compScore, round + 1, 1); }, (fastMode ? 0 : 250));
        
      }
    } else {
      $("#" + userDie).text(roll(userDie));
      $("#" + compDie).text(roll(compDie));

      setTimeout(function() { animateDice(userDie, compDie, userScore, compScore, round, frame + 1); }, (fastMode ? 0 : 50));
    }
  }

  function playGame(b) {
    $(".dice").unbind("click");
    resultDiv.empty();
    fastMode = $("#fastMode").is(':checked');
 
    var userPick = b.target.id;
    var compPick;
    if (userPick == "red") compPick = "blue";
    else if (userPick == "green") compPick = "red";
    else compPick = "green";

    resultDiv.append("You picked the <span style='color: " + userPick + ";'>" + userPick + "</span> die.<br />");
    resultDiv.append("I picked the <span style='color: " + compPick + ";'>" + compPick + "</span> die.<br /><br />");

    animateDice(userPick, compPick, 0, 0, 1, 1);
  };

  $(".dice").click(playGame);
});
