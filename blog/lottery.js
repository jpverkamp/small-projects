jQuery(function($) {
  var overallSpent = 0;
  var overallWon = 0;

  function roll(n) {
    return Math.floor(Math.random() * n) + 1;
  }

  function chooseNumbers() {
    var numbers = new Array(0, 0, 0, 0, 0)
    while (numbers[0] == numbers[1] || numbers[0] == numbers[2] || numbers[0] == numbers[3]
           || numbers[0] == numbers[4] || numbers[1] == numbers[2] || numbers[1] == numbers[3]
           || numbers[1] == numbers[4] || numbers[2] == numbers[3] || numbers[2] == numbers[4]
           || numbers[3] == numbers[4]) {
      numbers = new Array(roll(59), roll(59), roll(59), roll(59), roll(59));
    }
    numbers = numbers.sort();
    return new Array(numbers, roll(35));
  }

  function lotteryRandom() {
    var numbers = chooseNumbers();

    $('#lotteryNumbers').val(numbers[0].join(' '));
    $('#lotteryPowerball').val(numbers[1]);
  }

  function score(yours, theirs) {
    var matches = 0;
    for (var i = 0; i < 5; i++) {
      for (var j = 0; j < 5; j++) {
        if (yours[0][j] == theirs[0][i]) {
          matches += 1
          break;
        }
      }
    }
    
    var powerball = (yours[1] == theirs[1]);

    var result = 0;
    if (matches == 0 && powerball) result = 4;
    else if (matches == 1 && powerball) result = 4;
    else if (matches == 2 && powerball) result = 7;
    else if (matches == 3 && !powerball) result = 7;
    else if (matches == 3 && powerball) result = 100;
    else if (matches == 4 && !powerball) result = 100;
    else if (matches == 4 && powerball) result = 10000;
    else if (matches == 5 && !powerball) result = 1000000;
    else if (matches == 5 && powerball) result = 150000000;

    return new Array(matches, powerball, result);
  }

  function lotteryPlay() {
    $("#lotteryNumbersWarning").html("");
    $("#lotteryPowerballWarning").html("");
    $("#lotteryPlaysWarning").html("");
    var sanity = true;

    var numbers = new Array(
      $("#lotteryNumbers").val().split(" "),
      $("#lotteryPowerball").val()
    );

    if (numbers[0].length == 5) {
      for (var i = 0; i < 5; i++) {
        numbers[0][i] = parseInt(numbers[0][i]);
        if (isNaN(numbers[0][i]) || numbers[0][i] < 1 || numbers[0][i] > 59) {
          $("#lotteryNumbersWarning").html("Numbers must be in the range 1-59");
          sanity = false;
        }
      }
      numbers[0].sort();

      for (var i = 0; i < 5; i++) {
        for (var j = i + 1; j < 5; j++) {
          if (numbers[0][i] == numbers[0][j]) {
            $("#lotteryNumbersWarning").html("Numbers must be unique");
            sanity = false;
          }
        }
      }
    } else {
      $("#lotteryNumbersWarning").html("You must enter five numbers separated by spaces");
      sanity = false;
    }

    numbers[1] = parseInt(numbers[1]);
    if (isNaN(numbers[1]) || numbers[1] < 1 || numbers[1] > 59) {
      $("#lotteryPowerballWarning").html("You must enter a single number in the range 1-35");
      return;
    }

    var plays = parseInt($("#lotteryPlays").val());

    if (isNaN(plays) || plays < 1 || plays > 100) {
      $("#lotteryPlaysWarning").html("You can only play between 1 and 100 games at a time.");
      sanity = false;
    }

    if (!sanity) return;

    var result = '';
    var total = 0;
    for (var i = 0; i < plays; i++) {
      var winners = chooseNumbers();
      var scores = score(numbers, winners);
      total += scores[2];

      result += "<p>Round " + (i+1) + ": " + winners[0].join(" ") + 
        " <span style='color: red;'>" + winners[1] + "</span> -- $" + scores[2] + "</p>\n";
    }

    overallSpent += 2 * plays;
    overallWon += total;

    $("#lotteryResults").html(
      "<p>Overall: $" + (overallSpent) + " spent, $" + (overallWon) + " won, $" + (overallWon - overallSpent) + " net gain/loss</p>" +
      "<p>This round: $" + (2 * plays) + " spent, $" + (total) + " won, $" + (-2 * plays + total) + " net gain/loss</p>" +
      "<hr />" + result
    );
  }

  $("#lotteryRandomButton").click(lotteryRandom);
  $("#lotteryPlayButton").click(lotteryPlay);
});
