#lang racket

(require "bf.rkt")

(bf "
>           go to box 1
+++++++++   set box 1 to 9
[           if box 1 is 0 skip ahead
 <          go to box 0
 ++++++++   add 8 to box 0
 >          go to box 1
 -          subtrack 1 from box 1
]           if box 1 is not 0 skip back
            this will end up with 72 (9 * 8) in box 0 and 0 in box 1
<.>         go to box 0 output the 72 ('H') go to box 1
+++++++     set box 1 to 7
[           while box 1 is not 0
 <++++>-    add 4 to box 0 and subtract 1 from box 1
]           loop back until box 1 is 0 (7 times)
            now box 0 is 100 (72 minus 28) which would be 'd'
<+.         add 1 to box 0 (for 101 = 'e') and output
+++++++..   add 7 to box 0 (for 108 = 'l') and output twice
+++.        add 3 to box 0 (for 111 = 'o') and output
>>>         go to box 3
++++++++    set box 3 to 8
[           loop over that box
 <++++>-    add 4 to box 2 and subtract 1 from box 3
]           loop until 0
            now box 2 has 32 (8 * 4) and box 3 has 0
<.          go to box 2 and print (32 = ' ')
>>>         go to box 5
++++++++++  set box 5 to 10
[
 <
 +++++++++  add multiples of 9
 >-
]
<
---.        subtract 3 more for 87 (10 * 9 - 3)
<<<<.       go back to box 0 and output (111 = 'o')
+++.        add 3 and output (114 = 'r')
------.     subtract 6 and output (108 = 'l')
--------.   subtract 8 and output (100 = 'd')
>>+.        go to box 2 (which had 32) and add 1 (33 = '!') and output
")