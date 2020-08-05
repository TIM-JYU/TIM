``` {question="true" plugin="qst" #test1}
json:
  answerFieldType: radio
  headers: []
  questionText: What day is it today?
  questionTitle: Today
  questionType: radio-vertical
  rows:
  - Monday
  - Wednesday
  - Friday
  timeLimit: 90
points: '2:1'
defaultPoints: 0.5
```

``` {plugin="qst" #test2}
json:
  answerFieldType: radio
  headers: []
  questionText: What day is it today?
  questionTitle: Today
  questionType: radio-vertical
  rows:
  - Monday
  - Wednesday
  - Friday
  timeLimit: 1
points: '2:1'
```

``` {question="true" #test3}
json:
  answerFieldType: radio
  headers: []
  questionText: What day is it today?
  questionTitle: Today
  questionType: radio-vertical
  rows:
  - Monday
  - Wednesday
  - Friday
  timeLimit: 90
points: '2:1'
```
#-

Normal paragraph.


``` {question="true" plugin="qst" #test_shuffle_radio-vertical}
json:
    answerFieldType: radio
    doNotMove:
    - 5
    expl: {}
    headers: []
    points: 1:1;2:2;3:3;4:4;5:5
    questionText: "shuffle_radio-vertical"
    questionTitle: "shuffle_radio-vertical"
    questionType: radio-vertical
    randomizedRows: 3
    rows:
    - 1 point
    - 2 points
    - 3 points
    - 4 points
    - 5 points
```

``` {question="true" plugin="qst" #test_shuffle_true-false}
json:
    answerFieldType: radio
    answerLimit: 1
    defaultPoints: -1
    doNotMove: []
    expl: {}
    headers:
    - 'True'
    - 'False'
    points: 1:1|2:1|1:1
    questionText: True/false
    questionTitle: True/false
    questionType: true-false
    randomizedRows: 2
    rows:
    - True1
    - False2
    - True3
```

``` {question="true" plugin="qst" #test_shuffle_matrix}
json:
    answerFieldType: radio
    answerLimit: 1
    defaultPoints: -1
    doNotMove: []
    headers:
    - '1'
    - '2'
    - '3'
    - '4'
    - '5'
    matrixType: radiobutton-horizontal
    points: 1:1|2:2|3:3
    questionText: fivechoices
    questionTitle: fivechoices
    questionType: matrix
    randomizedRows: 2
    rows:
    - 'correct 1, points 1'
    - 'correct 2, points 2'
    - 'correct 3, points 3'
    timeLimit: 30
```

``` {question="true" plugin="qst" #test_checkbox-vertical}
json:
    answerFieldType: checkbox
    answerLimit: 1
    defaultPoints: -1
    doNotMove: []
    expl: {}
    headers: []
    points: 1:1;2:2;3:3
    questionText: mchoice checkbox
    questionTitle: mchoice checkbox
    questionType: checkbox-vertical
    randomizedRows: 2
    randomSeed: 1337
    rows:
    - '1 point'
    - '2 points'
    - '3 points'
    timeLimit: 30
```
