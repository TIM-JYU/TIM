from time import sleep

from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions

from timApp.tests.browser.browsertest import BrowserTest, PREV_ANSWER


class TimTableTest(BrowserTest):
    def test_timtable_initial(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  columns:
  - column:
    backgroundColor: blue
    formula: =(kurssinumero * opintopisteet)
    style: exampleStyle
    width: 1px
  - column:
      name: kurssinumero
      width: 50
  - column:
      name: opintopisteet
      width: 100
  - backgroundColor: yellow
    column:
  datablock:
    cells:
      A0: r1s1
      A1: 'Rami Pasanen '
      B0: Keijo Kukkanen
      B1: 1-11,5-6
      C0: 1-11,5-6
      C2: Keijo Kukkanen
      D0: Visa
      E0: kk
      F1: 1-11,5-6
      G2: 1-11,5-6
      H2: md:joo $$\int f(x^6) dx$$
    type: relative
  rows:
  - backgroundColor: cyan
    row:
    - cell: 'Rami Pasanen '
      type: text
    - cell: Keijo Kukkanen
      colspan: 2
      horizontal-align: right
    - border: 10px solid red
      cell: Visa
    - backgroundColor: blue
      cell: Visa
    - cell: kk
      colspan: 2
      rowspan: 2
      textAlign: center
      verticalAlign: middle
    - md:**Kasimir**
  - row:
    - borderBottom: 1px solid purple
      cell: 1-10
    - borderBottom: 1px none white
      cell: 1-11,5-6
    - Kukkuu
    - md:[1-11,5-6]{.red}
    - cell: 1-11,5-6
    - cell: 1-11,5-6
    - backgroundColor: coral
      cell: 1-11,5-6
    - cell: 1-11,5-6
  - row:
    - backgroundColor: blue
      cell: 1-10
      verticalAlign: bottom
    - md:![vesa](/images/108/vesa640.png)
    - cell: 1-11,5-6
    - cell: 1-11,5-6
    - md:[Paina]{.timButton}
    - cell: 1-11,5-6
    - cell: 1-11,5-6
    - cell: 'md: $$\int f(x) dx$$'

```

        """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableInitial")
