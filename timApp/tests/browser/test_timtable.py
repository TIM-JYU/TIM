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


    def test_timtable_simple(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - "Testi1"
      - "Testi2"
      - "Testi3"

```
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableSimple")


    def test_timtable_all_column_styles(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  columns:
    - column:
      backgroundColor: yellow
      borderLeft: 2px dotted red
      borderRight: 1px solid green
      borderTop: 10px solid black
      borderBottom: 3px solid orange
    - column:
      borderRight: 2px solid green
      textAlign: center
      verticalAlign: bottom
    - column:
    - column:
      border: 5px solid black
      width: 200px
  rows:
  - row:
    - Testi1
    - Testi2
    - Testi3
    - cell: Testi4
    height: 200px
  - row:
    - Testi1
    - Testi2
    - Testi3
    - Testi4
    height: 200px
  - row:
    - Testi1
    - Testi2
    - Testi3
    - Testi4

```

                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableAllColumnStyles")


    def test_timtable_all_row_styles(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "1"
      - cell: "2"
      - cell: "3"
      - cell: "Testi"
      backgroundColor: '#367890'
      borderLeft: 2px dotted red
      borderRight: 1px solid green
      borderTop: 10px solid black
      borderBottom: 3px solid orange
      height: 100px
      width: 100px
      fontFamily:"Calibri"
    - row:
      - cell: "5"
      - cell: "Testi"
      - cell: "7"
      - cell: "8"
      borderRight: 2px solid green
    - row:
      - cell: "9"
      - cell: "10"
      - cell: "11"
      - cell: "12"
      fontSize: 30
      color: red
    - row:
      - cell: "13"
      - cell: "14"
      - cell: "15"
      - cell: "16"
      backgroundColor: #367890
      border: 10px solid purple

```

                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableAllRowStyles")


    def test_timtable_all_cell_styles(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "1"
      - cell: "2"
      - cell: "3"
      - cell: "Testi"
        backgroundColor: '#367890'
        borderLeft: 2px dotted red
        borderRight: 1px solid green
        borderTop: 10px solid black
        borderBottom: 3px solid orange
        height: 100px
        width: 100px
        fontFamily:"Calibri"
    - row:
      - cell: "5"
      - cell: "Testi"
      - cell: "7"
      - cell: "8"
        borderRight: 2px solid green
    - row:
      - cell: "9"
      - cell: "10"
      - cell: "11"
      - cell: "12"
        fontSize: 30
        color: red
    - row:
      - cell: "13"
      - cell: "14"
      - cell: "15"
      - cell: "16"

```
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableAllCellStyles")


    def test_timtable_all_table_styles(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableTableStyles")


    def test_timtable_all_styles(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableAllStyles")


    def test_timtable_extra_styles(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableExtraStyles")


    def test_timtable_colspan(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableColspan")


    def test_timtable_rowspan(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableRowspan")

    def test_timtable_rowspan_over_rows(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableRowspanOverRows")

    def test_timtable_colspan_over_columns(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableOverColumns")

    def test_timtable_rowspan_colspan(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableRowspanColspan")

    def test_timtable_column_span(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
                    """)
        self.goto_document(d)
        sleep(2)
        we = self.find_element('tim-table')
        self.save_element_screenshot(we, "timTableColumnSpan")
