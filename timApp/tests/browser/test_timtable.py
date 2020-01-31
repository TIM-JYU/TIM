from time import sleep

from selenium.common.exceptions import StaleElementReferenceException

from timApp.tests.browser.browsertest import BrowserTest


class TimTableTest(BrowserTest):

    def find_and_save_timtable(self, name: str):
        while True:
            t = self.find_element_avoid_staleness('tim-table')
            try:
                self.assert_same_screenshot(t, f'timtable/{name}', attempts=5)
                break
            except StaleElementReferenceException:
                continue

    def test_timtable_simple(self):
        """
        Test case 1.2. Simple table.

        """
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
        self.find_and_save_timtable("timTableSimple")

    def test_timtable_all_column_styles(self):
        """
        Test case 1.3. All styles for column-element.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        # TODO: borderRight on first column doesn't work?
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
      width: 100px
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
        self.find_and_save_timtable("timTableAllColumnStyles")

    def test_timtable_all_row_styles(self):
        """
        Test case 1.4. All styles for row-elements.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        # TODO: borderRight on first row doesn't work?
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
      width: 500px
      fontFamily: Arial
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
      fontSize: 30px
      color: red
    - row:
      - cell: "13"
      - cell: "14"
      - cell: "15"
      - cell: "16"
      backgroundColor: '#367890'
      border: 10px solid purple

```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableAllRowStyles")

    def test_timtable_all_cell_styles(self):
        """
        Test case 1.5. All styles for cell-element.
        """
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
        fontFamily: Times
        textAlign: right
        verticalAlign: bottom
    - row:
      - cell: "5"
      - cell: "Testi"
        fontFamily: Times
      - cell: "7"
      - cell: "8"
        borderRight: 2px solid green
    - row:
      - cell: "9"
      - cell: "10"
      - cell: "11"
      - cell: "12"
        fontSize: 30px
        color: red
        border: 2px solid purple
    - row:
      - cell: "13"
      - cell: "14"
      - cell: "15"
      - cell: "16"

```
                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableAllCellStyles")

    def test_timtable_all_table_styles(self):
        """
        Test case 1.6. All styles for table-element.
        :return:
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
      - cell: "Claudius"
    - row:
      - cell: "Nero"
      - cell: "Galba"
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
      
  fontFamily: Times
  fontSize: 25px
  color: green
  backgroundColor: yellow
  width: 550px
  height: 500px
  textAlign: center
  verticalAlign: middle
  borderTop: 2px solid black
  borderBottom: 10px solid red
  borderRight: 15px dotted purple
  borderLeft: 5px solid green
```
        
                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableTableStyles")

    def test_timtable_all_styles(self):
        """
        Test case 1.7. All styles..
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  columns:
    - column:
      textAlign: left
      verticalAlign: bottom
      borderTop: 2px solid red
      borderBottom: 10px solid orange
      borderRight: 5px dotted purple
      borderLeft: 5px solid black
      backgroundColor: beige
    - column:
      width: 150px
    - column:
      border: 4px solid orange
      backgroundColor: crimson
    - column:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
        textAlign: right
        verticalAlign: bottom
        borderTop: 2px solid magenta
        borderBottom: 10px solid magenta
        borderRight: 20px solid magenta
        borderLeft: 5px solid magenta
        backgroundColor: gold
        fontSize: 30px
        fontFamily: Courier
        color: purple
        width: 200px
      - cell: "Claudius"
      textAlign: right
      verticalAlign: bottom
      borderTop: 2px solid blue
      borderBottom: 10px dotted green
      borderRight: 20px solid green
      borderLeft: 5px solid red
      backgroundColor: white
      fontSize: 15px
      fontFamily: Times
      color: crimson
    - row:
      - cell: "Nero"
      - cell: "Galba"
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
      
  fontFamily: Helvetica
  fontSize: 25px
  color: black
  backgroundColor: yellow
  width: 600px
  height: 500px
  textAlign: center
  verticalAlign: middle
  borderTop: 2px solid black
  borderBottom: 10px solid red
  borderRight: 15px dotted purple
  borderLeft: 5px solid green
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableAllStyles")

    def test_timtable_extra_properties(self):
        """
           Test case 1.8. Extra properties.
           Right way to function: Added extra properties
           senatus: "populusque romanus"
           Julius: Caesar
           Lucius: Vorenus
           Marcus: Aurelius
           have no effect. Screenshot should look just like timTableAllStyles.
           """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  columns:
    - column:
      textAlign: left
      verticalAlign: bottom
      borderTop: 2px solid red
      borderBottom: 10px solid orange
      borderRight: 5px dotted purple
      borderLeft: 5px solid black
      backgroundColor: beige
    - column:
    - column:
      border: 4px solid orange
      width: 150px
      backgroundColor: crimson
      senatus: "populusque romanus"
    - column:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
        textAlign: right
        verticalAlign: bottom
        borderTop: 2px solid magenta
        borderBottom: 10px solid magenta
        borderRight: 20px solid magenta
        borderLeft: 5px solid magenta
        backgroundColor: gold
        fontSize: 30px
        fontFamily: Script
        color: purple
      - cell: "Claudius"
      textAlign: right
      verticalAlign: bottom
      borderTop: 2px solid blue
      borderBottom: 10px dotted green
      borderRight: 20px solid green
      borderLeft: 5px solid red
      backgroundColor: white
      fontSize: 15px
      fontFamily: Times
      color: crimson
      Julius: Caesar
    - row:
      - cell: "Nero"
      - cell: "Galba"
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
        Lucius: Vorenus
      
  fontFamily:"Helvetica"
  fontSize: 25px
  color: black
  backgroundColor: yellow
  width: 600px
  height: 500px
  textAlign: center
  verticalAlign: middle
  borderTop: 2px solid black
  borderBottom: 10px solid red
  borderRight: 15px dotted purple
  borderLeft: 5px solid green
  Marcus: Aurelius
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableExtraStyles")

    def test_timtable_colspan(self):
        """
        Test case 1.9. Colspan
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
      - cell: "Claudius"
    - row:
      - cell: "Nero"
        colspan: 1
      - cell: "Galba"
        colspan: 2
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
        colspan: 3
      - cell: "Titus"
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
        colspan: 4
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableColspan")

    def test_timtable_rowspan(self):
        """
        Test case 1.10 Rowspan
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "Augustus"
        rowspan: 1
      - cell: "Tiberius"
        rowspan: 2
      - cell: "Caligula"
        rowspan: 3
      - cell: "Claudius"
        rowspan: 4
    - row:
      - cell: "Nero"
      - cell: "Galba"
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableRowspan")

    def test_timtable_rowspan_over_rows(self):
        """
        Test case 1.11 Rowspan over amount of rows.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
        rowspan:10
      - cell: "Caligula"
        rowspan: 100
      - cell: "Claudius"
    - row:
      - cell: "Nero"
      - cell: "Galba"
        rowspan:5
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
        rowspan: 10
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
        rowspan: 2
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableRowspanOverRows")

    def test_timtable_colspan_over_columns(self):
        """
        Test case 1.12. Colspan over the amount of columns.
        Right way to function is like HTML normally would with these colspan values.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
      - cell: "Claudius"
    - row:
      - cell: "Nero"
      - cell: "Galba"
        colspan: 10
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
      - cell: "Domitianus"
        colspan: 100
      - cell: "Nerva"
    - row:
      - cell: "Per"
        colspan: 15
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
```
                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableColspanOverColumns")

    def test_timtable_rowspan_colspan(self):
        """
        Test case 1.13. Rowspan and colspan together.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
      - cell: "Claudius"
    - row:
      - cell: "Nero"
      - cell: "Galba"
        rowspan: 3
        colspan: 3
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
        rowspan: 2
        colspan: 2
      - cell: "Titus"
      - cell: "Domitianus"
        rowspan: 2
        colspan: 2
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableRowspanColspan")

    def test_timtable_column_span(self):
        """
        Test case 1.14. Column span.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  columns:
    - column:
      span: 1
      backgroundColor: magenta
    - column:
      span: 2
      backgroundColor: crimson
      border: 2px solid black
    - column:
      span: 100
      backgroundColor: purple
  rows:
    - row:
      - cell: "Augustus"
      - cell: "Tiberius"
      - cell: "Caligula"
      - cell: "Claudius"
    - row:
      - cell: "Nero"
      - cell: "Galba"
      - cell: "Otho"
      - cell: "Vitellius"
    - row:
      - cell: "Vespasianus"
      - cell: "Titus"
      - cell: "Domitianus"
      - cell: "Nerva"
    - row:
      - cell: "Per"
      - cell: "aspera"
      - cell: "ad"
      - cell: "astra."
```

                    """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableColumnSpan")

    def test_timtable_many_things(self):
        """
        Test case 1.15. Many things.
        """
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
          A1: r1s1
          A2: 'Rami Pasanen '
          B1: Keijo Kukkanen
          B2: 1-11,5-6
          C1: 1-11,5-6
          C2: Keijo Kukkanen
          D1: Visa
          E1: kk
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
          border: 10px solid red
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
        - md:![Kuva](/images/471/testikuva.png)
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
        self.find_and_save_timtable("timTableManyThings")

    def test_timtable_tabledatablock(self):
        """
        Test case 1.16. Data block.
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {plugin="timTable"}
table:
  rows:
    - row:
      - cell: ''
      - cell: 'Hei1'
      - cell: 'Hei2'
    - row:
      - cell: 'Hei3'
      - cell: 'Hei4'
      - cell: 'Hei5'
    - row:
      - cell: 'Hei6'
      - cell: 'Hei7'
      - cell: 'Hei8'
  tabledatablock:
    cells:
      A1: 'd1'
      A2: 'd2'
      A3: 'd3'
      B1: 'd4'
      B2: 'd5'
      B3: 'd6'
      C1: 'd7'
      C2: 'd8'
      C3: 'd9'
    type: relative
      
```
                        """)
        self.goto_document(d)
        self.find_and_save_timtable("timTableTabledatablock")
