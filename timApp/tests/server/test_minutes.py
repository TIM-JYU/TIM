from timApp.tests.server.timroutetest import TimRouteTest
from timApp.document.docentry import DocEntry
from timApp.timdb.sqa import db


class MinutesHandlingTest(TimRouteTest):

    def test_minutes_creation(self):
        # Tests creation of minutes based on a meeting invitation document
        self.login_test1()
        knro = 3
        d = self.create_doc(settings={"macros": {"knro": knro}})
        minutes_document_path = f"{d.location}/PK/PK{knro}"
        self.json_post("/minutes/createMinutes",
                       json_data={"item_path": minutes_document_path,
                                  "item_title": f"PK{knro}",
                                  "copy": d.id})
        d2 = DocEntry.find_by_path(minutes_document_path, try_translation=False)
        self.assertIsNotNone(d2)
        self.assertTrue(d2.document.get_settings().is_minutes())

    def test_minute_extracts(self):
        # Tests creation of extracts from a full minutes document
        self.login_test1()
        knro = 3
        d = self.create_doc(initial_par=r"""
``` {settings=""}
macros:
   knro: 3
   year: "2018"
   testihenkilo: 'Yliopistonopettaja Maija Meikäläinen, puh. 0123456789, [maija.meikalainen@jyu.fi](mailto:maija.meikalainen@jyu.fi)'
   stampformat: Kokous {date} \newline LIITE {attachment} lista {issue}
editor_templates:
    templates:
       TDK:
         - text: "Listaotsikko"
           data: |
            %%lista(,testihenkilo)%%
         - text: "Lakikohta"
           data: |
            #- {rd="148037" ra="arvostelu" rl="no" .laki}
         - text: "Liite"
           data: |
             ``` {plugin="showPdf"}
             %%liite("Uusi liite", "A","1","URLHERE")%%
             ```

texmacros: 
 xtexdocumentclass: 'extbook'
 texautonumber: 1
 texforcetoplevel: 'chapter'
 texforcesonlysectionnumber: 1 
 tex: true
 texfont: \usepackage[default]{sourcesanspro}
 texmacros: |!!
 \newcommand{\alkuotsikko}[1]{#1}
 \newcommand{\listaotsikko}[1]{\newpage #1}
 \newcommand{\laki}[1]{\begin{adjustwidth}{0.5cm}{0.2cm}\footnotesize \it{#1}\end{adjustwidth}}
 \newcommand{\smallertable}[1] {
 {\footnotesize{#1}}
 }
 \newcommand{\valmistelija}[1]{\begin{adjustwidth}{0.2cm}{1cm}\footnotesize #1\end{adjustwidth}}
 \renewcommand{\smallVideoRunDiv}[1]{\begin{itemize}
   \tightlist
   \item
   #1
   \end{itemize}}
!!  
globalmacros:
    A:|!!
{% if tex %}
\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & KOKOUSKUTSU \\
Informaatioteknologian tiedekunta &      \\
\end{tabular}
{% else %}
<div class="listaotsikko alkuotsikko">
JYVÄSKYLÄN YLIOPISTO           | KOKOUSKUTSU
-------------------------------|----------------
Informaatioteknologian tiedekunta |
</div>
{% endif %}
# TIEDEKUNTANEUVOSTON KOKOUS %%kokous%%

Aika %%dates[knro]%% klo %%klo%%

Paikka %%paikka%%

!!
    ADDFOREVERY:|!!
{% macro lista(n,esittelija) -%}

{% if tex %}
\pagebreak
\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & Asialista %%kokous%% \\
Informaatioteknologian tiedekunta & %%pvm%%        \\
Tiedekuntaneuvosto                & {% if n %}Lista %%n%%{% endif %}         
\end{tabular}
{% if esittelija %}[Asian valmistelija: %%esittelija%%]{.valmistelija}{% endif %}
{% else %}
<div id="CSSpagebreak"><p>!================!Page Break!================!</p></div>
-----
<div class="listaotsikko">

JYVÄSKYLÄN YLIOPISTO           | Asialista %%kokous%%
-------------------------------|----------------
Informaatioteknologian tiedekunta |%%pvm%%
Tiedekuntaneuvosto                |{% if n %}Lista %%n%%{% endif %}

{% if esittelija %}<p>Asian valmistelija: %%esittelija%%</p>{% endif %}
</div>
{% endif %}
{%- endmacro %}
{% macro liite(selitys,liiteNro,lista,linkki) -%}
{% set server = "https://tim.jyu.fi" %}
{% if linkki.startswith('http') %}
{% set server = "" %}
{% endif %}
iframe: true
open: false
videoicon: false
xdocicon: false
stem: "%%selitys%%"
hidetext: Piilota liite
type: list
videoname: "(%%dates[knro]%% LIITE %%liiteNro%% / lista %%lista%%,"
text: Kokous %%dates[knro]%% \newline LIITE %%liiteNro%% / lista %%lista%%
doctext: ")"
doclink: %%linkki%%
stamped-file:
width: 800
height: 600
file: %%linkki%%
texprint: "- %%selitys%% ([%%teksti%%](%%server+linkki%%))"
{%- endmacro %}
!!
```
        """)

        d.document.add_paragraph(text="%%L%%%%lista()%%")
        d.document.add_paragraph(text="Test paragraph 1")
        d.document.add_paragraph(text="%%lista(1,testihenkilo)%%")
        d.document.add_paragraph(text="Test paragraph 2")
        d.document.add_paragraph(text="%%lista(2,testihenkilo)%%")
        d.document.add_paragraph(text="Test paragraph 3")
        d.document.add_paragraph(text="%%lista(3,testihenkilo)%%")
        d.document.add_paragraph(text="Test paragraph 4")
        db.session.commit()

        self.get(f"/minutes/createMinuteExtracts/{d.path_without_lang}", expect_status=302,
                 expect_content=f'view/{d.location}/otteet/kokous{knro}/kokous{knro}')
        self.assertIsNotNone(DocEntry.find_by_path(f"{d.location}/otteet/kokous{knro}/kokous{knro}"))
        self.assertIsNotNone(DocEntry.find_by_path(f"{d.location}/otteet/kokous{knro}/lista1"))
        self.assertIsNotNone(DocEntry.find_by_path(f"{d.location}/otteet/kokous{knro}/lista2"))
        self.assertIsNotNone(DocEntry.find_by_path(f"{d.location}/otteet/kokous{knro}/lista3"))
