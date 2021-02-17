``` {settings=""}
lazy: false
macros:
   stampformat: Kokous {date} \newline LIITE {attachment} lista {issue}
   olli: 'Yliopistonopettaja Olli Ollinen, puh. 0400123456, [olli.ollinen@example.com](mailto:olli.ollinen@example.com)'
   seppo: 'Opintopäällikkö Seppo Sepponen puh. 0500123456, [seponasiat@example.com](mailto:seponasiat@example.com)'
   olli2: 'Yliopistonopettaja Olli Ollinen2, puh. 0400123456, [olli.ollinen2@example.com](mailto:olli.ollinen2@example.com)'
   seppo2: 'Opintopäällikkö Seppo Sepponen2 puh. 0500123456, [seponasiat2@example.com](mailto:seponasiat2@example.com)'
   seppo3: 'Opintopäällikkö Seppo Sepponen3 puh. 0500123456, [seponasiat3@example.com](mailto:seponasiat3@example.com)'
editor_templates:
    templates:
       TDK:
         - text: "Listaotsikko"
           data: |
            %%lista(⁞,olli)%%
         - text: "Lakikohta"
           data: |
            #- {rd="LAIT_ID_HERE" ra="arvostelu⁞" rl="no" .laki}
         - text: "Liite"
           data: |
             ``` {plugin="showPdf"}
             %%liite("Uusi_liite", "A", "1", "Liitteen_linkki")%%
             ```
         - text: "Läsnä"
           data: |
            ``` {plugin="timTable"}
            table:
              columns:
                - width: 300px
                - width: 300px
                - width: 300px
              rows:
              - row:
                - cell: Ekan sarakkeen otsikko
                - cell: Tokan sarakkeen otsikko
                - cell: ''
              - row:
                - cell: 1. solun sisältö
                - cell: 2. solun sisältö
                - cell: ''
              - row:
                - cell: ''
                - cell: ''
                - cell: ''
              - row:
                - cell: ''
                - cell: ''
                - cell: ''
              - row:
                - cell: ''
                - cell: ''
                - cell: ''
              - row:
                - cell: ''
                - cell: ''
                - cell: ''
              tabledatablock:
                cells:
                  A1: '**Jäsen** / varajäsen'
                  A2: '**Dekaani, TkT Dekka Dekaani**'
                  A3: '**Professori, FT Proffa Proffanen**'
                  A4: '**Professori, FT Proffa Proffanen2**'
                  A5: '**Professori, FT Proffa Proffanen3**'
                  A6: '**Professori, FT Proffa Proffanen4**'
                  B1: '**Jäsen** / varajäsen'
                  B2: '**Projektitutkija, FT Proji Projinen**'
                  B3: '**Lehtori, FT Lehto Lehtonen**'
                  B4: '**Yliopistonopettaja, FT Ylppi Ylppinen**'
                  B5: Professori Proffa Proffanen5
                  B6: TkT Jaska Jokunen (ulkop. jäsen)
                  C1: '**Jäsen** / varajäsen'
                  C2: Opiskelija Oppi Oppinen
                  C3: Opiskelija Oppi Oppinen2
                  C4: Opiskelija Oppi Oppinen3
                type: relative
            
            ```

css: |!!
.listVideoRunDiv .stem {
    margin-left: 0em;
    margin-right: 0em;
}
.listaotsikko {
  border-top: navy 2pt solid;
  margin-top: 10em;
}
.alkuotsikko {
  border-top: navy 1pt solid;
  margin-top: 0em;
}
.listaotsikko td {
    width: 50em;
}
.listaotsikko p {
    font-size: smaller;
}
.paragraphs .listaotsikko table th {
    background-color: initial;
    border: none;
    font-weight: normal;
    padding: 0;
}
.paragraphs .listaotsikko table td {
    background-color: initial;
    border: none;
    font-weight: normal;
    padding: 0;
}
.paragraphs .listaotsikko table {
    background-color: initial;
    border: none;
    font-weight: normal;
    margin-bottom: 0;
}
.laki {
    font-style: italic;
    font-size: x-small;
}
.smallertable td {
    font-size: smaller;
    text-align: left;
}
.smallertable th {
    font-size: smaller;
    text-align: left;
}
.laki p {
    font-size: small;
    margin-left: 1em;
}
.laki ol {
    font-size: x-small;
}
a.xvideoname {
    display: list-item;
    text-align: -webkit-match-parent;
    margin-left: 2.5em;
    margin-top: -1.5em;
    padding-top: 0em;
}
.reveal .laki p {
    visibility: collapse;
}
@media print and (color) {
  .paragraphs .showVideo{
      display: block !important;
  }
}
!!
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
# TIEDEKUNTANEUVOSTON KOKOUS %%knro%%/%%year%%

Aika %%dates[knro][0]%% klo %%dates[knro][1]%%

Paikka %%dates[knro][2]%%

!!
    ADDFOREVERY:|!!
{% macro lista(n,esittelija, esittelija2) -%}

{% if tex %}
\pagebreak
\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & Asialista %%knro%%/%%year%% \\
Informaatioteknologian tiedekunta & %%dates[knro][0]%%         \\
Tiedekuntaneuvosto                & {% if n %}Lista %%n%%{% endif %}         
\end{tabular}
{% if esittelija2 %}[Asian valmistelijat: %%esittelija%%, %%esittelija2%%]{.valmistelija}
{% elif esittelija %}[Asian valmistelija: %%esittelija%%]{.valmistelija}{% endif %}
{% else %}
<div id="CSSpagebreak"><p>!================!Page Break!================!</p></div>
-----
<div class="listaotsikko" id="LISTA%%n%%">

JYVÄSKYLÄN YLIOPISTO           | %%knro%%/%%year%%
-------------------------------|----------------
Informaatioteknologian tiedekunta |%%dates[knro][0]%% 
Tiedekuntaneuvosto                |{% if n %}Lista %%n%%{% endif %}

{% if esittelija2 %}<p>Asian valmistelijat: %%esittelija%%, %%esittelija2%%</p>
{% elif esittelija %}<p>Asian valmistelija: %%esittelija%%</p>{% endif %}
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
videoname: "(LIITE %%liiteNro%% / lista %%lista%%,"
text: Kokous %%dates[knro][0]%% \newline LIITE %%liiteNro%% / lista %%lista%%
doctext: ")"
doclink: %%linkki%%
stamped-file:
width: 800
height: 600
file: %%linkki%%
texprint: "- %%selitys%% ([LIITE %%liiteNro%% / lista %%lista%%](%%server+linkki%%))"
{%- endmacro %}
!!
```

#- {.smalltext .hidden-print nocache="true"}
{% if "ittdk18" | belongs %}
[Kokoukset](/view/tiedekunnat/it/%%year%%/kokoukset) |
[Seuraava esityslista](https://tim.jyu.fi/view/tiedekunnat/it/%%year%%/kokous%%knro%%) |
[pdf](/print/tiedekunnat/it/%%year%%/kokous%%knro%%?template_doc_id=134613) |
[Ohjeet](https://tim.jyu.fi/view/tiedekunnat/it/uusien-ohje) |
[Keskustelu](https://tim.jyu.fi/view/tiedekunnat/it/%%year%%/keskustelu)
{% endif %}
{% if "ittdk1" | belongs %}
| [Ylläpito](/view/tiedekunnat/it/2018/yllapito)
{% endif %}
