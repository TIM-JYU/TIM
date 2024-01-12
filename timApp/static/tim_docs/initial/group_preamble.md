``` {settings=""}
form_mode: true
disable_answer: view
macros:
  fields:
    - info
  maxRows: 60em    
css: |!!
td input[type="text"] {
    width: 100%;
}
tim-table .timTableTable {
    width: min-content;
}
.pluginShow .zoom {
    display: none;
}
!!
```

# Ryhmän hallinta

## Ryhmä %%group%%

Ryhmän nimi: **%%group%%**

## Jäsenet

#-
Jos alapuolella ei näy nimiä, se tarkoittaa, että ryhmässä ei ole jäseniä
(sillä hetkellä kun tämä sivu avattiin).

``` {#usertable plugin="tableForm"}
groups: 
 - %%group%%
fields:
  %%fields%%
table: true
includeUsers: all
open: true
report: true
singleLine: true
filterRow: true
cbColumn: true
nrColumn: true
emails: true
showInView: true
autosave: true
maxRows: %%maxRows%%
removeUsersButtonText: "Poista valitut ryhmästä"
userListButtonText: "Käyttäjälista"
emailUsersButtonText: "Lähetä sähköpostia"
```

#- {.hidden-print}
\
\
<a href="/groups/show/%%group%%" target="group">Näytä jäsenet uudessa välilehdessä JSON-muodossa</a>

## Jäsenten lisääminen {.hidden-print}

#- {allowangular="true"}
<tim-add-member group="%%group%%"></tim-add-member>

## Ryhmän poistaminen {.hidden-print}

#- {.hidden-print}
Ryhmiä ei voi poistaa.

## Muita asetuksia {.hidden-print}

``` {#GLO_join_message plugin="csPlugin" .hidden-print}
type: text
stem: |!!
md:
**Tervetuloviesti**

Alla oleva viesti lähetetään automaattisesti kaikille ryhmään lisätyille käyttäjille.

Voit määritellä viestin otsikon lisäämällä alkuun `Subject:`.\
Malli:

~~~
Subject: Viestin otsikko

Tervetuloa!
~~~

!!
button: Tallenna
rows: 10
```
