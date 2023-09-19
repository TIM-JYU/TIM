from timApp.admin.replace_in_documents import perform_replace, ReplaceArguments
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PRINT_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
)
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup
from timApp.util.utils import decode_csplugin, static_tim_doc


# TODO: Logging in as different user does not work in BrowserTest classes
class MinutesCreation(TimRouteTest):
    def test_minutes_creation(self):
        # Tests creation of minutes based on a meeting invitation document
        self.login_test1()
        knro = 3
        d = self.create_doc(settings={"macros": {"knro": knro}})
        j = self.upload_file(d, b"GIF87a", "test.jpg")
        image_path = j["image"]
        minutes_document_path = f"{d.location}/pk/pk{knro}"
        self.json_post(
            "/minutes/createMinutes",
            json_data={
                "item_path": minutes_document_path,
                "item_title": f"pk{knro}",
                "copy": d.id,
            },
        )

        d2 = DocEntry.find_by_path(minutes_document_path)
        chg = d2.document.get_changelog()
        self.assertEqual(self.get_test_user_1_group_id(), chg.entries[0].group_id)
        self.assertTrue(d2.document.get_settings().memo_minutes() == "minutes")

        # Files should not get copied to minutes document.
        self.test_user_2.grant_access(d2, AccessType.view)
        db.session.commit()
        self.login_test2()
        self.get(f"/images/{image_path}", expect_status=403)


class MinutesHandling(TimRouteTest):
    def test_minute_extracts(self):
        with self.internal_container_ctx():
            self.impl_test_minute_extracts()

    def impl_test_minute_extracts(self):
        # Tests creation of extracts from a full minutes document
        self.login_test1()
        ug1 = UserGroup.create("ittdk18")
        ug2 = UserGroup.create("ittdk1")
        u = self.current_user
        u.groups.append(ug1)
        u.groups.append(ug2)
        db.session.commit()
        knro = 9
        d_kokous = self.create_doc(
            path=self.get_personal_item_path("tdk/2018/kokous9"),
            from_file=static_tim_doc("tdk/memo.md"),
        )
        d_lait = self.create_doc(
            path=self.get_personal_item_path("tdk/lait/lait"),
            from_file=static_tim_doc("tdk/lait.md"),
        )
        d_preamble = self.create_doc(
            path=self.get_personal_item_path(
                f"tdk/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/preamble"
            ),
            from_file=static_tim_doc("tdk/preamble.md"),
        )
        d_preamble_year = self.create_doc(
            path=self.get_personal_item_path(
                f"tdk/2018/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/preamble"
            ),
            from_file=static_tim_doc("tdk/preamble_year.md"),
        )
        for d in (d_preamble, d_preamble_year):
            perform_replace(d, ReplaceArguments(term="LAIT_ID_HERE", to=f"{d_lait.id}"))
        for p in d_kokous.document.get_paragraphs():
            if p.get_attr("rd") == "LAIT_ID_HERE":
                p.set_attr("rd", str(d_lait.id))
                p.save()

        self.get(
            f"/minutes/createMinuteExtracts/{d_kokous.path_without_lang}",
            expect_status=302,
            expect_content=f"/view/{d_kokous.location}/otteet/kokous{knro}/kokous{knro}",
        )
        ote_index = DocEntry.find_by_path(
            f"{d_kokous.location}/otteet/kokous{knro}/kokous{knro}"
        )
        t = self.get(ote_index.url, as_tree=True)
        self.assert_content(
            t,
            [
                "Kokoukset | Seuraava esityslista | pdf | Ohjeet | Keskustelu | Ylläpito",
                "Pöytäkirjan asiakohtien otteet",
                "Lista 1, (PDF) - Ilmoitusasiat\n"
                "Lista 2, (PDF) - Jaska Jokusen väitöskirjan arvostelu",
                "",
            ],
        )
        ote_index.document.clear_mem_cache()
        self.assertEqual(
            """## Pöytäkirjan asiakohtien otteet {area="kokous9"}


- [Lista 1](lista1), ([PDF](/print/users/test-user-1/tdk/2018/otteet/kokous9/lista1)) - Ilmoitusasiat
- [Lista 2](lista2), ([PDF](/print/users/test-user-1/tdk/2018/otteet/kokous9/lista2)) - Jaska Jokusen väitöskirjan arvostelu

#- {area_end="kokous9"}
""",
            ote_index.document.export_markdown(export_ids=False),
        )
        ote1 = DocEntry.find_by_path(f"{d_kokous.location}/otteet/kokous{knro}/lista1")
        self.assertEqual(
            r"""PÖYTÄKIRJANOTE - Lista 1 -  Ilmoitusasiat      
\

#- {rd="5" ra="ETUSIVU"}

#- {rd="5" rp="dsENZazDtd1Z"}

#- {rd="5" rp="sEj1vkUWmFmG"}

#- {rd="5" rp="ePZZ1MvVVb5S"}

#- {rd="5" rp="RIam8d8VXbQv"}

#- {rd="5" rp="yN0j9XeEOdZG"}

#- {rd="5" rp="PJxELAyKhZuu"}


%%OTE_ALLEKIRJOITUKSET()%%

%%allekirjoitus(olli)%%
""",
            ote1.document.export_markdown(export_ids=False),
        )
        ote_html = self.get(ote1.url, as_tree=True)
        self.assert_content(
            ote_html,
            [
                "Kokoukset | Seuraava esityslista | pdf | Ohjeet | Keskustelu | Ylläpito",
                "PÖYTÄKIRJANOTE - Lista 1 - Ilmoitusasiat",
                "",
                "JYVÄSKYLÄN YLIOPISTO\n"
                "KOKOUSKUTSU\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "Informaatioteknologian tiedekunta\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "TIEDEKUNTANEUVOSTON KOKOUS 9/2018\n"
                "Aika 11.9.2018 klo 9:00\n"
                "Paikka Jossakin Agoralla",
                "",
                "",
                "",
                "!================!Page Break!================!\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "JYVÄSKYLÄN YLIOPISTO\n"
                "9/2018\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "Informaatioteknologian tiedekunta\n"
                "11.9.2018\n"
                "\n"
                "\n"
                "Tiedekuntaneuvosto\n"
                "Lista 1\n"
                "\n"
                "\n"
                "\n"
                "\n"
                "Asian valmistelija: Yliopistonopettaja Olli Ollinen, puh. 0400123456, "
                "olli.ollinen@example.com",
                "1. Ilmoitusasiat\n"
                "1.1 Dekaanin ja varadekaanin päätökset\n"
                "1.1.1 Tutkinnot vuonna 2018 (liite A/ lista 1)",
                "",
                "Kandidaatin tutkinnot: Toteutunut yhteensä xxx (tavoite 2018: yyy)\n"
                "Kauppatieteiden kandidaatin tutkinto: Toteutunut xx\n"
                "1.1.2 Arvostellut pro gradu –tutkielmat (liite B/ lista 1)",
                "",
                "Esitys\nTodetaan ilmoitusasiat.",
                "Pöytäkirjanotteen vakuudeksi:\n\n\n\nOlli Ollinen\nYliopistonopettaja",
            ],
        )
        plugins = ote_html.cssselect("tim-video")
        self.assertEqual(
            {
                "doclink": "https://tim.jyu.fi/files/xxx/tutkinnot.pdf",
                "doctext": ")",
                "file": "https://tim.jyu.fi/files/xxx/tutkinnot.pdf",
                "height": 600,
                "hidetext": "Piilota liite",
                "iframe": True,
                "open": False,
                "stamped-file": None,
                "stem": "Tutkinnot vuonna 2018",
                "texprint": "- Tutkinnot vuonna 2018 ([LIITE A / lista "
                "1](https://tim.jyu.fi/files/xxx/tutkinnot.pdf))",
                "text": "Kokous 11.9.2018 \\newline LIITE A / lista 1",
                "type": "list",
                "videoicon": False,
                "videoname": "(LIITE A / lista 1,",
                "width": 800,
                "xdocicon": False,
            },
            decode_csplugin(plugins[0]),
        )

        self.assertEqual(
            {
                "doclink": "https://tim.jyu.fi/files/yyy/gradut.pdf",
                "doctext": ")",
                "file": "https://tim.jyu.fi/files/yyy/gradut.pdf",
                "height": 600,
                "hidetext": "Piilota liite",
                "iframe": True,
                "open": False,
                "stamped-file": None,
                "stem": "Arvostellut pro gradu –tutkielmat",
                "texprint": "- Arvostellut pro gradu –tutkielmat ([LIITE B / lista "
                "1](https://tim.jyu.fi/files/yyy/gradut.pdf))",
                "text": "Kokous 11.9.2018 \\newline LIITE B / lista 1",
                "type": "list",
                "videoicon": False,
                "videoname": "(LIITE B / lista 1,",
                "width": 800,
                "xdocicon": False,
            },
            decode_csplugin(plugins[1]),
        )

        DocEntry.create(
            f"{TEMPLATE_FOLDER_NAME}/{PRINT_FOLDER_NAME}/runko",
            initial_par="""
``` {.latex printing_template=""}
$body$
```""",
        )
        db.session.commit()
        ote1.document.clear_mem_cache()
        ote1_tex = self.get_no_warn(
            f"/print/{ote1.path}", query_string={"file_type": "latex"}
        )
        self.assertEqual(
            r"""
PÖYTÄKIRJANOTE - Lista 1 - Ilmoitusasiat\\
\strut \\

\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & KOKOUSKUTSU \\
Informaatioteknologian tiedekunta &      \\
\end{tabular}

\hypertarget{tiedekuntaneuvoston-kokous-92018}{%
\chapter{TIEDEKUNTANEUVOSTON KOKOUS
9/2018}\label{tiedekuntaneuvoston-kokous-92018}}

Aika 11.9.2018 klo 9:00

Paikka Jossakin Agoralla

\begin{table}[H]
\resizebox{\columnwidth}{!}{%
\begin{tabular}{cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc}

\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Jäsen}}}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Läsnä asiat:}}}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Varajäsen:}}}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Läsnä asiat:}}}}}}}
\tabularnewline[20.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Professorit}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Dekaani, TkT Henki Henkilö}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö2}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö3}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö4}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, KTT Henki Henkilö5}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö6}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö7}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö8}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Tutkimusprofessori, TkT Henki Henkilö9}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Muu opetus- ja tutkimushlöstö ja muu hlöstö}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Projektitutkija, FT Henki Henkilö9}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Lehtori, KTT, LitM Henki Henkilö10}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Lehtori, FT Henki Henkilö11}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Yliopistonopettaja, FT Henki Henkilö12}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Yliopistonopettaja, FT Henki Henkilö12}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Yliopistonopettaja, FT Henki Henkilö13}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Opiskelijat}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö14}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö15}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö16}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö17}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö18}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö19}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Ulkopuoliset jäsenet:}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori Henki Henkilö20}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{TkT Henki Henkilö21}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\end{tabular}%
}
\end{table}

\pagebreak
\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & Asialista 9/2018 \\
Informaatioteknologian tiedekunta & 11.9.2018         \\
Tiedekuntaneuvosto                & Lista 1\end{tabular}

{\valmistelija{Asian valmistelija: Yliopistonopettaja Olli Ollinen, puh.
0400123456,
\href{mailto:olli.ollinen@example.com}{\nolinkurl{olli.ollinen@example.com}}}}

\hypertarget{ilmoitusasiat}{%
\section{Ilmoitusasiat}\label{ilmoitusasiat}}

\hypertarget{dekaanin-ja-varadekaanin-puxe4uxe4tuxf6kset}{%
\subsection{Dekaanin ja varadekaanin
päätökset}\label{dekaanin-ja-varadekaanin-puxe4uxe4tuxf6kset}}

\hypertarget{tutkinnot-vuonna-2018-liite-a-lista-1}{%
\subsubsection{Tutkinnot vuonna 2018 (liite A/ lista
1)}\label{tutkinnot-vuonna-2018-liite-a-lista-1}}

\begin{itemize}
\tightlist
\item
  Tutkinnot vuonna 2018
  (\href{https://tim.jyu.fi/files/xxx/tutkinnot.pdf}{LIITE A / lista 1})
\end{itemize}

Kandidaatin tutkinnot: Toteutunut yhteensä xxx (tavoite 2018: yyy)

Kauppatieteiden kandidaatin tutkinto: Toteutunut xx

\hypertarget{arvostellut-pro-gradu-tutkielmat-liite-b-lista-1}{%
\subsubsection{Arvostellut pro gradu --tutkielmat (liite B/ lista
1)}\label{arvostellut-pro-gradu-tutkielmat-liite-b-lista-1}}

\begin{itemize}
\tightlist
\item
  Arvostellut pro gradu --tutkielmat
  (\href{https://tim.jyu.fi/files/yyy/gradut.pdf}{LIITE B / lista 1})
\end{itemize}

\textbf{Esitys}

Todetaan ilmoitusasiat.

\bigskip

\hfill\break
\hfill\break
\hfill\break
\hfill\break
Pöytäkirjanotteen vakuudeksi:

\hfill\break
\hfill\break
\hfill\break
Olli Ollinen\\
Yliopistonopettaja\\
        """.strip(),
            ote1_tex,
        )
        ote2 = DocEntry.find_by_path(f"{d_kokous.location}/otteet/kokous{knro}/lista2")
        ote2_tex = self.get_no_warn(
            f"/print/{ote2.path}", query_string={"file_type": "latex"}
        )
        self.assertEqual(
            r"""
PÖYTÄKIRJANOTE - Lista 2 - Jaska Jokusen väitöskirjan arvostelu\\
\strut \\

\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & KOKOUSKUTSU \\
Informaatioteknologian tiedekunta &      \\
\end{tabular}

\hypertarget{tiedekuntaneuvoston-kokous-92018}{%
\chapter{TIEDEKUNTANEUVOSTON KOKOUS
9/2018}\label{tiedekuntaneuvoston-kokous-92018}}

Aika 11.9.2018 klo 9:00

Paikka Jossakin Agoralla

\begin{table}[H]
\resizebox{\columnwidth}{!}{%
\begin{tabular}{cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc}

\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Jäsen}}}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Läsnä asiat:}}}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Varajäsen:}}}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\cellcolor{lightgray}\fontsize{12}{0}\selectfont{\textcolor{black}{{\textbf{\emph{Läsnä asiat:}}}}}}}
\tabularnewline[20.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Professorit}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Dekaani, TkT Henki Henkilö}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö2}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö3}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö4}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, KTT Henki Henkilö5}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö6}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö7}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori, FT Henki Henkilö8}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Tutkimusprofessori, TkT Henki Henkilö9}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Muu opetus- ja tutkimushlöstö ja muu hlöstö}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Projektitutkija, FT Henki Henkilö9}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Lehtori, KTT, LitM Henki Henkilö10}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Lehtori, FT Henki Henkilö11}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Yliopistonopettaja, FT Henki Henkilö12}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Yliopistonopettaja, FT Henki Henkilö12}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Yliopistonopettaja, FT Henki Henkilö13}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Opiskelijat}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö14}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö15}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö16}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö17}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö18}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Henki Henkilö19}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{4}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{\emph{Ulkopuoliset jäsenet:}}}}}} 
\tabularnewline[0.0pt]
\hhline{~~~~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{Professori Henki Henkilö20}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{TkT Henki Henkilö21}}}}} & \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{*}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}& \multicolumn{1}{l}{\multirow{1}{66.67pt}{\fontsize{12}{0}\selectfont{\textcolor{black}{{}}}}}
\tabularnewline[0.0pt]
\hhline{~~~~}
\end{tabular}%
}
\end{table}

\pagebreak
\begin{tabular}{p{10cm} l}
JYVÄSKYLÄN YLIOPISTO              & Asialista 9/2018 \\
Informaatioteknologian tiedekunta & 11.9.2018         \\
Tiedekuntaneuvosto                & Lista 2\end{tabular}

{\valmistelija{Asian valmistelijat: Opintopäällikkö Seppo Sepponen, puh.
0500123456,
\href{mailto:seponasiat@example.com}{\nolinkurl{seponasiat@example.com}},
Yliopistonopettaja Olli Ollinen, puh. 0400123456,
\href{mailto:olli.ollinen@example.com}{\nolinkurl{olli.ollinen@example.com}}}}

\hypertarget{jaska-jokusen-vuxe4ituxf6skirjan-arvostelu}{%
\section{Jaska Jokusen väitöskirjan
arvostelu}\label{jaska-jokusen-vuxe4ituxf6skirjan-arvostelu}}

Jaska Jokunen puolustaa 1.1.2018 julkisessa väitöstilaisuudessa
tietojärjestelmätieteen väitöskirjaansa.

\laki{

}

\laki{

\laki{

Arvostelu, tekstikappale 1.

Arvostelu, tekstikappale 2.

Arvostelu, tekstikappale 3.

}

}

\laki{

}

\begin{itemize}
\tightlist
\item
  Vastaväittäjän lausunto
  (\href{https://tim.jyu.fi/files/zzz/opponentti.pdf}{LIITE A / lista
  2})
\end{itemize}

\textbf{Esitys}

Tiedekuntaneuvosto arvostelee Jaska Jokusen väitöskirjan.

\bigskip

\hfill\break
\hfill\break
\hfill\break
\hfill\break
Pöytäkirjanotteen vakuudeksi:

\hfill\break
\hfill\break
\hfill\break
Seppo Sepponen\\
Opintopäällikkö\\

\hfill\break
\hfill\break
\hfill\break
Olli Ollinen\\
Yliopistonopettaja\\
        """.strip(),
            ote2_tex,
        )
        self.assertIsNone(
            DocEntry.find_by_path(f"{d_kokous.location}/otteet/kokous{knro}/lista3")
        )
