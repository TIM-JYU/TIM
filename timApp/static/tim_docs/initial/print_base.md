#- {area="fulltemplate" settings=""}
texmacros: 
 texfontsize: 12pt
 texauthorname: 'Tekijä'
 texlanguage: 'finnish'
 texpaper: 'a4paper'
 texdocumentclass: 'extarticle'
 texside: 'oneside'
 textitle: 'Otsikko'
 texversion: 'Versio: 0.0.1'
 texdate: '18.7.2017'
 texorganization: 'Organisaatio'
 texsectionstart: 0
 texautonumber: 0
 texcoverpage: 0
 textableofcontents: 0
 texfancyfooter: 0
 texforcetoplevel: ''
 texforcesonlysectionnumber: 0
 texcaption: 1
 texmargins: 'left=20mm, right=20mm, top=20mm, bottom=20mm'
 texfootrulewidth: 0.0pt
 texfancyfooterstyle: ''
 texbibname: ''
 texenvironment_classes:
  - huomautus

#- {area="beforemacros" collapse="true"}
beforemacros

``` {.latex printing_template=""}
\documentclass[%%texside%%, %%texfontsize%%,%%texpaper%%,%%texlanguage%%]%%'{'%%%%texdocumentclass%%}
\usepackage{extsizes}
{% if texbibname != "" %}
\usepackage[backend=biber,sortcites]{biblatex}
\addbibresource%%"{"%%%%texbibname%%.bib}
{% endif %}
$if(geometry)$
\usepackage[$for(geometry)$$geometry$$sep$,$endfor$]{geometry}
$else$
\usepackage{geometry}
 \geometry{
 %%texpaper%%,
 % total={140mm,257mm},
 %%texmargins%%
}
$endif$
\usepackage{changepage} 
\usepackage{caption} 
\usepackage{tabularx}
\usepackage{tabulary}
\usepackage{listings}
\usepackage{float}
\usepackage{pmboxdraw}
\usepackage{titlesec, blindtext, color}

% TimTable needs:
\usepackage[table,usenames,dvipsnames]{xcolor} % Colors.
\usepackage{multirow}
\usepackage{hhline}
\usepackage{graphics} % Scaling.
\usepackage{colortbl} % Line colors.

%%texfont%%

$if(beamerarticle)$
\usepackage{beamerarticle} % needs to be loaded first
$endif$
$if(fontfamily)$
\usepackage[$for(fontfamilyoptions)$$fontfamilyoptions$$sep$,$endfor$]{$fontfamily$}
$else$
$endif$
$if(linestretch)$
\usepackage{setspace}
\setstretch{$linestretch$}
$endif$
\usepackage{amssymb,amsmath}
\usepackage{pifont}
\usepackage{unicode-math}
\let\mathbb\relax % remove the definition by unicode-math
\DeclareMathAlphabet{\mathcal}{OMS}{cmsy}{m}{n}
\DeclareMathAlphabet{\mathbb}{U}{msb}{m}{n}
\AtBeginDocument{
  \renewcommand{\setminus}{\mathbin{\backslash}}%
}
\setmainfont
 [ BoldFont       = texgyrepagella-bold.otf ,
   ItalicFont     = texgyrepagella-italic.otf ,
   BoldItalicFont = texgyrepagella-bolditalic.otf ]
 {texgyrepagella-regular.otf}
\setmathfont{Latin Modern Math}
$for(fontfamilies)$
  \newfontfamily{$fontfamilies.name$}[$fontfamilies.options$]{$fontfamilies.font$}
$endfor$
$if(euro)$
  \newcommand{\euro}{€}
$endif$
$if(mainfont)$
    \setmainfont[$for(mainfontoptions)$$mainfontoptions$$sep$,$endfor$]{$mainfont$}
$endif$
$if(sansfont)$
    \setsansfont[$for(sansfontoptions)$$sansfontoptions$$sep$,$endfor$]{$sansfont$}
$endif$
$if(monofont)$
    \setmonofont[Mapping=tex-ansi$if(monofontoptions)$,$for(monofontoptions)$$monofontoptions$$sep$,$endfor$$endif$]{$monofont$}
$endif$
$if(mathfont)$
    \setmathfont(Digits,Latin,Greek)[$for(mathfontoptions)$$mathfontoptions$$sep$,$endfor$]{$mathfont$}
$endif$
$if(CJKmainfont)$
    \usepackage{xeCJK}
    \setCJKmainfont[$for(CJKoptions)$$CJKoptions$$sep$,$endfor$]{$CJKmainfont$}
$endif$
% use upquote if available, for straight quotes in verbatim environments
\IfFileExists{upquote.sty}{\usepackage{upquote}}{}
% use microtype if available
\IfFileExists{microtype.sty}{
\usepackage[$for(microtypeoptions)$$microtypeoptions$$sep$,$endfor$]{microtype}
\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts
}{}
\PassOptionsToPackage{hyphens}{url} % url is loaded by hyperref
$if(verbatim-in-note)$
\usepackage{fancyvrb}
$endif$
\usepackage[unicode=true]{hyperref}
$if(colorlinks)$
\PassOptionsToPackage{usenames,dvipsnames}{color} % color is loaded by hyperref
$endif$
\hypersetup{
$if(title-meta)$
            pdftitle={$title-meta$},
$endif$
$if(author-meta)$
            pdfauthor={$author-meta$},
$endif$
$if(keywords)$
            pdfkeywords={$for(keywords)$$keywords$$sep$, $endfor$},
$endif$
$if(colorlinks)$
            colorlinks=true,
            linkcolor=$if(linkcolor)$$linkcolor$$else$Maroon$endif$,
            citecolor=$if(citecolor)$$citecolor$$else$Blue$endif$,
            urlcolor=$if(urlcolor)$$urlcolor$$else$Blue$endif$,
$else$
            pdfborder={0 0 0},
$endif$
            breaklinks=true}
\urlstyle{same}  % don't use monospace font for urls
$if(verbatim-in-note)$
\VerbatimFootnotes % allows verbatim text in footnotes
$endif$
\ifnum 0\ifxetex 1\fi\ifluatex 1\fi=0 % if pdftex
  \usepackage[shorthands=off,main=%%texlanguage%%]{babel}
\else
  \usepackage{polyglossia}
  \setmainlanguage[]%%"{"%%%%texlanguage%%}
\fi
$if(natbib)$
\usepackage{natbib}
\bibliographystyle{$if(biblio-style)$$biblio-style$$else$plainnat$endif$}
$endif$
$if(biblatex)$
\usepackage[$if(biblio-style)$style=$biblio-style$,$endif$$for(biblatexoptions)$$biblatexoptions$$sep$,$endfor$]{biblatex}
$for(bibliography)$
\addbibresource{$bibliography$}
$endfor$
$endif$
$if(listings)$
\usepackage{listings}
$endif$
$if(lhs)$
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small\ttfamily}}{}
$endif$
$if(highlighting-macros)$
$highlighting-macros$
$endif$
$if(tables)$
\usepackage{longtable,booktabs,array}
\usepackage{calc} % for calculating minipage widths
% Fix footnotes in tables (requires footnote package)
\IfFileExists{footnote.sty}{\usepackage{footnote}\makesavenoteenv{long table}}{}
$endif$
$if(graphics)$
\usepackage{graphicx,grffile}
\makeatletter
\def\maxwidth{\ifdim\Gin@nat@width>\linewidth\linewidth\else\Gin@nat@width\fi}
\def\maxheight{\ifdim\Gin@nat@height>\textheight\textheight\else\Gin@nat@height\fi}
\makeatother
% Scale images if necessary, so that they will not overflow the page
% margins by default, and it is still possible to overwrite the defaults
% using explicit options in \includegraphics[width, height, ...]{}
\setkeys{Gin}{width=\maxwidth,height=\maxheight,keepaspectratio}
$endif$
$if(links-as-notes)$
% Make links footnotes instead of hotlinks:
\renewcommand{\href}[2]{#2\footnote{\url{#1}}}
$endif$
$if(strikeout)$
\usepackage[normalem]{ulem}
% avoid problems with \sout in headers with hyperref:
\pdfstringdefDisableCommands{\renewcommand{\sout}{}}
$endif$
$if(indent)$
$else$
\IfFileExists{parskip.sty}{
\usepackage{parskip}
}{ else
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt plus 2pt minus 1pt}
}
$endif$
\setlength{\emergencystretch}{3em}  % prevent overfull lines
\providecommand{\tightlist}{
  \setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
$if(numbersections)$
\setcounter{secnumdepth}{$if(secnumdepth)$$secnumdepth$$else$5$endif$}
$else$
\setcounter{secnumdepth}{0}
$endif$
$if(subparagraph)$
$else$
% Redefines (sub)paragraphs to behave more like sections
\ifx\paragraph\undefined\else
\let\oldparagraph\paragraph
\renewcommand{\paragraph}[1]{\oldparagraph{#1}\mbox{}}
\fi
\ifx\subparagraph\undefined\else
\let\oldsubparagraph\subparagraph
\renewcommand{\subparagraph}[1]{\oldsubparagraph{#1}\mbox{}}
\fi
$endif$
$if(dir)$
\ifxetex
  % load bidi as late as possible as it modifies e.g. graphicx
  $if(latex-dir-rtl)$
  \usepackage[RTLdocument]{bidi}
  $else$
  \usepackage{bidi}
  $endif$
\fi
\ifnum 0\ifxetex 1\fi\ifluatex 1\fi=0 % if pdftex
  \TeXXeTstate=1
  \newcommand{\RL}[1]{\beginR #1\endR}
  \newcommand{\LR}[1]{\beginL #1\endL}
  \newenvironment{RTL}{\beginR}{\endR}
  \newenvironment{LTR}{\beginL}{\endL}
\fi
$endif$

% set default figure placement to htbp
\makeatletter
\def\fps@figure{htbp}
\makeatother

$for(header-includes)$
$header-includes$
$endfor$

\ifnum 11=1%%texautonumber%%
  \ifcsname c@chapter\endcsname

  \setcounter{chapter}{ %%texsectionstart%%}
  \else
  \fi
\setcounter{section}{ %%texsectionstart%%}
\setcounter{secnumdepth}{6}
\setcounter{tocdepth}{6}
\fi

\ifnum 10=1%%texcaption%%
\captionsetup{labelformat=empty}
\fi


$if(title)$
\title{$title$$if(thanks)$\thanks{$thanks$}$endif$}
$endif$
$if(subtitle)$
\providecommand{\subtitle}[1]{}
\subtitle{$subtitle$}
$endif$
$if(author)$
\author{$for(author)$$author$$sep$ \and $endfor$}
$endif$
$if(institute)$
\providecommand{\institute}[1]{}
\institute{$for(institute)$$institute$$sep$ \and $endfor$}
$endif$

\date{$date$}

\usepackage{framed}
\usepackage[many]{tcolorbox}
\tcbuselibrary{breakable}
\usepackage{color}

\newcommand{\externalimagelink}[1]{{ \color{blue}{ (IMG: #1) } }}

\usepackage{adjustbox}

\definecolor{darkgreen}{RGB}{0,147,0}
\definecolor{tim}{RGB}{0,202,202}
\definecolor{bggray}{RGB}{223,223,223}
\definecolor{bglightgray}{RGB}{195,195,195}
```

#- {area_end="beforemacros"}

#- {area="macros" collapse="true"}
macros

``` {.latex printing_template=""}
\newcommand{\hiddenprint}[1]{\iffalse {#1} \fi}

\newenvironment{absolutelynopagebreak}
  {\par\nobreak\vfil\penalty0\vfilneg
   \vtop\bgroup}
  {\par\xdef\tpd{\the\prevdepth}\egroup
   \prevdepth=\tpd}


\newcommand{\verbx}{0.6em}
\newcommand{\pluginHeader}[1]{{\large \textbf{#1}}}
\newcommand{\plgfooter}[1]{{\begin{center}{#1}\end{center}}}
\newcommand{\videoRunDiv}[1]{{{#1}}}
\newcommand{\smallVideoRunDiv}[1]{{{#1}}}
\newcommand{\listVideoRunDiv}[1]{{{#1}}}
\newcommand{\stem}[1]{{{#1}}}
\newcommand{\hFour}[1]{{#1}}
\newcommand{\kaava}[1]{\begin{adjustwidth}{2em}{}{#1}\end{adjustwidth}}
\newcommand{\red}[1]{{\leavevmode\color{red}{#1}}}
\newcommand{\tim}[1]{{\leavevmode\color{tim}{#1}}}
\newcommand{\blue}[1]{{\leavevmode\color{blue}{#1}}}
\newcommand{\green}[1]{{\leavevmode\color{darkgreen}{#1}}}
\newcommand{\yellow}[1]{{\leavevmode\color{yellow}{#1}}}
\newcommand{\white}[1]{{\leavevmode\color{white}{#1}}}
\newcommand{\gray}[1]{{\leavevmode\color{darkgray}{#1}}}
\newcommand{\lightgray}[1]{{\leavevmode\color{lightgray}{#1}}}
\newcommand{\bggray}[1]{\colorbox{bggray}{#1}}
\newcommand{\bgred}[1]{\colorbox{red}{#1}}
\newcommand{\bgblue}[1]{\colorbox{blue}{#1}}
\newcommand{\bggreen}[1]{\colorbox{darkgreen}{#1}}
\newcommand{\bgyellow}[1]{\colorbox{yellow}{#1}}
\newcommand{\bglightgray}[1]{\colorbox{bglightgray}{#1}}
\newcommand{\bgtim}[1]{\colorbox{tim}{#1}}
\newcommand{\bgwhite}[1]{\colorbox{white}{#1}}
\newcommand{\border}[1]{\fbox{#1}}
\newcommand{\obs}[1]{\fcolorbox{black}{bggray}{#1}}
\newcommand{\radius}[1]{\marginbox{0 5}{#1}}
\newcommand{\timpluginerror}[2]{{ \begin{tcolorbox}[colframe=red!75!white, coltext=red!75!black, title=#1]{#2}\end{tcolorbox} }}
\newcommand{\smalltext}[1]{{\tiny{#1}}}
\newcommand{\linkit}[1]{#1}
\newcommand{\runo}[1]{\begin{center}\it{#1}\end{center}}
\newcommand{\kuvavasen}[1]{#1}
\newcommand{\timButton}[1]{\bgtim{\white{{#1}}}}
\newcommand{\troutofdate}[1]{#1}
\newcommand{\checktr}[1]{#1}

\newenvironment{huomautus}{\begin{tcolorbox}}{\end{tcolorbox}}

\newcommand{\smallhref}[2]{\hfill{\tiny\href{#1}{#2}}}
% \newcommand{\haskell}[1]{#1}
\def\haskell{} % verbatim does not work inside command easily

% ----- UI components begin --------------
\def\clap#1{\hbox to 0pt{\hss#1\hss}}

\newcommand{\cmark}{\text{\ding{51}}}
\newcommand{\xmark}{\text{\ding{55}}}

\makeatletter
\newcommand*{\checkbox}{\@ifstar{\checkedcheckbox}{\uncheckedcheckbox}}
\newcommand*{\radiobutton}{\@ifstar{\checkedradiobutton}{\uncheckedradiobutton}}
\makeatother

\newcommand{\button}[1]{\tcbox[on line, bottom=0pt, top=0pt, left=1pt, right=1pt] {\textbf{#1}}}
\newcommand{\smallimage}[1]{#1}

% ------------ Begin overlapped items like radio and checkbox ------------
\def \bw {20pt}% default width for overlapped items

\newcommand{\basicoverlappeditem}[1]{{\makebox[\bw][s]{\hfill{#1}\hfill}}}

\newcommand{\overlapitems}[2]{{\makebox[\bw][s]{\hfill{#1}\hfill}%
\hspace{-\bw}}%
{\makebox[\bw][s]{\hfill{#2}\hfill}}%
}

\newcommand{\uncheckedradiobutton}{\basicoverlappeditem{\(\bigcirc\)}}

\newcommand{\checkedradiobutton}{\overlapitems{\uncheckedradiobutton}{\(\bullet\)}}

\newcommand{\uncheckedcheckbox}{\basicoverlappeditem{\raisebox{-1.0pt}{\(\square\)}}}

\newcommand{\checkedcheckbox}{\overlapitems{\uncheckedcheckbox}{\hspace{2pt}\raisebox{0.5pt}\cmark}}

\newcommand{\corcircle}{\basicoverlappeditem{{\LARGE\(\bigcirc\)}}}

\newcommand{\correct}[1]{\overlapitems{#1}{\raisebox{-1.8pt}\corcircle}}

\newcommand{\lbpoints}[2]% poist to left bottom corner of itme #2
{\overlapitems{#2}{{\raisebox{-9.5pt}{\tiny\hspace{-12pt}#1\hspace{0pt}}}}%
}

% ------------ End overlapped items like radio and checkbox ------------

% ----- UI components end --------------


% ----- mcq BEGIN
\newcommand{\mcq}[4]{
%\begin{absolutelynopagebreak}
\begin{center}
\large \textbf{{#1}}
\end{center}
\renewcommand{\arraystretch}{1.5}
#2
\begin{center}
\begin{tabular}{#3}
#4
\end{tabular}
\vspace{1em}
\end{center}
%\end{absolutelynopagebreak}
}
% ----- mcq END


% ----- task BEGIN
\newcommand{\task}[4]{ % header,stem,content,footer
\ifx\hfuzz#1\hfuzz
\begin{tcolorbox}[
  boxrule=0.2pt, 
  colback=white]
{#2}
{#3}
\begin{center}{#4}\end{center}
\end{tcolorbox}
\else
\begin{tcolorbox}[
  toptitle=3mm,
  bottomtitle=2mm,
  boxrule=0.2pt, 
  colback=white, 
  colbacktitle=black!10,
  coltitle=black,
  title={\large \textbf{#1}}]
{#2}
{#3}
\begin{center}{#4}\end{center}
\end{tcolorbox}
\fi
}
% ----- task END

% ----- qst BEGIN
\newcommand{\qst}[6]% header,stem,questionText,tcolums,table,footer
{\vspace{10pt}
%\begin{absolutelynopagebreak}
\task{#1}{#2}{
\renewcommand{\arraystretch}{1.5}
\textbf{#3}
\begin{adjustwidth}{1.0cm}{}
\begin{tabular}{#4}
#5
\end{tabular}
\end{adjustwidth}
}{#6}
\vspace{1em}
%\end{absolutelynopagebreak}
}

\newcommand{\qsty}[6]% header,stem,questionText,tcolums,table,footer
{
\vspace{10pt}
%\begin{absolutelynopagebreak}
\task{#1}{#2}{
\renewcommand{\arraystretch}{1.5}
\textbf{#3}\vspace{5pt}

\begin{adjustwidth}{1.0cm}{}
\begin{tabulary}{\linewidth}{#4}
#5
\end{tabulary}
\end{adjustwidth}
}
{\vspace{5pt}#6}
\vspace{1em}
%\end{absolutelynopagebreak}
}
% ----- qst END


\newcommand{\console}[1]{
\begin{tcolorbox}[enhanced,colback=black!90!white, colupper=white, 
fontupper=\selectfont\bfseries\ttfamily,
borderline={0.3mm}{0.3mm}{white},
arc=0mm]
#1
\end{tcolorbox}
}

\newenvironment{consoleenv}{ %
\begin{tcolorbox}[
  enhanced,colback=black!90!white, colupper=white, 
  fontupper=\selectfont\bfseries\ttfamily,
  borderline={0.3mm}{0.3mm}{white},
  arc=0mm]
%\Verbatim[fontsize=\small, frame=leftline, rulecolor=\color{lightgray}, xleftmargin=2mm]
}
{
%\endVerbatim
\end{tcolorbox}%
}

\newcommand{\textbox}[2]{
\fbox{\begin{minipage}[c][][t]{#1}{#2} \end{minipage}}
}

\newcommand{\ttextbox}[2]{
\vspace{2pt}
\textbox{#1\linewidth}{
#2\par
\hfill

}
}

\usepackage{fancyvrb} 

\renewenvironment{verbatim}
{\Verbatim[fontsize=\small, frame=leftline, rulecolor=\color{lightgray}, xleftmargin=2mm]}
{\endVerbatim}


%---------------------- taskenv begin ---------------------------------
\newcommand{\taskenvArg}{} % dummy macro for env end params
\newenvironment{taskenv}[3] % header,stem,footer
{\renewcommand{\taskenvArg}{#3}% 
\ifx\hfuzz#1\hfuzz%
\begin{tcolorbox}[
  breakable,
  boxrule=0.2pt, 
  colback=white]%
\else
\begin{tcolorbox}[
  breakable,
  toptitle=3mm,
  bottomtitle=2mm,
  boxrule=0.2pt, 
  colback=white, 
  colbacktitle=black!10,
  coltitle=black,
  title={\large \textbf{#1}}]
\fi
\ifx\hfuzz#2\hfuzz{}\else{#2\vspace{10pt}}\fi
}
{\ifdefempty{\taskenvArg}{}{\begin{center}{\taskenvArg}\end{center}}%
\end{tcolorbox}%
}
%---------------------- taskenv end ---------------------------------

\definecolor{codegreen}{rgb}{0,0.6,0}
\definecolor{codegray}{rgb}{0.5,0.5,0.5}
\definecolor{codelightgray}{rgb}{0.65,0.65,0.65}
\definecolor{codepurple}{rgb}{0.58,0,0.82}
\definecolor{codeblue}{rgb}{0.0,0,0.9}
\definecolor{backcolour}{rgb}{0.95,0.95,0.92}

\lstdefinestyle{mystyle}{
    %backgroundcolor=\color{backcolour},   
    commentstyle=\color{codelightgray},
    keywordstyle=\color{codeblue},
    numberstyle=\tiny\color{codegray},
    stringstyle=\color{codepurple},
    basicstyle=\footnotesize,
    breakatwhitespace=false,         
    breaklines=true,                 
    captionpos=b,                    
    keepspaces=true,                 
    numbers=left,                    
    numbersep=5pt,                  
    showspaces=false,                
    showstringspaces=false,
    showtabs=false,                  
    tabsize=2
}
 

\lstset{
  style=mystyle,
  basicstyle=\linespread{0.9}\small\ttfamily,
  prebreak=\raisebox{0ex}[0ex][0ex]{\ensuremath{\hookleftarrow}},
  numberstyle=\tiny, 
  stepnumber=1, 
  numbersep=5pt, 
  xleftmargin=0em, 
  breaklines=true,
  columns=fixed,
  fontadjust=true,
  basewidth=0.5em,
  frame=,
  rulecolor=\color{blue!80!black},
  belowskip=0pt,
  aboveskip=0pt,
  inputencoding=utf8,
  extendedchars=true,
  literate=
    {á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
    {Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
    {à}{{\`a}}1 {è}{{\`e}}1 {ì}{{\`i}}1 {ò}{{\`o}}1 {ù}{{\`u}}1
    {À}{{\`A}}1 {È}{{\'E}}1 {Ì}{{\`I}}1 {Ò}{{\`O}}1 {Ù}{{\`U}}1
    {ä}{{\"a}}1 {ë}{{\"e}}1 {ï}{{\"i}}1 {ö}{{\"o}}1 {ü}{{\"u}}1
    {Ä}{{\"A}}1 {Ë}{{\"E}}1 {Ï}{{\"I}}1 {Ö}{{\"O}}1 {Ü}{{\"U}}1
    {â}{{\^a}}1 {ê}{{\^e}}1 {î}{{\^i}}1 {ô}{{\^o}}1 {û}{{\^u}}1
    {Â}{{\^A}}1 {Ê}{{\^E}}1 {Î}{{\^I}}1 {Ô}{{\^O}}1 {Û}{{\^U}}1
    {œ}{{\oe}}1 {Œ}{{\OE}}1 {æ}{{\ae}}1 {Æ}{{\AE}}1 {ß}{{\ss}}1
    {ç}{{\c c}}1 {Ç}{{\c C}}1 {ø}{{\o}}1 {å}{{\r a}}1 {Å}{{\r A}}1
    {€}{{\euro}}1 {£}{{\pounds}}1
    {½}{{\ensuremath{\frac12}}}1
    {₳}{A}1
}

```

#- {area_end="macros"}

#- {area="usermacros" collapse="true"}
user macros

``` {.latex printing_template=""}

%%texmacros%%

```

#- {area_end="usermacros"}

#- {area="aftermacros" collapse="true"}
aftermacros

``` {.latex printing_template=""}


\pagestyle{plain}

  
\usepackage{tabularx}
\usepackage{amsfonts}

\usepackage{listings}

\lstdefinestyle{verbo}{
    basicstyle=\ttfamily,
    breaklines=true,
    columns=fullflexible,
    breakatwhitespace=false,
    tabsize=1,
    resetmargins=true,
    xleftmargin=0pt,
    showstringspaces=false,
}

\lstnewenvironment{code}[1][]
 {\framed \lstset{style=verbo,#1}}
 {\endframed}
```

``` {.latex printing_template=""}


\ifnum 10<1%%texfancyfooter%% 
\usepackage{fancyhdr} 
\fancypagestyle{myfancyfooter}{
\ifnum 12>1%%texfancyfooter%% 
\fancyhead{}
\fancyfoot{}
\fancyfoot[LE,RO]{\thepage}
\fancyfoot[RE]{\textit{ %%textitle%%} }
\fancyfoot[LO]{\textit{\nouppercase{\rightmark}}}
  \ifnum 10=1%%texforcesonlysectionnumber%%
   \fancyfoot[LO]{\textit{\nouppercase{\leftmark}}} % give chapter text to left
  \fi
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{ %%texfootrulewidth%%}
%\renewcommand{\sectionmark}[1]{\markright{\thesection.\ ##1}}
\else
%%texfancyfooterstyle%%
\fi
}
\fi

\ifnum 11=1%%texforcesonlysectionnumber%%
\renewcommand{\thesection}{\arabic{section}}
\fi

%%beforedocument%%

\begin{document}

```

#- {area_end="aftermacros"}

#- {area="coverpage" collapse="true"}
coverpage

``` {.latex printing_template=""}
\ifnum 11=1%%texcoverpage%%
%-------------------------------------------------------------------------------
%-------------------------------COVERPAGE_BEGIN---------------------------------
%-------------------------------------------------------------------------------
\begin{titlepage}
	\centering
	\vspace*{5cm}
	{\scshape \Huge %%textitle%%\par}
	\vspace{2cm}
	{\Large\itshape %%texauthorname%%\par}
	\vfill
	%%texversion%% \par
	%%texdate%% \par
	\vfill
	{\large %%texorganization%%\par}
\end{titlepage}
%-------------------------------------------------------------------------------
%------------------------------COVERPAGE_END------------------------------------
%-------------------------------------------------------------------------------
\fi
```

#- {area_end="coverpage"}

#- {area="aftercoverpage" collapse="true"}
aftercoverpage

``` {.latex printing_template=""}

\let\cleardoublepage\clearpage

$if(abstract)$
\begin{abstract}
$abstract$
\end{abstract}
$endif$

$for(include-before)$
$include-before$

$endfor$
```

#- {area_end="aftercoverpage"}

#- {area="tableOfContents" collapse="true"}
tableOfContents

``` {.latex printing_template=""}

\ifnum 11=1%%textableofcontents%%
%-------------------------------------------------------------------------------
%---------------------------TABLE_OF_CONTENTS_BEGIN-----------------------------
%-------------------------------------------------------------------------------
\pagenumbering{roman}
{
\setcounter{tocdepth}{3}
\tableofcontents
\newpage
\let\cleardoublepage\clearpage
}
%-------------------------------------------------------------------------------
%---------------------------TABLE_OF_CONTENTS_END-------------------------------
%-------------------------------------------------------------------------------
\fi
 
```

#- {area_end="tableOfContents"}

#- {area="bodyAndAfter"}

#- {area="body" collapse="true"}
body

``` {.latex printing_template=""}

$if(lot)$
\listoftables
$endif$
$if(lof)$
\listoffigures
$endif$

\ifnum 10<1%%texfancyfooter%%
\pagestyle{myfancyfooter}
\makeatletter
\let\ps@plain\ps@fancy
\makeatother
\fi
\pagenumbering{arabic}


$body$
```

#- {area_end="body"}

#- {area="enddocument" collapse="true"}
enddocument

``` {.latex printing_template=""}

$if(natbib)$
$if(bibliography)$
$if(biblio-title)$
$if(book-class)$
\renewcommand\bibname{$biblio-title$}
$else$
\renewcommand\refname{$biblio-title$}
$endif$
$endif$
\bibliography{$for(bibliography)$$bibliography$$sep$,$endfor$}

$endif$
$endif$
$if(biblatex)$
\printbibliography$if(biblio-title)$[title=$biblio-title$]$endif$

$endif$

{% if texbibname != "" %}
\printbibliography
{% endif %}


$for(include-after)$
$include-after$

$endfor$

\end{document}
```

#- {area_end="enddocument"}

#- {area_end="bodyAndAfter"}

#- {area_end="fulltemplate"}
