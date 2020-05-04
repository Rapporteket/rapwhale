\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{kvalreg-rapport}[2019/08/26 Rapportar for kvalitetsregister]

% Krev bruk av LuaLaTeX
\RequirePackage{ifluatex}
\ifluatex
  \else
  \ClassError{kvalreg-rapport}{%
  Dokument ikkje kompilert med lualatex.\MessageBreak%
  Vurder Sys.setenv('RSTUDIO_PDFLATEX'='lualatex')}{%
  Dokumentet må kompilerast med kommandoen lualatex,\MessageBreak%
  ikkje pdflatex eller xelatex.\MessageBreak%
  Bruk Sys.setenv('RSTUDIO_PDFLATEX'='lualatex')\MessageBreak%
  i RStudio for å setta dette som standard.}
\fi

% Språkstøtte:
% fixme: Språkstøtta bør forbetrast. Men det er litt vrient
%        å få både språkstøtte i klassen og alle pakkar til
%        å fungera skikkeleg utan å lista opp alle språka
%        me støttar på denne måten. Undersøk beste løysinga
%        (sjå polyglossia- og babel-pakkane for litt info).
\newif\ifkvalreg@bokmal         \kvalreg@bokmaltrue     % Bokmål er standard
\newif\ifkvalreg@nynorsk        \kvalreg@nynorskfalse   % Nynorsk er ikkje standard
\newif\ifkvalreg@british        \kvalreg@britishfalse   % Britisk engelsk er ikkje standard

% Aktiver språkstøtta:
\DeclareOption{nynorsk}{\kvalreg@nynorsktrue\kvalreg@bokmalfalse}
\DeclareOption{bokmal}{\kvalreg@nynorskfalse\kvalreg@bokmaltrue}
\DeclareOption{british}{\kvalreg@britishtrue\kvalreg@bokmalfalse}
\ProcessOptions\relax

% Støtte for utkast (raskare kompilering + UTKAST-vassmerke)
\newif\iffinal \finalfalse
\newif\ifdraft \drafttrue
\DeclareOption{final}{\finaltrue\draftfalse}
\DeclareOption{draft}{\finalfalse\drafttrue}

% Me baserer oss på memoir-klassen
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{memoir}}
   \ifkvalreg@nynorsk
     \PassOptionsToClass{a4paper,11pt,twoside,openany,nynorsk}{memoir}
   \else\ifkvalreg@bokmal
     \PassOptionsToClass{a4paper,11pt,twoside,openany,norsk}{memoir}
   \else\ifkvalreg@british
     \PassOptionsToClass{a4paper,11pt,twoside,openany,british}{memoir}
   \fi\fi\fi
   \ProcessOptions\relax
\LoadClass{memoir}

% Brukar LuaLaTeX, så for språkstøtte må me lasta
% polyglossia-pakken, ikkje babel
\RequirePackage{polyglossia}
\ifkvalreg@nynorsk
  \setdefaultlanguage{nynorsk}
\else\ifkvalreg@bokmal
  \setdefaultlanguage{norsk}
\else\ifkvalreg@british
  \setdefaultlanguage{british}
\fi\fi\fi
\setotherlanguages{english}
\AtBeginDocument{%
  \lefthyphenmin=3
  \righthyphenmin=3
} % Minimum tre bokstavar til venstre eller høgre for bindestrek ved orddeling


% Støtte for fargar
\RequirePackage{xcolor}

% Fargar frå profilmanualen her:
% https://www.regjeringen.no/no/tema/helse-og-omsorg/sykehus/styringsdokumenter1/nasjonalt-profilprogram-for-helseforetak/id650208/
% https://www.regjeringen.no/contentassets/575f7de1edeb42fca6a952d3ca5f3260/4804-designmanual2011-web.pdf
\definecolor{MorkBlaCol}{cmyk}{1,.69,0,.11} % PMS 287 (mørk blå)
\definecolor{LysBlaCol}{cmyk}{.44,.19,0,0}  % PMS 287 (lys blå)

% Tilleggsfargar, som er halvoffisielle, henta frå
% https://unn.no/om-oss/profilhandboka#fargepalett
\definecolor{LysBrunCol}{cmyk}{.18,.35,.47,.05}      % PMS 4665 (lys raudbrun)
\definecolor{MiddelsBrunCol}{cmyk}{.30,.54,.57,.26}  % PMS 4715 (middels raudbrun)
\definecolor{MorkBrunCol}{cmyk}{.36,.80,.75,.53}     % PMS 483 (mørk raudbrun)
\definecolor{SkarpRaudCol}{cmyk}{0,1,.81,.04}        % PMS 1795 (skarp raud)
\definecolor{BleikGulCol}{cmyk}{.16,.14,.71,.01}     % PMS 459 (bleik gul)
\definecolor{OransjeCol}{cmyk}{0,.39,1,0}            % PMS 130 (oransje / skarp gul)
\definecolor{LysSjoGronCol}{cmyk}{.57,.24,.44,.06}   % PMS 624 (lys sjøgrøn)
\definecolor{SkarpGronCol}{cmyk}{.73,.13,1,.01}      % PMS 362 (skarp grøn (skogsgrøn?))
\definecolor{LysSkifergraCol}{cmyk}{.44,.28,.25,.06} % PMS 7543 (lys skifergrå)
\definecolor{MorkSjoGronCol}{cmyk}{.95,.32,.79,.26}     % PMS 342 (mørk sjøgrøn)

% Bruk ulike type fargar på ulike typar dokument
\newcommand{\doktypeverdi}{arsrapport}%
\newcommand{\doktype}[1]{%
  \renewcommand{\doktypeverdi}{#1}%
}
% Standardfargar
\RequirePackage{etoolbox} % For samanlikningar/if-setningar + patching
\AtBeginDocument{
\expandafter\ifstrequal\expandafter{\doktypeverdi}{arsrapport}{
  \colorlet{PrimCol}{MorkBlaCol}
  \colorlet{AltCol}{LysBlaCol}
}{}
\expandafter\ifstrequal\expandafter{\doktypeverdi}{kvalitetsrapport}{
  \colorlet{PrimCol}{MorkBrunCol}
  \colorlet{AltCol}{LysBrunCol}
}{}
\expandafter\ifstrequal\expandafter{\doktypeverdi}{dokumentasjon}{
  \colorlet{PrimCol}{MorkSjoGronCol}
  \colorlet{AltCol}{LysSjoGronCol}
}{}
}

% Mattestøtte
\RequirePackage[all]{onlyamsmath} % Ikkje godta Plain TeX-kommandoar
\RequirePackage{amsmath}          % Utvida mattestøtte
\RequirePackage{mathtools}        % Rett nokre mattegreier
\mathtoolsset{mathic}             % Ditto
\RequirePackage[warnings-off={mathtools-colon,mathtools-overbracket}]{unicode-math}     % Unicode-matte for LuaLaTeX

% Godta komma som desimalskiljeteikn i matte
\RequirePackage{ncccomma}

% Viser akronyom med kapitélar
% (Brukar \texorpdfstring for å hindra problem med bruk av kommandoen i PDF-innhaldslista)
\RequirePackage{hyphenat}
\newcommand{\kort}[1]{\texorpdfstring{\nohyphens{\addfontfeatures{Letters=UppercaseSmallCaps}#1}}{#1}}

% Vis programkode, pakkenamn o.l. med fastbreiddskrift, fin farge,
% og ikkje del teksten over fleire linjer
\newcommand{\kode}[1]{\nohyphens{\texttt{\textcolor{PrimCol}{#1}}}} % \codefontsize?

% Støtte for e-postadresser
\newcommand{\epost}[1]{\href{mailto:#1}{\nolinkurl{#1}}}

% Bruk Cambria, Calibri og Consolas som standardskrift:
%   Brødtekst og matte: Cambria
%   Overskrifter: Calibri
%   Programkode: Consolas
% I brødtekst og vanlege overskrifter brukar me *proporsjonale* renessansetal
\setmainfont[Ligatures=TeX, Numbers={Proportional,Lowercase}]{Cambria} % Problem med Fractions=On
\setsansfont[Ligatures=TeX, Numbers={Proportional,Lowercase}]{Calibri}
\setmonofont[Scale=MatchLowercase]{Consolas}
\setmathfont{Cambria Math}

% Skriftstil med ikkje-proporsjonale tal, for brødtekst,
% for eksempel for bruk til sidetal i innhaldslista
\newfontfamily\mainupropskrift[Ligatures=TeX,Numbers={Monospaced,Lowercase}]{Cambria}

% Bruk proporsjonale tabelltal for overskrifter, då det ser best ut
% (sjå for eksempel botnen på 1-tal)
\newfontfamily\altskrift[Ligatures=TeX,Numbers={Proportional,Uppercase}]{Calibri}
\newfontfamily\lettskrift[Ligatures=TeX,Numbers={Proportional,Uppercase}]{Calibri Light Italic}

% Skrift for teikn som ikkje finst i standardskriftene
\newfontfamily\uniskrift[Scale=MatchLowercase]{DejaVu Sans}

% Definer formatering for overskrifter på ulike nivå
\chapterstyle{section}
% Fargelagde overskrifter med fin skrift.
% Det logiske ville vore \color, ikkje \textcolor her, men det kan
% (i sjeldne tilfelle) føra til sideskift mellom to overskrifter,
% av grunnen forklart her: https://tex.stackexchange.com/a/101643
% Ved å bruka \textcolor i staden for, omgår me problemet
% (merk at \overskrift då alltid må vera sistargument).
\newcommand{\overskrift}{\altskrift\textcolor{PrimCol}}
\newcommand{\overskriftColor}{\altskrift\color{PrimCol}} % Alternativ variant som må brukast for kapitteloverskrifter
\renewcommand*{\partnamefont}{\Huge\bfseries\overskrift}
\renewcommand*{\partnumfont}{\Huge\bfseries\overskrift}
\renewcommand*{\parttitlefont}{\Huge\mdseries\overskrift}
\renewcommand*{\chapterheadstart}{\noindent\hspace*{\margjuster}}
\renewcommand*{\chapnumfont}{\huge\bfseries\raggedright\overskriftColor}
\renewcommand{\printchapternum}{\chapnumfont \thechapter\space\space}
\renewcommand*{\chaptitlefont}{\huge\bfseries\overskrift}
\setsecheadstyle{\Large\bfseries\raggedright\overskrift}
\setsubsecheadstyle{\large\bfseries\raggedright\overskrift}
\setsubsubsecheadstyle{\large\bfseries\raggedright\overskrift}
\setlength{\aftersubsecskip}{.02ex plus 0.01ex minus -0.1ex}
\setlength{\aftersubsubsecskip}{.02ex plus 0.01ex minus -0.1ex}

% Som \hangnum, men med fast avstand mellom tal og tekst i
% overskriftene. Dette ser mykje finare ut når ein \section
% vert direkte følgt av ein \subsection (ein dårleg idé,
% men noko som førekjem). (\hangnum brukar ein \quad, som
% avheng av skriftstorleiken, og derfor vert forskjellig for
% dei ulike overskriftsnivåa).
\setsecnumformat{\llap{\csname the#1\endcsname\hspace{.25cm}}} 


% Nummerer òg \subsection-overskrifter og ta dei med i innhaldslista.
% Merk at skrifta for \subsection og \subsubsection er lik / like stor,
% så det einaste som skil dei to nivåa er nummereringa (\subsubsection
% har ikkje nummerering, mens \subsection har det).
\setsecnumdepth{subsection}
\settocdepth{subsection}

% Begynn alle rapportdelar på høgreside (sjølv om me brukar 'openany')
\renewcommand{\@setuppart}{
	\cleartorecto
	\thispagestyle{part}%
	\@tempswafalse
	\beforepartskip
}

% For kapittel i innhaldslista
\renewcommand{\cftpartfont}{\bfseries\overskrift} 
\renewcommand{\cftchapterfont}{\bfseries\overskrift}

% Bruk ikkjeproporsjonale tal i innhaldslista
% (elles vert for eksempel 11 med 12 under
% sjåande veldig rart ut)
\renewcommand*{\cftpartpagefont}{\mainupropskrift\bfseries}
\renewcommand*{\cftchapterpagefont}{\mainupropskrift\bfseries}
\renewcommand*{\cftsectionpagefont}{\mainupropskrift}
\renewcommand*{\cftsubsectionpagefont}{\mainupropskrift}

% Ikkje blokkjuster tekst i innhaldslista
\setrmarg{3.55em plus 1fil}

% På norsk brukar me strekpunkt, ikkje kulepunkt, i lister
\renewcommand{\labelitemi}{\normalfont–}
\renewcommand{\labelitemii}{\normalfont–}

% Ikkje blokkjuster punktlister
% (\RaggedyRight[1.5cm] er litt freistande, men sjå
% http://tex.stackexchange.com/questions/134303/does-combining-microtype-with-ragged-right-make-any-sense )
\AtBeginEnvironment{itemize}{\raggedright}

% Støtt kompakte lister, og ikkje bruk punktum etter
% talet i nummererte lister (det skal ikkje vera det på norsk)
\RequirePackage[pointlessenum]{paralist}
\tightlists

% Støtte for fancy kryssreferansar.
% Rekkjefølgja for pakkelisting må visstnok vera varioref, hyperref, cleveref
\ifkvalreg@nynorsk
  \RequirePackage[nynorsk]{varioref}
\else\ifkvalreg@bokmal
  \RequirePackage[norsk]{varioref}
\else\ifkvalreg@british
  \RequirePackage[english]{varioref}
\fi\fi\fi

% Støtte for elementære utrekningar
\RequirePackage{calc}

% Støtte for å skriva ut verdiar til lengder
\usepackage{printlen}
\uselengthunit{cm}


% Margar
\setlrmarginsandblock{8cm}{2cm}{*} % Venstremarg, høgremarg
\setulmarginsandblock{1.7cm}{2.8cm}{*} % Toppmarg, botnmarg
\setlength{\topskip}{1.6\topskip} % For \sloppybottom, jf. memoir-manualen
%\setmarginnotes{1.5em}{5.5cm}{11pt} % Avstand til sidestolpe, breidd av sidesolpe, min. vertikal avstand
\checkandfixthelayout[nearest]
\sloppybottom % For å unngå «horungar» øvst og nedst på sidene
% Me brukar «twoside», for tosidig utskrift, men me vil ha alle sidene på same format likevel,
% altså med *ikkje-reflekterte* margar på par- og oddetalssider.
\newlength\margjuster
\newlength\smalmarg
\newlength\breimarg
\newlength\breitekst
\setlength{\smalmarg}{\evensidemargin}             % Breidd på høgremarg minus 1in
\setlength{\breimarg}{\oddsidemargin}              % Breidd på venstremarg minus 1in
\setlength{\margjuster}{-\breimarg+\smalmarg}      % Det som må justerast på brei venstremarg for å få smal venstremarg
\setlength{\breitekst}{\stockwidth-\smalmarg-2in+2\marginparsep} % Tekstbreidda viss me har smal marg på begge sider
\newcommand{\brukbreimarg}{% Bruk brei marg som standard
  \setlength{\evensidemargin}{\breimarg}%
  \setlength{\evensidemargin}{\breimarg}%
}
\brukbreimarg

%\setlength{\parindent}{0pt}
%\nonzeroparskip % Problem med punktlister
%\raggedyright[1.2cm]

% Litt større linjeavstand
\setSingleSpace{1.05}
\SingleSpacing

% Rett ymse ting.
% \RequirePackage{fixltx2e} % Ikkje lenger nødvendig i TeXLive 2015 og seinare

% Lag minst mulig PDF-fil (berre mogleg med LuaLaTex)
\iffinal
  \pdfvariable compresslevel=9
  \pdfvariable minorversion=5
  \pdfvariable objcompresslevel=3
\fi

% Støtte for kommando brukt i LaTeX-malen til SKDE
\newcommand{\guide}[1] {
	\textit{\textcolor{darkgray}{[#1]}}
	}

% For rett orddeling av ord som buss\_sjåfør
% (anten bussjåfør eller buss-sjåfør)
\newcommand{\dbind}[1]{\discretionary{-}{#1}{}}


% Bruk *ikkje-proporsjonale* renessansetal i alle tabellar
\newfontfamily\tablefont[Numbers={Monospaced,Lowercase}]{Calibri}
\AtBeginEnvironment{tabular}{\tablefont}%

% La tabelloverskrifter ha farga tekst, med mindre linjeavstand
% og ikkje blokkjustert. Og la linje to og nedover ha innrykk.
% Vert ikkje brukt, sidan me lastar caption-pakken etterpå,
% men lar det stå i tilfelle me vil gå vekk frå caption/subfig
% på eit seinare tidspunkt.
\captionnamefont{\sffamily\color{PrimCol}\scshape}%
\captiontitlefont{\linespread{1}\sffamily\color{PrimCol}}%
\captionstyle{\raggedright}
\hangcaption

% Støtte for delfigurar slik knitr lagar dei, då knitr
% dessverre ikkje støttar memoir-metoden (over).
%
% Når me lastar caption-pakken, gjev memoir-pakken 
% ei åtvaring (men seier i grunnen at alt vil gå bra).
% Å sjå denne åvaringa i loggen og andre plassar
% er irriterande, so me gøymer ho.
\RequirePackage{silence}
\WarningFilter*{memoir}{You are using the caption package with the memoir class}
\usepackage[format=hang,justification=raggedright,%
            font={sf,color=PrimCol},labelfont=sc]{caption}
\usepackage[font={sf,color=PrimCol},labelfont=sf]{subfig}
\setFloatSpacing{1}
% Avsnittet «10.13 The class versus the caption package (and its friends)»
% i memoir-manualen
\setlength{\abovecaptionskip}{0.5\onelineskip}
\setlength{\belowcaptionskip}{0.5\onelineskip}

% Fiks sentrering av tabellar (slik latex()-kommandoen
% i R-pakken Hmisc har definert \centering fungerer det 
% dessverre ikkje)
\BeforeBeginEnvironment{tabular}{\hfill}
\AfterEndEnvironment{tabular}{\hfill\null}

% Støtte for tabellar som går over fleire sider
\usepackage{longtable}

% Mykje betre avsnittsformatering, ved å strekka bitte litt på bokstavane
% (Før tok dette veldig lang tid, og vart berre brukt \iffinal,
% men i TeX Live 2016 tar det knapt lengre tid, og vert brukt som standard.)
\RequirePackage[stretch=25,final]{microtype}


% Plassering av sidetal
\makeevenfoot{plain}{\hspace*{\margjuster}\thepage}{}{}
\makeoddfoot{plain}{}{}{\thepage}

% I utkastmodus, vis tid for kompilering i toppmargen
\ifdraft
  \RequirePackage{clock}
  \makeevenhead{plain}{\hspace*{\margjuster}\today~-- \texttime}{}{}
  \makeoddhead{plain}{}{}{\today~-- \texttime}
\fi

% I utkastmodus, vis strek viss ting kjem utanfor margen
\ifdraft
  \setlength\overfullrule{5pt}
\fi

\RequirePackage{layout}
\title{Norsk kvalitetsregister for leppe-kjeve-ganespalte}
\author{Karl Ove Hufthammer}

% Bruk tomme sider for alle Part-sider
\pagestyle{plain}
\aliaspagestyle{part}{empty}

% For absolutt posisjonering, for forsida
\RequirePackage[absolute,overlay]{textpos}

% For teikningar, diagram og liknande
\RequirePackage{tikz}

% Definer nokre metadatakommandoar for rapporten.
% Vert blant anna brukt på forsida.
% Namn på registeret
\DeclareRobustCommand{\@registernamn}{Namn på kvalitetsregisteret}%
\DeclareRobustCommand{\registernamn}[1]{%
  \DeclareRobustCommand{\@registernamn}{#1}%
}
% Året rapporten er frå
\DeclareRobustCommand{\@rapportaar}{\the\year}%
\DeclareRobustCommand{\rapportaar}[1]{%
  \DeclareRobustCommand{\@rapportaar}{#1}%
}
% Tittel og undertittel, for forsida
\DeclareRobustCommand{\@tittel}{Årsrapport for \@rapportaar}%
\DeclareRobustCommand{\tittel}[1]{%
  \DeclareRobustCommand{\@tittel}{#1}%
}
\DeclareRobustCommand{\@undertittel}{med plan for forbedringstiltak}%
\DeclareRobustCommand{\undertittel}[1]{%
  \DeclareRobustCommand{\@undertittel}{#1}%
}
% Logo for forsida
\DeclareRobustCommand{\logo}[1]{%
  \newcommand{\@logo}{#1}%
}
% Forfattarliste, til bruk både i sjølve rapporten
% og som PDF-metadata
\DeclareRobustCommand{\forfattarar}[1]{%
  \forcsvlist{\listadd\@forfattarar}{#1}%
}
% I sjølve teksten, skriv kvar forfattar på ny linje
\newcommand{\forfattarliste}{%
    \begingroup
	\medskip\raggedleft\itshape%
	\def\prosesser{\def\prosesser{\\}}%
    \def\do##1{\prosesser##1}%
	\dolistloop{\@forfattarar}%
	\par\endgroup%
}

% Kommando for å laga forsida, innhaldslista osv.
\newcommand{\lagforside}{%

% Anna sidenummerering for alt som kjem før innhaldsbolken i dokumentet
% Vert òg vist i enkelte PDF-lesarar
\pagenumbering{Alph}

% Start forsida
\begin{titlingpage}
\begingroup
\null % For å unngå at tekst på neste side kjem med her.

% Logo oppe til høgre
\ifdefined\@logo
  \begin{textblock}{14}[0,0](1,.5)%
  \noindent\hfill\includegraphics[height=5em]{\@logo}
  \end{textblock}
\fi

% Farga boks nede på forsida
\begin{textblock}{14}[0,0](1,10)%
\noindent\begin{tikzpicture}
\node[fill=AltCol,outer sep=0pt,inner sep=0pt] (bcbox)%
{
\begin{minipage}{\textwidth}{%
\hfill\vspace*{0.33\paperheight}%
}
\end{minipage}
};
\end{tikzpicture}
\end{textblock}

% Tittel og undertittel i farga boks
\begin{textblock}{13}[0,0](1.7,10.5)
\sffamily
\lettskrift\fontsize{50}{53}\selectfont
\raggedright\textcolor{white}{\@tittel\\\@undertittel}
\end{textblock}

% Namn på register oppe til høgre
\begin{textblock}{11}[1,1](14,7.5) % Tittel
\fontsize{40}{43}\selectfont
\raggedleft
\@registernamn
\end{textblock}

% Dagens dato rett etter registernamnet
\begin{textblock}{10}[1,0](14,7.8) % Dato
\raggedleft\Large\itshape%
\today
\end{textblock}
\endgroup
\end{titlingpage}

\frontmatter
\cleartorecto % Hopp til ny høgreside

% Vis innhadsliste
{\hypersetup{hidelinks}
\tableofcontents*
}

% Hovudbolk (med annan sidenummerering, og anna
\mainmatter
} % Ferdig med definisjon av \lagforside


% Skriv alle forfattarane som ei liste skild med semikolon
% (der siste namn ikkje skal ha semikolon etter seg).
% Grusom men velfungerande kode, basert på
% http://tex.stackexchange.com/questions/33832/special-handling-of-first-and-or-last-item-in-an-etoolbox-list/33836#33836
\newcommand{\xcommaorandlist}[2]{%
  \count255=0
  \def\do##1{\advance\count255 1 \chardef\finalitem=\count255 }%
  \dolistloop{#1}%
  \count255=0
  \toks0={}% 
  \def\do##1{\advance\count255 1
    \edef\next{%
      \ifnum\count255=1
      \else
        ;\space
      \fi
    }
    \toks2={##1}%

    \edef\next{\the\toks0 \next \the\toks2}%
    \toks0=\expandafter{\next}%
  }%
  \dolistloop{#1}%
  \edef#2{\the\toks0 }%
}

% Lenkjestøtte
\PassOptionsToPackage{hyphens,obeyspaces}{url}
\RequirePackage[linktoc=all,%
                    linkcolor=PrimCol,%
                    citecolor=PrimCol,%
                    urlcolor=PrimCol,%
                    filecolor=PrimCol,%
                    colorlinks=true,%
					unicode=true,
					linktoc=all,
					pdfpagemode=UseOutlines, bookmarksopen=true,
					pdfpagelayout=OneColumn, pdfview=FitH, pdfstartview=Fit,
          pdfescapeform=true,bookmarksnumbered=true]{hyperref}
\AtBeginDocument{%
	\xcommaorandlist\@forfattarar\@forfattarsemikolonliste
	\hypersetup{%
		pdftitle={\@registernamn{}: \@tittel{} \@undertittel},
		pdfauthor={\@forfattarsemikolonliste}
	}
}

% Fargelegg alle lenkjer/nettadresser
\let\oldurl\url
\renewrobustcmd{\url}[1]{\textcolor{PrimCol}{\oldurl{#1}}}

% cleverref finst ikkje på nynorsk enno, så bruk bokmål mellombels (fixme)
\ifkvalreg@british
  \RequirePackage[nameinlink,noabbrev,capitalise,english]{cleveref}
\else
  \RequirePackage[nameinlink,norsk]{cleveref}
\fi

% Kommando for å skriva prosentteiknet, med rett avstand før
\RequirePackage{xspace}
\newcommand{\prosent}{\:\%\xspace}

% Formater tal på fin måte, med tusenskiljeteikn, rett minusteikn og sånt
\RequirePackage{siunitx}
\sisetup{detect-all, group-four-digits=true, round-mode=places, output-decimal-marker = {,}}

% Kommandoar for å skriva avkryssingsteikn og avkryssingsboksar
\newcommand{\sjekk}{{\uniskrift ✓}}
\newcommand{\sjekknei}{{\uniskrift ☐}}
\newcommand{\sjekkja}{{\uniskrift ☑}}
% For lettare å fylla ut dei to kolonnane i stadieinndelingstabellen
\newcommand{\sjekkXO}{\sjekkja & \sjekknei}
\newcommand{\sjekkOX}{\sjekknei & \sjekkja}

% Støtte for QR-kodar (hovudsakleg for adressa til 
% heimesida til register, vist på baksida av rapportar)
\RequirePackage{qrcode}

% La R-kjeldekode (og utdata frå køyring av denne) dekka heile sidebreidda
\iffalse
\AtBeginEnvironment{knitrout}{%
\begin{adjustwidth}{\margjuster}{0pt}%
  \setlength\columnwidth{\breitekst}%
  \setlength\linewidth{\breitekst}%
  \setlength\hsize{\breitekst}%
}
\AtEndEnvironment{knitrout}{\end{adjustwidth}}
\fi

% Ein figur som dekker heile breidda
\newenvironment{widefigure}[1][htbp]{
  \begin{figure}[#1]
  \begin{adjustwidth}{\margjuster}{0pt}
  \centerfloat
  \setlength\hsize{\breitekst}%
  }{%
  \end{adjustwidth}
  \end{figure}
}

% Ein tabell som dekker heile breidda
\newenvironment{widetable}[1][htbp]{
  \begin{table}[#1]
  \begin{adjustwidth}{\margjuster}{0pt}
  \centerfloat
  \setlength\hsize{\breitekst}%
  }{%
  \end{adjustwidth}
  \end{table}
}

% Legg ev. til UTKAST-vassmerke
\ifdraft
  \usepackage{draftwatermark}
  \ifkvalreg@british
    \SetWatermarkText{DRAFT}
  \else
    \SetWatermarkText{UTKAST}
  \fi
  \SetWatermarkLightness{.95}
\fi


% Godta (til nød) dårleg avsnittsformatering. Merk at dette ikkje
% *fører* til dårleg avsnittsformatering; formateringa er framleis optimal.
% Men ein slepp å få ein stygg strek i margen (bokstavleg talt)
% viss ei avsnittsformatering er for dårleg.
\sloppy

% Støtte for autogenerert eksempeltekst (nyttig ved testing)
\usepackage{lipsum}

% Gjev veldig tydeleg melding om at ein har gjort noko
% frykteleg gale dersom ein brukar å bruka \underline-kommandoen
% For det første skal aldri understreking brukast i skikkeleg
% typografi (jf. https://practicaltypography.com/underlining.html)
% og for det andre er \underline-kommandoen feil måte
% å få understreking i LaTeX.
%
% Understreking skal endrast til kursivering (\emph) eller
% eller til vanlege overskrifter (\section, \paragraph e.l.).
\renewcommand{\underline}[1]{%
  \textcolor{red}{\textls{BRUK AV FORBUDT \textbackslash underline-kommando\footnote{\url{https://practicaltypography.com/underlining.html}}:}} #1%
  \ClassWarning{kvalreg-rapport}{Bruk av forbudt underline-kommando}
}

% Rett feilmelding frå tikz (fixme: finn ut korfor ein får denne)
% http://tex.stackexchange.com/questions/165929/semiverbatim-with-tikz-in-beamer
\global\let\tikz@ensure@dollar@catcode=\relax

% Feil i TeX Live 2018 (LaTeX eller memoir-klassen) gjer at punktum
% i figurnummer forsvinn (for eksempel vert «figur 3.5» til «figur 35»).
% Sjå forklaring på https://tex.stackexchange.com/q/427451
% Dette rettar feilen. fixme: Fjern dette når me har fått TeX Live 2019.
\makeatletter
\renewcommand{\counterwithin}{\@ifstar{\@csinstar}{\@csin}}
\makeatother

% Lagra tekst (typisk for samandragskapittelet) i eiga fil for
% bruk ein annan plass i dokumentet (gjerne *før* definisjonen).
% Er nyttig når samandragskapittelet brukar utrekningar som
% vert gjort seinare i dokumentet. Merk at ein må kompilera
% dokumentet minst to gongar etter å ha gjort endringar.
%
% Inspirert av løysinga her, men med nokre endringar:
% http://tex.stackexchange.com/a/150840/8540
\newcommand{\introtekst}[1]{%
   \newwrite\introfil
   \immediate\openout\introfil=introtekst.tex
   \immediate\write\introfil{\unexpanded{#1}}
   \immediate\closeout\introfil
}