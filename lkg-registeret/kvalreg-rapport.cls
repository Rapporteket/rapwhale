\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{kvalreg-rapport}[2014/05/15 Rapportar for kvalitetsregister]

% Me baserer oss på memoir-klassen
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{memoir}}
   \PassOptionsToClass{a4paper,11pt,twoside,openany,norsk}{memoir}
   \ProcessOptions\relax
\LoadClass{memoir}

\RequirePackage{polyglossia}
\setdefaultlanguage{norsk}
\AtBeginDocument{%
  \lefthyphenmin=3
  \righthyphenmin=3
} % Minimum tre bokstavar til venstre eller høgre for bindestrek ved orddeling


\RequirePackage[cmyk]{xcolor}

%\addtokomafont{disposition}{\overskrift}

% Fargar frå profilmanualen her:
% http://www.regjeringen.no/pages/16744078/4804-Designmanual2011-WEB.pdf
\definecolor{MorkBlaCol}{cmyk}{1,.69,0,.11} % PMS 287 (mørk blå)
\definecolor{LysBlaCol}{cmyk}{.44,.19,0,0}  % PMS 287 (lys blå)

% Tilleggsfargar, som er halvoffisielle, henta frå
% http://www.unn.no/profilhandbok/fargepalett.html
\definecolor{LysBrunCol}{cmyk}{.18,.35,.47,.05}      % PMS 4665 (lys raudbrun)
\definecolor{MiddelsBrunCol}{cmyk}{.30,.54,.57,.26}  % PMS 4715 (middels raudbrun)
\definecolor{MorkBrunCol}{cmyk}{.36,.80,.75,.53}     % PMS 483 (mørk raudbrun)
\definecolor{SkarpRaudCol}{cmyk}{0,1,.81,.04}        % PMS 1795 (skarp raud)
\definecolor{BleikGulCol}{cmyk}{.16,.14,.71,.01}     % PMS 459 (bleik gul)
\definecolor{OransjeCol}{cmyk}{0,.39,1,0}            % PMS 130 (oransje / skarp gul)
\definecolor{LysSjoGronCol}{cmyk}{.57,.24,.44,.06}   % PMS 624 (lys sjøgrøn)
\definecolor{SkarpGronCol}{cmyk}{.73,.13,1,.01}      % PMS 362 (skarp grøn (skogsgrøn?))
\definecolor{LysSkifergraCol}{cmyk}{.44,.28,.25,.06} % PMS 7543 (lys skifergrå)
\definecolor{MorkSjoGron}{cmyk}{.95,.32,.79,.26}     % PMS 342 (mørk sjøgrøn)

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
  \colorlet{PrimCol}{MorkSjoGron}
  \colorlet{AltCol}{LysSjoGronCol}
}{}
}

% Mattestøtte
\RequirePackage[all]{onlyamsmath} % Ikkje godta Plain TeX-kommandoar
\RequirePackage{amsmath}          % Utvida mattestøtte
\RequirePackage{mathtools}        % Rett nokre mattegreier
\mathtoolsset{mathic}             % Ditto
\RequirePackage{unicode-math}     % Unicode-matte for LuaLaTeX

\RequirePackage{hyphenat}
\newcommand{\kort}[1]{\nohyphens{\addfontfeatures{Letters=UppercaseSmallCaps}#1}}


\setmainfont[Ligatures=TeX, Numbers={Proportional,Lowercase}]{Cambria} % Problem med Fractions=On
\setsansfont[Ligatures=TeX, Numbers={Proportional,Lowercase}]{Calibri}
\setmonofont[Scale=MatchLowercase]{Consolas}
\setmathfont{Cambria Math}

% Bruk ikkje-proporsjonale tal, for eksempel for bruk til sidetal i innhaldslista
\newfontfamily\mainupropskrift[Ligatures=TeX,Numbers={Monospaced,Lowercase}]{Cambria}

% Bruk proporsjonale tabelltal for overskrifter, då det ser best ut
% (sjå for eksempel botnen på 1-tal)
\newfontfamily\altskrift[Ligatures=TeX,Numbers={Proportional,Uppercase}]{Calibri}
\newfontfamily\lettskrift[Ligatures=TeX,Numbers={Proportional,Uppercase}]{Calibri Light}

% Skrift for teikn som ikkje finst i standardskriftene
\newfontfamily\uniskrift[Scale=MatchLowercase]{Arial Unicode MS}

% For overskrifter
\chapterstyle{section}
\newcommand{\overskrift}{\color{PrimCol}\altskrift}
\renewcommand*{\partnamefont}{\overskrift\Huge\bfseries}
\renewcommand*{\partnumfont}{\overskrift\Huge\bfseries}
\renewcommand*{\parttitlefont}{\overskrift\Huge\mdseries}
\renewcommand*{\chapterheadstart}{\noindent\hspace*{\margjuster}}
\renewcommand*{\chapnumfont}{\overskrift\huge\bfseries\raggedright}
\renewcommand{\printchapternum}{\chapnumfont \thechapter\space\space}
\renewcommand*{\chaptitlefont}{\overskrift\huge\bfseries}
\setsecheadstyle{\overskrift\Large\bfseries\raggedright}
\setsubsecheadstyle{\overskrift\large\bfseries\raggedright}
\setsubsubsecheadstyle{\overskrift\large\bfseries\raggedright}
\hangsecnum
\setlength{\aftersubsecskip}{.02ex plus 0.01ex minus -0.1ex}

% Begynn alle rapportdelar på høgreside (sjølv om me brukar 'openany')
\renewcommand{\@setuppart}{
	\cleartorecto
	\thispagestyle{part}%
	\@tempswafalse
	\beforepartskip
}

% Rekkjefølgja må visstnok vera varioref, hyperref, cleveref
\RequirePackage[norsk]{varioref}

% Støtte for e-postadresser
\newcommand{\epost}[1]{\href{mailto:#1}{\nolinkurl{#1}}}

% For kapittel i innhaldslista
\renewcommand{\cftpartfont}{\overskrift\bfseries} 
\renewcommand{\cftchapterfont}{\overskrift\bfseries}

% Bruk ikkjeproporsjonale tal i innhaldslista
% (elles vert for eksempel 11 med 12 under
% sjåande veldig rart ut)
\renewcommand*{\cftpartpagefont}{\mainupropskrift\bfseries}
\renewcommand*{\cftchapterpagefont}{\mainupropskrift\bfseries}
\renewcommand*{\cftsectionpagefont}{\mainupropskrift}

% Ikkje vis namnet på innhaldslista på forsida
% (ville elles blitt formatert som andre kapitteloverskrifter)
%\renewcommand{\printtoctitle}[1]{\strut}

% Ikkje blokkjuster tekst i innhaldslista
\setrmarg{3.55em plus 1fil}

% På norsk brukar me strekpunkt, ikkje kulepunkt, i lister
\renewcommand{\labelitemi}{\normalfont–}
\renewcommand{\labelitemii}{\normalfont–}

% Støtt kompakte lister, og ikkje bruk punktum etter
% talet i nummererte lister (det skal ikkje vera det på norsk)
\RequirePackage[pointlessenum]{paralist}
\tightlists


% Godta komma desimalskiljeteikn i matte
\RequirePackage{ncccomma}

\RequirePackage{calc}


% Margar
\setlrmarginsandblock{8.5cm}{2cm}{*} % Venstremarg, høgremarg
\setulmarginsandblock{1.8cm}{2.8cm}{*} % Toppmarg, botnmarg
%\setmarginnotes{1.5em}{5.5cm}{11pt} % Avstand til sidestolpe, breidd av sidesolpe, min. vertikal avstand
\checkandfixthelayout[nearest]
% Me brukar «twoside», for tosidig utskrift, men me vil ha alle sidene
% på same format likevel, altså med ikkje reflekterte margar på par-
% og oddetalssider.
\newlength\margjuster
\newlength\smalmarg
\newlength\breimarg
\newlength\breitekst
\setlength{\smalmarg}{\evensidemargin}             % Breidd på høgremarg minus 1in
\setlength{\breimarg}{\oddsidemargin}              % Breidd på venstremarg minus 1in
\setlength{\margjuster}{-\breimarg+\smalmarg}      % Det som må justerast på brei venstremarg for å få smal venstremarg
\setlength{\breitekst}{\stockwidth-2\smalmarg-2in} % Tekstbreidda viss me har smal marg på begge sider
\newcommand{\brukbreimarg}{% Bruk brei marg som standard
  \setlength{\evensidemargin}{\breimarg}%
  \setlength{\evensidemargin}{\breimarg}%
}
\brukbreimarg

\usepackage{printlen}
\uselengthunit{cm}

%\setlength{\parindent}{0pt}
%\nonzeroparskip % Problem med punktlister
%\raggedyright[1.2cm]

% Litt større linjeavstand
\setSingleSpace{1.05}
\SingleSpacing

% Rett ymse ting.
\RequirePackage{fixltx2e}

% Lag minst mulig PDF-fil
%\pdfcompresslevel=9
%\pdfminorversion=5
%\pdfobjcompresslevel=3


\newcommand{\guide}[1] {
	\textit{\textcolor{darkgray}{[#1]}}
	}

% For rett orddeling av ord som buss\_sjåfør
\newcommand{\dbind}[1]{\discretionary{-}{#1}{}}


\newfontfamily\tablefont[Numbers={Monospaced,Lowercase}]{Calibri}
\captionnamefont{\sffamily\color{PrimCol}\scshape}%
\captiontitlefont{\linespread{1}\sffamily\color{PrimCol}}%
\captionstyle{\raggedright}
\AtBeginEnvironment{tabular}{\tablefont}%
\hangcaption

% Fiks sentrering av tabellar (slik latex() har definert
% \centering fungerer det dessverre ikkje
\BeforeBeginEnvironment{tabular}{\hfill}
\AfterEndEnvironment{tabular}{\hfill\null}

% Ikkje blokkjuster punktlister
% (\RaggedyRight[1.5cm] er litt freistande, men sjå
% http://tex.stackexchange.com/questions/134303/does-combining-microtype-with-ragged-right-make-any-sense )
\AtBeginEnvironment{itemize}{\raggedright}

% Mykje betre avsnittsformatering, ved å strekka bitte litt på bokstavane
\RequirePackage[stretch=30,final]{microtype}

% Plassering av sidetal
\makeevenfoot{plain}{\hspace*{\margjuster}\thepage}{}{}
\makeoddfoot{plain}{}{}{\thepage}

\RequirePackage{layout}
\title{Norsk kvalitetsregister for leppe-kjeve-ganespalte}
\author{Karl Ove Hufthammer}

\pagestyle{plain}
\aliaspagestyle{part}{empty}

% For absolute positioning (for making the title page).
\RequirePackage[absolute,overlay]{textpos}

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

\newcommand{\lagforside}{%

\pagenumbering{Alph}

\begin{titlingpage}
\begingroup
\null % For å unngå at tekst på neste side kjem med her.

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

\begin{textblock}{13}[0,0](1.7,10.5)
\sffamily
\lettskrift\fontsize{50}{53}\selectfont
\raggedright\textcolor{white}{\@tittel\\\@undertittel}
\end{textblock}

\begin{textblock}{11}[1,1](14,7.5) % Tittel
\fontsize{40}{43}\selectfont
\raggedleft
\@registernamn
\end{textblock}

\begin{textblock}{10}[1,0](14,7.8) % Dato
\raggedleft\Large\itshape%
\today
\end{textblock}
\endgroup
\end{titlingpage}

\frontmatter
\cleartorecto

{\hypersetup{hidelinks}
\tableofcontents*
}

\mainmatter

}

% Skriv alle forfattarane som ei liste skild med semikolon
% (der siste namn ikkje skal ha semikolon etter seg).
% Løysing basert frå http://tex.stackexchange.com/questions/33832/special-handling-of-first-and-or-last-item-in-an-etoolbox-list/33836#33836
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
\PassOptionsToPackage{hyphens}{url}
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
    pdfescapeform=true]{hyperref}
\AtBeginDocument{%
	\xcommaorandlist\@forfattarar\@forfattarsemikolonliste
	\hypersetup{%
		pdftitle={\@tittel{} \@undertittel},
		pdfauthor={\@forfattarsemikolonliste}
	}
}

\RequirePackage[nameinlink]{cleveref}


% Kommando for å skriva prosentteiknet, med rett avstand før
\RequirePackage{xspace}
\newcommand{\prosent}{\:\%\xspace}


\newcommand{\sjekk}{{\uniskrift ✓}}
\newcommand{\sjekknei}{{\uniskrift\Large ☐}}
\newcommand{\sjekkja}{{\uniskrift\Large ☑}}

\let\oldurl\url
\renewrobustcmd{\url}[1]{\textcolor{PrimCol}{\oldurl{#1}}}

% La kjeldekode (og utdata frå denne) dekka heile sidebreidda
\iffalse
\AtBeginEnvironment{knitrout}{%
\begin{adjustwidth}{\margjuster}{0pt}%
  \setlength\columnwidth{\breitekst}%
  \setlength\linewidth{\breitekst}%
  \setlength\hsize{\breitekst}%
}
\AtEndEnvironment{knitrout}{\end{adjustwidth}}
\fi

\newenvironment{widefigure}[1][htbp]{
  \begin{figure}[#1]
  \begin{adjustwidth}{\margjuster}{0pt}
  \centerfloat
  }{%
  \end{adjustwidth}
  \end{figure}
}

\usepackage{longtable}


\sloppy

\usepackage{lipsum}