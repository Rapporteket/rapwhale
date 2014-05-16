\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{kvalreg-rapport}[2014/05/15 Rapportar for kvalitetsregister]

% Me baserer oss på memoir-klassen
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{memoir}}
   \PassOptionsToClass{a4paper,11pt,twoside,openany,norsk}{memoir}
   \ProcessOptions\relax
\LoadClass{memoir}

\RequirePackage{polyglossia}
\setdefaultlanguage{norsk}

\RequirePackage[cmyk]{xcolor}

%\addtokomafont{disposition}{\overskrift}

% Fargar frå profilmanualen her:
% http://www.regjeringen.no/pages/16744078/4804-Designmanual2011-WEB.pdf
\definecolor{PrimCol}{cmyk}{1,.69,0,.11} % PMS 287
\definecolor{AltCol}{cmyk}{.44,.19,0,0}  % PMS 287

% Alternative CMYK-fargar på (ser betre ut konvertert til RGB?)
% http://encycolorpedia.com/003896
% http://encycolorpedia.com/7aabde
\definecolor{AltColAlt}{cmyk}{.45,.23,0,.13} % PMS 284
\definecolor{PrimColAlt}{cmyk}{1,.63,0,.41}  % PMS 287


\RequirePackage{unicode-math}

\RequirePackage{hyphenat}
\newcommand{\kort}[1]{\nohyphens{\addfontfeatures{Letters=UppercaseSmallCaps}#1}}


\setmainfont[Ligatures=TeX, Numbers={Proportional,Lowercase}]{Cambria} % Problem med Fractions=On
\setsansfont[Ligatures=TeX, Numbers={Proportional,Lowercase}]{Calibri}
\setmonofont[Scale=MatchLowercase]{Consolas}
\setmathfont{Cambria Math}

% Bruk proporsjonale tabelltal for overskrifter, då det ser best ut
% (sjå for eksempel botnen på 1-tal)
\newfontfamily\altskrift[Ligatures=TeX,Numbers={Proportional,Uppercase}]{Calibri}
\newfontfamily\lettskrift[Ligatures=TeX,Numbers={Proportional,Uppercase}]{Calibri Light}

% For overskrifter
\chapterstyle{section}
\newcommand{\overskrift}{\color{PrimCol}\altskrift}
\renewcommand*{\partnamefont}{\overskrift\Huge\bfseries}
\renewcommand*{\partnumfont}{\overskrift\Huge\bfseries}
\renewcommand*{\parttitlefont}{\overskrift\Huge\mdseries}
\renewcommand*{\chapterheadstart}{\noindent\hspace*{\margjuster}}
\renewcommand*{\chapnumfont}{\overskrift\huge\bfseries}
\renewcommand{\printchapternum}{\chapnumfont \thechapter\space\space}
\renewcommand*{\chaptitlefont}{\overskrift\huge\bfseries}
\setsecheadstyle{\overskrift\Large\bfseries}
\setsubsecheadstyle{\overskrift\large\bfseries}
\setsubsubsecheadstyle{\overskrift\large\bfseries}
\hangsecnum
\setlength{\aftersubsecskip}{.02ex plus 0.01ex minus -0.1ex}

% Lenkjestøtte
\RequirePackage[linkcolor=AltCol,linktoc=all,hidelinks,%
                    citecolor=PrimCol,%
                    urlcolor=PrimCol,%
                    filecolor=PrimCol,%
                    colorlinks=false,%
    pdfescapeform=true]{hyperref}
\hypersetup{pdfencoding=auto, pdfpagemode=UseOutlines, bookmarksopen=true, pdfpagelayout=OneColumn, linktoc=all, pdfview=FitH, pdfstartview=Fit}

% For kapittel i innhaldslista
\renewcommand{\cftpartfont}{\overskrift\bfseries} 
\renewcommand{\cftchapterfont}{\overskrift\bfseries} 

% Ikkje vis namnet på innhaldslista på forsida
% (ville elles blitt formatert som andre kapitteloverskrifter)
%\renewcommand{\printtoctitle}[1]{\strut}


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
\setlength{\margjuster}{-\oddsidemargin+\evensidemargin}
\setlength{\evensidemargin}{\oddsidemargin}

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
\def\_#1{\discretionary{-}{#1}{}}


\newfontfamily\tablefont[Numbers={Monospaced,Lowercase}]{Calibri}
\RequirePackage{etoolbox}
\captionnamefont{\sffamily\color{PrimCol}\scshape}%
\captiontitlefont{\sffamily\color{PrimCol}}%
\AtBeginEnvironment{tabular}{\tablefont}%
\hangcaption

\AtBeginEnvironment{itemize}{\raggedyright[1.5cm]}

% Mykje betre avsnittsformatering, ved å strekka bitte litt på bokstavane
\RequirePackage[stretch=20,final]{microtype}

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

\makeatletter
\newcommand{\makenewtitle}{%
\begin{titlingpage}
\begingroup
\null % For å unngå at tekst på neste side kjem med her.

\begin{textblock}{16}[1,1](15,15.293) % Logo nede til høgre.
logo% \raggedleft\includegraphics{uib-emblem-kvit}
\end{textblock}

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
\raggedright\textcolor{white}{Årsrapport 2014}
\end{textblock}


\begin{textblock}{11}[1,1](14,7.5) % Tittel
\fontsize{40}{43}\selectfont
\raggedleft
%\@title
Norsk kvalitets-\\register for\\leppe-kjeve-ganespalte
\end{textblock}


\begin{textblock}{10}[1,0](14,7.8) % Dato
\raggedleft\Large\itshape%
\today
\end{textblock}
\endgroup
\end{titlingpage}
}
\makeatother

% Kommando for å skriva prosentteiknet, med rett avstand før
\RequirePackage{xspace}
\newcommand{\prosent}{\:\%\xspace}

\sloppy
