% -----------------------------------------------------------------------
% 
% Class file for PI3, WS 2016/17, Universität Bremen.
%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{pi3}[2016/10/27 PI3 document class]

\LoadClass[a4paper,11pt,twoside,onecolumn,final]{article}

\RequirePackage{ngerman}
\RequirePackage[utf8]{inputenc}
\RequirePackage{fancyhdr}
\RequirePackage[a4paper,hmargin={10mm, 20mm},vmargin={15mm}]{geometry}

\RequirePackage[noshortinline,numbers]{haskell-code}

\setlength\parskip{1ex}
\setlength\parindent{0ex}
\setlength\headheight{14pt}

\pagestyle{fancy}
\thispagestyle{empty}

\def\thismodule{\jobname}
\renewcommand\include[1]{%
\def\thismodule{#1}\setcounter{lstnumber}{1}% reset listings counter
\input{#1.lhs}}


\newcommand{\IncludeFun}[3]{\lstinputlisting[basicstyle=\tiny, linerange={#1-#2},firstnumber=#1]{#3}}
\newcommand{\IncludeFile}[1]{\lstinputlisting[firstnumber=1]{#1}}

\renewcommand\thesection{Aufgabe \arabic{section}}
\renewcommand\labelenumi{\alph{enumi})}
\renewcommand\labelenumii{-}

\newcommand\thissemester{SS 2017}
\newcommand\thistitle{Reaktive Programmierung}
\newcommand\thisshorttitle{RP}

\gdef\@tutor{\ClassError{pi3}{Kein Tutor angegeben}
                             {Bitte mit \@backslashchar tutor{} den Namen des
                              Tutors angeben.}}
\gdef\@uebungsgruppe{\ClassError{pi3}{Kein Uebungsgruppe angegeben}
                                     {Bitte mit \@backslashchar uebungsgruppe{}
                                      die Nummer der Uebungsgruppe angeben.}}
\gdef\@teilnehmer{\ClassError{pi3}{Keine Teilnehmer angegeben}
                                  {Bitte mit \@backslashchar teilnehmer{} Namen
                                   der Teilnehmer der Uebungsgruppe angeben.}}

\newcommand{\tutor}[1]{\gdef\@tutor{#1}}
\newcommand{\uebungsgruppe}[1]{\gdef\@uebungsgruppe{#1}}
\newcommand{\teilnehmer}[1]{\gdef\@teilnehmer{#1}}

% Counter for the exercises 
\newcounter{exercisenumber}

% Solutions for exercises
\newcommand{\exercise}[1]{%
 \medskip %
 \stepcounter{exercisenumber} %
 \fbox{\Large\@currentnumber.\arabic{exercisenumber}} %
 {\large \bfseries\itshape{#1}}\par
 }


% Typesets the title:
\renewcommand{\maketitle}[2]%
  {\hspace*{-3ex}
   \begin{tabular}[t]{l}
     \begin{tabular}[t]{@{}l@{\hskip 20mm}l@{}}
       \thistitle & \thissemester 
     \end{tabular} \vspace{5mm} \\
     {\Huge\sf Übungsblatt {#1} } \\[1.5ex]
     {\Large Aufgabenlösung} \\[1ex]
     {\Large Abgabe: {#2}} \\[1ex]
   \end{tabular}
   \hfill{}
   \begin{tabular}[t]{ll}
     \textbf{Tutor}: & \@tutor \\
     \textbf{Gruppe}: & \hspace*{-2ex}
     \begin{tabular}[t]{l}
       \@uebungsgruppe \\
       \@teilnehmer 
     \end{tabular}
   \end{tabular} \\[2ex]
   \rule{\linewidth}{1pt}   
   \def\@currentnumber{#1}
%
   \fancyhead{}
   \fancyhead[LO,RE]{\thisshorttitle, \thissemester}
   \fancyhead[RO,LE]{Tutor: \@tutor, Gruppe: \@uebungsgruppe}
   \fancyfoot{}
   \fancyfoot[RO,LE]{\thepage}
}
