# `LaTeX`

$\LaTeX$ allows for the typesetting of academic articles in the realm of math and science.

## Comments

```tex
% single-line comment
% there are no multi-line comments 
```

## Quickstart

```tex
% ---------- QUICKSTART -----------
    % LaTeX files end with the file extension .tex
    % \ => backslash prefixes every LaTex command 

% ---------- FORMAT ------------
    % every LaTeX document comprises the following composite elements...
        % 1. PREAMBLE
        % 2. DOCUMENT BODY

% ----- 1. PREAMBLE -----
  % the preamble contains all document metadata that is relevant for compilation and typesetting of out LaTeX file

% --- DOCUMENT DETAILS ---
  % specify document type for later compilation
  % specify fontsize for the document

% \documentclass[fontSize]{documentType} 
    % fontSize => any integer ending with pt
    % documentType => other classes can be found here https://ctan.org/topic/class
        % article
        % report
        % book
        % slides
        % memoir
        % letter
        % beamer
        % proc
        % minimal

\documentclass[12pt]{article}

% --- PACKAGES ---
  % specifies any LaTeX packages the document requires for compilation
  % other LaTeX packages can be found here https://ctan.org/

% \usepackage{packageName} 

\usepackage{caption}
\usepackage{float}
\usepackage{hyperref}

% --- AUTHOR, DATE, TITLE ---
  % specify the author(s)
  % specify the date
    % \today => returns the current date
  % specify the document title

% \author{authorNames}
% \date{date}
% \title{documentTitle}

\author{Chaitanya Krishna Ande, Colton Kohnke}
\date{\today}
\title{Learn \LaTeX{} in Y Minutes!}

% ----- 2. DOCUMENT BODY -----
  % any combination of LaTeX commands can be called here

% --- GENERAL COMMANDS ---
  % empty lines between text create seperations between paragraphs
  % \begin{documentComponent} => signposts the start of the given document component
  % \end{documentComponent} => singposts the end of the given document component
    % document => REQUIRED for every LaTeX document at the beginning and end of the document body
    % abstract => creates an abstract environment (must be called before the main section of the document body)
    % enumerate => creates an enumeration environment within which \item list items can be called
      % \item => indicates a single list item within an enumeration environment
    % thebibliography => creates a bibliography environment for references within which \bibitem list items can be called
      % \bibitem => indicates a single bibliography list item that can be cited directly within the document body
    % equation => creates an equation environment within which mathematical equations can be defined with LaTeX commands
    % figure => creates a figure environment within which a figure and its placement can be specified
  % \section{sectionTitle} => creates a numbered secton with the given title
  % \subsection{subsectionTitle} => creates a numbered subsection with the given title
  % \makeTitle => creates a title page from details within the preamble
  % \tableofcontents => creates a table of contents page
  % \newpage => creates a new page, equivalent to \n in programming languages
  % * => adding the asterisk after any LaTeX command supresses the inbuilt numbering system to create an unnumber area
  % \label{componentType:componentTitle} => creates a label that allows for later referencing of a given component within the document body

\begin{document}
\maketitle % create a title page

\newpage
\tableofcontents % create a table of contents
\newpage

\begin{abstract} % create an abstract
 \LaTeX{} documentation written as \LaTeX! How novel and totally not my idea!
\end{abstract}

\section{Introduction} % a section
Hello, my name is Colton and together we're going to explore \LaTeX!

\subsection{This is a subsection} % a subsection
Steven Universe looking ass.

\section*{This is an unnumbered section} % an unnumbered section
Yeah not all sections have to be numbered!

\section{Lists}
Lists are one of the easiest things to create in \LaTeX! I need to go shopping
tomorrow, so let's make a grocery list.
\begin{enumerate} % creates a list environment
  \item salad
  \item 27 watermelon
  \item a single jackrabbit
\end{enumerate} 

% ----- 2.5 MATHEMATICS -----
  % one of the primary functions of LaTeX is creating stylised mathematical equations in the compiled document, below are some examples for different fields of mathematics
  % more detailed math commands can be found here https://tug.ctan.org/info/undergradmath/undergradmath.pdf

% --- MATHEMATICS COMMANDS ---
  % greek letters => other greek letters can be found here https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
    % \xi
    % \beta
    % \gamma
    % \sigma
    % \theta
  % $ => prefixes and suffixes any math section in LaTeX
  % \[ and \] => an alternative that also prefixes and suffixes any math section in LaTeX
  % \frac{numerator}{denominator} => creates a fraction with the given numerator and denominator
  % sum_{} => draws the summation symbol
  % int_{} => draws the integral symbol

% MATHEMATICAL FUNCTIONS
($\sin$, $\cos$, $\tan$) % trigonometric functions
($\log$, $\exp$) % logarithms and exponentials
($\lim$) % limits

% SET THEORY
$\forall x \in X$ % all of X belong to X

% EQUATION
$a^2 + b^2 = c^2$ % pythogarous theorom
\[a^2 + b^2 = c^2 \] % pythogarous theorom, using the alternative syntax for signposting the start and end of a math section 

% --- FIGURES ---
  % inkscape (https://inkscape.org/) is often used to draw diagrams that can be later embedded in the LaTeX document
  % for more details on captioning figures, see here https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions 
  % \includegraphics{imageFilePath} => draws a specified figure at the given file path, can be further customised with augmenters within the []
  % \caption{captionTitle} => draws a specified caption within the figure

\begin{figure}[H] % H here denotes the placement of the figure
    \includegraphics[width=0.8\linewidth]{right-triangle.png}
    \caption{Right triangle with sides $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\begin{thebibliography}{1} % bibliography section
  \bibitem{latexwiki} The amazing \LaTeX{} wikibook: \emph{https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} An actual tutorial: \emph{http://www.latex-tutorial.com}
\end{thebibliography}

\end{document}
```

## Tooling

* [pdflatex](https://gist.github.com/rain1024/98dd5e2c6c8c28f9ea9d)
* [miktex](https://miktex.org/)
* [tex live](https://tug.org/texlive/)

## More on

* [LaTeX math cheatsheet for undergraduates](https://tug.ctan.org/info/undergradmath/undergradmath.pdf)
* [LaTeX for beginners](https://youtu.be/p5Yr0dkrKG8?si=rJmg6cck1Op1-ALm)
* [LaTeX for students](https://youtu.be/lgiCpA4zzGU?si=RB3nN03LF328nROK)
* [Inkscape diagrams within LaTeX documents](https://inkscape.org/learn/tutorials/latex/)
* [LaTeX table generator](https://www.tablesgenerator.com/)
* [LaTeX equation editor](https://latex.codecogs.com/eqneditor/editor.php)
* [learn LaTeX in y minutes](https://learnxinyminutes.com/docs/latex/)
* [LaTeX project](https://www.latex-project.org/)
* [learn LaTeX.org](https://www.learnlatex.org/)
* [comprehensive LaTeX wiki](https://en.wikibooks.org/wiki/LaTeX)
* [mathematics in college with LaTeX and Vim](https://castel.dev/post/lecture-notes-1/)
