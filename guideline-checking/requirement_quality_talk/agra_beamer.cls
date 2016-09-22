%%  %% Copyright (C) 2009 Herbert Voss <hvoss@tug.org>
%%  %% modified      2010 Finn Haedicke <finn@informatik.uni-bremen.de>
%%  %% modified      2011 Melanie Diepenbeck <diepenbeck@informatik.uni-bremen.de>
%%  %% modified      2013 Mathias Soeken <msoeken@informatik.uni-bremen.de>
%%  %%
%%  %% It may be distributed and/or modified under the conditions
%%  %% of the LaTeX Project Public License, either version 1.3
%%  %% of this license or (at your option) any later version.
%%  %%
%%  %% See http://www.latex-project.org/lppl.txt for details.
%%  %%

\ProvidesClass{RAbeamer}[2013/03/11 v0.03 Vorlage Arbeitsgruppe Rechnerarchitektur Uni-Bremen]

\PassOptionsToPackage{bookmarks}{hyperref}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{beamer}}
\ProcessOptions\relax
\LoadClass[covered]{beamer}
\mode<presentation>{\beamer@suppressreplacementstrue}

\RequirePackage[english]{babel}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}

\RequirePackage{graphicx}
\RequirePackage{palatino}
\usefonttheme{serif}

\definecolor{RAblue}{RGB}{4,78,134}
\definecolor{RAgreen}{RGB}{153,204,0}
\definecolor{RAred}{RGB}{128,0,0}
\definecolor{RAorange}{RGB}{255,153,51}
\definecolor{RAgray}{RGB}{229,226,227}

\setbeamercolor*{title}{fg=RAblue}
\setbeamerfont*{title}{series=\bfseries,size=\LARGE}

\setbeamercolor*{footline}{fg=white,bg=RAblue}
\setbeamercolor*{section in toc}{parent=structure}
\setbeamercolor*{structure}{fg=RAblue}

\setbeamercolor*{block title}{use=structure,fg=structure.fg}
\setbeamercolor*{frametitle}{bg=white,fg=RAblue}
\setbeamerfont*{frametitle}{series=\bfseries,size=\Large}

\pgfdeclareimage[height=5mm]{logo}{images/uni-logo.png}

\newdimen\beamer@headheight
\usebeamerfont{frametitle}
\beamer@headheight=7mm
\reset@font

\def\beamer@lefttext{left}

%\DeclareOptionBeamer{hideothersubsections}[]{\beamer@nav@subsectionstyle{show/show/hide}}
%\DeclareOptionBeamer{hideallsubsections}[]{\beamer@nav@subsectionstyle{hide}}

\DeclareOptionBeamer{height}{\beamer@headheight=#1}
\DeclareOptionBeamer{left}{\def\beamer@RAside{left}}
\DeclareOptionBeamer{right}{\def\beamer@RAside{right}}
\ExecuteOptionsBeamer{left}
\ProcessOptionsBeamer

% Helper Functions To Create Slides
\newcommand{\f}[2]{%
 \begin{frame}{#1}
   \vfill
   \begin{center}
     \expandafter\ifx\csname slide#2\endcsname\relax {\color{red}Missing slide: \textbf{#2}}\else\csname slide#2\endcsname\fi
   \end{center}
 \end{frame}
}



% Frame Title Template
\defbeamertemplate*{frametitle}{RA theme}{%
  \nointerlineskip%
  \vskip-\beamer@headheight%
  \vbox to \beamer@headheight{}%
    {\usebeamercolor[fg]{frametitle}%
     \usebeamerfont{frametitle}\insertframetitle\par}%
    {\usebeamercolor[fg]{framesubtitle}\usebeamerfont{framesubtitle}\insertframesubtitle\par}%
}

% Headline Template
\newif\ifshowlogo \showlogotrue
\defbeamertemplate*{headline}{RA theme}{%
  \begin{beamercolorbox}[wd=\paperwidth]{frametitle}
    \vskip1ex
    \hskip1ex\ifshowlogo\pgfuseimage{logo}\else\phantom{\pgfuseimage{logo}}\fi\par
    \vskip1ex
    \begin{beamercolorbox}[wd=0.8\paperwidth]{footline}
      \vskip 2ex
    \end{beamercolorbox}
  \end{beamercolorbox}
}

% Footline Template
\defbeamertemplate*{footline}{RA theme}{%
  \begin{beamercolorbox}[wd=\paperwidth]{footline}
    \parbox{1em}{\vskip 3ex}\hfill\insertframenumber\hspace*{1ex}
  \end{beamercolorbox}
}

\setbeamertemplate{title page}%
{%
  \vskip.7cm
  
    % skip 1.45cm on left
         \begin{beamercolorbox}[center]{title}
        \usebeamerfont{title}\inserttitle\par%
      \end{beamercolorbox}

      \vskip.5cm

      % \begin{beamercolorbox}[sep=8pt,colsep=-4bp,center]{subtitle}
      %   \usebeamerfont{subtitle}\insertsubtitle
      % \end{beamercolorbox}
      % \vskip0.5em
      % %{\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic\par}
      % \vskip1em\par

      \begin{beamercolorbox}[sep=8pt,colsep=-4bp,center]{author}
        \usebeamerfont{author}\insertauthor
      \end{beamercolorbox}

      \begin{beamercolorbox}[sep=8pt,colsep=-4bp,center]{institue}
        \usebeamerfont{institute}\insertinstitute
      \end{beamercolorbox}


      \begin{beamercolorbox}[sep=8pt,colsep=-4bp,center]{date}
        \usebeamerfont{date}\insertdate
      \end{beamercolorbox}
    \vspace{-1cm}
\begin{center}
 
 \includegraphics[width=0.25\linewidth]{images/image17.png}\hspace{0.4cm}
  \includegraphics[width=.2\linewidth]{images/image17.jpg}\hspace{0.8cm}
 \includegraphics[width=0.18\linewidth]{images/image15.png}
\end{center}
}%



\def\maketitle{%

\begin{frame}[plain]
  \titlepage
\end{frame}

}

\mode<all>
\mode<presentation>


%%% Local Variables:
%%% mode: latex
%%% TeX-master: "talk"
%%% End:
