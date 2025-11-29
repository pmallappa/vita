%% Start of file `awesome-cv.cls'.
% Awesome CV Class File
%
% This class has been downloaded from:
% https://github.com/posquit0/Awesome-CV
%
% Author:
% Claud D. Park <posquit0.bj@gmail.com>
% http://www.posquit0.com
%
% Notes:
% 1) This class file defines the structure and layout of the template file (cv.tex, resume.tex).
% 2) It has been written in such a way that under most circumstances you
% should not need to edit it.
%
% Class license:
% LPPL v1.3c (http://www.latex-project.org/lppl)
%
% Modularized version 2.0 (2025/11/28):
% - Split into separate .sty files for better maintainability
% - cv-core.sty: Core packages and utilities
% - cv-colors.sty: Color definitions
% - cv-layout.sty: Page geometry and layout
% - cv-styles.sty: Font and style definitions
% - cv-commands.sty: Standard CV commands
% - cv-projects.sty: Project-specific commands
% - cv-header-footer.sty: Header and footer commands


%-------------------------------------------------------------------------------
%                Identification
%-------------------------------------------------------------------------------
\ProvidesClass{awesome-cv}[2025/11/28 v2.0 Awesome Curriculum Vitae Class]
\NeedsTeXFormat{LaTeX2e}


%-------------------------------------------------------------------------------
%                Class options
%
% (need to be done before the external package loading, for example because
% we need \paperwidth, \paperheight and \@ptsize to be defined before loading
% geometry and fancyhdr)
%-------------------------------------------------------------------------------
% Options for draft or final
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}
% Inherit options of article
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}
}
\ProcessOptions\relax
\LoadClass{article}


%-------------------------------------------------------------------------------
%                Load modular components
%-------------------------------------------------------------------------------
% Core packages and utilities
\RequirePackage{cv-core}

% Color definitions
\RequirePackage{cv-colors}

% Page layout and geometry
\RequirePackage{cv-layout}

% Font and style definitions
\RequirePackage{cv-styles}

% Standard CV commands
\RequirePackage{cv-commands}

% Project-specific commands
\RequirePackage{cv-projects}

% Header and footer commands
\RequirePackage{cv-header-footer}

\endinput
%% End of file `awesome-cv.cls'.
