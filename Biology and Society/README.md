# Biology and Society portfolio — how to use

1. Upload this zip to Overleaf (New Project → Upload Project).
2. Menu → Compiler → **XeLaTeX** (or LuaLaTeX). Needed for the Inter font.
3. Fill in your student number at the top of `main.tex`.

## Where things go
| What | File / folder |
|---|---|
| Name, student number, cover image | top of `main.tex` |
| Seminar notes | `seminars/seminar01.tex` … `seminar08.tex` |
| Debate notes + readings | `debates/debate01.tex` … `debate04.tex` |
| Thesis title | `sections/thesis.tex` |
| Company visit / career days | `sections/company-visit.tex`, `sections/career-days.tex` |
| Photos | `images/seminar02/`, `images/debate01/`, … |
| Signed PDFs | `pdfs/` (names below) |

Signed PDFs (a grey placeholder page shows until the file exists):
- `pdfs/signature-sheet.pdf`
- `pdfs/seminar01-attendance.pdf`
- `pdfs/thesis-opening.pdf`
- `pdfs/company-visit-signed.pdf`

## Layout commands (see seminar01.tex for real examples)
```latex
\begin{imagerow}                              % photos in a row
  \rowimg{0.48}{images/x.jpg}{Caption} \hfill
  \rowimg{0.48}{images/y.jpg}{}
\end{imagerow}

\sidebyside{0.45}{Text on the left}{images/z.jpg}   % text next to an image

\begin{infotable}                             % grey-column table
Title & ... \row
Speaker & ...
\end{infotable}

\begin{callout} Reflection text \end{callout} % grey box

\attachpdf{Title in contents}{pdfs/file.pdf}{subsection}  % add a signed PDF

\tocheck{text}                                % yellow highlight = check before submitting
```

Table of contents depth: `\setcounter{tocdepth}{1}` in `preamble.tex`
(set to 2 to also list About / Notes / Reflection).
