# Codebook: `procoast_wave2_religion_cpeb.tsv`

Tab-separated, UTF-8, one row per respondent, 821 rows, 13 variables. The data come
from Wave 2 of the PRO-COAST stakeholder questionnaire. Rows are in random order
within case study, and there is no respondent identifier.

**Missing values.** A blank cell means no response or "Prefer not to say". The file
does not distinguish the two; both are treated as missing in the analysis.

**Values are as collected**, on the original response scales. All recoding and
standardising is done in `src/data_prep.R`.

| Variable | Used for | Question or meaning | Values |
|---|---|---|---|
| `case` | Random factor; descriptives | Case study | See below |
| `A26` | CPEB composite | "I participate in protests against current environmental conditions." | 1 = Disagree to 5 = Agree |
| `A27` | CPEB composite | "I participate in events organised by environmental groups." | 1 = Disagree to 5 = Agree |
| `A28` | CPEB composite | "I vote for a government proposing environmentally conscious policies." | 1 = Disagree to 5 = Agree |
| `F7` | Religiosity | "How religious would you consider yourself?" | 1 = Not at all religious to 5 = Very religious |
| `F16` | Anti-diversity (reverse-coded in analysis) | "The different ways of life and increasing diversity in [OWN COUNTRY] enrich us." | 1 = Disagree to 5 = Agree |
| `F9` | The reported correlation with F16 only | "Discrimination and disadvantage against women must be eliminated." | 1 = Disagree to 5 = Agree |
| `F17` | Left-right ideology | "In political matters, people talk of "the left" and "the right". How would you place your views on this scale, generally speaking?" | 1 = Left to 7 = Right; `DK` = Don't know (recoded to 4 in the analysis) |
| `F1` | Gender covariate (collapsed to man vs non-man); descriptives | "Gender" | `Man`, `Woman`, `Non-binary`, `Other` |
| `F4` | Education covariate; descriptives | "What is the highest educational level that you have attained?" | 1 to 7, see below |
| `F2` | Descriptives only | Age | Years, see below |
| `F6` | Descriptives only | "Which religious community, if any, do you belong to?" | `None`, `Catholic`, `Protestant`, `Free` (Free Church), `Orthodox`, `Islam`, `Judaism`, `Hinduism`, `Buddhism`, `Other` |
| `stakeholder_category` | Descriptives only | Grouped from each respondent's role in the case study area, whose response options differed between case studies | `Local community`, `Tourists`, `Commercial`, `Government`, `Public sector workers`, `Professionals and protectors`, `Wild resource users` |

## `case`

| Code | Case study | Respondents | Enters models |
|---|---|---|---|
| `ee` | Estonia | 107 | Yes |
| `ie` | Ireland | 47 | Yes |
| `it` | Italy | 57 | Yes |
| `me` | Montenegro | 102 | **No** |
| `mt` | Malta | 91 | Yes |
| `no` | Norway | 79 | Yes |
| `ro` | Romania | 117 | Yes |
| `si` | Slovenia | 126 | Yes |
| `uk` | UK | 95 | Yes |

Coastal areas and recruitment methods are in `recruitment_methods.csv`.

**Montenegro.** The questionnaire there did not include religiosity (`F7`),
denomination (`F6`) or left-right ideology (`F17`), so these respondents enter no
model. They are in the file because they count towards the "Full sample" columns of
the descriptive tables, and because variables are standardised across all
respondents, so they contribute to the means and SDs used for z-scoring.

## `F4` (education)

1 = No formal education; 2 = Primary school; 3 = Secondary school
technical/vocational type; 4 = Secondary school: university-preparatory type;
5 = University-level education, Bachelor or equivalent; 6 = University-level
education, Master or equivalent; 7 = PhD or equivalent.

## `F2` (age)

Most questionnaires collected age in bands, recorded here as band midpoints: 18-24
as 21, 25-34 as 29.5, 35-44 as 39.5, 45-54 as 49.5, 55-64 as 59.5, 65-74 as 69.5,
and 75+ as 80. Where age was given as a number (Estonia and Ireland), that number is
used. Two irregular entries were also interpreted: "40+" as 45, and the data-entry
error "3e" as 35.
