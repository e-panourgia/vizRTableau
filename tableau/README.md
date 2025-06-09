# PISA 2018 Interactive Dashboard – Tableau Prototype

This project visualizes international student performance from the PISA 2018 dataset, with a focus on:
- Greece’s position relative to other countries
- Gender gaps across subjects

Built using **Tableau**, this dashboard offers fully interactive exploration via maps, filters, and dynamic tables.

---

## MAP VIEW – Country Performance

### Configuration
- **Type**: Symbol Map
- **Geography**: Country (`CNT`)
- **Bubble Size**: Average score in **selected subject**:
  - `Math`, `Reading`, `Science`, `GLCM`, or `ALL`
- **Bubble Color**: Gender gap (Female − Male) in selected subject
  - 🔵 Blue → boys outperform
  - 🟣 Purple → girls outperform
- **Tooltip Displays**:
  - Country name
  - Selected subject
  - Average scores: Female / Male
  - Gender gap
  - All 4 subjects (Math, Reading, Science, GLCM)
- **Highlighting**:
  - Greece styled differently (e.g., pin or red border)
- **Click Interaction**:
  - Clicking a bubble filters the **table to show only that country**

> Note: "ALL" = average of Math, Reading, and Science (GLCM is excluded from the “ALL” aggregate)

---

## TABLE VIEW – Country Statistics

### Columns
- **Country**
- **Avg Scores by Gender**:
  - `Math (F/M)`, `Reading (F/M)`, `Science (F/M)`, `GLCM (F/M)`
- **Gender Gaps**:
  - `Gap Math`, `Gap Reading`, `Gap Science`, `Gap GLCM`
- **Total Gender Gap**:
  - Sum of absolute gaps in core subjects only (Math + Reading + Science)
- **Subject Rank**:
  - Rank by selected subject or average (if `ALL`)
- **Highlight**:
  - Country row from map click is emphasized

---

## FILTERS & CONTROLS

- **Subject Selector**:
  - `Math`, `Reading`, `Science`, `GLCM`, or `ALL` *(average of Math, Reading, Science)*
- **Gender Filter**:
  - `Female`, `Male`, or `Both`
- **Top N Countries**:
  - Show only top-performing countries in selected subject
- **Country Search**:
  - Text input for quick country lookup

> All filters update **both map and table views** simultaneously.

---

## INTERACTIVITY SUMMARY

| Action                    | Map Response                         | Table Response                                |
|---------------------------|--------------------------------------|-----------------------------------------------|
| Subject = `Reading`       | Bubble size/color reflect Reading    | Table ranks by Reading scores                 |
| Subject = `ALL`           | Bubble shows average score (M+R+S)   | Table ranks by average of M+R+S               |
| Gender = `Female only`    | Bubble size = female scores only     | Only female columns shown; gaps still shown   |
| Gender = `Male only`      | Bubble size = male scores only       | Only male columns shown; gaps still shown     |
| Click on Greece           | Bubble selected                      | Table filters to **only Greece**              |

---

##  TECHNICAL LOGIC

- **Gender Gaps**:
  - Calculated using `{ FIXED [Country] : AVG(...) }`
  - Remain visible even when only one gender is selected
- **"ALL" Subject Option**:
  - Computed as the mean of Math, Reading, and Science
  - GLCM is never included in "ALL"

---

## Dashboard Benefits

- Clean, responsive layout
- Gender-specific analysis
- Focus on Greece
- Visual encoding of performance and equity
- Fully interactive and self-explanatory

---

## 🚫 Exclusions

| Feature                | Status               | Reason                                     |
|------------------------|----------------------|--------------------------------------------|
| GLCM in "ALL" average  | ❌ Excluded           | Not a core academic subject                |
| Multiple subject select| ❌ Not implemented    | Simplified user interface (single-select)  |
| Mini charts in tooltip | ❌ Optional feature   | Can be added later if needed               |

---

## 📁 Files Delivered

- `pisa2018_tableau_ready.csv` – preprocessed dataset for Tableau
- Tableau workbook (`.twb` or `.twbx`)
- Supporting README and dashboard documentation
