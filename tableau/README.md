# Interactive Tableau Dashboard – PISA 2018 Analysis

This dashboard explores the 2018 PISA dataset with a focus on:
- Greece’s position among participating countries
- Gender gaps across subjects (Math, Reading, Science, GLCM)

The dashboard includes a **linked interactive map and data table**, with filters and hover details.

---

## Map View – Country Performance

- **Type**: Symbol map using country codes (`CNT`)
- **Bubble Size**: Average score in selected subject (Math, Reading, Science, or GLCM)
- **Bubble Color**: Gender gap (absolute or directional) in the selected subject
- **Tooltip**:
  - Country name
  - Scores by gender
  - Gender gaps
- **Highlighting**: Greece styled differently (e.g., red border)
- **Interactive Link**: Clicking a country filters/highlights the table below

---

## Table View – Country Statistics

| Column             | Description                                         |
|--------------------|-----------------------------------------------------|
| Country            | Name of participating country                      |
| Avg Scores (M/F)   | Math, Reading, Science, GLCM (male and female)     |
| Gender Gaps        | Absolute differences per subject and total         |
| Subject Rank       | Rank based on selected subject                     |
| Highlighted Row    | Selected country from map is emphasized            |

- Fully sortable columns
- Optional: highlight rows with large gender gaps

---

## Filters and Controls

- **Subject Selector**: Choose between Math, Reading, Science, GLCM
- **Gender Filter**: Show data for Male, Female, or Both
- **Top N Countries**: Slider or dropdown filter for top-performing countries
- **Country Search**: Type-ahead selector for direct lookup

All filters affect both the **map and table simultaneously**.

---

## Interactivity

- Clicking a country bubble on the map will:
  - Highlight and scroll to that row in the table
  - Apply optional filters (if configured)
- Hovering over a bubble shows a detailed tooltip
- Optional: embedded mini-chart (`viz in tooltip`) for score breakdown


