# LaTeX Class Modularization

## Overview

The `awesome-cv.cls` file has been modularized into separate `.sty` files for better maintainability and fine-grained control. This allows you to easily customize specific aspects of the CV without affecting other components.

## File Structure

### Main Class File
- **`src/awesome-cv.cls`** - Main class file that loads all modules

### Modular Components

1. **`src/cv-core.sty`** - Core packages and utilities
   - All required LaTeX packages
   - Font directory configuration
   - Utility commands (column types, conditional statements)
   - Spacing macros

2. **`src/cv-colors.sty`** - Color definitions
   - Grayscale colors (white, black, darkgray, gray, lightgray)
   - Basic colors (green, orange, purple, red, blue)
   - Text colors (darktext, text, graytext, lighttext)
   - Awesome theme colors
   - Link colorization commands

3. **`src/cv-layout.sty`** - Page geometry and layout
   - Page margins configuration
   - Header and footer setup
   - Fancy header settings

4. **`src/cv-styles.sty`** - Font and style definitions
   - Nokia Sans font family definitions
   - Font commands (headerfont, bodyfont, etc.)
   - All style commands for CV elements:
     - Header styles (name, position, address, social, quote)
     - Footer styles
     - Section and subsection styles
     - Entry styles (title, position, date, location, description)
     - Subentry styles
     - Project date styles
     - Honor/award styles
     - Skill styles
     - Cover letter styles

5. **`src/cv-commands.sty`** - Standard CV commands
   - Personal information commands (name, email, phone, etc.)
   - Social media commands (github, linkedin, twitter, etc.)
   - CV structure commands (cvsection, cvsubsection, cvparagraph)
   - Entry environments and commands (cventry, cvsubentry)
   - Honor/award commands (cvhonor)
   - Skill commands (cvskill)
   - Item list environment (cvitems)
   - Cover letter commands

6. **`src/cv-projects.sty`** - Project-specific commands
   - `\projectcompany{name}{start_year}{end_year}{location}` - Company header
   - `\projectrole{title}` - Role title
   - `\projectentry{name}{start}{end}{description}` - Project entry

7. **`src/cv-header-footer.sty`** - Header and footer commands
   - `\makecvheader[alignment]` - Creates CV header with photo support
   - `\makecvfooter{left}{center}{right}` - Creates CV footer

## Benefits

### 1. **Better Organization**
Each component is in its own file, making it easier to find and modify specific elements.

### 2. **Fine-Grained Control**
You can customize individual aspects without affecting others:
- Change colors without touching layout
- Modify styles without affecting commands
- Update layout without changing fonts

### 3. **Easier Maintenance**
- Smaller files are easier to understand and modify
- Changes are isolated to specific modules
- Reduced risk of breaking unrelated functionality

### 4. **Reusability**
- Individual modules can be reused in other projects
- Easy to create variations by swapping modules
- Can create alternative style files while keeping the same structure

### 5. **Version Control**
- Clearer git diffs showing exactly what changed
- Easier to track changes to specific components
- Better collaboration with multiple contributors

## Customization Examples

### Changing Colors
Edit `src/cv-colors.sty` to modify the color scheme:
```latex
\colorlet{awesome}{awesome-skyblue}  % Change theme color
\definecolor{darktext}{HTML}{000000}  % Change text color
```

### Adjusting Layout
Edit `src/cv-layout.sty` to change page margins:
```latex
\geometry{left=2.5cm, top=2.0cm, right=4.0cm, bottom=2.0cm}
```

### Modifying Styles
Edit `src/cv-styles.sty` to change font sizes or styles:
```latex
\newcommand*{\entrytitlestyle}[1]{{\fontsize{11pt}{1em}\bodyfont\bfseries\color{darktext} #1}}
```

### Adding New Commands
Add custom commands to the appropriate module or create a new `.sty` file and load it in `awesome-cv.cls`.

## Migration Notes

The modularization is backward compatible. Existing `.tex` files using `\documentclass{awesome-cv}` will continue to work without modification.

## Known Issues

### Overfull \hbox Warnings
There are minor overfull hbox warnings (1.46pt) in the `cvitems` environment. These are cosmetic and don't affect the PDF output significantly. They can be addressed by:
1. Adjusting the `leftmargin` parameter in the `cvitems` environment
2. Fine-tuning the column widths in entry commands
3. Modifying the item spacing

These warnings are documented in the original task and can be addressed in future updates.

## Version History

- **v2.0 (2025/11/28)** - Modularized into separate `.sty` files
- **v1.6.1 (2017/02/05)** - Original monolithic version



