# Modular Elisp Extensions for ox-awesomecv

This directory contains a modular implementation of extensions to `ox-awesomecv` that adds support for custom CV entry types.

## File Structure

### Main Entry Point
- **`ox-awesomecv-extensions.el`** - Main module that loads all sub-modules and installs advice to override formatting functions

### Utility Modules
- **`ox-awesomecv-ext-utils.el`** - Shared utility functions
  - `org-awesomecv-ext--build-marginnote` - Builds margin note strings from headline properties
  - `org-awesomecv-ext--margin-props` - Constant defining standard margin note properties

### Formatting Handlers
- **`ox-awesomecv-ext-project.el`** - Handlers for project-type entries
  - `org-awesomecv-ext--format-cvproject` - Formats `cvproject` entries → `\projectsubentry`
  - `org-awesomecv-ext--format-cvopensource` - Formats `cvopensource` entries → `\opensourcesubentry`

- **`ox-awesomecv-ext-entries.el`** - Handlers for other custom entry types
  - `org-awesomecv-ext--format-cventryshort` - Formats `cventryshort` entries → `\shortstintentry`
  - `org-awesomecv-ext--format-cvsubsection` - Formats `cvsubsection` entries → `\cvsubsection`
  - `org-awesomecv-ext--format-cvrole` - Formats `cvrole` entries → `\cvrole`

### Core Dispatcher
- **`ox-awesomecv-ext-core.el`** - Core formatting dispatcher
  - `org-awesomecv--format-cventry-extended` - Main dispatcher that routes to appropriate formatter based on entry type
  - `org-awesomecv--format-cventry-with-extensions` - Advice wrapper for the original formatter

### Headline Handler
- **`ox-awesomecv-ext-headline.el`** - Extended headline processing
  - `org-awesomecv-headline-extended` - Extended headline handler that recognizes new entry types

## How It Works

1. **Loading**: `ox-awesomecv-extensions.el` is loaded by `org-cv-init.el`
2. **Initialization**: The main file loads all sub-modules in dependency order
3. **Advice Installation**: Two pieces of advice are installed:
   - `org-awesomecv--format-cventry-with-extensions` wraps the original cventry formatter
   - `org-awesomecv-headline-extended` overrides the headline handler
4. **Entry Processing**: When Org exports a headline:
   - The headline handler checks the `CV_ENV` property
   - If it matches a custom type, it routes to the extended formatter
   - The extended formatter dispatches to the appropriate handler
   - The handler generates LaTeX output for that entry type

## Supported Entry Types

| CV_ENV Value | LaTeX Command | Description |
|--------------|---------------|-------------|
| `cvproject` | `\projectsubentry` | Project entries with margin notes |
| `cvopensource` | `\opensourcesubentry` | Open source contribution entries |
| `cventryshort` | `\shortstintentry` | Short employment entries |
| `cvsubsection` | `\cvsubsection` | Subsection headers |
| `cvrole` | `\cvrole` | Role/position headers |

## Margin Note Properties

Both `cvproject` and `cvopensource` entries support margin notes built from these properties (in display order):
- LINK / URL
- LANGUAGES
- TOOLS
- TECH
- STACK
- ARCH (Architecture)
- OS
- KEYWORDS
- PLATFORM
- FRAMEWORK

## Benefits of Modular Structure

1. **Easier Debugging** - Each module is small and focused on one responsibility
2. **Better Organization** - Related functions are grouped together
3. **Testability** - Individual modules can be tested in isolation
4. **Maintainability** - Changes to one entry type don't affect others
5. **Extensibility** - New entry types can be added by creating new handler modules

## Adding New Entry Types

To add a new entry type:

1. Create a new formatter function in an appropriate module (or create a new one)
2. Add the entry type to the dispatcher in `ox-awesomecv-ext-core.el`
3. Add the entry type to the environment list in `ox-awesomecv-ext-headline.el`
4. Define the corresponding LaTeX command in `src/cv-commands.sty`

## Backup

The original monolithic implementation is backed up as `ox-awesomecv-extensions.el.backup`
