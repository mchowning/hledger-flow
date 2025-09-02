# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

hledger-flow is a Haskell CLI tool that provides an automated workflow for importing and classifying financial statements using hledger. The project focuses on automated processing of electronic statements (primarily CSV files) and generating classified Hledger journals.

## Development Commands

### Building and Testing
- **Build and test**: `./bin/build-and-test` or `stack test --interleaved-output --pedantic && stack install`
- **Build only**: `stack build`
- **Run tests**: `stack test`
- **Install locally**: `stack install` (installs to `~/.local/bin/hledger-flow`)

### Development Environment
- **Nix shell**: `nix develop` (provides stack, haskell-language-server, hoogle, etc.)
- **Stack resolver**: Uses LTS 21.13 (see stack.yaml)

### Running the Application
- **Import CSV files**: `hledger-flow import [dir] [--start-year YEAR] [--new-files-only]`
- **Generate reports**: `hledger-flow report [basedir] [--ascii-reports]`
- **Version info**: `hledger-flow --version`

## Architecture and Code Structure

### Module Hierarchy
The codebase follows a clear hierarchical structure under `Hledger.Flow.*`:

- **Core modules**:
  - `Types.hs`: Core types and type classes
  - `RuntimeOptions.hs`: Runtime configuration and options
  - `Common.hs`: Shared utilities and functions
  - `PathHelpers.hs`: File path utilities using the Path library

- **Import processing**:
  - `Import/CSVImport.hs`: Main CSV import orchestration
  - `Import/ImportHelpers.hs`: Core import logic
  - `Import/ImportHelpersTurtle.hs`: Shell-based import operations using Turtle
  - `Import/Types.hs`: Import-specific type definitions

- **Supporting modules**:
  - `BaseDir.hs`: Base directory handling
  - `Reports.hs`: Report generation
  - `Logging.hs`: Logging infrastructure
  - `DateTime.hs`: Date/time utilities
  - `DocHelpers.hs`: Documentation helpers

### Key Design Patterns
- **Type classes** for behavior: `HasVerbosity`, `HasBaseDir`, `HasRunDir`, etc.
- **Turtle shell** for system operations and file processing
- **STM channels** for concurrent logging
- **Path library** for type-safe file system operations
- **Parallel processing** with configurable batch sizes
- **Newtypes over type aliases**: Prefer `newtype` over `type` for better type safety and self-documenting code

### Directory Structure Conventions
The application follows specific directory conventions for financial data:
```
import/
├── {owner}/
│   └── {bank}/
│       └── {account}/
│           ├── 1-in/           # Input CSV files
│           ├── 2-preprocessed/ # Processed files
│           ├── 3-journal/      # Generated journal files
│           ├── preprocess*     # Custom preprocessing scripts
│           └── construct*      # Custom construction scripts
```

## Testing

### Test Structure
- **Unit tests**: Individual module testing (e.g., `CSVImport.Unit`, `Common.Unit`)
- **Integration tests**: End-to-end testing (e.g., `CSVImport.Integration`, `BaseDir.Integration`)
- **Test helpers**: `TestHelpers.hs`, `TestHelpersTurtle.hs`

### Test Framework
- Uses **HUnit** for test cases
- Test main entry point: `test/Spec.hs`
- Tests are organized in subdirectories matching source structure

### Running Specific Tests
- All tests: `stack test`
- With verbose output: `stack test --interleaved-output`

## Key Dependencies and Libraries

### Core Dependencies
- **turtle**: Shell programming and system operations
- **path**: Type-safe file system paths
- **optparse-applicative**: Command-line argument parsing
- **stm**: Software Transactional Memory for concurrent logging
- **exceptions**: Exception handling
- **time**: Date/time operations
- **HUnit**: Testing framework (test dependencies)

### Important Implementation Details
- **Concurrency**: Uses STM channels for thread-safe logging during parallel processing
- **File processing**: Batch processing with configurable batch sizes (default: 20)
- **Path handling**: Extensive use of the Path library for type-safe file operations
- **Shell operations**: Turtle library for cross-platform shell scripting

## Common Development Tasks

### Adding New Features
1. Add types to appropriate modules under `Hledger.Flow.Types` or create new type modules
2. Implement core logic in appropriate `Hledger.Flow.*` modules
3. Add command-line options in `app/Main.hs` and `RuntimeOptions.hs`
4. Add both unit and integration tests
5. Update documentation in README.org if user-facing

### Working with File Operations
- Use the `Path` library types (`AbsFile`, `RelFile`, `AbsDir`, `RelDir`) consistently
- Path conversions via `PathHelpers` module functions
- Turtle shell operations for actual file system work

### Error Handling
- Uses standard Haskell exception handling via the `exceptions` library
- Turtle shell operations handle exit codes appropriately
- Logging infrastructure supports both stdout and stderr channels