# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2025-10-18

### Added
- Initial release of Switchback
- Zero-dependency SPA navigation library (~300 lines)
- Link interception for async navigation
- Form interception for async form submissions
- History management (back/forward button support)
- Progress indicators with XHR option for download progress
- Scroll restoration (remembers scroll position)
- Partial reloads (only refresh specific props)
- Error handling (server errors, validation errors)
- `newSwitchback(config)` - initialize the app
- `visit(url, options)` - programmatic navigation
- `page()` - get current page data
- `reload(options)` - reload current page
- TypeScript support with full type definitions
- Server-agnostic design (works with any backend)
- Framework-agnostic client (works with just-jsx, vanilla, etc.)
- Comprehensive test suite with vitest
- Full-stack demo examples:
  - PHP - Vanilla PHP with no framework dependencies
  - Deno - TypeScript type sharing between client and server
  - Go - Concurrent worker pools with true parallelism
  - Rust - Embedded SQLite database with type safety
  - Erlang - Actor model with real-time chat
  - Zig - Blazingly fast with zero dependencies
  - C - Optimistic updates for instant UX
  - C# - .NET integration
  - Assembly - Low-level control
- Docker setup for all demos
- Vendorable via git submodules
- Auditability emphasis (small, readable codebase)

[Unreleased]: https://github.com/ge3224/switchback/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/ge3224/switchback/releases/tag/v0.1.0
