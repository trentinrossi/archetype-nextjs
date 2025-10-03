# Copilot Instructions for Next.js Archetype

## Project Overview
This is a **Next.js 15 archetype** project that serves as a template/boilerplate for code generation. It's part of a larger `poc-legacy-macro-feature-01` system located in the code generator frontend archetype directory structure.

## Architecture & Stack
- **Next.js 15.5.3** with App Router (not Pages Router)
- **React 19.1.0** - Latest React with concurrent features
- **TypeScript 5** with strict mode enabled  
- **TailwindCSS v4** (alpha) with PostCSS integration
- **Turbopack** for development and builds (not Webpack)
- **ESLint 9** with flat config format

## Key Development Patterns

### Build & Development
```bash
# Development with Turbopack (not webpack)
npm run dev            # Uses --turbopack flag
npm run build          # Uses --turbopack flag for builds
npm run start          # Production server
npm run lint           # ESLint with flat config
```

### File Structure Conventions
- Use `src/app/` directory structure (App Router)
- TypeScript path aliasing: `@/*` maps to `./src/*`
- Global styles in `src/app/globals.css` with TailwindCSS v4 imports
- Static assets in `public/` directory (SVG icons included)

### Styling & UI Patterns
- **TailwindCSS v4** uses `@import "tailwindcss"` (not v3 syntax)
- Custom CSS variables for theming:
  ```css
  :root {
    --background: #ffffff;
    --foreground: #171717;
  }
  ```
- Dark mode via `@media (prefers-color-scheme: dark)`
- Use `@theme inline` directive for Tailwind theme extension
- Geist font family variables: `--font-geist-sans`, `--font-geist-mono`

### Component Patterns
- Server Components by default (App Router)
- Use `Image` from `next/image` with proper optimization
- CSS Grid and Flexbox for responsive layouts
- Semantic HTML with proper accessibility attributes

### Configuration Specifics
- **ESLint**: Uses flat config (`eslint.config.mjs`) with `@eslint/eslintrc` compatibility
- **TypeScript**: Strict mode, ES2017 target, bundler module resolution
- **Next.js**: Minimal config in `next.config.ts`, relies on conventions
- **PostCSS**: Only loads `@tailwindcss/postcss` plugin

## Code Generation Context
This archetype appears to be used for generating frontend code structures. When working on this:
- Maintain the archetype's genericity - avoid hardcoded business logic
- Preserve the file structure as a template for code generation
- Keep dependencies minimal and focused on the core stack
- Maintain compatibility with the parent code generation system

## Critical Dependencies
- Always use Turbopack flags in scripts (already configured)
- TailwindCSS v4 has different syntax - don't use v3 patterns
- ESLint flat config format is required (not legacy .eslintrc)
- React 19 features available but maintain compatibility patterns

## Development Notes
- This is an archetype, not a final application
- Changes should consider impact on code generation workflows
- The `.next` directory contains Turbopack artifacts, not Webpack
- Font optimization uses Google Fonts (Geist) with CSS variables