# Next.js Application Archetype Guide

## Overview

This is a Next.js 15.5.3 application archetype with React 19, TypeScript 5, TailwindCSS v4, and Turbopack. It follows a modern component-based architecture pattern for building frontend applications with App Router and server components.

**Important**: This archetype is a **clean template** ready for implementation. The documentation uses generic examples (like 'items', 'products') to demonstrate patterns. When implementing features, you should should follow the established patterns without modifying the existing UI components, CSS, or core application structure.

## Project Structure

```text
archetype-nextjs/
├── eslint.config.mjs                    # ESLint configuration (flat config)
├── next.config.ts                       # Next.js configuration
├── package.json                         # Project dependencies and scripts
├── postcss.config.mjs                   # PostCSS configuration for TailwindCSS
├── tsconfig.json                        # TypeScript configuration
├── archetype.md                         # This archetype guide
├── public/                              # Static assets
│   ├── file.svg
│   ├── globe.svg
│   ├── next.svg
│   ├── vercel.svg
│   └── window.svg
└── src/
    ├── app/                             # Next.js App Router directory
    │   ├── api/                         # API routes (Next.js API routes)
    │   |   ├── auth/                    # Example API routes for 'auth' feature
    │   |   |   └── login/
    │   |   |       └── route.ts          # POST login route
    │   |   ├── users/                   # Example API routes for 'users' feature
    │   |   |   ├── [id]/
    │   |   |   |   └── route.ts          # GET, PUT, DELETE user by ID
    │   |   |   ├── email/               # Nested route for user by email
    │   |   |   |   └── [email]/
    │   |   |   |       └── route.ts      # GET user by email
    │   |   |   └── route.ts              # GET all users, POST new user
    │   │   └── items/                   # Example API routes for 'items' feature
    │   │       ├── [id]/
    │   │       |   └── route.ts          # GET, PUT, DELETE item by ID
    │   │       ├── name/                # Nested route for item by name
    │   │       │   └── [name]/
    │   │       |       └── route.ts      # GET item by name
    │   │       └── route.ts              # GET all items, POST new item
    │   ├── favicon.ico                  # Application favicon
    │   ├── globals.css                  # Global styles with TailwindCSS v4
    │   ├── layout.tsx                   # Root layout component
    │   └── page.tsx                     # Home page component
    ├── components/                      # Reusable UI components
    │   ├── ProtectedRoute.tsx           # Route protection component for authentication
    │   └── ui/                          # Base UI component library
    │       ├── Button.tsx               # Button component
    │       ├── Input.tsx                # Input component
    │       ├── Modal.tsx                # Modal component
    │       ├── Select.tsx               # Select component
    │       ├── Table.tsx                # Table component
    │       └── index.ts                 # Component exports
    ├── contexts/                        # React Context providers
    │   └── AuthContext.tsx              # Authentication context provider
    ├── lib/                             # Library utilities and helpers
    │   └── auth-middleware.ts           # Authentication middleware utilities
    ├── services/                        # API service layer
    │   └── authService.ts               # Authentication service
    └── types/                           # TypeScript type definitions
        └── auth.ts                      # Authentication type definitions
```

## Architecture Layers

### 1. App Layer (`/src/app`)

- **Purpose**: Next.js App Router for routing and layout management
- **Responsibilities**:
  - Define application routes and pages
  - Manage layouts and metadata
  - Handle server and client components
  - Configure global styles and fonts

### 2. Components Layer (`/src/components`)

- **Purpose**: Reusable UI components and component library
- **Responsibilities**:
  - Define reusable UI elements
  - Implement component logic and styling
  - Provide consistent design system
  - Handle component state and events

### 3. Contexts Layer (`/src/contexts`)

- **Purpose**: React Context providers for global state management
- **Responsibilities**:
  - Manage application-wide state
  - Provide authentication state and methods
  - Handle user session management
  - Coordinate between services and components

### 4. Library Layer (`/src/lib`)

- **Purpose**: Utility functions and middleware helpers
- **Responsibilities**:
  - Provide reusable utility functions
  - Handle authentication middleware logic
  - Forward requests with proper headers
  - Process API responses

### 5. API Layer (`/src/app/api`)

- **Purpose**: API routes and backend communication
- **Responsibilities**:
  - Define API endpoints and request methods
  - Implement data fetching logic
  - Handle API responses and errors

### 6. Services Layer (`/src/services`)

- **Purpose**: API communication and data fetching
- **Responsibilities**:
  - Handle HTTP requests to backend APIs
  - Implement data fetching strategies
  - Manage API error handling
  - Provide data transformation

### 7. Types Layer (`/src/types`)

- **Purpose**: TypeScript type definitions and interfaces
- **Responsibilities**:
  - Define data models and interfaces
  - Provide type safety across the application
  - Document API contracts
  - Enable better IDE support

## Getting Started

- Uses strict mode
- ES2017 target
- Bundler module resolution
- Path aliasing: `@/*` maps to `./src/*`

### TailwindCSS v4 Configuration

- Uses `@import "tailwindcss"` syntax (not v3)
- PostCSS integration via `@tailwindcss/postcss`
- Custom CSS variables for theming

## Development Guidelines

### Important: Clean Template

This archetype is provided as a **clean template** ready for feature implementation. The documentation uses generic examples (like 'items', 'products') to demonstrate the established patterns. When implementing new features, you should:

- **Use the existing UI components** (`Button`, `Input`, `Modal`, `Select`, `Table`) without modification
- **Keep the existing CSS and styling** intact
- **Follow the established layer structure** for new features
- **Only add new files** in the appropriate directories

### Implementing a New Feature (e.g., 'Products')

To implement a new feature, follow these **4 steps exactly** and **do not modify** existing application parts:

#### Step 1: Add New Types (`/src/types`)

#### Step 2: Add New API routes (`/src/app/api`)

#### Step 3: Add New Service (`/src/services`)

#### Step 4: Add New Page (`/src/app/products`)

### What NOT to Modify

When implementing new features, **DO NOT** change:

- ❌ **UI Components** (`/src/components/ui/`) - These are reusable across all features
- ❌ **CSS Files** (`globals.css`, TailwindCSS configuration) - Styling is already configured
- ❌ **Root Layout** (`app/layout.tsx`) - Global layout should remain unchanged
- ❌ **Configuration Files** (`next.config.ts`, `tsconfig.json`, etc.) - Core setup is complete
- ❌ **Existing Features** - This is a clean template with no existing feature implementations

### What TO Add/Modify

When implementing new features, **ONLY**:

- ✅ **Add new type files** in `/src/types/`
- ✅ **Add new API route files** in `/src/app/api/`
- ✅ **Add new service files** in `/src/services/`
- ✅ **Add new page directories** in `/src/app/`

### File Naming Conventions

- **Components**: PascalCase (e.g., `Button.tsx`, `ItemCard.tsx`)
- **Pages**: lowercase (e.g., `page.tsx`, `layout.tsx`, `loading.tsx`)
- **Services**: camelCase (e.g., `itemService.ts`, `apiClient.ts`)
- **Types**: camelCase (e.g., `item.ts`, `apiTypes.ts`)
- **Utilities**: camelCase (e.g., `formatDate.ts`, `validation.ts`)
- **API Routes**: lowercase (e.g., `route.ts`, `[id].ts`)

### Naming Conventions

- **Components**: PascalCase (e.g., `Button`, `ItemCard`, `ProductTable`)
- **Files**: kebab-case or PascalCase (e.g., `item-service.ts`, `ItemService.ts`)
- **Variables**: camelCase (e.g., `itemData`, `isLoading`, `handleClick`)
- **Constants**: UPPER_SNAKE_CASE (e.g., `API_BASE_URL`, `DEFAULT_PAGE_SIZE`)
- **Types/Interfaces**: PascalCase (e.g., `Item`, `ApiResponse`, `CreateItemData`)
- **Props Interfaces**: PascalCase with "Props" suffix (e.g., `ButtonProps`, `ItemCardProps`)
- **Event Handlers**: "handle" prefix (e.g., `handleClick`, `handleSubmit`, `handleChange`)

### Standard Patterns

- **Server Components**: Default for pages and layouts (no 'use client')
- **Client Components**: Use `'use client'` directive when needed
- **Async Pages**: Use `async function` for server components with data fetching
- **Error Handling**: Use `error.tsx` and `not-found.tsx` for route-level errors
- **Loading States**: Use `loading.tsx` for route-level loading UI
- **Layouts**: Use `layout.tsx` for shared UI between routes
- **Metadata**: Export `metadata` object or `generateMetadata` function

## Available Dependencies

- **Next.js 15.5.3**: React framework with App Router
- **React 19.1.0**: UI library with concurrent features
- **TypeScript 5**: Type safety and enhanced developer experience
- **TailwindCSS v4**: Utility-first CSS framework (alpha version)
- **Turbopack**: Fast bundler for development and builds
- **ESLint 9**: Code linting with flat config format
- **PostCSS**: CSS processing for TailwindCSS integration
- **MUI (Material-UI)**: Component library for React

### Key Technology Features

- **App Router**: File-system based routing with layouts and nested routes
- **Server Components**: React components that render on the server
- **Turbopack**: Faster alternative to Webpack for builds and dev server
- **TailwindCSS v4**: Latest version with `@import "tailwindcss"` syntax
- **TypeScript Path Mapping**: `@/*` alias for clean imports
- **Font Optimization**: Automatic optimization of Google Fonts (Geist)
- **Image Optimization**: Built-in Next.js Image component for performance
