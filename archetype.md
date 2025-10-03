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
└── src/
    ├── app/                             # Next.js App Router directory
    │   ├── favicon.ico                  # Favicon
    │   ├── globals.css                  # Global styles with TailwindCSS v4
    │   ├── layout.tsx                   # Root layout component
    │   └── page.tsx                     # Home page component
    ├── components/                      # Reusable UI components
    │   └── ui/                          # Base UI component library
    │       ├── Button.tsx               # Button component
    │       ├── Input.tsx                # Input component
    │       ├── Modal.tsx                # Modal component
    │       ├── Select.tsx               # Select component
    │       ├── Table.tsx                # Table component
    │       └── index.ts                 # Component exports
    ├── services/                        # API service layer (empty - ready for implementation)
    └── types/                           # TypeScript type definitions (empty - ready for implementation)
```

## Architecture Layers

### 1. App Layer (`/src/app`)

- **Purpose**: Next.js App Router for routing and layout management
- **Responsibilities**:
  - Define application routes and pages
  - Manage layouts and metadata
  - Handle server and client components
  - Configure global styles and fonts

**Example Structure:**

```tsx
// app/layout.tsx - Root Layout
import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import "./globals.css";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "Next.js Archetype",
  description: "Modern Next.js application archetype",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased`}
      >
        {children}
      </body>
    </html>
  );
}

// app/page.tsx - Home Page
export default function HomePage() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-24">
      <div className="z-10 w-full max-w-5xl items-center justify-between font-mono text-sm">
        <h1 className="text-4xl font-bold mb-8">Welcome to Next.js Archetype</h1>
        <p className="text-lg">A modern frontend application template</p>
      </div>
    </main>
  );
}

// app/items/page.tsx - Items Page
import { ItemTable } from '@/components/ui/Table';
import { itemService } from '@/services/itemService';

function ItemsPage() {
  const [items, setItems] = useState<Item[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const data = await itemService.getAllItems();
        setItems(data);
      } catch (error) {
        setError("Failed to fetch items");
      } finally {
        setLoading(false);
      }
    };

    fetchData();
  }, []);

  if (loading) {
    return <div>Loading...</div>;
  }

  if (error) {
    return <div>{error}</div>;
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-6">Items Management</h1>
      <ItemTable data={items} />
    </div>
  );
}
```

### 2. Components Layer (`/src/components`)

- **Purpose**: Reusable UI components and component library
- **Responsibilities**:
  - Define reusable UI elements
  - Implement component logic and styling
  - Provide consistent design system
  - Handle component state and events

**Example Structure:**

```tsx
// components/ui/Button.tsx
import { ButtonHTMLAttributes, forwardRef } from 'react';

interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: 'primary' | 'secondary' | 'destructive' | 'outline';
  size?: 'sm' | 'md' | 'lg';
}

const Button = forwardRef<HTMLButtonElement, ButtonProps>(
  ({ variant = 'primary', size = 'md', className = '', ...props }, ref) => {
    const baseClasses = 'inline-flex items-center justify-center rounded-md font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50';
    
    const variants = {
      primary: 'bg-primary text-primary-foreground hover:bg-primary/90 focus-visible:ring-primary',
      secondary: 'bg-secondary text-secondary-foreground hover:bg-secondary/80 focus-visible:ring-secondary',
      destructive: 'bg-destructive text-destructive-foreground hover:bg-destructive/90 focus-visible:ring-destructive',
      outline: 'border border-border bg-background hover:bg-muted text-foreground focus-visible:ring-primary',
    };
    
    const sizes = {
      sm: 'h-8 px-3 text-sm',
      md: 'h-10 px-4',
      lg: 'h-12 px-6 text-lg',
    };
    
    const classes = `${baseClasses} ${variants[variant]} ${sizes[size]} ${className}`;
    
    return (
      <button ref={ref} className={classes} {...props} />
    );
  }
);

Button.displayName = 'Button';

export { Button };

// components/ui/Input.tsx
import { InputHTMLAttributes, forwardRef } from 'react';

interface InputProps extends InputHTMLAttributes<HTMLInputElement> {
  label?: string;
  error?: string;
}

const Input = forwardRef<HTMLInputElement, InputProps>(
  ({ label, error, className = '', ...props }, ref) => {
    return (
      <div className="w-full">
        {label && (
          <label className="block text-sm font-medium mb-2">
            {label}
          </label>
        )}
        <input
          ref={ref}
          className={`flex h-10 w-full rounded-md border border-border bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50 ${error ? 'border-destructive' : ''} ${className}`}
          {...props}
        />
        {error && (
          <p className="text-sm text-destructive mt-1">{error}</p>
        )}
      </div>
    );
  }
);

Input.displayName = 'Input';

export { Input };
```

### 3. API Layer (`/src/api`)

- **Purpose**: API routes and backend communication
- **Responsibilities**:
  - Define API endpoints and request methods
  - Implement data fetching logic
  - Handle API responses and errors

**Example Structure:**

```typescript
// api/items/[id].ts
import { NextRequest, NextResponse } from 'next/server';

export async function GET(request: NextRequest) {
  // Fetch items from database or external API
  const response = await fetch(`${API_BASE_URL}/api/v1/items/`);
  return NextResponse.json(response);
}

export async function POST(request: NextRequest) {
  const body = await request.json();
  // Create new item in database or external API
  const response = await fetch(`${API_BASE_URL}/api/v1/items/`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  return NextResponse.json(response);
}

// Another methods here (PUT, DELETE, etc.)
```

### 4. Services Layer (`/src/services`)

- **Purpose**: API communication and data fetching
- **Responsibilities**:
  - Handle HTTP requests to backend APIs
  - Implement data fetching strategies
  - Manage API error handling
  - Provide data transformation

**Example Structure:**

```typescript
// services/itemService.ts
import { Item } from '@/types/item';

const API_BASE_URL = '/api';

class ItemService {
  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorData: APIError = await response.json().catch(() => ({}));
      throw new Error(
        errorData.detail?.[0]?.msg || 
        `HTTP ${response.status}: ${response.statusText}`
      );
    }
    return response.json();
  }

  async listItems(): Promise<Item[]> {
    const response = await fetch(`${API_BASE_URL}/items`, {
      headers: this.getAuthHeaders(),
    });
    const data = await this.handleResponse<Record<string, unknown>>(response);
    
    // Handle the response format based on API behavior
    if (Array.isArray(data)) {
      return data as Item[];
    } else if (data.items && Array.isArray(data.items)) {
      return data.items as Item[];
    } else {
      return [];
    }
  }

  async getItemById(id: string): Promise<Item> {
    const response = await fetch(`${API_BASE_URL}/items/${id}`, {
      headers: this.getAuthHeaders(),
    });
    return this.handleResponse<Item>(response);
  }

  async getItemByName(name: string): Promise<Item> {
    const response = await fetch(`${API_BASE_URL}/items/name/${name}`, {
      headers: this.getAuthHeaders(),
    });
    return this.handleResponse<Item>(response);
  }

  async createItem(item: Omit<Item, 'id'>): Promise<Item> {
    const response = await fetch(`${API_BASE_URL}/items`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(item),
    });
    return this.handleResponse<Item>(response);
  }

  async updateItem(id: string, item: Omit<Item, 'id'>): Promise<Item> {
    const response = await fetch(`${API_BASE_URL}/items/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(item),
    });
    return this.handleResponse<Item>(response);
  }

  async deleteItem(id: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/items/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    await this.handleResponse<void>(response);
  }
}

export const itemService = new ItemService();
```

### 4. Types Layer (`/src/types`)

- **Purpose**: TypeScript type definitions and interfaces
- **Responsibilities**:
  - Define data models and interfaces
  - Provide type safety across the application
  - Document API contracts
  - Enable better IDE support

**Example Structure:**

```typescript
// types/item.ts
export interface Item {
  id: string;
  name: string;
  description: string;
  category: string;
  status: 'active' | 'inactive';
  createdAt: string;
  updatedAt: string;
}
```

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
#### Step 2: Add New API routes (`/src/api`)
#### Step 3: Add New Service (`/src/services`)
#### Step 4: Add New Page (`/src/app/products`)
#### Step 5: Update Main Page (`/src/app/page.tsx`)

Add navigation to your new feature in the main page:

```typescript
// app/page.tsx
import Link from 'next/link';
import { Button } from '@/components/ui/Button';

export default function HomePage() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-24">
      <div className="z-10 w-full max-w-5xl items-center justify-between font-mono text-sm">
        <h1 className="text-4xl font-bold mb-8">Welcome to Next.js Archetype</h1>
        <p className="text-lg mb-8">A modern frontend application template</p>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 max-w-md">
          <Link href="/items">
            <Button className="w-full">Items Management</Button>
          </Link>
          <Link href="/products">
            <Button className="w-full">Products Management</Button>
          </Link>
          {/* Add more feature links as needed */}
        </div>
      </div>
    </main>
  );
}
```

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
- ✅ **Add new service files** in `/src/services/`
- ✅ **Add new page directories** in `/src/app/`
- ✅ **Update main page navigation** in `/src/app/page.tsx`

### File Naming Conventions

- **Components**: PascalCase (e.g., `Button.tsx`, `ItemCard.tsx`)
- **Pages**: lowercase (e.g., `page.tsx`, `layout.tsx`, `loading.tsx`)
- **Services**: camelCase (e.g., `itemService.ts`, `apiClient.ts`)
- **Types**: camelCase (e.g., `item.ts`, `apiTypes.ts`)
- **Utilities**: camelCase (e.g., `formatDate.ts`, `validation.ts`)

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

### Key Technology Features

- **App Router**: File-system based routing with layouts and nested routes
- **Server Components**: React components that render on the server
- **Turbopack**: Faster alternative to Webpack for builds and dev server
- **TailwindCSS v4**: Latest version with `@import "tailwindcss"` syntax
- **TypeScript Path Mapping**: `@/*` alias for clean imports
- **Font Optimization**: Automatic optimization of Google Fonts (Geist)
- **Image Optimization**: Built-in Next.js Image component for performance