# Next.js Application Archetype Guide

## Overview

This is a Next.js 15.5.3 application archetype with React 19, TypeScript 5, TailwindCSS v4, and Turbopack. It follows a modern component-based architecture pattern for building frontend applications with App Router and server components.

**Important**: This archetype is a **working template** with established patterns. The documentation references the **widgets** feature as a **real, working implementation example** in the codebase. When implementing new features:

1. **Follow the established patterns** shown in the widgets implementation
2. **Use existing UI components** without modification
3. **Do not alter** the core application structure, CSS, or configuration files
4. **Only add new files** in the appropriate layer directories
5. **Reference `/src/app/widgets`, `/src/types/widget.ts`, and `/src/services/widgetService.ts`** as working examples

## Project Structure

```text
archetype-nextjs/
├── eslint.config.mjs                    # ESLint configuration (flat config)
├── next.config.ts                       # Next.js configuration
├── package.json                         # Project dependencies and scripts
├── postcss.config.mjs                   # PostCSS configuration for TailwindCSS
├── tsconfig.json                        # TypeScript configuration
├── archetype.md                         # This archetype guide
├── public/                              # Static assets (images, icons, etc.)
└── src/
    ├── app/                             # Next.js App Router (pages and API routes)
    │   ├── api/                         # Backend API route handlers
    │   │   ├── auth/                    # Authentication endpoints
    │   │   │   └── login/
    │   │   │       └── route.ts         # POST /api/auth/login
    │   │   ├── widgets/                 # Example: Widget feature API routes
    │   │   │   ├── route.ts             # GET /api/widgets, POST /api/widgets
    │   │   │   ├── [id]/
    │   │   │   │   └── route.ts         # GET/PUT/DELETE /api/widgets/:id
    │   │   │   └── category/            # Nested query routes
    │   │   │       └── [category]/
    │   │   │           └── route.ts     # GET /api/widgets/category/:category
    │   │   └── gadgets/                 # Example: Gadget feature API routes
    │   │       ├── route.ts             # GET /api/gadgets, POST /api/gadgets
    │   │       ├── [id]/
    │   │       │   └── route.ts         # GET/PUT/DELETE /api/gadgets/:id
    │   │       └── status/              # Nested query routes
    │   │           └── [status]/
    │   │               └── route.ts     # GET /api/gadgets/status/:status
    │   ├── widgets/                     # Widget feature pages
    │   │   ├── page.tsx                 # List page: /widgets
    │   │   ├── [id]/
    │   │   │   ├── page.tsx             # Detail page: /widgets/:id
    │   │   │   └── edit/
    │   │   │       └── page.tsx         # Edit page: /widgets/:id/edit
    │   │   └── new/
    │   │       └── page.tsx             # Create page: /widgets/new
    │   ├── gadgets/                     # Gadget feature pages
    │   │   ├── page.tsx                 # List page: /gadgets
    │   │   ├── [id]/
    │   │   │   ├── page.tsx             # Detail page: /gadgets/:id
    │   │   │   └── edit/
    │   │   │       └── page.tsx         # Edit page: /gadgets/:id/edit
    │   │   └── new/
    │   │       └── page.tsx             # Create page: /gadgets/new
    │   ├── favicon.ico                  # Application favicon
    │   ├── globals.css                  # Global styles with TailwindCSS v4
    │   ├── layout.tsx                   # Root layout (navigation, providers)
    │   └── page.tsx                     # Home page: /
    ├── components/                      # Reusable UI components
    │   ├── ProtectedRoute.tsx           # HOC for route authentication
    │   └── ui/                          # Base UI component library (DO NOT MODIFY)
    │       ├── Button.tsx               # Reusable button component
    │       ├── Input.tsx                # Reusable input component
    │       ├── Modal.tsx                # Reusable modal component
    │       ├── Select.tsx               # Reusable select/dropdown component
    │       ├── Table.tsx                # Reusable table component
    │       └── index.ts                 # Component barrel export
    ├── contexts/                        # React Context providers (global state)
    │   └── AuthContext.tsx              # Authentication state management
    ├── lib/                             # Shared utilities and middleware
    │   └── auth-middleware.ts           # Auth token forwarding & response handling
    ├── services/                        # API client services (frontend to API routes)
    │   ├── authService.ts               # Authentication API calls
    │   ├── widgetService.ts             # Widget API calls (example)
    │   └── gadgetService.ts             # Gadget API calls (example)
    └── types/                           # TypeScript type definitions
        ├── auth.ts                      # Authentication types
        ├── widget.ts                    # Widget types (example)
        └── gadget.ts                    # Gadget types (example)
```

## Architecture Layers

This application follows a **7-layer architecture** where each layer has a specific responsibility. Data flows from the user through these layers and back:

```
User Interface (Browser)
    ↕
Pages Layer (/src/app/[feature]/)
    ↕
Components Layer (/src/components/)
    ↕
Services Layer (/src/services/)
    ↕
API Routes Layer (/src/app/api/)
    ↕
Backend API (External)
```

### Layer 1: Types Layer (`/src/types`)

**Purpose**: Define TypeScript interfaces and types for type safety

**Responsibilities**:
- Define data models (e.g., `Widget`, `Gadget`, `User`)
- Define API request/response types
- Define form data types
- Provide type safety across all layers

**Example**: `/src/types/widget.ts`
```typescript
export interface Widget {
  id: string;
  name: string;
  category: string;
  price: number;
  isActive: boolean;
  createdAt: string;
}

export interface CreateWidgetRequest {
  name: string;
  category: string;
  price: number;
}
```

---

### Layer 2: API Routes Layer (`/src/app/api`)

**Purpose**: Backend API route handlers that forward requests to external APIs

**Responsibilities**:
- Define Next.js API route endpoints
- Forward requests to backend services with authentication
- Handle request validation
- Return standardized responses
- Manage error handling

**Pattern**: Each feature has its own API directory with `route.ts` files

**Example**: `/src/app/api/widgets/route.ts`
```typescript
import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/widgets - List all widgets
export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/v1/widgets', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to fetch widgets' }, { status: 500 });
  }
}

// POST /api/widgets - Create a new widget
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/v1/widgets', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to create widget' }, { status: 500 });
  }
}
```

**Dynamic Routes**: `/src/app/api/widgets/[id]/route.ts`
```typescript
// GET /api/widgets/:id - Get widget by ID
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/widgets/${params.id}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to fetch widget' }, { status: 500 });
  }
}

// PUT /api/widgets/:id - Update widget
export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      `/api/v1/widgets/${params.id}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to update widget' }, { status: 500 });
  }
}

// DELETE /api/widgets/:id - Delete widget
export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/widgets/${params.id}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to delete widget' }, { status: 500 });
  }
}
```

**Nested Query Routes**: `/src/app/api/widgets/category/[category]/route.ts`
```typescript
// GET /api/widgets/category/:category - Filter by category
export async function GET(
  request: NextRequest,
  { params }: { params: { category: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/widgets?category=${params.category}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to fetch widgets' }, { status: 500 });
  }
}
```

---

### Layer 3: Services Layer (`/src/services`)

**Purpose**: Frontend API client services that communicate with Next.js API routes

**Responsibilities**:
- Make HTTP requests to API routes (`/api/*`)
- Include authentication headers
- Handle response parsing
- Throw errors for error handling in pages
- Transform data if needed

**Pattern**: One service class per feature

**Example**: `/src/services/widgetService.ts`
```typescript
import { Widget, CreateWidgetRequest } from '@/types/widget';

const API_BASE_URL = '/api'; // <-- DO NOT MODIFY THIS LINE

class WidgetService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getWidgets(): Promise<Widget[]> {
    const response = await fetch(`${API_BASE_URL}/widgets`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch widgets');
    return response.json();
  }

  async getWidgetById(id: string): Promise<Widget> {
    const response = await fetch(`${API_BASE_URL}/widgets/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch widget');
    return response.json();
  }

  async createWidget(data: CreateWidgetRequest): Promise<Widget> {
    const response = await fetch(`${API_BASE_URL}/widgets`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) throw new Error('Failed to create widget');
    return response.json();
  }

  async updateWidget(id: string, data: Partial<Widget>): Promise<Widget> {
    const response = await fetch(`${API_BASE_URL}/widgets/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) throw new Error('Failed to update widget');
    return response.json();
  }

  async deleteWidget(id: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/widgets/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to delete widget');
  }

  async getWidgetsByCategory(category: string): Promise<Widget[]> {
    const response = await fetch(`${API_BASE_URL}/widgets/category/${category}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch widgets');
    return response.json();
  }
}

export const widgetService = new WidgetService();
```

---

### Layer 4: Components Layer (`/src/components`)

**Purpose**: Reusable UI components for building user interfaces

**Responsibilities**:
- Render UI elements
- Handle user interactions
- Manage local component state
- Emit events to parent components
- Provide consistent styling

**DO NOT MODIFY** the UI components in `/src/components/ui/`. These are reusable and stable.

**Existing UI Components**:
- `Button` - Buttons with variants (primary, secondary, danger)
- `Input` - Text inputs with labels and validation
- `Select` - Dropdown selects
- `Modal` - Modal dialogs
- `Table` - Data tables with sorting and pagination

---

### Layer 5: Pages Layer (`/src/app/[feature]`)

**Purpose**: Feature pages that compose components and manage page-level state

**Responsibilities**:
- Fetch data using services
- Manage page state (loading, errors, data)
- Compose UI components
- Handle user actions (create, edit, delete)
- Navigate between pages

**Pattern**: Each feature has 4 standard pages:
1. **List page** (`/widgets/page.tsx`) - Display all items
2. **Detail page** (`/widgets/[id]/page.tsx`) - View single item
3. **Edit page** (`/widgets/[id]/edit/page.tsx`) - Edit existing item
4. **Create page** (`/widgets/new/page.tsx`) - Create new item

**Example List Page**: `/src/app/widgets/page.tsx`
```typescript
'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { widgetService } from '@/services/widgetService';
import { Widget } from '@/types/widget';
import { Table, Button } from '@/components/ui';

export default function WidgetsPage() {
  const router = useRouter();
  const [widgets, setWidgets] = useState<Widget[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetchWidgets();
  }, []);

  const fetchWidgets = async () => {
    try {
      setLoading(true);
      const data = await widgetService.getWidgets();
      setWidgets(data);
    } catch (err) {
      setError('Failed to load widgets');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (id: string) => {
    if (!confirm('Are you sure?')) return;
    try {
      await widgetService.deleteWidget(id);
      fetchWidgets();
    } catch (err) {
      alert('Failed to delete widget');
    }
  };

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Widgets</h1>
        <Button onClick={() => router.push('/widgets/new')}>
          Create Widget
        </Button>
      </div>
      
      <Table
        columns={[
          { key: 'name', label: 'Name' },
          { key: 'category', label: 'Category' },
          { key: 'price', label: 'Price' },
          { key: 'isActive', label: 'Status' },
        ]}
        data={widgets}
        onRowClick={(widget) => router.push(`/widgets/${widget.id}`)}
        actions={(widget) => (
          <>
            <Button size="sm" onClick={() => router.push(`/widgets/${widget.id}/edit`)}>
              Edit
            </Button>
            <Button size="sm" variant="danger" onClick={() => handleDelete(widget.id)}>
              Delete
            </Button>
          </>
        )}
      />
    </div>
  );
}
```

**Example Detail Page**: `/src/app/widgets/[id]/page.tsx`
```typescript
'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { widgetService } from '@/services/widgetService';
import { Widget } from '@/types/widget';
import { Button } from '@/components/ui';

export default function WidgetDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [widget, setWidget] = useState<Widget | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.id) {
      fetchWidget(params.id as string);
    }
  }, [params.id]);

  const fetchWidget = async (id: string) => {
    try {
      const data = await widgetService.getWidgetById(id);
      setWidget(data);
    } catch (err) {
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  if (loading) return <div>Loading...</div>;
  if (!widget) return <div>Widget not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">{widget.name}</h1>
        <div className="space-x-2">
          <Button onClick={() => router.push(`/widgets/${widget.id}/edit`)}>
            Edit
          </Button>
          <Button variant="secondary" onClick={() => router.push('/widgets')}>
            Back
          </Button>
        </div>
      </div>
      
      <div className="space-y-4">
        <div>
          <label className="font-semibold">Category:</label>
          <p>{widget.category}</p>
        </div>
        <div>
          <label className="font-semibold">Price:</label>
          <p>${widget.price}</p>
        </div>
        <div>
          <label className="font-semibold">Status:</label>
          <p>{widget.isActive ? 'Active' : 'Inactive'}</p>
        </div>
      </div>
    </div>
  );
}
```

**Example Create Page**: `/src/app/widgets/new/page.tsx`
```typescript
'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { widgetService } from '@/services/widgetService';
import { CreateWidgetRequest } from '@/types/widget';
import { Input, Select, Button } from '@/components/ui';

export default function CreateWidgetPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateWidgetRequest>({
    name: '',
    category: '',
    price: 0,
  });
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      setLoading(true);
      await widgetService.createWidget(formData);
      router.push('/widgets');
    } catch (err) {
      alert('Failed to create widget');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create Widget</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <Input
          label="Name"
          value={formData.name}
          onChange={(e) => setFormData({ ...formData, name: e.target.value })}
          required
        />
        
        <Input
          label="Category"
          value={formData.category}
          onChange={(e) => setFormData({ ...formData, category: e.target.value })}
          required
        />
        
        <Input
          label="Price"
          type="number"
          value={formData.price}
          onChange={(e) => setFormData({ ...formData, price: Number(e.target.value) })}
          required
        />
        
        <div className="flex gap-2">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/widgets')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
```

**Example Edit Page**: `/src/app/widgets/[id]/edit/page.tsx`
```typescript
'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { widgetService } from '@/services/widgetService';
import { Widget } from '@/types/widget';
import { Input, Button } from '@/components/ui';

export default function EditWidgetPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<Partial<Widget>>({});
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.id) {
      fetchWidget(params.id as string);
    }
  }, [params.id]);

  const fetchWidget = async (id: string) => {
    try {
      const data = await widgetService.getWidgetById(id);
      setFormData(data);
    } catch (err) {
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      setLoading(true);
      await widgetService.updateWidget(params.id as string, formData);
      router.push(`/widgets/${params.id}`);
    } catch (err) {
      alert('Failed to update widget');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  if (loading) return <div>Loading...</div>;

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Edit Widget</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <Input
          label="Name"
          value={formData.name || ''}
          onChange={(e) => setFormData({ ...formData, name: e.target.value })}
          required
        />
        
        <Input
          label="Category"
          value={formData.category || ''}
          onChange={(e) => setFormData({ ...formData, category: e.target.value })}
          required
        />
        
        <Input
          label="Price"
          type="number"
          value={formData.price || 0}
          onChange={(e) => setFormData({ ...formData, price: Number(e.target.value) })}
          required
        />
        
        <div className="flex gap-2">
          <Button type="submit" disabled={loading}>
            {loading ? 'Saving...' : 'Save'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/widgets/${params.id}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
```

---

### Layer 6: Contexts Layer (`/src/contexts`)

**Purpose**: Global state management using React Context API

**Responsibilities**:
- Manage authentication state
- Provide user session data
- Handle login/logout operations
- Share state across components

**Example**: Authentication is already implemented in `AuthContext.tsx`

---

### Layer 7: Library/Middleware Layer (`/src/lib`)

**Purpose**: Shared utilities and middleware functions

**Responsibilities**:
- Forward API requests with authentication headers
- Handle API responses consistently
- Process errors uniformly
- Provide reusable helper functions

**Example**: Authentication middleware is in `auth-middleware.ts`



## Getting Started

### TypeScript Configuration

- Uses strict mode for maximum type safety
- ES2017 target for modern JavaScript features
- Bundler module resolution for optimal bundling
- Path aliasing: `@/*` maps to `./src/*` for clean imports

### TailwindCSS v4 Configuration

- Uses new `@import "tailwindcss"` syntax (not v3 `@tailwind` directives)
- PostCSS integration via `@tailwindcss/postcss`
- Custom CSS variables available for theming
- Configured in `postcss.config.mjs` and `globals.css`

## Development Guidelines

### Important: Working Example Implementation

This archetype includes a **complete, working widget feature** that serves as the reference implementation:

- **Types**: `/src/types/widget.ts` - Widget interfaces and types
- **API Routes**: 
  - `/src/app/api/widgets/route.ts` - List and create endpoints
  - `/src/app/api/widgets/[id]/route.ts` - Get, update, delete by ID
  - `/src/app/api/widgets/category/[category]/route.ts` - Filter by category
- **Service**: `/src/services/widgetService.ts` - Widget API client
- **Pages**:
  - `/src/app/widgets/page.tsx` - List page with filtering
  - `/src/app/widgets/[id]/page.tsx` - Detail page
  - `/src/app/widgets/[id]/edit/page.tsx` - Edit page
  - `/src/app/widgets/new/page.tsx` - Create page

**Use this widget implementation as your guide when creating new features!**

---

### Template Philosophy

This archetype provides a **working template** with established patterns. When implementing new features:

**✅ DO:**
- Follow the 4-step implementation process (Types → API Routes → Services → Pages)
- Use existing UI components from `/src/components/ui/`
- Create new files in the appropriate layer directories
- Follow the naming conventions consistently
- Use the standard page structure (list, detail, edit, create)

**❌ DO NOT:**
- Modify existing UI components (`Button`, `Input`, `Modal`, `Select`, `Table`)
- Change global CSS or TailwindCSS configuration
- Alter the root layout or core application structure
- Modify configuration files (`next.config.ts`, `tsconfig.json`, etc.)

---

### 4-Step Implementation Process for New Features

When implementing a new feature (e.g., "Gadgets"), follow these steps **in order**:

#### Step 1: Create Type Definitions

**File**: `/src/types/gadget.ts`

```typescript
// Define the main entity interface
export interface Gadget {
  id: string;
  name: string;
  status: 'active' | 'inactive' | 'pending';
  quantity: number;
  description?: string;
  createdAt: string;
  updatedAt: string;
}

// Define request/response types
export interface CreateGadgetRequest {
  name: string;
  status: 'active' | 'inactive' | 'pending';
  quantity: number;
  description?: string;
}

export interface UpdateGadgetRequest {
  name?: string;
  status?: 'active' | 'inactive' | 'pending';
  quantity?: number;
  description?: string;
}
```

---

#### Step 2: Create API Routes

Create the following route files:

**Main Routes**: `/src/app/api/gadgets/route.ts`

```typescript
import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/gadgets - List all gadgets
export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/v1/gadgets', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching gadgets:', error);
    return NextResponse.json({ error: 'Failed to fetch gadgets' }, { status: 500 });
  }
}

// POST /api/gadgets - Create new gadget
export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest('/api/v1/gadgets', 'POST', request, body);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error creating gadget:', error);
    return NextResponse.json({ error: 'Failed to create gadget' }, { status: 500 });
  }
}
```

**ID Routes**: `/src/app/api/gadgets/[id]/route.ts`

```typescript
import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/gadgets/${params.id}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching gadget:', error);
    return NextResponse.json({ error: 'Failed to fetch gadget' }, { status: 500 });
  }
}

export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      `/api/v1/gadgets/${params.id}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error updating gadget:', error);
    return NextResponse.json({ error: 'Failed to update gadget' }, { status: 500 });
  }
}

export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/gadgets/${params.id}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Gadget deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting gadget:', error);
    return NextResponse.json({ error: 'Failed to delete gadget' }, { status: 500 });
  }
}
```

**Query Routes** (if needed): `/src/app/api/gadgets/status/[status]/route.ts`

```typescript
import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { status: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/v1/gadgets?status=${params.status}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching gadgets by status:', error);
    return NextResponse.json({ error: 'Failed to fetch gadgets' }, { status: 500 });
  }
}
```

---

#### Step 3: Create Service

**File**: `/src/services/gadgetService.ts`

```typescript
import { Gadget, CreateGadgetRequest, UpdateGadgetRequest } from '@/types/gadget';

const API_BASE_URL = '/api';

class GadgetService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getGadgets(): Promise<Gadget[]> {
    const response = await fetch(`${API_BASE_URL}/gadgets`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch gadgets');
    }
    return response.json();
  }

  async getGadgetById(id: string): Promise<Gadget> {
    const response = await fetch(`${API_BASE_URL}/gadgets/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch gadget');
    }
    return response.json();
  }

  async createGadget(data: CreateGadgetRequest): Promise<Gadget> {
    const response = await fetch(`${API_BASE_URL}/gadgets`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to create gadget');
    }
    return response.json();
  }

  async updateGadget(id: string, data: UpdateGadgetRequest): Promise<Gadget> {
    const response = await fetch(`${API_BASE_URL}/gadgets/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      throw new Error('Failed to update gadget');
    }
    return response.json();
  }

  async deleteGadget(id: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/gadgets/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete gadget');
    }
  }

  async getGadgetsByStatus(status: string): Promise<Gadget[]> {
    const response = await fetch(`${API_BASE_URL}/gadgets/status/${status}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch gadgets by status');
    }
    return response.json();
  }
}

export const gadgetService = new GadgetService();
```

---

#### Step 4: Create Pages

Create 4 standard pages:

**1. List Page**: `/src/app/gadgets/page.tsx`

```typescript
'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { gadgetService } from '@/services/gadgetService';
import { Gadget } from '@/types/gadget';
import { Table, Button } from '@/components/ui';

export default function GadgetsPage() {
  const router = useRouter();
  const [gadgets, setGadgets] = useState<Gadget[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetchGadgets();
  }, []);

  const fetchGadgets = async () => {
    try {
      setLoading(true);
      const data = await gadgetService.getGadgets();
      setGadgets(data);
      setError(null);
    } catch (err) {
      setError('Failed to load gadgets');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (id: string) => {
    if (!confirm('Are you sure you want to delete this gadget?')) return;
    
    try {
      await gadgetService.deleteGadget(id);
      fetchGadgets(); // Refresh list
    } catch (err) {
      alert('Failed to delete gadget');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Gadgets</h1>
        <Button onClick={() => router.push('/gadgets/new')}>
          Create Gadget
        </Button>
      </div>
      
      <Table
        columns={[
          { key: 'name', label: 'Name' },
          { key: 'status', label: 'Status' },
          { key: 'quantity', label: 'Quantity' },
          { key: 'createdAt', label: 'Created' },
        ]}
        data={gadgets}
        onRowClick={(gadget) => router.push(`/gadgets/${gadget.id}`)}
        actions={(gadget) => (
          <div className="flex gap-2">
            <Button 
              size="sm" 
              onClick={(e) => {
                e.stopPropagation();
                router.push(`/gadgets/${gadget.id}/edit`);
              }}
            >
              Edit
            </Button>
            <Button 
              size="sm" 
              variant="danger" 
              onClick={(e) => {
                e.stopPropagation();
                handleDelete(gadget.id);
              }}
            >
              Delete
            </Button>
          </div>
        )}
      />
    </div>
  );
}
```

**2. Detail Page**: `/src/app/gadgets/[id]/page.tsx`

```typescript
'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { gadgetService } from '@/services/gadgetService';
import { Gadget } from '@/types/gadget';
import { Button } from '@/components/ui';

export default function GadgetDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [gadget, setGadget] = useState<Gadget | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.id) {
      fetchGadget(params.id as string);
    }
  }, [params.id]);

  const fetchGadget = async (id: string) => {
    try {
      const data = await gadgetService.getGadgetById(id);
      setGadget(data);
    } catch (err) {
      console.error('Failed to load gadget:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this gadget?')) return;
    
    try {
      await gadgetService.deleteGadget(params.id as string);
      router.push('/gadgets');
    } catch (err) {
      alert('Failed to delete gadget');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!gadget) return <div className="p-6">Gadget not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">{gadget.name}</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/gadgets/${gadget.id}/edit`)}>
            Edit
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/gadgets')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 space-y-4">
        <div>
          <label className="block text-sm font-semibold text-gray-700">Status</label>
          <p className="mt-1 text-gray-900">{gadget.status}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Quantity</label>
          <p className="mt-1 text-gray-900">{gadget.quantity}</p>
        </div>
        
        {gadget.description && (
          <div>
            <label className="block text-sm font-semibold text-gray-700">Description</label>
            <p className="mt-1 text-gray-900">{gadget.description}</p>
          </div>
        )}
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Created At</label>
          <p className="mt-1 text-gray-900">{new Date(gadget.createdAt).toLocaleString()}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Updated At</label>
          <p className="mt-1 text-gray-900">{new Date(gadget.updatedAt).toLocaleString()}</p>
        </div>
      </div>
    </div>
  );
}
```

**3. Create Page**: `/src/app/gadgets/new/page.tsx`

```typescript
'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { gadgetService } from '@/services/gadgetService';
import { CreateGadgetRequest } from '@/types/gadget';
import { Input, Select, Button } from '@/components/ui';

export default function CreateGadgetPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateGadgetRequest>({
    name: '',
    status: 'active',
    quantity: 0,
    description: '',
  });
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    try {
      setLoading(true);
      await gadgetService.createGadget(formData);
      router.push('/gadgets');
    } catch (err) {
      alert('Failed to create gadget');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create New Gadget</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <Input
          label="Name"
          value={formData.name}
          onChange={(e) => setFormData({ ...formData, name: e.target.value })}
          required
        />
        
        <Select
          label="Status"
          value={formData.status}
          onChange={(e) => setFormData({ ...formData, status: e.target.value as any })}
          options={[
            { value: 'active', label: 'Active' },
            { value: 'inactive', label: 'Inactive' },
            { value: 'pending', label: 'Pending' },
          ]}
          required
        />
        
        <Input
          label="Quantity"
          type="number"
          value={formData.quantity}
          onChange={(e) => setFormData({ ...formData, quantity: Number(e.target.value) })}
          required
        />
        
        <Input
          label="Description"
          value={formData.description}
          onChange={(e) => setFormData({ ...formData, description: e.target.value })}
          multiline
        />
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Gadget'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/gadgets')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
```

**4. Edit Page**: `/src/app/gadgets/[id]/edit/page.tsx`

```typescript
'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { gadgetService } from '@/services/gadgetService';
import { Gadget, UpdateGadgetRequest } from '@/types/gadget';
import { Input, Select, Button } from '@/components/ui';

export default function EditGadgetPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateGadgetRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);

  useEffect(() => {
    if (params.id) {
      fetchGadget(params.id as string);
    }
  }, [params.id]);

  const fetchGadget = async (id: string) => {
    try {
      const data = await gadgetService.getGadgetById(id);
      setFormData({
        name: data.name,
        status: data.status,
        quantity: data.quantity,
        description: data.description,
      });
    } catch (err) {
      console.error('Failed to load gadget:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    try {
      setSaving(true);
      await gadgetService.updateGadget(params.id as string, formData);
      router.push(`/gadgets/${params.id}`);
    } catch (err) {
      alert('Failed to update gadget');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Edit Gadget</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <Input
          label="Name"
          value={formData.name || ''}
          onChange={(e) => setFormData({ ...formData, name: e.target.value })}
          required
        />
        
        <Select
          label="Status"
          value={formData.status || 'active'}
          onChange={(e) => setFormData({ ...formData, status: e.target.value as any })}
          options={[
            { value: 'active', label: 'Active' },
            { value: 'inactive', label: 'Inactive' },
            { value: 'pending', label: 'Pending' },
          ]}
          required
        />
        
        <Input
          label="Quantity"
          type="number"
          value={formData.quantity || 0}
          onChange={(e) => setFormData({ ...formData, quantity: Number(e.target.value) })}
          required
        />
        
        <Input
          label="Description"
          value={formData.description || ''}
          onChange={(e) => setFormData({ ...formData, description: e.target.value })}
          multiline
        />
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/gadgets/${params.id}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
```

---

### Summary: Files to Create for Each Feature

For a feature named "Gadgets", create exactly these files:

1. **Type**: `/src/types/gadget.ts`
2. **API Routes**:
   - `/src/app/api/gadgets/route.ts`
   - `/src/app/api/gadgets/[id]/route.ts`
   - `/src/app/api/gadgets/status/[status]/route.ts` (if needed)
3. **Service**: `/src/services/gadgetService.ts`
4. **Pages**:
   - `/src/app/gadgets/page.tsx`
   - `/src/app/gadgets/[id]/page.tsx`
   - `/src/app/gadgets/[id]/edit/page.tsx`
   - `/src/app/gadgets/new/page.tsx`

**Total**: 8-9 new files per feature

---

### File and Code Naming Conventions

Follow these naming conventions consistently across the application:

#### File Naming

- **Components**: PascalCase (e.g., `Button.tsx`, `GadgetCard.tsx`, `UserProfile.tsx`)
- **Pages**: lowercase (e.g., `page.tsx`, `layout.tsx`, `loading.tsx`, `error.tsx`)
- **Services**: camelCase with "Service" suffix (e.g., `gadgetService.ts`, `widgetService.ts`)
- **Types**: lowercase singular (e.g., `gadget.ts`, `widget.ts`, `user.ts`)
- **API Routes**: `route.ts` (always named `route.ts` in Next.js)
- **Utilities/Lib**: kebab-case (e.g., `auth-middleware.ts`, `date-utils.ts`)

#### Code Naming

- **Components**: PascalCase (e.g., `Button`, `GadgetList`, `UserProfile`)
- **Variables**: camelCase (e.g., `gadgetData`, `isLoading`, `userCount`)
- **Functions**: camelCase (e.g., `fetchGadgets`, `calculateTotal`, `formatDate`)
- **Constants**: UPPER_SNAKE_CASE (e.g., `API_BASE_URL`, `MAX_RETRY_COUNT`)
- **Types/Interfaces**: PascalCase (e.g., `Gadget`, `CreateGadgetRequest`, `ApiResponse`)
- **Props Interfaces**: PascalCase with "Props" suffix (e.g., `ButtonProps`, `GadgetCardProps`)
- **Event Handlers**: "handle" prefix + PascalCase (e.g., `handleClick`, `handleSubmit`, `handleChange`)
- **State Variables**: camelCase, descriptive (e.g., `isModalOpen`, `selectedGadget`, `formErrors`)

#### Directory Naming

- **Feature directories**: lowercase plural (e.g., `/gadgets`, `/widgets`, `/users`)
- **Component directories**: PascalCase for specific components (e.g., `/components/UserProfile`)
- **Utility directories**: lowercase (e.g., `/utils`, `/lib`, `/hooks`)

#### Examples

```text
Good Naming:
✅ /src/app/gadgets/page.tsx
✅ /src/types/gadget.ts
✅ /src/services/gadgetService.ts
✅ /src/app/api/gadgets/route.ts
✅ const handleSubmit = () => {}
✅ const isLoading = true
✅ interface GadgetProps {}

Bad Naming:
❌ /src/app/Gadgets/Page.tsx
❌ /src/types/Gadget.ts
❌ /src/services/GadgetService.ts
❌ const HandleSubmit = () => {}
❌ const IsLoading = true
```

---

## Available Dependencies

### Core Framework
- **Next.js 15.5.3**: React framework with App Router and server components
- **React 19.1.0**: UI library with concurrent features and React Server Components
- **TypeScript 5**: Static type checking for enhanced developer experience and code quality

### Styling
- **TailwindCSS v4 (alpha)**: Utility-first CSS framework with new CSS-first configuration
- **PostCSS**: CSS processing tool for TailwindCSS integration

### Build Tools
- **Turbopack**: Next-generation bundler for fast development and optimized builds
- **ESLint 9**: JavaScript/TypeScript linter with flat config format

### UI Components
- **Material-UI (MUI)**: Comprehensive React component library (if needed for advanced components)

### Project Dependencies

**Commonly Used Packages:**
- Form handling: `react-hook-form`, `zod` (validation)
- Data fetching: `swr`, `react-query`
- Date/time: `date-fns`, `dayjs`
- Utilities: `lodash`, `classnames`

---

## Quick Reference

### Standard Feature Implementation Checklist

When implementing a new feature called "Gadgets":

- [ ] **Step 1**: Create `/src/types/gadget.ts` with interfaces
- [ ] **Step 2**: Create API routes:
  - [ ] `/src/app/api/gadgets/route.ts` (GET, POST)
  - [ ] `/src/app/api/gadgets/[id]/route.ts` (GET, PUT, DELETE)
  - [ ] Optional: `/src/app/api/gadgets/[query]/[param]/route.ts`
- [ ] **Step 3**: Create `/src/services/gadgetService.ts` with API methods
- [ ] **Step 4**: Create pages:
  - [ ] `/src/app/gadgets/page.tsx` (list)
  - [ ] `/src/app/gadgets/[id]/page.tsx` (detail)
  - [ ] `/src/app/gadgets/[id]/edit/page.tsx` (edit)
  - [ ] `/src/app/gadgets/new/page.tsx` (create)

### Common Patterns

**API Route Pattern:**
```typescript
import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const response = await forwardAuthRequest('/api/v1/endpoint', 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    return NextResponse.json({ error: 'Error message' }, { status: 500 });
  }
}
```

**Service Pattern:**
```typescript
class FeatureService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getItems(): Promise<Item[]> {
    const response = await fetch('/api/items', {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch');
    return response.json();
  }
}
```

**Page Pattern:**
```typescript
'use client';

import { useEffect, useState } from 'react';
import { featureService } from '@/services/featureService';

export default function FeaturePage() {
  const [data, setData] = useState([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchData();
  }, []);

  const fetchData = async () => {
    try {
      const result = await featureService.getItems();
      setData(result);
    } catch (error) {
      console.error(error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) return <div>Loading...</div>;
  
  return <div>{/* Render data */}</div>;
}
```

---

## Troubleshooting

### Common Issues

**Issue**: TypeScript errors about missing types
**Solution**: Make sure to create type definitions in `/src/types/` first

**Issue**: API routes returning 404
**Solution**: Verify the folder structure matches the route pattern exactly

**Issue**: Authentication not working
**Solution**: Check that `forwardAuthRequest` and `handleAuthApiResponse` are imported correctly

**Issue**: Components not rendering
**Solution**: Add `'use client'` directive at the top of files using hooks or browser APIs

**Issue**: Styles not applying
**Solution**: Verify TailwindCSS classes are used correctly and `globals.css` imports Tailwind

---

## Notes for Code Generators

**When generating code for this archetype:**

1. **Always follow the 4-step process** in order: Types → API Routes → Services → Pages
2. **Use the exact patterns** shown in the examples (copy structure, change names)
3. **Never modify** existing UI components, CSS, or configuration files
4. **Use 'use client'** directive for all pages (they use React hooks)
5. **Include error handling** in all async functions
6. **Use existing UI components** from `/src/components/ui/`
7. **Follow naming conventions** strictly for consistency
8. **Create all 4 standard pages** for each feature (list, detail, edit, create)
9. **Use dynamic routes** `[id]` for detail and edit pages
10. **Export service as singleton** instance: `export const gadgetService = new GadgetService()`
