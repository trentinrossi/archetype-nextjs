# Account Management System - Implementation Guide

## Overview

This document provides a comprehensive technical guide for the Account Data Management System implementation. It covers the complete architecture, code patterns, and implementation details.

## Project Structure

```
src/
├── types/
│   └── account.ts                          # Type definitions
├── services/
│   └── accountService.ts                   # API client service
├── app/
│   ├── page.tsx                            # Home page
│   ├── accounts/                           # Account feature pages
│   │   ├── page.tsx                        # List page
│   │   ├── new/page.tsx                    # Create page
│   │   └── [id]/
│   │       ├── page.tsx                    # Detail page
│   │       └── edit/page.tsx               # Edit page
│   └── api/
│       └── accounts/                       # API route handlers
│           ├── route.ts                    # List & Create
│           ├── sequential/route.ts         # Sequential processing
│           ├── active/route.ts             # Active accounts
│           ├── expired/route.ts            # Expired accounts
│           └── [id]/
│               ├── route.ts                # Get, Update, Delete
│               └── exists/route.ts         # Check existence
├── components/
│   └── ui/                                 # Reusable UI components
│       ├── Button.tsx
│       ├── Input.tsx
│       ├── Select.tsx
│       ├── Table.tsx
│       └── Modal.tsx
├── contexts/
│   └── AuthContext.tsx                     # Authentication context
└── lib/
    └── auth-middleware.ts                  # Auth middleware utilities
```

## Architecture Layers

### Layer 1: Types Layer

**File**: `src/types/account.ts`

**Purpose**: Define TypeScript interfaces for type safety across the application.

**Key Interfaces**:

```typescript
// Main entity interface
export interface Account {
  // Core fields from backend
  acctId: string;
  acctActiveStatus: 'A' | 'I';
  acctCurrBal: number;
  // ... other fields
  
  // Computed fields from backend
  availableCredit: number;
  availableCashCredit: number;
  currentCycleNetAmount: number;
  isActive: boolean;
  isExpired: boolean;
  hasBeenReissued: boolean;
  activeStatusDisplayName: string;
  
  // System fields
  createdAt: string;
  updatedAt: string;
}

// Request DTOs
export interface CreateAccountRequest {
  acctId: string;
  acctActiveStatus: 'A' | 'I';
  // ... required fields
}

export interface UpdateAccountRequest {
  acctId: string;
  // ... optional fields for update
}

// Response DTOs
export interface AccountsPageResponse {
  content: Account[];
  pageable: {
    pageNumber: number;
    pageSize: number;
  };
  totalElements: number;
  totalPages: number;
}
```

**Best Practices**:
- Use strict types (no `any`)
- Define union types for enums ('A' | 'I')
- Separate request and response types
- Include JSDoc comments for complex types

---

### Layer 2: API Routes Layer

**Location**: `src/app/api/accounts/`

**Purpose**: Next.js API route handlers that forward requests to the backend API.

**Pattern**:

```typescript
import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    // Forward request to backend with authentication
    const response = await forwardAuthRequest(
      '/api/accounts',
      'GET',
      request
    );
    
    // Handle response and extract data
    const result = await handleAuthApiResponse(response);
    
    // Return JSON response
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error:', error);
    return NextResponse.json(
      { error: 'Error message' },
      { status: 500 }
    );
  }
}
```

**Key Files**:

1. **`route.ts`** - Main collection endpoints
   - `GET` - List accounts (with pagination)
   - `POST` - Create new account

2. **`[id]/route.ts`** - Single resource endpoints
   - `GET` - Get account by ID
   - `PUT` - Update account
   - `DELETE` - Delete account

3. **`sequential/route.ts`** - Business rule BR-001
   - `GET` - Sequential processing

4. **`active/route.ts`** - Filter endpoint
   - `GET` - Active accounts only

5. **`expired/route.ts`** - Filter endpoint
   - `GET` - Expired accounts only

6. **`[id]/exists/route.ts`** - Validation endpoint
   - `GET` - Check if account exists

**Authentication Flow**:

```
Client Request
    ↓
API Route Handler
    ↓
forwardAuthRequest() - Adds auth headers
    ↓
Backend API
    ↓
handleAuthApiResponse() - Processes response
    ↓
API Route Handler
    ↓
Client Response
```

---

### Layer 3: Services Layer

**File**: `src/services/accountService.ts`

**Purpose**: Frontend API client that communicates with Next.js API routes.

**Pattern**:

```typescript
class AccountService {
  // Private method to get auth headers
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  // Public method for API call
  async getAccounts(page: number = 0, size: number = 20): Promise<AccountsPageResponse> {
    const response = await fetch(
      `${API_BASE_URL}/accounts?page=${page}&size=${size}`,
      {
        method: 'GET',
        headers: this.getAuthHeaders(),
      }
    );

    if (!response.ok) {
      throw new Error('Failed to fetch accounts');
    }

    return response.json();
  }
}

// Export singleton instance
export const accountService = new AccountService();
```

**Key Methods**:

| Method | Purpose | Returns |
|--------|---------|---------|
| `getAccounts()` | List accounts with pagination | `AccountsPageResponse` |
| `getAccountById()` | Get single account | `Account` |
| `createAccount()` | Create new account | `Account` |
| `updateAccount()` | Update existing account | `Account` |
| `deleteAccount()` | Delete account | `void` |
| `getAccountsSequentially()` | BR-001 implementation | `Account[]` |
| `getActiveAccounts()` | Filter active accounts | `Account[]` |
| `getExpiredAccounts()` | Filter expired accounts | `Account[]` |
| `checkAccountExists()` | Validate existence | `boolean` |

**Error Handling**:

```typescript
try {
  const account = await accountService.getAccountById(id);
  // Success handling
} catch (error) {
  // Error handling
  console.error('Failed to fetch account:', error);
  alert('Failed to load account');
}
```

---

### Layer 4: Pages Layer

**Location**: `src/app/accounts/`

**Purpose**: React components that compose UI and manage page-level state.

#### 4.1 List Page (`page.tsx`)

**Features**:
- Paginated account listing
- Multiple filter options
- Inline actions (Edit, Delete)
- Click-through navigation
- Loading and error states

**State Management**:

```typescript
const [accounts, setAccounts] = useState<Account[]>([]);
const [loading, setLoading] = useState(true);
const [error, setError] = useState<string | null>(null);
const [currentPage, setCurrentPage] = useState(0);
const [totalPages, setTotalPages] = useState(0);
const [pageSize, setPageSize] = useState(20);
const [filterType, setFilterType] = useState<'all' | 'active' | 'expired' | 'sequential'>('all');
```

**Data Fetching**:

```typescript
useEffect(() => {
  fetchAccounts();
}, [currentPage, pageSize, filterType]);

const fetchAccounts = async () => {
  try {
    setLoading(true);
    setError(null);

    if (filterType === 'active') {
      const data = await accountService.getActiveAccounts();
      setAccounts(data);
    } else if (filterType === 'all') {
      const response = await accountService.getAccounts(currentPage, pageSize);
      setAccounts(response.content);
      setTotalPages(response.totalPages);
    }
    // ... other filters
  } catch (err) {
    setError('Failed to load accounts');
  } finally {
    setLoading(false);
  }
};
```

**Table Configuration**:

```typescript
<Table
  columns={[
    { key: 'acctId', label: 'Account ID' },
    {
      key: 'activeStatusDisplayName',
      label: 'Status',
      render: (account: Account) => (
        <span className={`badge ${account.isActive ? 'badge-success' : 'badge-secondary'}`}>
          {account.activeStatusDisplayName}
        </span>
      ),
    },
    // ... more columns
  ]}
  data={accounts}
  onRowClick={(account) => router.push(`/accounts/${account.acctId}`)}
  actions={(account) => (
    <div className="flex gap-2">
      <Button size="sm" onClick={() => router.push(`/accounts/${account.acctId}/edit`)}>
        Edit
      </Button>
      <Button size="sm" variant="danger" onClick={() => handleDelete(account.acctId)}>
        Delete
      </Button>
    </div>
  )}
/>
```

#### 4.2 Detail Page (`[id]/page.tsx`)

**Features**:
- Complete account information display
- Organized into logical sections
- Computed fields display
- Color-coded indicators
- Action buttons

**Data Fetching**:

```typescript
useEffect(() => {
  if (params.id) {
    fetchAccount(params.id as string);
  }
}, [params.id]);

const fetchAccount = async (id: string) => {
  try {
    setLoading(true);
    const data = await accountService.getAccountById(id);
    setAccount(data);
  } catch (err) {
    setError('Failed to load account');
  } finally {
    setLoading(false);
  }
};
```

**Section Organization**:

1. **Account Identification**
   - Account ID
   - Status
   - Group ID
   - Expiration status

2. **Financial Information**
   - Current Balance
   - Credit Limit
   - Available Credit
   - Cash Credit Limit
   - Available Cash Credit

3. **Current Cycle Information**
   - Current Cycle Credit
   - Current Cycle Debit
   - Current Cycle Net Amount

4. **Date Information**
   - Open Date
   - Expiration Date
   - Reissue Date
   - Reissue Status

5. **System Information**
   - Created At
   - Updated At

#### 4.3 Create Page (`new/page.tsx`)

**Features**:
- Comprehensive form with all fields
- Client-side validation
- Real-time error feedback
- Field-level help text
- Cancel action

**Form State**:

```typescript
const [formData, setFormData] = useState<CreateAccountRequest>({
  acctId: '',
  acctActiveStatus: 'A',
  acctCurrBal: 0,
  acctCreditLimit: 0,
  acctCashCreditLimit: 0,
  acctOpenDate: new Date().toISOString().split('T')[0],
  acctExpirationDate: '',
  acctReissueDate: '',
  acctCurrCycCredit: 0,
  acctCurrCycDebit: 0,
  acctGroupId: '',
});
const [errors, setErrors] = useState<Record<string, string>>({});
```

**Validation Logic**:

```typescript
const validateForm = (): boolean => {
  const newErrors: Record<string, string> = {};

  // Account ID validation
  if (!formData.acctId) {
    newErrors.acctId = 'Account ID is required';
  } else if (!/^\d{11}$/.test(formData.acctId)) {
    newErrors.acctId = 'Account ID must be exactly 11 numeric digits';
  }

  // Balance validation
  if (formData.acctCurrBal < 0) {
    newErrors.acctCurrBal = 'Current balance cannot be negative';
  }

  // Date validation
  if (formData.acctExpirationDate &&
      new Date(formData.acctExpirationDate) <= new Date(formData.acctOpenDate)) {
    newErrors.acctExpirationDate = 'Expiration date must be after open date';
  }

  setErrors(newErrors);
  return Object.keys(newErrors).length === 0;
};
```

**Form Submission**:

```typescript
const handleSubmit = async (e: React.FormEvent) => {
  e.preventDefault();

  if (!validateForm()) {
    return;
  }

  try {
    setLoading(true);
    
    // Remove empty optional fields
    const submitData = { ...formData };
    if (!submitData.acctReissueDate) delete submitData.acctReissueDate;
    if (!submitData.acctGroupId) delete submitData.acctGroupId;

    await accountService.createAccount(submitData);
    router.push('/accounts');
  } catch (err: any) {
    alert(err.message || 'Failed to create account');
  } finally {
    setLoading(false);
  }
};
```

#### 4.4 Edit Page (`[id]/edit/page.tsx`)

**Features**:
- Pre-populated form
- Account ID read-only
- Same validation as create
- Display of computed fields
- Cancel returns to detail page

**Key Differences from Create**:

1. **Data Loading**:
```typescript
useEffect(() => {
  if (params.id) {
    fetchAccount(params.id as string);
  }
}, [params.id]);

const fetchAccount = async (id: string) => {
  const data = await accountService.getAccountById(id);
  setFormData({
    acctId: data.acctId,
    acctActiveStatus: data.acctActiveStatus,
    // ... populate all fields
  });
};
```

2. **Read-only Account ID**:
```typescript
<Input
  label="Account ID"
  value={formData.acctId}
  disabled
  readOnly
/>
```

3. **Computed Fields Display**:
```typescript
<div className="bg-gray-50 shadow rounded-lg p-6">
  <h2>Computed Information (Read-only)</h2>
  <div>
    <label>Available Credit</label>
    <p>{formatCurrency(originalAccount.availableCredit)}</p>
  </div>
  {/* ... other computed fields */}
</div>
```

---

## Business Rules Implementation

### BR-001: Sequential Account Record Processing

**Backend Endpoint**: `GET /api/accounts/sequential`

**Frontend Implementation**:

1. **API Route** (`src/app/api/accounts/sequential/route.ts`):
```typescript
export async function GET(request: NextRequest) {
  const response = await forwardAuthRequest(
    '/api/accounts/sequential',
    'GET',
    request
  );
  const result = await handleAuthApiResponse(response);
  return NextResponse.json(result.data, { status: result.status });
}
```

2. **Service Method** (`src/services/accountService.ts`):
```typescript
async getAccountsSequentially(): Promise<Account[]> {
  const response = await fetch(`${API_BASE_URL}/accounts/sequential`, {
    method: 'GET',
    headers: this.getAuthHeaders(),
  });
  if (!response.ok) throw new Error('Failed to fetch accounts sequentially');
  return response.json();
}
```

3. **UI Integration** (`src/app/accounts/page.tsx`):
```typescript
const [filterType, setFilterType] = useState<'all' | 'active' | 'expired' | 'sequential'>('all');

const fetchAccounts = async () => {
  if (filterType === 'sequential') {
    const data = await accountService.getAccountsSequentially();
    setAccounts(data);
  }
  // ... other filters
};
```

### BR-002: Account Data Display Requirements

**Implementation**: All account fields are displayed in the detail page.

**Sections**:
1. Account Identification (4 fields)
2. Financial Information (5 fields)
3. Current Cycle Information (3 fields)
4. Date Information (4 fields)
5. System Information (2 fields)

**Total**: 18 fields displayed

### BR-003: Account File Access Control

**Implementation**: Authentication middleware ensures proper access control.

**Flow**:
```
Request → API Route → forwardAuthRequest() → Backend
                      (adds auth token)
```

### BR-004: End of File Detection

**Implementation**: Sequential processing handles EOF gracefully.

**Behavior**: Returns all available records without errors.

---

## Validation Rules

### Client-Side Validation

Implemented in create and edit pages:

```typescript
const validationRules = {
  acctId: {
    required: true,
    pattern: /^\d{11}$/,
    message: 'Account ID must be exactly 11 numeric digits'
  },
  acctCurrBal: {
    required: true,
    min: 0,
    message: 'Current balance cannot be negative'
  },
  acctCreditLimit: {
    required: true,
    min: 0,
    message: 'Credit limit cannot be negative'
  },
  acctExpirationDate: {
    required: true,
    afterDate: 'acctOpenDate',
    message: 'Expiration date must be after open date'
  },
  // ... more rules
};
```

### Server-Side Validation

Handled by backend API:
- Data type validation
- Business rule validation
- Database constraint validation

---

## UI Components

### Reusable Components

Located in `src/components/ui/`:

1. **Button** - Action buttons with variants
2. **Input** - Text inputs with labels and validation
3. **Select** - Dropdown selects
4. **Table** - Data tables with sorting and actions
5. **Modal** - Modal dialogs

**Usage Example**:

```typescript
import { Button, Input, Select, Table } from '@/components/ui';

// Button
<Button onClick={handleClick} variant="primary">
  Click Me
</Button>

// Input
<Input
  label="Account ID"
  value={formData.acctId}
  onChange={(e) => setFormData({ ...formData, acctId: e.target.value })}
  error={errors.acctId}
  required
/>

// Select
<Select
  label="Status"
  value={formData.acctActiveStatus}
  onChange={(e) => setFormData({ ...formData, acctActiveStatus: e.target.value })}
  options={[
    { value: 'A', label: 'Active' },
    { value: 'I', label: 'Inactive' },
  ]}
/>

// Table
<Table
  columns={columns}
  data={accounts}
  onRowClick={handleRowClick}
  actions={renderActions}
/>
```

---

## Styling

### TailwindCSS v4

**Configuration**: `src/app/globals.css`

```css
@import "tailwindcss";

/* Custom styles */
```

**Utility Classes**:

```typescript
// Layout
className="p-6 max-w-4xl mx-auto"

// Grid
className="grid grid-cols-2 gap-4"

// Flexbox
className="flex justify-between items-center"

// Colors
className="bg-white text-gray-900"

// Spacing
className="mb-6 mt-4 space-y-4"

// Typography
className="text-2xl font-bold"

// Borders
className="border-b pb-2 rounded-lg"

// Shadows
className="shadow-lg"
```

**Color Coding**:

```typescript
// Status badges
const statusClass = account.isActive
  ? 'bg-green-100 text-green-800'
  : 'bg-gray-100 text-gray-800';

// Financial indicators
const balanceClass = account.availableCredit < 0
  ? 'text-red-600 font-semibold'
  : 'text-green-600';
```

---

## Error Handling

### Patterns

1. **Try-Catch Blocks**:
```typescript
try {
  const data = await accountService.getAccounts();
  setAccounts(data);
} catch (err) {
  setError('Failed to load accounts');
  console.error(err);
}
```

2. **Error State**:
```typescript
const [error, setError] = useState<string | null>(null);

if (error) {
  return <div className="text-red-600">Error: {error}</div>;
}
```

3. **User Feedback**:
```typescript
try {
  await accountService.deleteAccount(id);
  fetchAccounts(); // Refresh list
} catch (err) {
  alert('Failed to delete account');
}
```

---

## Performance Optimization

### Strategies

1. **Pagination**: Limit data transfer
2. **Lazy Loading**: Load data on demand
3. **Memoization**: Cache computed values
4. **Debouncing**: Delay API calls
5. **Code Splitting**: Split bundles

### Implementation

```typescript
// Pagination
const [pageSize, setPageSize] = useState(20);

// Lazy loading
useEffect(() => {
  fetchAccounts();
}, [currentPage]);

// Memoization
const computedValue = useMemo(() => {
  return expensiveCalculation(data);
}, [data]);
```

---

## Testing

### Unit Tests

Test service methods:

```typescript
describe('AccountService', () => {
  it('should fetch accounts', async () => {
    const accounts = await accountService.getAccounts();
    expect(accounts).toBeDefined();
    expect(Array.isArray(accounts.content)).toBe(true);
  });
});
```

### Integration Tests

Test API routes:

```typescript
describe('GET /api/accounts', () => {
  it('should return paginated accounts', async () => {
    const response = await fetch('/api/accounts?page=0&size=20');
    expect(response.status).toBe(200);
    const data = await response.json();
    expect(data.content).toBeDefined();
  });
});
```

### E2E Tests

Test complete user flows:

```typescript
describe('Account Management', () => {
  it('should create and view account', async () => {
    // Navigate to create page
    await page.goto('/accounts/new');
    
    // Fill form
    await page.fill('[name="acctId"]', '12345678901');
    
    // Submit
    await page.click('button[type="submit"]');
    
    // Verify redirect
    expect(page.url()).toContain('/accounts');
  });
});
```

---

## Deployment

### Build Process

```bash
# Install dependencies
npm install

# Build for production
npm run build

# Start production server
npm start
```

### Environment Configuration

```env
# .env.local
NEXT_PUBLIC_API_URL=http://localhost:8080
```

### Docker Deployment

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY . .
RUN npm run build
EXPOSE 3000
CMD ["npm", "start"]
```

---

## Maintenance

### Code Quality

- Use ESLint for linting
- Use Prettier for formatting
- Use TypeScript strict mode
- Write comprehensive tests

### Monitoring

- Log all API calls
- Track error rates
- Monitor performance metrics
- Set up alerts

### Documentation

- Keep README updated
- Document API changes
- Update type definitions
- Maintain changelog

---

## Troubleshooting

### Common Issues

**Issue**: TypeScript errors
**Solution**: Run `npm run type-check`

**Issue**: Build fails
**Solution**: Clear `.next` folder and rebuild

**Issue**: API calls fail
**Solution**: Check backend API is running

**Issue**: Authentication fails
**Solution**: Verify token in localStorage

---

## Best Practices

1. **Type Safety**: Use TypeScript strictly
2. **Error Handling**: Always handle errors
3. **Loading States**: Show loading indicators
4. **Validation**: Validate on client and server
5. **Accessibility**: Use semantic HTML
6. **Performance**: Optimize bundle size
7. **Security**: Sanitize user input
8. **Testing**: Write comprehensive tests
9. **Documentation**: Keep docs updated
10. **Code Review**: Review all changes

---

## Conclusion

This implementation guide provides a complete reference for understanding and maintaining the Account Data Management System. Follow these patterns and best practices to ensure consistent, high-quality code.

For questions or clarifications, refer to the archetype documentation or contact the development team.
