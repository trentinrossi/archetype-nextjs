# Account Data Management System

## Overview

This is a modern web application for managing customer account information, implementing the **CBACT01C - Account Data File Reader and Printer** system. The application provides a complete interface for viewing, creating, editing, and managing customer accounts with comprehensive financial information.

## Application Details

- **Application ID**: CBACT01C
- **Description**: Account Data File Reader and Printer
- **Frontend**: Next.js 15.5.3 with React 19, TypeScript 5, TailwindCSS v4
- **Backend**: Spring Boot REST API with PostgreSQL
- **Architecture**: 7-layer architecture with complete separation of concerns

## Features Implemented

### Core Functionality

1. **Account List Page** (`/accounts`)
   - Paginated account listing with configurable page sizes (10, 20, 50, 100)
   - Multiple filter options:
     - All Accounts (paginated)
     - Active Accounts Only
     - Expired Accounts Only
     - Sequential Processing (BR-001)
   - Real-time display of account status, balances, and credit information
   - Color-coded status indicators (Active/Inactive, Expired/Valid)
   - Inline edit and delete actions for each account
   - Click-through navigation to account details

2. **Account Detail Page** (`/accounts/[id]`)
   - Complete display of all account information (BR-002)
   - Organized into logical sections:
     - Account Identification
     - Financial Information
     - Current Cycle Information
     - Date Information
     - System Information
   - Real-time computed fields:
     - Available Credit
     - Available Cash Credit
     - Current Cycle Net Amount
   - Color-coded financial indicators (positive/negative balances)
   - Quick actions: Edit, Delete, Back to List

3. **Create Account Page** (`/accounts/new`)
   - Comprehensive form with all required and optional fields
   - Organized into logical sections matching detail page
   - Client-side validation:
     - Account ID must be exactly 11 numeric digits
     - All monetary values must be non-negative
     - Expiration date must be after open date
     - Reissue date (if provided) must be after open date
     - Group ID maximum 10 characters
   - Real-time error feedback
   - Field-level help text

4. **Edit Account Page** (`/accounts/[id]/edit`)
   - Pre-populated form with existing account data
   - Account ID is read-only (cannot be changed)
   - Same validation rules as create page
   - Display of computed fields (read-only)
   - Cancel action returns to detail page

5. **Home Page** (`/`)
   - Modern landing page with system overview
   - Quick access to account management
   - Feature highlights
   - Business rules documentation
   - System information display

### Business Rules Implementation

#### BR-001: Sequential Account Record Processing
- **Endpoint**: `GET /api/accounts/sequential`
- **Implementation**: Processes account records sequentially from the account file
- **UI**: Available as a filter option in the account list page
- **Behavior**: Returns all accounts ordered by account ID in ascending order

#### BR-002: Account Data Display Requirements
- **Implementation**: All account information fields are displayed
- **Fields Displayed**:
  - Account ID
  - Active Status
  - Current Balance
  - Credit Limit
  - Cash Credit Limit
  - Open Date
  - Expiration Date
  - Reissue Date
  - Current Cycle Credit
  - Current Cycle Debit
  - Group ID
  - Computed fields (Available Credit, Available Cash Credit, Current Cycle Net Amount)
  - Status indicators (Active/Inactive, Expired/Valid, Reissued)

#### BR-003: Account File Access Control
- **Implementation**: All API routes use authentication middleware
- **Behavior**: Account file operations require proper authentication
- **Error Handling**: File access errors are properly handled and reported

#### BR-004: End of File Detection
- **Implementation**: Sequential processing detects and handles EOF conditions
- **Behavior**: Processing stops when end-of-file is reached
- **UI**: Displays all available records without errors

## File Structure

### Generated Files (13 files)

```
src/
├── types/
│   └── account.ts                          # TypeScript type definitions
├── services/
│   └── accountService.ts                   # API client service
├── app/
│   ├── page.tsx                            # Home page
│   ├── accounts/
│   │   ├── page.tsx                        # Account list page
│   │   ├── new/
│   │   │   └── page.tsx                    # Create account page
│   │   └── [id]/
│   │       ├── page.tsx                    # Account detail page
│   │       └── edit/
│   │           └── page.tsx                # Edit account page
│   └── api/
│       └── accounts/
│           ├── route.ts                    # GET (list), POST (create)
│           ├── sequential/
│           │   └── route.ts                # GET (sequential processing)
│           ├── active/
│           │   └── route.ts                # GET (active accounts)
│           ├── expired/
│           │   └── route.ts                # GET (expired accounts)
│           └── [id]/
│               ├── route.ts                # GET, PUT, DELETE by ID
│               └── exists/
│                   └── route.ts            # GET (check existence)
```

## API Integration

### Backend Endpoints Used

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/accounts` | GET | List accounts (paginated) |
| `/api/accounts` | POST | Create new account |
| `/api/accounts/{id}` | GET | Get account by ID |
| `/api/accounts/{id}` | PUT | Update account |
| `/api/accounts/{id}` | DELETE | Delete account |
| `/api/accounts/sequential` | GET | Sequential processing (BR-001) |
| `/api/accounts/active` | GET | Get active accounts |
| `/api/accounts/expired` | GET | Get expired accounts |
| `/api/accounts/{id}/exists` | GET | Check account existence |

### Request/Response Flow

```
User Action → Page Component → Service Layer → API Route → Backend API
                                                              ↓
User Interface ← Page Component ← Service Layer ← API Route ← Backend Response
```

## Data Model

### Account Entity

```typescript
interface Account {
  // Core Fields
  acctId: string;                    // 11-digit account identifier
  acctActiveStatus: 'A' | 'I';       // Active or Inactive
  acctCurrBal: number;               // Current balance
  acctCreditLimit: number;           // Maximum credit limit
  acctCashCreditLimit: number;       // Maximum cash credit limit
  acctOpenDate: string;              // Account open date (YYYY-MM-DD)
  acctExpirationDate: string;        // Account expiration date
  acctReissueDate: string | null;    // Account reissue date (optional)
  acctCurrCycCredit: number;         // Current cycle credit
  acctCurrCycDebit: number;          // Current cycle debit
  acctGroupId: string | null;        // Account group identifier (optional)
  
  // Computed Fields
  availableCredit: number;           // Credit limit - current balance
  availableCashCredit: number;       // Cash credit limit - current balance
  currentCycleNetAmount: number;     // Cycle credit - cycle debit
  isActive: boolean;                 // Active status flag
  isExpired: boolean;                // Expiration status flag
  hasBeenReissued: boolean;          // Reissue status flag
  activeStatusDisplayName: string;   // "Active" or "Inactive"
  
  // System Fields
  createdAt: string;                 // Record creation timestamp
  updatedAt: string;                 // Record update timestamp
}
```

## Validation Rules

### Account ID
- **Required**: Yes
- **Format**: Exactly 11 numeric digits
- **Pattern**: `^\d{11}$`
- **Examples**: "12345678901", "00000000001"
- **Invalid**: "123456789", "1234567890A", "123456789012"

### Active Status
- **Required**: Yes
- **Values**: 'A' (Active) or 'I' (Inactive)
- **Case-sensitive**: Yes

### Monetary Fields
- **Required**: Yes (for all balance and limit fields)
- **Minimum**: 0.0 (non-negative)
- **Precision**: 15 digits
- **Scale**: 2 decimal places
- **Fields**: 
  - Current Balance
  - Credit Limit
  - Cash Credit Limit
  - Current Cycle Credit
  - Current Cycle Debit

### Date Fields
- **Format**: YYYY-MM-DD (ISO 8601)
- **Open Date**: Required
- **Expiration Date**: Required, must be after open date
- **Reissue Date**: Optional, must be after open date if provided

### Group ID
- **Required**: No
- **Maximum Length**: 10 characters

## User Interface Features

### Modern UI Patterns

1. **Action Buttons**: Each row has Edit and Delete buttons (no selection codes)
2. **Pagination Controls**: Previous/Next buttons with page indicators
3. **Filter Dropdowns**: Select-based filtering (no function keys)
4. **Inline Validation**: Real-time error feedback on form fields
5. **Color Coding**: 
   - Green: Active, Valid, Positive balances
   - Red: Inactive, Expired, Negative balances
   - Blue: Reissued accounts
   - Gray: Neutral states

### Responsive Design
- Mobile-friendly layouts
- Adaptive grid systems
- Touch-friendly buttons
- Readable typography

### Loading States
- Loading indicators during data fetch
- Disabled buttons during save operations
- Skeleton screens for better UX

### Error Handling
- User-friendly error messages
- Field-level validation feedback
- Toast notifications for actions
- Confirmation dialogs for destructive actions

## Usage Examples

### Creating a New Account

1. Navigate to `/accounts`
2. Click "Create New Account" button
3. Fill in required fields:
   - Account ID (11 digits)
   - Active Status (A or I)
   - Current Balance
   - Credit Limit
   - Cash Credit Limit
   - Open Date
   - Expiration Date
   - Current Cycle Credit
   - Current Cycle Debit
4. Optionally fill:
   - Reissue Date
   - Group ID
5. Click "Create Account"
6. System validates and creates account
7. Redirects to account list

### Editing an Account

1. Navigate to `/accounts`
2. Click on an account row or click "Edit" button
3. On detail page, click "Edit Account"
4. Modify desired fields
5. Click "Save Changes"
6. System validates and updates account
7. Redirects to account detail page

### Filtering Accounts

1. Navigate to `/accounts`
2. Use "Filter Accounts" dropdown:
   - **All Accounts**: Paginated view with all accounts
   - **Active Accounts Only**: Shows only active accounts
   - **Expired Accounts Only**: Shows only expired accounts
   - **Sequential Processing**: BR-001 implementation
3. Results update automatically

### Viewing Account Details

1. Navigate to `/accounts`
2. Click on any account row
3. View complete account information organized by section
4. See computed fields (available credit, etc.)
5. Use action buttons: Edit, Delete, Back to List

## Technical Implementation

### Architecture Layers

1. **Types Layer** (`/src/types/account.ts`)
   - TypeScript interfaces for type safety
   - Request/response type definitions

2. **API Routes Layer** (`/src/app/api/accounts/`)
   - Next.js API route handlers
   - Forward requests to backend with authentication
   - Handle errors and return standardized responses

3. **Services Layer** (`/src/services/accountService.ts`)
   - Frontend API client
   - HTTP request handling
   - Authentication header management
   - Response parsing

4. **Pages Layer** (`/src/app/accounts/`)
   - React components for UI
   - State management
   - User interaction handling
   - Navigation

### State Management

- Local component state using React hooks
- No global state management needed
- Server state managed through API calls
- Form state managed locally

### Authentication

- Token-based authentication
- Stored in localStorage
- Included in all API requests
- Handled by auth middleware

## Development Guidelines

### Adding New Features

Follow the 4-step process:

1. **Create Types** (`/src/types/`)
2. **Create API Routes** (`/src/app/api/`)
3. **Create Service** (`/src/services/`)
4. **Create Pages** (`/src/app/`)

### Code Standards

- Use TypeScript for type safety
- Follow Next.js 15 App Router patterns
- Use existing UI components from `/src/components/ui/`
- Implement proper error handling
- Add loading states
- Validate user input client-side

### Testing Recommendations

1. **Unit Tests**: Test service methods
2. **Integration Tests**: Test API routes
3. **E2E Tests**: Test complete user flows
4. **Validation Tests**: Test all validation rules

## Deployment

### Prerequisites

- Node.js 18+ installed
- Backend API running and accessible
- Environment variables configured

### Build and Run

```bash
# Install dependencies
npm install

# Run development server
npm run dev

# Build for production
npm run build

# Start production server
npm start
```

### Environment Variables

```env
# Backend API URL (configured in auth-middleware.ts)
NEXT_PUBLIC_API_URL=http://localhost:8080
```

## Browser Support

- Chrome (latest)
- Firefox (latest)
- Safari (latest)
- Edge (latest)

## Performance Considerations

- Server-side rendering for initial page load
- Client-side navigation for subsequent pages
- Pagination to limit data transfer
- Lazy loading of components
- Optimized bundle size

## Security Features

- Authentication required for all operations
- CSRF protection
- Input validation and sanitization
- SQL injection prevention (backend)
- XSS prevention

## Accessibility

- Semantic HTML
- ARIA labels where needed
- Keyboard navigation support
- Screen reader friendly
- Color contrast compliance

## Future Enhancements

Potential improvements:

1. **Search Functionality**: Search accounts by ID, group, or status
2. **Bulk Operations**: Select and operate on multiple accounts
3. **Export Features**: Export account data to CSV/Excel
4. **Advanced Filtering**: Filter by date ranges, balance ranges
5. **Audit Trail**: Track all changes to accounts
6. **Dashboard**: Summary statistics and charts
7. **Notifications**: Email/SMS notifications for account events
8. **Batch Import**: Import accounts from file

## Support and Maintenance

### Common Issues

**Issue**: Account ID validation fails
**Solution**: Ensure ID is exactly 11 numeric digits

**Issue**: Date validation fails
**Solution**: Ensure expiration date is after open date

**Issue**: API connection fails
**Solution**: Check backend API is running and accessible

### Logging

- All API calls are logged
- Errors are logged to console
- User actions are tracked

## License

This application is part of the CBACT01C system modernization project.

## Contact

For questions or support, contact the development team.

---

**Generated**: 2024
**Version**: 1.0.0
**Status**: Production Ready
