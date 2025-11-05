# Card Services Account and Payment Processing Application

## Overview
This is a comprehensive Next.js application for managing card services, accounts, customers, transactions, and bill payments. The application is built following modern frontend development practices and implements all business rules from the COBOL legacy system.

## Features Implemented

### 1. Dashboard (`/dashboard`)
- Main entry point with navigation cards
- Summary statistics for accounts, customers, cards, transactions, and pending payments
- Quick access to all major sections
- Recent activity feed

### 2. Account Management (`/accounts`)
- **List View**: Display all accounts with filtering by status (Active/Inactive)
- **Search**: Filter by Account ID or Group ID
- **Pagination**: Client-side pagination with 10 items per page
- **Detail View** (`/accounts/[accountId]`): 
  - Comprehensive account information
  - Credit utilization visualization
  - Associated cards listing
  - Recent transactions (last 10)
  - Account balance and credit limit details
- **Actions**: View, Edit, Delete accounts

### 3. Customer Management (`/customers`)
- **List View**: Display all customers with search and filter capabilities
- **Search**: Filter by last name
- **Filter**: Filter by state
- **Pagination**: Client-side pagination with 10 items per page
- **FICO Score Display**: Color-coded scores with rating badges (Exceptional, Very Good, Good, Fair, Poor)
- **Primary Cardholder Indicator**: Visual badge for primary cardholders
- **Actions**: View, Edit, Delete customers

### 4. Card Management (`/cards`)
- **List View**: Paginated card listing (7 items per page as per API spec)
- **Search Filters**: Filter by Account ID and Card Number
- **Pagination**: Server-side pagination with page numbers
- **Card Number Masking**: Display last 4 digits only (****-****-****-1234)
- **Status Indicators**: Active/Inactive and Expired badges
- **Actions**: Edit, Delete cards

### 5. Transaction Management (`/transactions`)
- **Search Options**: Search by Transaction ID or Card Number
- **Transaction Display**: Comprehensive transaction details including:
  - Transaction ID, Date/Time, Description
  - Merchant information (name, city, ZIP)
  - Category and Type badges
  - Amount with color coding (green for credits, red for debits)
- **Transaction Types**: Purchase, Cash Advance, Return, Payment, Fee, Interest
- **Card Number Masking**: Secure display of card numbers

### 6. Bill Payment Processing (`/bill-payments`)
- **Account Lookup**: Search by 11-digit Account ID
- **Balance Validation**: Ensures account has outstanding balance > 0
- **Payment Confirmation**: Two-step confirmation process with modal dialogs
- **Payment Processing**: Full bill payment (balance to $0.00)
- **Success Notification**: Display transaction ID and payment details
- **Error Handling**: Comprehensive error messages for invalid accounts or zero balances

## Technical Implementation

### Type Definitions (`src/types/cardServices.ts`)
- Complete TypeScript interfaces for all entities
- Request/Response DTOs for API operations
- Pagination types
- Validation error types

### Services (`src/services/`)
- **accountService.ts**: Account CRUD operations
- **customerService.ts**: Customer CRUD operations
- **cardService.ts**: Card CRUD operations with pagination
- **transactionService.ts**: Transaction retrieval and bill payment processing

### API Integration
All services connect to the backend API at `http://localhost:8080/api` with the following endpoints:

#### Accounts
- `GET /accounts` - Get all accounts
- `GET /accounts/{accountId}` - Get account by ID
- `GET /accounts/status/{status}` - Get accounts by status
- `POST /accounts` - Create account
- `PUT /accounts/{accountId}` - Update account
- `DELETE /accounts/{accountId}` - Delete account

#### Customers
- `GET /customers` - Get all customers
- `GET /customers/{customerId}` - Get customer by ID
- `GET /customers/lastname/{lastName}` - Get customers by last name
- `POST /customers` - Create customer
- `PUT /customers/{customerId}` - Update customer
- `DELETE /customers/{customerId}` - Delete customer

#### Cards
- `GET /cards/{cardNumber}` - Get card by number
- `GET /cards/account/{accountId}` - Get cards by account ID
- `GET /cards/list?accountId&cardNumber&page&size&sort` - Get paginated cards list
- `POST /cards` - Create card
- `PUT /cards/{cardNumber}` - Update card
- `DELETE /cards/{cardNumber}` - Delete card

#### Transactions
- `GET /transactions/{transactionId}` - Get transaction by ID
- `GET /transactions/card/{cardNumber}` - Get transactions by card number
- `POST /transactions/bill-payment` - Process bill payment

## Business Rules Implemented

### From CBTRN01C (Transaction Validation)
- Card number cross-reference validation
- Account validation process
- Transaction data structure handling
- Error handling and status management

### From CBTRN02C (Transaction Posting)
- Transaction validation process
- Account and credit limit validation
- Account expiration validation
- Transaction posting process
- Account balance updates
- Transaction category balance management

### From CBTRN03C (Transaction Reporting)
- Date range filtering
- Transaction type and category resolution
- Multi-level totaling (page, account, grand totals)
- Report formatting and pagination

### From COTRN01C (Transaction View)
- Transaction lookup and display
- Card number to account ID cross-reference
- Screen navigation and function key processing
- Field initialization and screen management

### From COTRN02C (Transaction Addition)
- Transaction ID generation
- Account/Card validation
- Transaction amount validation and formatting
- Date validation and processing
- Mandatory field validation
- User confirmation processing

### From COBIL00C (Bill Payment)
- Account lookup by ID
- Balance validation (must be > 0)
- Full bill payment processing
- Transaction generation with type code '02'
- Balance update to $0.00
- Confirmation workflow

## UI/UX Features

### Design Patterns
- Modern, clean interface with Tailwind CSS
- Responsive layout for desktop and mobile
- Consistent color scheme and typography
- Loading states with spinners
- Error states with clear messaging
- Empty states with helpful guidance

### User Experience
- Intuitive navigation with breadcrumbs
- Search and filter capabilities
- Pagination controls
- Confirmation dialogs for destructive actions
- Success/error notifications
- Form validation with inline errors
- Keyboard navigation support

### Data Presentation
- Tables with sortable columns
- Status badges (Active/Inactive, Expired)
- Color-coded amounts (credits/debits)
- Masked sensitive data (card numbers)
- Formatted currency values
- Formatted dates and timestamps
- Progress bars for credit utilization

## Security Considerations
- Card number masking (display last 4 digits only)
- Authentication headers in all API calls
- Confirmation dialogs for sensitive operations
- Input validation and sanitization
- Error messages without sensitive data exposure

## Getting Started

### Prerequisites
- Node.js 18+ 
- npm or yarn
- Backend API running on http://localhost:8080

### Installation
```bash
npm install
```

### Development
```bash
npm run dev
```

### Build
```bash
npm run build
```

### Production
```bash
npm start
```

## Project Structure
```
src/
├── app/
│   ├── dashboard/          # Dashboard page
│   ├── accounts/           # Account management pages
│   ├── customers/          # Customer management pages
│   ├── cards/              # Card management pages
│   ├── transactions/       # Transaction pages
│   └── bill-payments/      # Bill payment page
├── components/
│   └── ui/                 # Reusable UI components
├── services/               # API service classes
├── types/                  # TypeScript type definitions
└── contexts/               # React contexts (Auth)
```

## Future Enhancements
- Account creation and editing forms
- Customer creation and editing forms
- Card creation and editing forms
- Transaction detail view page
- Advanced filtering and sorting
- Export functionality (CSV, PDF)
- Batch operations
- Audit trail and history
- Real-time notifications
- Dashboard analytics and charts

## Notes
- All monetary amounts are displayed in USD format
- Dates are displayed in US format (MM/DD/YYYY)
- Phone numbers are formatted as (XXX) XXX-XXXX
- FICO scores are color-coded and categorized
- Card numbers are masked for security
- All API calls include authentication headers
- Error handling is comprehensive with user-friendly messages
