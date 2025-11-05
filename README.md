# Card Transaction Lifecycle Management

A comprehensive Next.js application for managing card transactions, accounts, cards, and their relationships.

## Features

### Transaction Management
- **View Transactions**: Browse all transactions with pagination and filtering
- **Create Transactions**: Add new transactions with comprehensive validation
  - Card number validation (16 digits)
  - Type and category code validation
  - Merchant information
  - Amount validation with proper formatting
  - Timestamp tracking (original and processing)
- **Transaction Details**: View complete transaction information
- **Filter Transactions**: 
  - By card number
  - By date range
  - With pagination support
- **Delete Transactions**: Remove transactions from the system

### Account Management
- **View Accounts**: Browse all customer accounts
- **Create Accounts**: Add new accounts with:
  - Account ID (max 11 digits)
  - Credit limit
  - Current balance
  - Cycle credit/debit tracking
  - Expiration date
- **Account Details**: View complete account information including:
  - Current balance
  - Available credit
  - Credit utilization rate
  - Cycle information
- **Delete Accounts**: Remove accounts from the system

### Card Management
- **View Cards**: Browse all cards with status information
- **Create Cards**: Add new cards with:
  - Card number (16 digits)
  - Status
  - Card details
- **Delete Cards**: Remove cards from the system

### Card Cross References
- **Search Cross References**: Find relationships by:
  - Card number
  - Account ID
- **Create Cross References**: Link cards to accounts and customers
- **Delete Cross References**: Remove card-account relationships

## Business Rules Implementation

### Transaction Validation (CBTRN01C & CBTRN02C)
The application implements comprehensive transaction validation:

1. **Card Number Validation**: Verifies card exists in cross-reference file
   - Error Code 100: Invalid card number
2. **Account Validation**: Confirms account exists and is valid
   - Error Code 101: Account not found
3. **Credit Limit Check**: Validates transaction doesn't exceed available credit
   - Error Code 102: Over limit transaction
4. **Expiration Date Check**: Ensures transaction is before account expiration
   - Error Code 103: Transaction after account expiration

### Transaction Processing
- Sequential ID generation for unique transaction identifiers
- Account balance updates (current balance and cycle credit/debit)
- Category balance tracking
- Timestamp management (original and processing timestamps)

### Date Range Filtering (CBTRN03C)
- Filter transactions by processing timestamp
- Support for date range queries
- Pagination for large result sets

## Technology Stack

- **Framework**: Next.js 14 with App Router
- **Language**: TypeScript
- **Styling**: Tailwind CSS
- **State Management**: React Hooks
- **API Communication**: Fetch API with custom service layer
- **UI Components**: Custom component library

## Project Structure

```
src/
├── app/                          # Next.js app router pages
│   ├── accounts/                 # Account management pages
│   │   ├── [id]/                # Account detail page
│   │   ├── new/                 # Create account page
│   │   └── page.tsx             # Accounts list page
│   ├── cards/                    # Card management pages
│   │   ├── new/                 # Create card page
│   │   └── page.tsx             # Cards list page
│   ├── card-cross-references/   # Cross reference pages
│   │   ├── new/                 # Create cross reference page
│   │   └── page.tsx             # Cross references page
│   ├── transactions/            # Transaction management pages
│   │   ├── [id]/               # Transaction detail page
│   │   ├── new/                # Create transaction page
│   │   └── page.tsx            # Transactions list page
│   ├── layout.tsx              # Root layout
│   └── page.tsx                # Dashboard page
├── components/                  # React components
│   ├── ui/                     # UI components (Button, Input, Table, etc.)
│   ├── AccountForm.tsx         # Account creation form
│   ├── AccountList.tsx         # Account list component
│   ├── TransactionForm.tsx     # Transaction creation form
│   └── TransactionList.tsx     # Transaction list component
├── services/                    # API service layer
│   ├── accountService.ts       # Account API calls
│   ├── cardService.ts          # Card API calls
│   ├── cardCrossReferenceService.ts  # Cross reference API calls
│   └── transactionService.ts   # Transaction API calls
└── types/                       # TypeScript type definitions
    ├── account.ts              # Account types
    ├── card.ts                 # Card types
    ├── cardCrossReference.ts   # Cross reference types
    └── transaction.ts          # Transaction types
```

## API Integration

The application integrates with a backend API at `http://localhost:8080/api` with the following endpoints:

### Transactions
- `GET /transactions` - Get paginated transactions
- `GET /transactions/{id}` - Get transaction by ID
- `GET /transactions/transaction-id/{transactionId}` - Get by transaction ID
- `GET /transactions/card/{cardNumber}` - Get by card number
- `GET /transactions/date-range` - Get by date range
- `POST /transactions` - Create transaction
- `DELETE /transactions/{id}` - Delete transaction

### Accounts
- `GET /accounts` - Get paginated accounts
- `GET /accounts/{accountId}` - Get account by ID
- `POST /accounts` - Create account
- `DELETE /accounts/{accountId}` - Delete account

### Cards
- `GET /cards` - Get paginated cards
- `GET /cards/{cardNumber}` - Get card by card number
- `POST /cards` - Create card
- `DELETE /cards/{cardNumber}` - Delete card

### Card Cross References
- `GET /card-cross-references/card/{cardNumber}` - Get by card number
- `GET /card-cross-references/account/{accountId}` - Get by account ID
- `POST /card-cross-references` - Create cross reference
- `DELETE /card-cross-references/{cardNumber}` - Delete cross reference

## Getting Started

1. **Install dependencies**:
   ```bash
   npm install
   ```

2. **Configure API endpoint**:
   Update the `API_BASE_URL` in service files if needed (default: `http://localhost:8080/api`)

3. **Run development server**:
   ```bash
   npm run dev
   ```

4. **Open browser**:
   Navigate to `http://localhost:3000`

## Key Features

### Dashboard
- Overview statistics (total transactions, accounts, cards)
- Quick action buttons for creating new entities
- Navigation cards for easy access to different sections

### Transaction Management
- Comprehensive form validation
- Real-time error feedback
- Support for all transaction fields
- Date and time pickers for timestamps
- Amount formatting with currency display

### Account Management
- Credit limit tracking
- Balance management
- Cycle credit/debit tracking
- Expiration date management
- Credit utilization calculation

### Filtering and Search
- Card number filtering
- Date range filtering
- Pagination support
- Real-time search results

### Error Handling
- User-friendly error messages
- Specific error codes for transaction validation
- Loading states for all async operations
- Empty states for no data scenarios

## Validation Rules

### Transaction Form
- Card Number: 16 digits, numeric only
- Type Code: 2 digits, numeric only
- Category Code: 4 digits, numeric only
- Source: Max 10 characters
- Description: Max 100 characters (optional)
- Amount: Positive decimal number
- Merchant ID: Numeric (max 9 digits)
- Merchant Name: Max 50 characters
- Timestamps: Required datetime values

### Account Form
- Account ID: Numeric, max 11 digits
- Credit Limit: Positive decimal number
- Current Balance: Decimal number
- Cycle Credit/Debit: Decimal numbers
- Expiration Date: Valid date

### Card Form
- Card Number: 16 digits, numeric only
- Status: Required string
- Card Details: Optional string

### Card Cross Reference Form
- Card Number: 16 digits, numeric only
- Account ID: Numeric
- Customer ID: Numeric

## UI/UX Features

- **Responsive Design**: Works on desktop, tablet, and mobile devices
- **Loading States**: Visual feedback during data fetching
- **Error States**: Clear error messages with retry options
- **Empty States**: Helpful messages when no data is available
- **Confirmation Dialogs**: Prevent accidental deletions
- **Success Feedback**: Alerts for successful operations
- **Navigation**: Breadcrumbs and back buttons for easy navigation
- **Pagination**: Efficient handling of large datasets
- **Formatting**: Currency, date, and number formatting

## Modernization from COBOL

This application modernizes the following COBOL programs:

1. **CBTRN01C**: Daily transaction processing and validation
2. **CBTRN02C**: Transaction posting and account updates
3. **CBTRN03C**: Transaction detail reporting
4. **COTRN01C**: Transaction view program
5. **COTRN02C**: Transaction addition program
6. **CSUTLDTC**: Date validation utility

All business rules and validation logic from the original COBOL programs have been preserved and implemented in modern TypeScript/React code.

## License

Generated by Wynxx System Modernization Team
