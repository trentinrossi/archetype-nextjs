# CardDemo - Account and Customer Data Processing System

A comprehensive Next.js application for managing accounts, customers, cards, transactions, and generating statements with interest calculations.

## Features

### Core Functionality

1. **Account Management**
   - View all accounts with filtering by status and group ID
   - View detailed account information
   - Create, update, and delete accounts
   - Track account balances, credit limits, and cycle information
   - View associated cards and transactions per account

2. **Customer Management**
   - Browse all customers with search functionality
   - View detailed customer profiles
   - Create, update, and delete customer records
   - Manage customer personal information, addresses, and banking details
   - Track FICO credit scores

3. **Card Management**
   - List all cards with filtering by status
   - View card details including account associations
   - Create, update, and delete cards
   - Track card expiration dates and status

4. **Transaction Processing**
   - View all transactions with search capabilities
   - Filter transactions by card number or account ID
   - Create new transactions
   - View transaction details including merchant information
   - Track transaction types and categories

5. **Interest Calculation**
   - Calculate monthly interest for all accounts
   - Specify processing date for calculations
   - View detailed interest calculation results
   - Formula: (Balance × Interest Rate) ÷ 1200
   - Generate interest transactions automatically

6. **Statement Generation**
   - Generate account statements for all accounts
   - View statements by account ID or card number
   - Display customer information, account summary, and transaction history
   - Calculate total transaction amounts
   - Print-friendly statement views

## Technology Stack

- **Framework**: Next.js 14 with App Router
- **Language**: TypeScript
- **Styling**: Tailwind CSS
- **State Management**: React Hooks
- **API Integration**: Fetch API with custom service classes

## Project Structure

```
src/
├── app/
│   ├── accounts/
│   │   ├── [accountId]/
│   │   │   └── page.tsx          # Account detail page
│   │   └── page.tsx               # Accounts list page
│   ├── cards/
│   │   └── page.tsx               # Cards list page
│   ├── customers/
│   │   ├── [customerId]/
│   │   │   └── page.tsx          # Customer detail page
│   │   └── page.tsx               # Customers list page
│   ├── transactions/
│   │   └── page.tsx               # Transactions list page
│   ├── statements/
│   │   ├── [accountId]/
│   │   │   └── page.tsx          # Statement detail page
│   │   └── page.tsx               # Statements list page
│   ├── interest/
│   │   └── page.tsx               # Interest calculation page
│   ├── layout.tsx                 # Root layout with navigation
│   └── page.tsx                   # Dashboard/home page
├── components/
│   └── Navigation.tsx             # Main navigation component
├── services/
│   ├── accountService.ts          # Account API calls
│   ├── cardService.ts             # Card API calls
│   ├── customerService.ts         # Customer API calls
│   ├── transactionService.ts      # Transaction API calls
│   ├── interestService.ts         # Interest calculation API calls
│   └── statementService.ts        # Statement generation API calls
└── types/
    └── cardDemo.ts                # TypeScript type definitions
```

## API Endpoints

### Account Management
- `GET /api/accounts` - List all accounts
- `GET /api/accounts/{accountId}` - Get account by ID
- `GET /api/accounts/status/{activeStatus}` - Get accounts by status
- `GET /api/accounts/group/{groupId}` - Get accounts by group
- `POST /api/accounts` - Create account
- `PUT /api/accounts/{accountId}` - Update account
- `DELETE /api/accounts/{accountId}` - Delete account

### Card Management
- `GET /api/cards` - List all cards
- `GET /api/cards/{cardNumber}` - Get card by number
- `GET /api/cards/account/{accountId}` - Get cards by account
- `GET /api/cards/status/{activeStatus}` - Get cards by status
- `POST /api/cards` - Create card
- `PUT /api/cards/{cardNumber}` - Update card
- `DELETE /api/cards/{cardNumber}` - Delete card

### Customer Management
- `GET /api/customers` - List all customers
- `GET /api/customers/{customerId}` - Get customer by ID
- `GET /api/customers/lastname/{lastName}` - Get customers by last name
- `POST /api/customers` - Create customer
- `PUT /api/customers/{customerId}` - Update customer
- `DELETE /api/customers/{customerId}` - Delete customer

### Transaction Management
- `GET /api/transactions` - List all transactions
- `GET /api/transactions/{transactionId}` - Get transaction by ID
- `GET /api/transactions/card/{cardNumber}` - Get transactions by card
- `GET /api/transactions/account/{accountId}` - Get transactions by account
- `POST /api/transactions` - Create transaction

### Interest Calculation
- `POST /api/interest/calculate?processingDate={date}` - Calculate monthly interest

### Statement Generation
- `GET /api/statements` - Generate statements for all accounts
- `GET /api/statements/card/{cardNumber}` - Generate statement for card
- `GET /api/statements/account/{accountId}` - Generate statements for account

## Key Features

### Dashboard
- Overview of total accounts, customers, cards, and transactions
- Quick action buttons for creating new records
- Easy navigation to all major sections

### Data Display
- Responsive tables with sorting and filtering
- Search functionality across all list pages
- Status badges with color coding
- Currency formatting for monetary values
- Date formatting for temporal data

### User Experience
- Loading states for all async operations
- Error handling with user-friendly messages
- Empty states with helpful guidance
- Confirmation dialogs for destructive actions
- Mobile-responsive design

### Business Logic
- Interest calculation using the formula: (Balance × Interest Rate) ÷ 1200
- Automatic transaction generation for interest charges
- Statement generation with transaction summaries
- Account balance tracking with available credit calculation
- Transaction categorization and type tracking

## Data Models

### Account
- Account ID (11 characters)
- Active Status
- Current Balance
- Credit Limit
- Cash Credit Limit
- Open Date, Expiration Date, Reissue Date
- Current Cycle Credit/Debit
- Address Zip Code
- Group ID

### Customer
- Customer ID (9 characters)
- Name (First, Middle, Last)
- Address (3 lines, State, Country, ZIP)
- Phone Numbers (2)
- SSN (9 digits)
- Government Issued ID
- Date of Birth
- EFT Account ID
- Primary Cardholder Indicator
- FICO Credit Score

### Card
- Card Number (16 characters)
- Account ID
- CVV Code (3 characters)
- Embossed Name
- Expiration Date
- Active Status

### Transaction
- Transaction ID (16 characters)
- Card Number
- Account ID
- Transaction Type Code
- Transaction Category Code
- Transaction Source
- Description
- Amount
- Merchant Information (ID, Name, City, ZIP)
- Timestamps (Original, Processing)

## Getting Started

1. Install dependencies:
   ```bash
   npm install
   ```

2. Run the development server:
   ```bash
   npm run dev
   ```

3. Open [http://localhost:3000](http://localhost:3000) in your browser

## Business Rules Implementation

The application implements the following business rules from the COBOL programs:

1. **CBACT01C** - Account Data File Reader: Browse and display all account records
2. **CBACT02C** - Card Data File Reader: Browse and display all card records
3. **CBACT03C** - Card Cross Reference Reader: Link cards to accounts and customers
4. **CBACT04C** - Interest Calculator: Calculate monthly interest charges
5. **CBCUS01C** - Customer Data File Reader: Browse and display all customer records
6. **CBSTM03A** - Statement Generation: Generate comprehensive account statements

## Notes

- All monetary amounts are stored as strings to preserve precision
- Dates are stored in YYYY-MM-DD format
- The application uses client-side rendering for all pages
- Authentication context is available but not enforced in this implementation
- All API calls include proper error handling and loading states

## Future Enhancements

- Form validation for create/edit operations
- Pagination for large datasets
- Advanced filtering and sorting options
- Export functionality (PDF, CSV)
- Batch operations for multiple records
- Audit trail and history tracking
- Real-time updates with WebSockets
- Enhanced reporting and analytics
