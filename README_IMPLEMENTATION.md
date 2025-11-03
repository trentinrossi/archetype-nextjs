# Account and Customer Data Processing - Implementation Guide

## Overview

This is a comprehensive Next.js 15 microfrontend application for managing accounts, customers, cards, transactions, and statements. The application follows modern React patterns with TypeScript, TailwindCSS v4, and a clean architecture.

## Features Implemented

### 1. **Account Management** (`/accounts`)
- View all accounts with filtering by status and group ID
- Create, update, and delete accounts
- Display account details including balances, credit limits, and dates
- Real-time search and filtering capabilities

### 2. **Customer Management** (`/customers`)
- View all customers with search functionality
- Create, update, and delete customer records
- Display customer demographics, addresses, and FICO scores
- Masked SSN display for security

### 3. **Card Management** (`/cards`)
- View all credit cards with filtering by status
- Create, update, and delete cards
- Masked card number display for security
- Link cards to accounts

### 4. **Transaction Management** (`/transactions`)
- View all transactions with search and filtering
- Filter by transaction source (System, POS, Online, ATM)
- Display transaction details including amounts, descriptions, and timestamps
- Create new transactions

### 5. **Statement Generation** (`/statements`)
- Generate account statements for all accounts
- View statements by card number or account ID
- Display transaction summaries and totals
- Calculate interest charges

## Project Structure

```
src/
├── app/
│   ├── accounts/          # Account management pages
│   ├── cards/             # Card management pages
│   ├── customers/         # Customer management pages
│   ├── transactions/      # Transaction management pages
│   ├── statements/        # Statement generation pages
│   ├── api/               # API routes (Next.js API routes)
│   │   ├── accounts/      # Account API endpoints
│   │   ├── cards/         # Card API endpoints
│   │   ├── customers/     # Customer API endpoints
│   │   ├── transactions/  # Transaction API endpoints
│   │   ├── statements/    # Statement API endpoints
│   │   └── interest/      # Interest calculation endpoints
│   ├── page.tsx           # Home page with navigation
│   └── layout.tsx         # Root layout
├── components/
│   └── ui/                # Reusable UI components
│       ├── Button.tsx
│       ├── Input.tsx
│       ├── Modal.tsx
│       ├── Select.tsx
│       └── Table.tsx
├── services/              # API service layer
│   ├── accountService.ts
│   ├── cardService.ts
│   ├── customerService.ts
│   ├── transactionService.ts
│   └── statementService.ts
└── types/
    └── account.ts         # TypeScript type definitions
```

## API Endpoints Implemented

### Account Endpoints
- `GET /api/accounts` - Get all accounts
- `GET /api/accounts/{accountId}` - Get account by ID
- `GET /api/accounts/status/{activeStatus}` - Get accounts by status
- `GET /api/accounts/group/{groupId}` - Get accounts by group
- `POST /api/accounts` - Create new account
- `PUT /api/accounts/{accountId}` - Update account
- `DELETE /api/accounts/{accountId}` - Delete account

### Card Endpoints
- `GET /api/cards` - Get all cards
- `GET /api/cards/{cardNumber}` - Get card by number
- `GET /api/cards/account/{accountId}` - Get cards by account
- `GET /api/cards/status/{activeStatus}` - Get cards by status
- `POST /api/cards` - Create new card
- `PUT /api/cards/{cardNumber}` - Update card
- `DELETE /api/cards/{cardNumber}` - Delete card

### Customer Endpoints
- `GET /api/customers` - Get all customers
- `GET /api/customers/{customerId}` - Get customer by ID
- `GET /api/customers/lastname/{lastName}` - Get customers by last name
- `POST /api/customers` - Create new customer
- `PUT /api/customers/{customerId}` - Update customer
- `DELETE /api/customers/{customerId}` - Delete customer

### Transaction Endpoints
- `GET /api/transactions` - Get all transactions
- `GET /api/transactions/{transactionId}` - Get transaction by ID
- `GET /api/transactions/card/{cardNumber}` - Get transactions by card
- `GET /api/transactions/account/{accountId}` - Get transactions by account
- `POST /api/transactions` - Create new transaction

### Statement Endpoints
- `GET /api/statements` - Get all statements
- `GET /api/statements/card/{cardNumber}` - Get statement by card
- `GET /api/statements/account/{accountId}` - Get statements by account

### Interest Calculation
- `POST /api/interest/calculate?processingDate={date}` - Calculate monthly interest

## Key Features

### UI/UX Features
- **Responsive Design**: Works on desktop, tablet, and mobile devices
- **Loading States**: Spinner animations during data fetching
- **Empty States**: Helpful messages when no data is available
- **Error Handling**: User-friendly error messages with retry options
- **Search & Filtering**: Real-time search and multiple filter options
- **Confirmation Modals**: Delete confirmations to prevent accidental data loss
- **Currency Formatting**: Proper USD currency display
- **Date Formatting**: Human-readable date formats
- **Security**: Masked SSN and card numbers

### Technical Features
- **TypeScript**: Full type safety across the application
- **Service Layer**: Centralized API communication
- **Error Handling**: Comprehensive error handling in services and components
- **Authentication Ready**: Headers prepared for token-based auth
- **Reusable Components**: Consistent UI with shared components
- **Clean Architecture**: Separation of concerns (pages, services, types)

## Environment Variables

Create a `.env.local` file with:

```env
NEXT_PUBLIC_API_URL=http://localhost:8080
```

## Running the Application

1. Install dependencies:
```bash
npm install
```

2. Run development server:
```bash
npm run dev
```

3. Open browser:
```
http://localhost:3000
```

## Business Rules Implemented

### Account Data Processing
- Sequential reading and display of account records
- Account filtering by status and group ID
- Account balance tracking with credit limits
- Account lifecycle management (open, expiration, reissue dates)

### Customer Data Processing
- Customer demographic information management
- Address and contact information tracking
- FICO credit score monitoring
- SSN and government ID security

### Card Data Processing
- Card issuance and management
- Card-to-account linking
- Card status tracking (active/inactive)
- Expiration date management

### Transaction Processing
- Transaction recording and tracking
- Transaction categorization by type and source
- Merchant information capture
- Timestamp tracking (original and processing)

### Statement Generation
- Account statement generation with transaction summaries
- Customer and account information display
- Transaction listing with totals
- Interest calculation integration

### Interest Calculation
- Monthly interest calculation based on balances
- Interest rate lookup by account group
- Default rate fallback mechanism
- Transaction generation for interest charges

## Data Models

All data models are defined in `src/types/account.ts` and match the API specifications:

- **Account**: Account details with balances and limits
- **Card**: Card information with security details
- **Customer**: Customer demographics and credit information
- **Transaction**: Transaction details with merchant information
- **InterestCalculation**: Interest calculation results
- **AccountStatement**: Complete statement with transactions

## Security Considerations

- SSN masking (shows only last 4 digits)
- Card number masking (shows only last 4 digits)
- Authentication headers prepared for token-based auth
- Secure data transmission through API routes

## Future Enhancements

- Account detail pages with full CRUD forms
- Customer detail pages with address management
- Card detail pages with transaction history
- Transaction detail pages with merchant information
- Statement PDF generation
- Interest calculation scheduling
- Batch operations for multiple records
- Advanced filtering and sorting options
- Export functionality (CSV, Excel)
- Audit trail and activity logging

## Notes

- All monetary amounts use proper currency formatting
- Dates are displayed in human-readable formats
- The application follows the archetype patterns strictly
- No mock data is used - all data comes from API endpoints
- Error states and loading states are handled throughout
- The UI is built with existing components from `@/components/ui`
