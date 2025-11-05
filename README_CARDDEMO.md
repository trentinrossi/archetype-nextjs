# CardDemo - User and Security Administration

A modern Next.js application for user management and transaction reporting, modernized from legacy COBOL mainframe systems.

## Overview

CardDemo is a comprehensive user and security administration system that provides:

- **User Authentication**: Secure login with user ID and password
- **User Management**: Complete CRUD operations for user accounts
- **Transaction Reporting**: View and analyze transaction data
- **Role-Based Access**: Separate interfaces for administrators and regular users

## Features

### Authentication (COSGN00C)
- User login with credentials validation
- Session management
- Automatic routing based on user type (Admin/Regular)

### Admin Functions (COADM01C)
1. **User List (COUSR00C)**: View paginated list of all users with search functionality
2. **User Add (COUSR01C)**: Create new user accounts with validation
3. **User Update (COUSR02C)**: Modify existing user information
4. **User Delete (COUSR03C)**: Remove user accounts with confirmation
5. **Transaction Reports (CORPT00C)**: Generate monthly, yearly, and custom date range reports

### Regular User Functions (COMEN01C)
1. **View Transactions (COTRN00C)**: Browse personal transaction history
2. **Account Information**: View account details

## Technology Stack

- **Framework**: Next.js 14 with App Router
- **Language**: TypeScript
- **Styling**: Tailwind CSS
- **UI Components**: Custom component library
- **State Management**: React Hooks
- **API Integration**: RESTful services

## Project Structure

```
src/
├── app/                    # Next.js app router pages
│   ├── admin/             # Admin-only pages
│   │   ├── users/         # User management pages
│   │   │   ├── add/       # Add user page
│   │   │   ├── update/    # Update user page
│   │   │   └── delete/    # Delete user page
│   │   └── reports/       # Transaction reports page
│   ├── main/              # Regular user pages
│   │   ├── transactions/  # Transaction list page
│   │   └── account/       # Account info page
│   └── login/             # Login page
├── components/            # Reusable React components
│   ├── ui/               # Base UI components
│   ├── UserForm.tsx      # User form component
│   ├── UserList.tsx      # User list component
│   └── TransactionList.tsx # Transaction list component
├── services/             # API service layer
│   ├── userService.ts    # User API calls
│   └── transactionService.ts # Transaction API calls
└── types/                # TypeScript type definitions
    └── user.ts           # User and transaction types
```

## API Endpoints

### Authentication
- `POST /api/auth/login` - User login

### User Management
- `GET /api/users` - Get paginated user list
- `GET /api/users/{userId}` - Get user by ID
- `POST /api/users` - Create new user
- `PUT /api/users/{userId}` - Update user
- `DELETE /api/users/{userId}` - Delete user

### Transactions
- `GET /api/transactions` - Get paginated transactions
- `GET /api/transactions/{transactionId}` - Get transaction by ID
- `GET /api/transactions/date-range` - Get transactions by date range
- `GET /api/transactions/monthly` - Get current month transactions
- `GET /api/transactions/yearly` - Get current year transactions
- `GET /api/transactions/user/{userId}` - Get user transactions

## User Types

- **Admin (A)**: Full access to all system functions including user management
- **Regular (R)**: Access to personal transactions and account information

## Validation Rules

### User Fields
- **User ID**: Required, max 8 characters, must be unique
- **First Name**: Required, max 20 characters
- **Last Name**: Required, max 20 characters
- **Password**: Required, max 8 characters
- **User Type**: Required, must be 'A' (Admin) or 'R' (Regular)

### Transaction Fields
- **Transaction ID**: Max 16 characters
- **Transaction Date**: Valid timestamp
- **Description**: Max 26 characters
- **Amount**: Decimal with 2 decimal places

## Navigation

### Function Keys (Mainframe Legacy)
- **PF3**: Return to previous menu/Sign out
- **PF4**: Clear current screen
- **PF5**: Save/Submit changes
- **PF7**: Previous page
- **PF8**: Next page
- **PF12**: Return to admin menu

### Modern Navigation
All function keys are also available as clickable buttons in the UI.

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

## Default Users

The application expects users to be authenticated through the backend API. Contact your system administrator for credentials.

## Business Rules Implementation

This application implements the following COBOL programs:

1. **COSGN00C**: Sign-on screen with authentication
2. **COADM01C**: Administrative menu
3. **COMEN01C**: Main menu for regular users
4. **COUSR00C**: User list with pagination
5. **COUSR01C**: User addition
6. **COUSR02C**: User update
7. **COUSR03C**: User deletion
8. **CORPT00C**: Transaction report generation
9. **COTRN00C**: Transaction list display

## Security Features

- Session-based authentication
- Role-based access control
- Input validation on all forms
- Confirmation dialogs for destructive actions
- Secure password handling

## Error Handling

- Comprehensive error messages
- Field-level validation feedback
- Loading states for async operations
- Empty state handling
- Network error recovery

## Responsive Design

The application is fully responsive and works on:
- Desktop computers
- Tablets
- Mobile devices

## Browser Support

- Chrome (latest)
- Firefox (latest)
- Safari (latest)
- Edge (latest)

## License

© 2024 CardDemo. All rights reserved.

## Support

For technical support or questions, please contact your system administrator.
