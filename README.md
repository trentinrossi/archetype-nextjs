# CardDemo Application - User and Security Administration

A modern Next.js application for user and security administration, migrated from legacy COBOL/CICS mainframe system.

## Features

### Authentication
- User login with User ID and Password
- Session management with localStorage
- Role-based access control (Admin/Regular users)
- Automatic routing based on user type

### Admin Functions (Admin Users Only)
1. **User List (Security)** - View paginated list of all users
2. **User Add (Security)** - Create new user accounts
3. **User Update (Security)** - Modify existing user information
4. **User Delete (Security)** - Remove user accounts from system

### Regular User Functions
1. **View Transactions** - Browse transaction records with pagination
2. **Transaction Reports** - Generate monthly, yearly, or custom date range reports
3. **My Profile** - View and update personal information

## Technology Stack

- **Framework**: Next.js 14 with App Router
- **Language**: TypeScript
- **Styling**: Tailwind CSS
- **State Management**: React Context API
- **Authentication**: Custom auth service with localStorage

## Project Structure

```
src/
├── app/                    # Next.js app router pages
│   ├── admin/             # Admin-only pages
│   │   ├── users/         # User management
│   │   │   ├── add/       # Add user page
│   │   │   ├── [userId]/  # Dynamic user routes
│   │   │   │   ├── edit/  # Edit user page
│   │   │   │   └── delete/# Delete user page
│   │   │   └── page.tsx   # User list page
│   │   └── page.tsx       # Admin menu
│   ├── login/             # Login page
│   ├── menu/              # Main menu (regular users)
│   ├── transactions/      # Transaction pages
│   │   ├── [transactionId]/# Transaction detail
│   │   └── page.tsx       # Transaction list
│   ├── reports/           # Report generation
│   ├── profile/           # User profile
│   └── page.tsx           # Home page (redirects)
├── components/            # Reusable components
│   └── ui/               # UI components (Button, Input)
├── contexts/             # React contexts
│   └── AuthContext.tsx   # Authentication context
├── services/             # API services
│   ├── authService.ts    # Authentication service
│   ├── userService.ts    # User management service
│   └── transactionService.ts # Transaction service
└── types/                # TypeScript type definitions
    ├── auth.ts           # Auth types
    ├── user.ts           # User types
    └── transaction.ts    # Transaction types
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

## Getting Started

### Prerequisites
- Node.js 18+ 
- npm or yarn

### Installation

1. Clone the repository
```bash
git clone <repository-url>
cd carddemo-app
```

2. Install dependencies
```bash
npm install
```

3. Set up environment variables
```bash
cp .env.example .env.local
```

Edit `.env.local` and configure:
```
NEXT_PUBLIC_API_URL=http://localhost:3000/api
```

4. Run the development server
```bash
npm run dev
```

5. Open [http://localhost:3000](http://localhost:3000)

## Usage

### Login
- Navigate to `/login`
- Enter User ID and Password
- Admin users are redirected to `/admin`
- Regular users are redirected to `/menu`

### Admin Menu
Administrators can access:
- User List: View all users with pagination (10 per page)
- User Add: Create new users with validation
- User Update: Modify user information
- User Delete: Remove users with confirmation

### Main Menu (Regular Users)
Regular users can access:
- View Transactions: Browse transaction history
- Transaction Reports: Generate various reports
- My Profile: Update personal information

### Navigation
- **PF3**: Return to previous menu/screen
- **PF4**: Clear current form
- **PF5**: Save/Submit changes
- **PF7**: Previous page (pagination)
- **PF8**: Next page (pagination)
- **PF12**: Return to main menu

## Validation Rules

### User Fields
- **User ID**: Required, max 8 characters, must be unique
- **First Name**: Required, max 20 characters
- **Last Name**: Required, max 20 characters
- **Password**: Required, max 8 characters
- **User Type**: Required, 'A' (Admin) or 'R' (Regular)

### Transaction Reports
- **Monthly**: Automatically uses current month
- **Yearly**: Automatically uses current year
- **Custom**: Requires valid start and end dates (yyyy-MM-dd format)

## Error Handling

The application provides user-friendly error messages for:
- Authentication failures
- Validation errors
- Not found errors (404)
- Duplicate entries (400)
- Server errors (500)

## Security Features

- Password fields are masked
- Passwords converted to uppercase for consistency
- Role-based access control
- Session management with automatic logout
- Protected routes with authentication checks

## Modernization from Legacy System

This application is a modern replacement for the legacy COBOL/CICS CardDemo system:

### Legacy Programs Replaced
- **COSGN00C** → `/login` - Sign-on screen
- **COADM01C** → `/admin` - Administrative menu
- **COMEN01C** → `/menu` - Main menu
- **COUSR00C** → `/admin/users` - User list
- **COUSR01C** → `/admin/users/add` - User add
- **COUSR02C** → `/admin/users/[userId]/edit` - User update
- **COUSR03C** → `/admin/users/[userId]/delete` - User delete
- **COTRN00C** → `/transactions` - Transaction list
- **COTRN01C** → `/transactions/[transactionId]` - Transaction detail
- **CORPT00C** → `/reports` - Report generation

### Key Improvements
- Modern web-based UI instead of 3270 terminal
- Responsive design for mobile and desktop
- Real-time validation and feedback
- Improved user experience with visual indicators
- RESTful API architecture
- Type-safe TypeScript implementation

## Development

### Build for Production
```bash
npm run build
```

### Run Production Build
```bash
npm start
```

### Linting
```bash
npm run lint
```

## Contributing

1. Create a feature branch
2. Make your changes
3. Test thoroughly
4. Submit a pull request

## License

Copyright © 2024 CardDemo Application. All rights reserved.
