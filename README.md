# CardDemo User Management System

A modernized NextJS implementation of the COBOL User Management System, faithfully recreating the business logic and user experience of the original CICS programs.

## Overview

This application modernizes four core COBOL programs into a cohesive NextJS microfrontend:

- **COSGN00C** - User Sign-on (Authentication)
- **COUSR00C** - User List Management 
- **COUSR01C** - Add New User
- **COUSR02C** - Update User
- **COUSR03C** - Delete User

## Features

### Authentic COBOL Experience
- Terminal-style green-on-black interface
- Function key navigation (F3, F4, F5, F7, F8, F12)
- Field-level validation with cursor positioning
- COBOL-style error messages and screen flow
- Exact field length restrictions (8 chars for User ID, 20 for names, etc.)

### Business Logic Compliance
- Complete implementation of all COBOL business rules
- Proper validation sequences and error handling
- State management equivalent to COBOL COMMAREA
- Screen-to-screen navigation matching original program flow

### Modern Architecture
- NextJS 15 with App Router
- TypeScript for type safety
- RESTful API endpoints
- Responsive design with Tailwind CSS
- Component-based architecture

## Project Structure

```
src/
├── api/                    # API routes (backend)
│   ├── auth/              # Authentication endpoints
│   └── users/             # User management endpoints
├── app/                   # NextJS pages
│   ├── users/             # User management pages
│   └── layout.tsx         # Root layout
├── components/            # React components
│   ├── ui/                # Reusable UI components
│   └── user/              # User-specific components
├── hooks/                 # Custom React hooks
├── services/              # API service layer
└── types/                 # TypeScript type definitions
```

## API Endpoints

### Authentication
- `POST /api/auth/signon` - User authentication
- `POST /api/auth/exit` - Handle PF3 exit
- `POST /api/auth/invalid-key` - Handle invalid key press

### User Management
- `GET /api/users` - List users with pagination
- `GET /api/users/[userId]` - Get user by ID
- `POST /api/users` - Create new user
- `PUT /api/users/[userId]` - Update user
- `DELETE /api/users/[userId]` - Delete user
- `PATCH /api/users/[userId]/activate` - Activate user
- `PATCH /api/users/[userId]/deactivate` - Deactivate user
- `PATCH /api/users/[userId]/change-password` - Change password

## Business Rules Implementation

### COSGN00C - Sign-on Logic
- User ID and Password validation (max 8 characters each)
- Case-insensitive User ID lookup with uppercase conversion
- User type-based redirection (ADMIN → COADM01C, GENERAL → COMEN01C)
- PF3 exit with thank you message
- Invalid key handling

### COUSR00C - User List Management
- Paginated display (10 users per page)
- PF7/PF8 navigation (page up/down)
- User selection with U (Update) or D (Delete) actions
- Search from specific User ID
- Transfer to appropriate programs based on selection

### COUSR01C - Add User
- Sequential field validation (First Name → Last Name → User ID → Password → User Type)
- Duplicate User ID detection
- Success message with User ID confirmation
- PF4 clear screen functionality
- Form reset after successful addition

### COUSR02C - Update User
- User lookup with detailed display
- Change detection (only update if modifications made)
- Field-by-field validation
- PF5 update, PF3 save & exit, PF12 cancel
- "Please modify to update" message for unchanged data

### COUSR03C - Delete User
- User lookup with confirmation display
- Read-only field display
- PF5 delete confirmation
- Success message with deleted User ID
- Form reset after successful deletion

## Key Components

### SignonForm
Implements COSGN00C business logic with authentication, validation, and proper error handling.

### UserListComponent
Recreates COUSR00C functionality with pagination, selection, and navigation.

### AddUserForm
Implements COUSR01C with sequential validation and proper field management.

### UpdateUserForm
Handles COUSR02C logic including lookup, modification detection, and updates.

### DeleteUserForm
Manages COUSR03C deletion workflow with confirmation and safety checks.

### AdminMenu
Central navigation hub matching COBOL program flow.

## Function Key Mappings

- **ENTER** - Submit/Search/Confirm action
- **F3/PF3** - Exit to previous screen
- **F4/PF4** - Clear screen/Reset form
- **F5/PF5** - Update/Delete action
- **F7/PF7** - Page up (User List)
- **F8/PF8** - Page down (User List)
- **F12/PF12** - Cancel operation
- **ESC** - Alternative exit key

## Data Validation

All validation follows COBOL business rules:

- **User ID**: Required, max 8 characters, uppercase conversion
- **Password**: Required, max 8 characters
- **First Name**: Required, max 20 characters
- **Last Name**: Required, max 20 characters
- **User Type**: Required, 1 character (A=Admin, U=User)

## Error Messages

Authentic COBOL error messages:
- "Please enter User ID"
- "Please enter Password"
- "User ID can NOT be empty..."
- "First Name can NOT be empty..."
- "User ID already exist..."
- "User ID NOT found..."
- "Invalid key pressed"
- "Please modify to update..."

## Getting Started

1. **Install dependencies**:
   ```bash
   npm install
   ```

2. **Run development server**:
   ```bash
   npm run dev
   ```

3. **Access the application**:
   Open [http://localhost:3000](http://localhost:3000)

4. **Test credentials**:
   - Admin: `ADMIN001` / `ADMIN123`
   - User: `USER001` / `USER123`

## Usage Instructions

1. **Sign On**: Enter User ID and Password, press ENTER
2. **Admin Menu**: Select user management options (1-4)
3. **User List**: Use PF7/PF8 for navigation, select U/D for actions
4. **Add User**: Fill all fields, press ENTER to add
5. **Update User**: Enter User ID, press ENTER to lookup, modify fields, press PF5 to update
6. **Delete User**: Enter User ID, press ENTER to lookup, press PF5 to delete
7. **Navigation**: Use PF3 to exit screens, PF4 to clear forms

## Technical Features

- **Type Safety**: Full TypeScript implementation
- **State Management**: React hooks with COBOL-equivalent state handling
- **API Integration**: RESTful services with proper error handling
- **Responsive Design**: Works on desktop and mobile devices
- **Accessibility**: Keyboard navigation and screen reader support
- **Performance**: Optimized with NextJS features

## Modernization Benefits

- **Maintainability**: Modern codebase with clear separation of concerns
- **Scalability**: Component-based architecture for easy extension
- **User Experience**: Familiar interface for COBOL users with modern enhancements
- **Integration**: RESTful APIs for easy integration with other systems
- **Deployment**: Modern deployment options with containerization support

## Development Notes

This implementation prioritizes business logic fidelity over modern UX patterns to ensure a smooth transition for users familiar with the original COBOL system. The terminal-style interface and function key navigation preserve the original user experience while providing the benefits of modern web technology.

## License

Generated by Wynxx System Modernization Team