# Quick Start Guide - Account Management System

## 🚀 Get Started in 5 Minutes

This guide will help you get the Account Data Management System up and running quickly.

---

## Prerequisites

Before you begin, ensure you have:

- ✅ Node.js 18 or higher installed
- ✅ npm or yarn package manager
- ✅ Backend API running (Spring Boot on port 8080)
- ✅ Git (optional, for version control)

---

## Installation

### Step 1: Navigate to Project Directory

```bash
cd /tmp/archetypes/trentinrossi_archetype-nextjs_3eda6daa-33c5-42d3-a1d0-e721b04314c4
```

### Step 2: Install Dependencies

```bash
npm install
```

This will install all required packages including:
- Next.js 15.5.3
- React 19.1.0
- TypeScript 5
- TailwindCSS v4
- And all other dependencies

---

## Configuration

### Step 3: Configure Backend API URL

The backend API URL is configured in `src/lib/auth-middleware.ts`. By default, it points to:

```typescript
const BACKEND_API_URL = 'http://localhost:8080';
```

If your backend API is running on a different URL, update this value.

---

## Running the Application

### Step 4: Start Development Server

```bash
npm run dev
```

The application will start on `http://localhost:3000`

You should see:
```
✓ Ready in 2.5s
○ Local:        http://localhost:3000
```

### Step 5: Open in Browser

Open your browser and navigate to:
```
http://localhost:3000
```

You should see the home page with:
- Account Management card
- Key Features list
- Business Rules overview
- System Information

---

## First Steps

### 1. View Accounts

Click **"Manage Accounts"** or navigate to:
```
http://localhost:3000/accounts
```

You'll see:
- List of all accounts (if any exist)
- Filter options (All, Active, Expired, Sequential)
- Pagination controls
- Create New Account button

### 2. Create Your First Account

1. Click **"Create New Account"** button
2. Fill in the required fields:
   - **Account ID**: 11-digit number (e.g., `12345678901`)
   - **Active Status**: Select "Active" or "Inactive"
   - **Current Balance**: Enter amount (e.g., `1500.50`)
   - **Credit Limit**: Enter amount (e.g., `5000.00`)
   - **Cash Credit Limit**: Enter amount (e.g., `1000.00`)
   - **Open Date**: Select date
   - **Expiration Date**: Select date (must be after open date)
   - **Current Cycle Credit**: Enter amount (e.g., `250.00`)
   - **Current Cycle Debit**: Enter amount (e.g., `150.00`)
3. Optionally fill:
   - **Reissue Date**: Select date (if account was reissued)
   - **Group ID**: Enter group identifier (max 10 characters)
4. Click **"Create Account"**

### 3. View Account Details

1. From the account list, click on any account row
2. You'll see complete account information organized in sections:
   - Account Identification
   - Financial Information
   - Current Cycle Information
   - Date Information
   - System Information

### 4. Edit an Account

1. From account details page, click **"Edit Account"**
2. Modify any fields (except Account ID)
3. Click **"Save Changes"**

### 5. Delete an Account

1. From account list or detail page, click **"Delete"**
2. Confirm deletion in the dialog
3. Account will be removed

---

## Features to Try

### Filtering Accounts

Use the **"Filter Accounts"** dropdown to:

1. **All Accounts (Paginated)**: View all accounts with pagination
2. **Active Accounts Only**: View only active accounts
3. **Expired Accounts Only**: View only expired accounts
4. **Sequential Processing (BR-001)**: View accounts in sequential order

### Pagination

When viewing "All Accounts":
1. Use **"Page Size"** dropdown to change items per page (10, 20, 50, 100)
2. Use **"Previous"** and **"Next"** buttons to navigate pages
3. See current page number and total pages

### Validation

Try these to see validation in action:

1. **Invalid Account ID**:
   - Enter less than 11 digits → Error: "Account ID must be exactly 11 numeric digits"
   - Enter letters → Error: "Account ID must be exactly 11 numeric digits"

2. **Negative Balance**:
   - Enter negative number → Error: "Current balance cannot be negative"

3. **Invalid Dates**:
   - Set expiration date before open date → Error: "Expiration date must be after open date"

---

## Keyboard Shortcuts

The application uses modern UI patterns (no legacy PF keys):

- **Tab**: Navigate between form fields
- **Enter**: Submit forms
- **Escape**: Close modals (if implemented)
- **Click**: All actions use mouse clicks

---

## Troubleshooting

### Issue: Application won't start

**Solution**:
```bash
# Clear Next.js cache
rm -rf .next

# Reinstall dependencies
rm -rf node_modules
npm install

# Try again
npm run dev
```

### Issue: "Failed to fetch accounts"

**Possible causes**:
1. Backend API is not running
2. Backend API URL is incorrect
3. CORS issues

**Solution**:
1. Verify backend API is running on `http://localhost:8080`
2. Check `src/lib/auth-middleware.ts` for correct URL
3. Ensure backend allows CORS from `http://localhost:3000`

### Issue: TypeScript errors

**Solution**:
```bash
# Check for type errors
npm run type-check

# If errors persist, check generated files
```

### Issue: Styling not working

**Solution**:
```bash
# Ensure TailwindCSS is properly configured
# Check src/app/globals.css has:
# @import "tailwindcss";

# Restart dev server
npm run dev
```

---

## Project Structure Overview

```
src/
├── app/
│   ├── page.tsx                    # Home page
│   ├── accounts/                   # Account pages
│   │   ├── page.tsx               # List
│   │   ├── new/page.tsx           # Create
│   │   └── [id]/
│   │       ├── page.tsx           # Detail
│   │       └── edit/page.tsx      # Edit
│   └── api/
│       └── accounts/              # API routes
├── components/
│   └── ui/                        # Reusable components
├── services/
│   └── accountService.ts          # API client
└── types/
    └── account.ts                 # Type definitions
```

---

## Available Scripts

```bash
# Development
npm run dev          # Start dev server
npm run build        # Build for production
npm start            # Start production server

# Code Quality
npm run lint         # Run ESLint
npm run type-check   # Check TypeScript types

# Testing (if configured)
npm test             # Run tests
npm run test:watch   # Run tests in watch mode
```

---

## API Endpoints Reference

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/accounts` | GET | List accounts |
| `/accounts` | POST | Create account |
| `/accounts/:id` | GET | Get account |
| `/accounts/:id` | PUT | Update account |
| `/accounts/:id` | DELETE | Delete account |
| `/accounts/sequential` | GET | Sequential processing |
| `/accounts/active` | GET | Active accounts |
| `/accounts/expired` | GET | Expired accounts |

---

## Sample Data

### Valid Account Example

```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "A",
  "acctCurrBal": 1500.50,
  "acctCreditLimit": 5000.00,
  "acctCashCreditLimit": 1000.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2026-01-15",
  "acctReissueDate": "2024-01-15",
  "acctCurrCycCredit": 250.00,
  "acctCurrCycDebit": 150.00,
  "acctGroupId": "GRP001"
}
```

### Test Account IDs

Use these for testing:
- `00000000001` - First test account
- `00000000002` - Second test account
- `12345678901` - Sample account
- `99999999999` - Last test account

---

## Next Steps

After getting started:

1. ✅ **Explore Features**: Try all CRUD operations
2. ✅ **Read Documentation**: Check `ACCOUNTS_FEATURE_README.md`
3. ✅ **Review Code**: Check `IMPLEMENTATION_GUIDE.md`
4. ✅ **Customize**: Modify as needed for your requirements
5. ✅ **Deploy**: Follow deployment guide when ready

---

## Getting Help

### Documentation

- **User Guide**: `ACCOUNTS_FEATURE_README.md`
- **Technical Guide**: `IMPLEMENTATION_GUIDE.md`
- **Generation Summary**: `GENERATION_SUMMARY.md`
- **Archetype Guide**: `archetype.md`

### Common Questions

**Q: How do I add a new field to accounts?**
A: Update types, API routes, service, and pages. See `IMPLEMENTATION_GUIDE.md` for details.

**Q: How do I change the page size?**
A: Use the "Page Size" dropdown on the accounts list page.

**Q: How do I filter accounts?**
A: Use the "Filter Accounts" dropdown on the accounts list page.

**Q: Can I customize the UI?**
A: Yes! Modify the page components in `src/app/accounts/`. Use existing UI components from `src/components/ui/`.

---

## Production Deployment

When ready for production:

```bash
# Build optimized production bundle
npm run build

# Start production server
npm start

# Or use PM2 for process management
pm2 start npm --name "account-app" -- start
```

---

## Environment Variables

Create `.env.local` for local development:

```env
# Backend API URL
NEXT_PUBLIC_API_URL=http://localhost:8080

# Optional: Enable debug logging
NEXT_PUBLIC_DEBUG=true
```

---

## Browser DevTools

### Useful Console Commands

```javascript
// Check if service is available
console.log(accountService);

// Test API call
accountService.getAccounts().then(console.log);

// Check auth token
console.log(localStorage.getItem('access_token'));
```

---

## Tips & Tricks

### 1. Fast Navigation

Bookmark these URLs:
- Home: `http://localhost:3000`
- Accounts: `http://localhost:3000/accounts`
- Create: `http://localhost:3000/accounts/new`

### 2. Quick Testing

Use browser DevTools to:
- Inspect network requests
- Check console for errors
- Test responsive design
- Debug React components

### 3. Development Workflow

1. Make changes to code
2. Save file (auto-reload enabled)
3. Check browser for updates
4. Test functionality
5. Commit changes

---

## Success Checklist

After setup, verify:

- [ ] Application starts without errors
- [ ] Home page loads correctly
- [ ] Can view account list
- [ ] Can create new account
- [ ] Can view account details
- [ ] Can edit account
- [ ] Can delete account
- [ ] Filters work correctly
- [ ] Pagination works
- [ ] Validation shows errors
- [ ] Loading states appear
- [ ] Error messages display

---

## What's Next?

Now that you're up and running:

1. **Explore the Code**: Review generated files
2. **Test Features**: Try all functionality
3. **Customize**: Adapt to your needs
4. **Deploy**: Move to production when ready
5. **Enhance**: Add new features as needed

---

## Support

Need help? Check:

1. **Documentation**: Read the comprehensive guides
2. **Code Comments**: Review inline documentation
3. **Archetype**: Refer to `archetype.md`
4. **Console**: Check browser console for errors
5. **Logs**: Review server logs for issues

---

**Happy Coding! 🚀**

The Account Data Management System is ready to use. Start managing accounts with a modern, user-friendly interface!
